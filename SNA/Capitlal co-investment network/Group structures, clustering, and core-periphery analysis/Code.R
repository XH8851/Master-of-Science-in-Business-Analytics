library(igraph)
library(data.table)
library(reshape)
library(tidyr)
library(reshape2)
library(expm)
library(plyr)
library(readxl)
library(purrr)
library(sqldf)
library(ggplot2)
library(zoo)
library(ITNr)
library(dplyr)
library(sna)
library(cluster)
library(tidyverse) 
library(factoextra)
library(lubridate)
setwd('...')



# Load the Data ----------

data1 = fread("Funding_events_7.14.csv", header = TRUE, encoding="Latin-1")
data2 = read_excel("Funding_events_7.14_page2.xlsx")
outcome = read.csv("Venture_capital_firm_outcomes.csv", header = TRUE)

data1$`Deal Date` = mdy(data1$`Deal Date`)
data2$`Deal Date` = as.Date(data2$`Deal Date`, format = "%Y-%m-%d")

all_edges1 = data1[,c("Investors", "Deal Date")]
all_edges2 = data2[,c("Investors", "Deal Date")]

all_edges = rbind(all_edges1,all_edges2)



# Create the network ----------

myFUN<- function(x,c1,c2) {
  edge = unlist(strsplit(x[c1], split = ","))
  if(length(edge) >= 2){
    df = t(combn(edge,2))
    df = as.data.frame(df)
    df$date = x[c2]
    return(as.data.frame(df))
  }
}

edges = apply(all_edges, 1, myFUN, c1 = "Investors", c2 = "Deal Date")
edges = map(edges, as.data.frame)
edges = bind_rows(edges)



#1 Kevin Bacon Hollywood Actor exercise --------


## Get renewal windows --------

edges$date = as.Date(edges$date, format = "%Y-%m-%d")
newDate = filter(edges, ((!V1 %in% c(' Inc.',' Ltd.',' Co.',' LLC.',' Corp.')) & (!V2 %in% c(' Inc.',' Ltd.',' Co.',' LLC.',' Corp.'))))

newDate$date = as.Date(newDate$date, format = "%Y-%m-%d")
newDate = sqldf("select * from newDate order by V1, V2, date")

dt = data.table(V1 = newDate$V1, V2 = newDate$V2, date = newDate$date)
dif = dt[ , diff := date - shift(date), by = list(V1,V2)]
threshold = as.numeric(quantile(dif$diff, 0.9, na.rm = TRUE))


## Remove decayed ties --------

#newDate$date = as.Date(newDate$date, format = "%Y-%m-%d")
maxDate = max(newDate$date)
ties1 = sqldf("select V1, V2, max(date) as latest from newDate group by V1, V2")
ties1$window = as.numeric(maxDate) - ties1$latest
ties1 = ties1[ties1$window < threshold,]
network1 = graph.data.frame(ties1, directed = FALSE)



## Fine the center --------

closeness_v = igraph::closeness(network1)
names(which(closeness_v == max(closeness_v)))

x = as.data.frame(closeness_v)

#sqldf("select * from x order by x desc limit 5")


## Answer 1 --------

# The center is Kleiner Perkins Caufield & Byers. Additionally, the 2nd center is New Enterprise Associates, and the 3rd center is Intel Capital.



#2 Local density of the network increase over time --------


## Load the data --------

maxDate = max(newDate$date)
minDate = min(newDate$date)

ties2 = newDate
names(ties2) = c("V1","V2","yearMonth")
ties2$yearMonth = as.Date(ties2$yearMonth)
ties2$month = (as.yearmon(ties2$yearMonth)-as.yearmon(minDate))*12


## Calculate the coreness over time --------

# Note that in question 1, we have simplified the graph, so that the ties would be fewer and this would make the coreness smaller than that when we do not simplify. 

df2 = data.frame(aveCor = -1, month = -1)

for(i in c(min(ties2$month):max(ties2$month))){
  
  # ties2ud: ties in question 2 which updates monthly
  
  ties2ud = ties2[ties2$month <= i,]
  
  # Trim the ties as we did in question 1 by using the same threshold. ties2ud$window is the renewal time window. 
  
  ties2ud = sqldf("select V1, V2, max(yearMonth) as latest from ties2ud group by V1, V2")
  ties2ud$window = as.numeric(max(ties2ud$latest)) - ties2ud$latest
  ties2ud = ties2ud[ties2ud$window < threshold,]
 
  # Store the average coreness of the network
  
  network = graph.data.frame(ties2ud, directed = FALSE)
  df2 = rbind(df2, data.frame(aveCor = mean(coreness(network)), month = i))
  
}
  
df2 = df2[-1,]


## Answer 2: the plot --------

ggplot(df2,
       aes(x = month, y = aveCor)) +
  geom_line() +
  geom_point() + 
  labs(x = "months", y = "Average coreness") + 
  scale_y_continuous(breaks=seq(0, 8, 1), limits=c(0, 8))



#3 Global core-periphery structure --------

# ties3$year is the year of the tie, because the plot will be annual.

ties3 = ties2
ties3$year = year(ties3$yearMonth)


##3.A Co-investment network's concentration --------

par(mfrow = c(5, 7))

for(i in c(min(ties3$year):max(ties3$year))){
  
  ties3ud = ties3[ties3$year <= i,]
  
  # tiesud: ties in question 3 which updates annually, but are still trimmed by the rule we set up in question 1 and 2.
  
  ties3ud = sqldf("select V1, V2, max(yearMonth) as latest from ties3ud group by V1, V2")
  ties3ud$window = as.numeric(max(ties3ud$latest)) - ties3ud$latest
  ties3ud = ties3ud[ties3ud$window < threshold,]
  
  network3 = graph.data.frame(ties3ud, directed = FALSE)
  ev = sort(igraph::evcent(network3)$vector, decreasing = TRUE)

  if(min(ev) == max(ev)) next
  
  # Calculate the co-investment network's concentration scores
  
  df3 = data.frame(parti = integer(), concent = double())
  for(j in c(1:(length(ev)-1))){
    df3 = rbind(df3, data.frame(parti = j, concent = abs(cor(ev, c(rep(1,j),rep(0,length(ev)-j))))))
  }
  
  df3 = na.omit(df3)
  
  # Illustrate a figure that shows the range of concentration scores
  
  p = ggplot(df3,
             aes(x = parti, y = concent)) +
    geom_point() + 
    geom_line() + 
    labs(x = "Partition", y = "Concentration score") + 
    ggtitle(i)
  
  print(p)
  
}


## Answer 3.A --------

# The network appears to conform to a core-periphery structure after 1992. In ties before 1992, the partition with highest concentration score is close to either the max or the min number of nodes, mostly because the number of nodes is small. After 1992, the partition number is moderate and the concentration score is over 0.8, indicating that the network would conform to a core-periphery structure.



#3.B Alternative explanation --------

# df3: shows visually through development of well-defined core in the network over time: a graph which shows best partition for each month

df3 = data.frame(month = integer(), para = integer())

# df32: shows the percentage of number of nodes in the best partition. For example, if the number of nodes in the network is 200, and the best partition is 50, the percentage will be 50/200 = 25%. 

df32 = data.frame(month = integer(), para_percentage = double())

for(i in c(min(ties3$month):max(ties3$month))){
  
  ties3ud = ties3[ties3$month <= i,]
  
  ties3ud = sqldf("select V1, V2, max(yearMonth) as latest from ties3ud group by V1, V2")
  ties3ud$window = as.numeric(max(ties3ud$latest)) - ties3ud$latest
  ties3ud = ties3ud[ties3ud$window < threshold,]
  
  network3 = graph.data.frame(ties3ud, directed = FALSE)
  
  ev = sort(igraph::evcent(network3, scale = TRUE, weights = NULL)$vector,
            decreasing = TRUE)
  
  if(min(ev) == max(ev)) next
  
  # para: the best partition; concent: the corresponding best concentration score
  
  para = 1
  concent = 0
  
  # store the best partition and highest concentration score in this month
  
  for(j in c(1:(length(ev)-1))){
    if(abs(cor(ev, c(rep(1,j),rep(0,length(ev)-j)))) > concent) 
    {para = j
    concent = abs(cor(ev, c(rep(1,j),rep(0,length(ev)-j))))}
  }
  
  df3 = rbind(df3, data.frame(month = i, para = para))
  df32 = rbind(df32, data.frame(month = i, para_percentage = (para/(length(ev)*1.0))))
  
}

ggplot(df3, aes(x = month, y = para)) +
  geom_point() +
  labs(x = "Month", y = "Best p") 

ggplot(df32, aes(x = month, y = para_percentage)) +
  geom_point() +
  labs(x = "Month", y = "Best p (percentage of the number of nodes)") 


##Answer 3.B --------

# The descriptive evidence is to show visually through development of well-defined core in the network. 

# I assume that when p < 10 and the percentage of nodes in the core is less than 0.01 or more than 0.5, it means that this is not a good core-periphery structure. 

# P, the number of nodes for a best-fitting core, is consistently low which shows only a few companies are in the core, but it becomes moderate (100<p<300) when month > 200, when the companies are more than 1000. After 150, 0.02 < percentage < 0.2. P is still moderate.



#4 Partitioning around medoids --------

ties4 = edges[edges$date < '1996-07-01',]
network4 = graph.data.frame(ties4, directed = FALSE)

sil_width <- c(NA)
for(k in 2:15){
  
  pam4 <- pam(dist(as_adjacency_matrix(network4)),k = k)
  sil_width[k] <- pam4$silinfo$avg.width
  
}


## Plot sihouette width (higher is better) --------

plot(1:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:15, sil_width)


## greater than 0.7 --------


which(sil_width > 0.7)


## greater than 0.5 --------

which(sil_width > 0.5)


## Answer 4

# No clustering solutions achieves an average silhouette width greater than 0.7 (or even the weaker threshold of 0.5). 



#5 Extra Point --------


#5.A Successful Investments --------

# Method: I collected and merged the annual data. In df51, every row means how many successful investment made by one company, its degree centrality, closeness centrality, betweenness centrality, eigenvector centrality and PageRank centrality.

df51 = data.frame(company = character(), suc = integer(), deg = double(),clo = double(), bet = double(), eig = double(), pag = double(), year = integer())

ties51 = newDate
ties51$date = year(ties51$date)

year = sort(unique(outcome$year))

for(i in year){
  
  network = graph.data.frame(ties51[ties51$date <= i,], directed = FALSE)
  success = outcome[outcome$year == i,c(1,3)]
  names(success) = c("company", "suc")
  
  df511 = data.frame(company = names(igraph::closeness(network)),
                     degree = igraph::degree(network),
                     closeness = igraph::closeness(network),
                     betweenness = igraph::betweenness(network),
                     eigenvector = igraph::eigen_centrality(network)$vector,
                     pagerank = igraph::page_rank(network)$vector)
  
  df511 = sqldf("select success.*, df511.degree as deg, df511.closeness as clo, df511.betweenness as bet, df511.eigenvector as eig, df511.pagerank as pag from success, df511 where success.company = df511.company")
  
  df511$year = i
  
  df51 = rbind(df51, df511)
  
}

names(df51) = c("company", "success", "degree", "closeness", "betweenness", "eigenvector", "pagerank", "year")

# Normalize the x variable.

normalization <- function(x){(x-min(x))/(max(x)-min(x))}
df51[,c(3:7)] = normalization(df51[,c(3:7)])

# Use poisson() because y is a count variable.

summary(glm(success ~ .,data = df51[,c("success", "degree", "closeness", "betweenness", "eigenvector", "pagerank")], family = poisson()))


## Answer 5.A ---------

# glm returns the coefficients and p value. In the summary, all centralities are statistically significant. The coefficients of degree and PageRank are positive, and others are negative. 


#5.B Going out of Business --------

outofBusiness = sqldf("select company, year, out_of_business from success where out_of_business = 1")

# To do the survival regression analysis, I would use the model like this:

# Target variables: year (how many years the company is in business, integer), out_of_business (whether the company has been out of business, binary)

# If out_of_business = 1, "year" is the real number of years when the company is in business. Otherwise, it means the company is still in business in this year, but the exact in-business time is longer than "year" and cannot be observed in this year.

# X variables: the five centralities.

# The function will be:

# surverg(formula = Surv(dataframe$year, dataframe$out_of_business) ~ 1 + dataframe$degree + dataframe$closeness + dataframe$betweenness + dataframe$eigenvector + dataframe$pagerank)

