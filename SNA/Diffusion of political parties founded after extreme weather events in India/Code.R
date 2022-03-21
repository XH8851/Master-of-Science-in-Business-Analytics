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
library(cluster)
library(tidyverse) 
library(factoextra)
library(lubridate)
library(PearsonDS)
library(plm)
library(pglm)
library(hhi)

setwd('...')



# Load the Data ----------

data1 = fread("election_results.csv", header = TRUE, encoding="Latin-1")
data2 = fread("border_information.csv", header = TRUE, encoding="Latin-1")
data3 = fread("monthly_rainfall.csv", header = TRUE, encoding="Latin-1")



# Question 1 ----------

## Load the data ---------

rain1 = data3

# The year in rain dataset has decimals. It seems that they are the 12 months of one year, so I use floor() to process the data.

rain1$time = floor(rain1$time)


## Calculate the pearson distribution's parameters ----------

# I sum up rainfall in one year as the total rainfall (annual rainfall) in one year and one district.

rain_year = sqldf("select district, time, sum(rainfall) as rainfall from rain1 group by district, time")

rain_year = data.table(rain_year)

# Calculate the yearly rainfall of each district
#rain_year = rain_year[,aveRain := ave(rainfall), by = list(district)]

# Calculate x bar for each district.
# Previously I thought we should calculate x bar based on district level.
# rain_year = rain_year[,xBar := mean(rainfall), by = list(district)]
# Now I know we should use the mean across the districts.

rain_year = rain_year[,xBar := mean(rainfall)]
rain_year = na.omit(rain_year)

c = 0

# Calculate sd for each district
rain_year = rain_year[,sd := sd(rainfall)]
rain_year = na.omit(rain_year)

# Calculate pnorm(rainfall) for each district
rain_year = rain_year[,pnormRainfall := pnorm(rainfall, xBar, sd)]
rain_year = na.omit(rain_year)

# Calculate pnorm(x bar) for each district

#rain_year = rain_year[,pnormXBar := mean(pnormRainfall), by = list(district)]

rain_year = rain_year[,pnormXBar := mean(pnormRainfall)]
rain_year = na.omit(rain_year)

# Calculate pnorm(sd) for each district
rain_year = rain_year[,pnormsd := sd(pnormRainfall)]
rain_year = na.omit(rain_year)

# Calculate scale for each district
rain_year = rain_year[,scale := pnormsd/(pnormXBar-c)]
rain_year = na.omit(rain_year)

# Calculate shape for each district
rain_year = rain_year[,shape := ((pnormXBar-c)**2) / pnormsd]
rain_year = na.omit(rain_year)

# Calculate the SPI of each row

rain_year$pearson = qpearsonIII(p = rain_year$pnormRainfall, shape = rain_year$shape, location = 0, scale = rain_year$scale)

#rain_year = rain_year[,pearson := qpearsonIII(p = pnormRainfall, shape = shape, location = 0, scale = scale), by = list(district)]
rain_year = na.omit(rain_year)

# Calculate the mean pearson for each district
rain_year = rain_year[,meanPearson := mean(pearson), by = list(district)]
rain_year = na.omit(rain_year)

# Calculate the sd of pearson for each district
rain_year = rain_year[,sdPearson := sd(pearson), by = list(district)]
rain_year = na.omit(rain_year)

# Calculate the SPI for each district
rain_year = rain_year[,SPI := (pearson - meanPearson) / sdPearson]
rain_year = na.omit(rain_year)


## Founded parties ----------

electionTime = sort(unique(data1$year))
election1 = data.table(data1)

# find the earliest election a party in a district participated in, to get their founded year.

founded = sqldf("select district, party_name as party_name, min(year) as year from election1 group by district, party_name")
founded = na.omit(founded)

# set "new" to be 1 if party was founded in this year
founded$new = 1
founded = data.table(founded)

newfounded = election1[founded, on = c("district", "party_name", "year"), found := i.new]

setorderv(newfounded, c('district', 'party_name','year'), order = 1)


## Question 1.A ----------


### Election-period level rainfall within a district ----------

df1aSPI = rain_year[,c("district", "time", "SPI")]

# Dataframe df1aFound stores the information: is this party founded in this district this year?

df1aFound = sqldf("select distinct party_name, district, year, found from newfounded")

df1aFound = na.omit(df1aFound)


# Calculate how many parties are founded in each election year in each region, stored in dataframe df1aRain. Firstly we consider the 1st election.

thisElection = electionTime[1]
previousElection = 0
df1aRain = sqldf(paste("select district, avg(SPI) as averageSPI from df1aSPI where time >= ",previousElection, " and time < ", thisElection, " group by district", sep = ""))
df1aRain$electionYear = thisElection


# Then we consider other elections.

for (i in c(2:length(electionTime))){
  
  previousElection = electionTime[i-1]
  thisElection = electionTime[i]
  df1aRainSub = sqldf(paste("select district, avg(SPI) as averageSPI from df1aSPI where time >= ",previousElection, " and time < ", thisElection, " group by district", sep = ""))
  df1aRainSub$electionYear = thisElection
  
  df1aRain = rbind(df1aRain, df1aRainSub)
}


# Dataframe df1aFound1 includes the foundation information excluding average SPI.

df1aFound1 = sqldf("select district, year, count(found) as foundedPartyNumber from df1aFound group by year,district")


# Dataframe df1aFound2 includes the foundation information and average SPI on period level.

df1aFound2 = sqldf("select df1aFound1.*, df1aRain.averageSPI as averageSPI from df1aFound1, df1aRain where df1aFound1.district = df1aRain.district and df1aFound1.year = df1aRain.electionYear")


### Draw the scatter plot of election-period level average SPI vs. the number of parties founded in a district ----------

ggplot(df1aFound2, aes(x = averageSPI, y = foundedPartyNumber)) + 
  geom_point() + 
  labs(x = "election-period level average SPI", y = "the number of parties founded in a district")



## Question 1.B ----------


### (1) Find a district’s current level SPI and its lagged value ----------

# First, we consider the first and second election.

i = 2

previousElection = electionTime[i-1]
thisElection = electionTime[i]


# Select the SPI in 2nd election as "currentSPI", the SPI in 1st election as "previousSPI", and put them together by joining district.

df1b = sqldf(paste("select a.district as district, a.year as year, a.averageSPI as currentSPI, b.averageSPI as previousSPI from df1aFound2 a, df1aFound2 b where a.district = b.district and a.year = ",thisElection, " and b.year = ", previousElection, sep = ""))


# Use a for-loop repeat the process for the 3nd, 4th...election and the previous election.

for (i in c(3:length(electionTime))){
  
  previousElection = electionTime[i-1]
  thisElection = electionTime[i]
  
  df1bSub = sqldf(paste("select a.district as district, a.year as year, a.averageSPI as currentSPI, b.averageSPI as previousSPI from df1aFound2 a, df1aFound2 b where a.district = b.district and a.year = ",thisElection, " and b.year = ", previousElection, sep = ""))
  
  df1b = rbind(df1b, df1bSub)
}


### (2) Find a district’s current level SPI and the lagged value of its neighbors’ rainfall variable ----------

borderGraph = graph.data.frame(data.table(data2), directed = FALSE)

for (i in c(1:as.numeric(count(df1b)))){
  
  name = df1b[i,1]
  year = df1b[i,2]
  
  
  # Some districts are not in the border information, so we skip them.
  
  if(!(name %in% V(borderGraph)$name)) next
  
  
  # Get the list of neighbors of a district: the neighbors of the district in the network created based on border information. I ignore the difference between focal district and district, making the network undirected.
  
  neighbor = rownames(as.data.frame(unlist(ego(borderGraph,node = name))))[-1]
  
  
  # Calculate the average SPI of the neighbors in last period.
  
  df1b[i,5] = mean(df1b[(df1b$district %in% neighbor) & (df1b$year == year),4], na.rm = TRUE)
  
}


### Regression ----------

df1b = data.table(df1b)

names(df1b)[5] = "neighborPreviousSPI"

summary(plm(currentSPI ~ previousSPI + neighborPreviousSPI,
            df1b, effect = "twoways", model = "within", index = "district"))


### Answer 1.B ------------

# The coefficients of previous SPI and neighbors' previous SPI are 0.274379 and -0.110190, with p-value < 0.001 and p-value = 0.11. 

# It means that SPI is not independent from (1) its lagged value but independent from (2) the lagged value of its neighbors’ rainfall variable. 

# (1) has a positive impact on it. It means that high previous SPI stimulated more parties to appear.



##
## Question 1.C ----------


### Election-period level extreme conditions within a district ----------


# Calculate how many time moderate droughts and floods happened in each election period in each region, stored in dataframe df1cExtreme. 

df1cSPI = df1aSPI
df1cSPI$moderateDrought = as.numeric(df1cSPI$SPI < -1)
df1cSPI$moderateFlood = as.numeric(df1cSPI$SPI > 1)
df1cSPI$extreme = df1cSPI$moderateDrought + df1cSPI$moderateFlood


# Firstly we consider the 1st election.

thisElection = electionTime[1]
previousElection = 0

df1cExtreme = sqldf(paste("select district, count(*) as extremeNumber from df1cSPI where extreme = 1 and time >= ",previousElection, " and time < ", thisElection, " group by district", sep = ""))

df1cExtreme$electionYear = thisElection


# Then we consider other elections.

for (i in c(2:length(electionTime))){
  
  previousElection = electionTime[i-1]
  thisElection = electionTime[i]
  df1cExtremeSub = sqldf(paste("select district, count(*) as extremeNumber from df1cSPI where extreme = 1 and time >= ",previousElection, " and time < ", thisElection, " group by district", sep = ""))

  df1cExtremeSub$electionYear = thisElection
  
  df1cExtreme = rbind(df1cExtreme, df1cExtremeSub)
  
  #print(unique(df1cExtreme$electionYear))
}

names(df1cExtreme)[3] =  'year'


# Dataframe df1c includes the information of extreme conditions.

df1c = left_join(df1aFound2[,c(1:2)], df1cExtreme, by = c('district', 'year'))
df1c[is.na(df1c$extremeNumber),"extremeNumber"] = 0


### (1) Find a district’s current extreme conditions and its lagged value --------

# First, we consider the first and second election.

i = 2

previousElection = electionTime[i-1]
thisElection = electionTime[i]


# Select the extreme in 2nd election as "currentExtreme", the SPI in 1st election as "previousExtreme", and put them together by joining district.

df1cReg = sqldf(paste("select a.district as district, a.year as year, a.extremeNumber as currentExtreme, b.extremeNumber as previousExtreme from df1c a, df1c b where a.district = b.district and a.year = ",thisElection, " and b.year = ", previousElection, sep = ""))


# Use a for-loop repeat the process for the 3nd, 4th...election and the previous election.

for (i in c(3:length(electionTime))){
  
  previousElection = electionTime[i-1]
  thisElection = electionTime[i]
  
  df1cRegSub = sqldf(paste("select a.district as district, a.year as year, a.extremeNumber as currentExtreme, b.extremeNumber as previousExtreme from df1c a, df1c b where a.district = b.district and a.year = ",thisElection, " and b.year = ", previousElection, sep = ""))
  
  df1cReg = rbind(df1cReg, df1cRegSub)
}



### (2) Find a district’s current level SPI and the lagged value of its neighbors’ rainfall variable ----------

for (i in c(1:as.numeric(count(df1cReg)))){
  
  name = df1cReg[i,1]
  year = df1cReg[i,2]
  
  
  # Some districts are not in the border information, so we skip them.
  
  if(!(name %in% V(borderGraph)$name)) next
  
  
  # Get the list of neighbors of a district: the neighbors of the district in the network created based on border information. I ignore the difference between focal district and district, making the network undirected.
  
  neighbor = rownames(as.data.frame(unlist(ego(borderGraph,node = name))))[-1]
  
  
  # Calculate the average SPI of the neighbors in last period.
  
  df1cReg[i,5] = mean(df1cReg[(df1cReg$district %in% neighbor) & (df1cReg$year == year),4], na.rm = TRUE)
  
}


### Regression ----------

df1cReg = data.table(df1cReg)

names(df1cReg)[5] = "neighborPreviousExtreme"

summary(pglm(currentExtreme~ previousExtreme + neighborPreviousExtreme,
             df1cReg, effect = "twoways", model = "within", index = "district",
             family = "poisson"))



### Answer 1.C ------------

# The coefficients of previous extreme conditions (either moderate drought or flood) and neighbors' previous extreme conditions are -0.03205 and -0.05104, with p-value > 0.3. 

# It means that the extreme conditions is independent from (1) its lagged value and (2) the lagged value of its neighbors’ rainfall variable. 



# Question 2  ----------

## Process the data ----------

# We can obtain the list of party types: liberal, far left, ethnic regional, economic farming, religious, far right, economic nonfarming. I excluded the blank.

partyIssue = unique(election1$party_issue)
partyIssue[which(partyIssue == "")] = NA
partyIssue = na.omit(partyIssue)


## Question 2.A ----------

founded2 = founded


### Control in the regression for the number of years in the election period -------

# Calculate the interval between each election.

electionTime2 = c(min(sort(unique(rain1$time))),electionTime)
electionInterval = data.frame(electionTime = electionTime, interval = electionTime - electionTime2[1:length(electionTime)])
names(electionInterval)[1] = "year"


### Regression on each party issue ----------

for (i in partyIssue){
  
  
  # Calculate how many parties are founded in each election year in each region, stored in dataframe df1aRain. Firstly we consider the 1st election.
  
  df2aFound = sqldf(paste("select distinct party_name, district, year, found from newfounded where party_issue = \"", i, "\"", sep = ""))
  
  df2aFound1 = sqldf("select district, year, count(found) as foundedPartyNumber from df2aFound group by year,district")
  
  
  # Dataframe df2a includes the foundation information on period level.
  
  df2a = left_join(df1aFound2[,c(1:2)], df2aFound1, by = c('district', 'year'))
  df2a[is.na(df2a$foundedPartyNumber),]$foundedPartyNumber = 0
  
  
  # Dataframe df2aReg includes the foundation information on period level and the extreme conditions.
  
  df2aReg = left_join(df1c, df2a, by = c('district', 'year'))
  
  
  # Now df2aReg includes election interval information.
  
  df2aReg = left_join(df2aReg, electionInterval, by = 'year')
  
  
  # Print with format.
  
  print(paste("The regression predicting the number of new political parties of PARTY ISSUE: ", i))
  
  print(summary(pglm(foundedPartyNumber ~ extremeNumber + interval + year,
                     df2aReg, effect = "twoways", model = "within", index = "district",
                     family = "poisson")))
  
  cat('\n')
  
}


### Answer 2.A ---------

# From the result of regression, only for ethnic regional, economic_farming, and economic-nonfarming parties, the number of new-founded parties is statistically significantly correlated with extreme weather. 

# The coefficent and p-value:
# ethnic regional: -0.271370, p = 0.00151 < 0.002
# economic_farming: 0.352965, p = 0.01157 < 0.02
# economic-nonfarming: -0.46832, p = 0.056105 < 0.06

# Extreme number will have a positive impact on the foundation of economic_farming, and a negative impact on the ethnic regional and economic-nonfarming parties. 

# The foundation of liberal, religious, far left and far right parties is independent from the extreme weather.

# It makes perfect sense that extreme weather affects the foundation of economic farming and non economic farming especially. Extreme weather might stimulate more economic farming parties to appear and weaken the power of non economic farming parties to found, because people tend to be more aware of the environmental problem in extreme weather.



## Question 2.B ---------

# Note: Calculate the number of years its neighboring districts experience years of droughts or flooding in the period before the last


for (j in partyIssue){
  
  # Calculate how many parties are founded in each election year in each region, stored in dataframe df1aRain. Firstly we consider the 1st election.
  
  df2aFound = sqldf(paste("select distinct party_name, district, year, found from newfounded where party_issue = \"", j, "\"", sep = ""))
    
  #newfounded[newfounded$party_issue == j, c("state", "district", "year", "found", "party_name")]
  
  df2aFound1 = sqldf("select district, year, count(found) as foundedPartyNumber from df2aFound group by year,district")
  
  
  # Dataframe df2a includes the foundation information on period level.
  
  df2a = left_join(df1aFound2[,c(1:2)], df2aFound1, by = c('district', 'year'))
  df2a[is.na(df2a$foundedPartyNumber),]$foundedPartyNumber = 0
  
  
  # Dataframe df2aReg includes the foundation information on period level and the extreme conditions.
  
  df2aReg = left_join(df1c, df2a, by = c('district', 'year'))
  
  
  # Now df2aReg includes election interval information.
  
  df2aReg = left_join(df2aReg, electionInterval, by = 'year') 
  
  df2bReg = df2aReg
  
  
  # Calculate neighbor information
  
  for (i in c(1:as.numeric(count(df2bReg)))){
    
    name = df2bReg[i,1]
    year = df2bReg[i,2]
    
    # Make this row the period before the next and change the year to the next election year accordingly
    
    year = electionTime[which(electionTime == year) + 1]
    df2bReg[i,2] = year
    
    if(is.na(year)) next
    
    
    # Some districts are not in the border information, so we skip them.
    
    if(!(name %in% V(borderGraph)$name)) next
    
    
    # Get the list of neighbors of a district: the neighbors of the district in the network created based on border information. I ignore the difference between focal district and district, making the network undirected.
    
    neighbor = rownames(as.data.frame(unlist(ego(borderGraph,node = name))))[-1]
    
    
    # Calculate the average SPI of the neighbors in last period.
    
    df2bReg[i,6] = mean(df2bReg[(df2bReg$district %in% neighbor) & (df2bReg$year == year),3], na.rm = TRUE)
    
  }
  
  names(df2bReg)[6] = 'neighborExtreme'
  
  # Print with format.
  
  print(paste("The regression predicting the number of new political parties of PARTY ISSUE: ", j))
  
  print(summary(pglm(foundedPartyNumber ~ extremeNumber + neighborExtreme + interval + year,
                     df2bReg, effect = "twoways", model = "within", index = "district",
                     family = "poisson")))
  
  cat('\n')
  
  
  
}



### Answer 2B ----------

# The foundation of parties is statistically significantly corelated with extreme conditions of neighbors in types including liberal, far left, far right, economic-farming, and economic-nonfarming.

# The extreme conditions of neighbors have a positive impact on foundation in the types of far left, economic-farming, and a negative impact on liberal, far right, economic-nonfarming.

# Extreme weather in neighbors will stimulate the foundation of far left and economic-farming parties in this region, maybe because people are more aware of environmental problem. On the contrary, it inhibits the foundation of far right and economic-nonfarming parties, and liberal parties.



# Question 3 ------------

## Calculate the votes share ------------

# First get the number of votes of one party in one district in one year.

df3votesParty = sqldf("select district, year, party_name, sum(vote_count) as vote_party from newfounded group by district, year, party_name")


# Then get the total numebr of votes in one district in one year.

df3votesAnnual = sqldf("select district, year, sum(vote_count) as vote_annual from newfounded group by district, year")


# Merge the table.

df3votesParty = sqldf("select a.*, b.vote_annual from df3votesParty a, df3votesAnnual b where a.district = b.district and a.year = b.year")


# Calculate the vote share.

df3Reg = data.frame(district = character(), year = integer(), hhi = double())

for (i in electionTime){

  df3votesParty$vote_Share = 100 * df3votesParty$vote_party / df3votesParty$vote_annual
  
  df3 = sqldf(paste("select district, year, party_name, vote_Share from df3votesParty where year = ", i, sep = ""))
  
  df3 = na.omit(df3)
  
  df3hhiGeneral = as.data.frame(df3[1,c(1:2)])
  df3hhiGeneral$hhi = 0
  
  for (k in unique(df3$district)){
    
    df3hhi = sqldf(paste("select * from df3 where district = \"", k, "\"", sep = ""))
    #df3hhi = df3hhi[,c(3:4)]
    vote_Share = df3hhi$vote_Share
    
    df3hhiGeneral = rbind(df3hhiGeneral, data.frame(district = k, year = i, hhi = hhi(df3hhi, "vote_Share")))

  }
  
  df3hhiGeneral = df3hhiGeneral[-1,]

  df3Reg = rbind(df3Reg, df3hhiGeneral)

}

df3Reg = sqldf("select a.*, b.hhi from df2bReg a, df3Reg b where a.district = b.district and a.year = b.year")
  


## Regression ----------

summary(pglm(hhi ~ extremeNumber + neighborExtreme + interval + year,
             df3Reg, effect = "twoways", model = "within", index = "district",
             family = "poisson"))



## Answer 3 ---------

#                   Estimate Std. error t value  Pr(> t)   
# extremeNumber   -1.073e-02  4.583e-04  -23.422  < 2e-16 ***
# neighborExtreme -2.858e-03  6.438e-04   -4.438 9.07e-06 ***
# interval        -9.292e-03  3.953e-04  -23.504  < 2e-16 ***
# year            -4.342e-03  4.001e-05 -108.536  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Extreme weather in neighbors in the last election period has a negative impact on political concentration. It is reasonable that the weather situation in neighbors in the last election period would "erode the stability of political systems and wear away at the entrenched power" during current election.


# Question 4 ------------

# Dataframe election4 stores the election information

election4 = election1[election1$party_name != "",]
election4 = sqldf("select distinct district, year, party_name, found from election4")

founded4 = na.omit(election4)


## This is a function to find if the party is in the previous selection in neighbors ---------

findParty <- function(x, c1, c2, c3){
  
  district = x[c1]
  year = x[c2]
  name = x[c3]
  
  if(!(district %in% V(borderGraph)$name)) return(2)
  else{
    
    # Get the list of neighbors.
    
    neighbor = rownames(as.data.frame(unlist(ego(borderGraph,node = district))))[-1]
    
    # Filter the election data: find the parties in the previous election in neighbor districts.
    
    neighborPrevious = election4[(election4$district %in% neighbor) & election4$year < year,]
    
    # Note: the question only asks whether the party appeared previously, so here I use binary variable to describe if it appeared or not, instead of counting.
    
    return(as.integer(name %in% neighborPrevious$party_name))
    
  }
}


## Merge the tables which store (not) appear information and the table in Question 3---------

# Dataframe founded4 stores the information of "appeared or not" of each party in each district in each year.

founded4$appear = apply(founded4, 1, findParty, c1 = "district", c2 = "year", c3 = "party_name")


# Dataframe df4Appear stores the founded parties that appeared before.

df4Appear = sqldf("select district, year, sum(appear) as appearNum from founded4 group by district, year")


# Dataframe df4NotAppear stores the founded parties that never appeared in neighbors before.

df4NotAppear = sqldf("select district, year, sum(appear-1) as notAppearNum from founded4 group by district, year")
df4NotAppear$notAppearNum = abs(df4NotAppear$notAppearNum)


# Join the table for regression in question 3, df4Appear, and df4NotAppear.

df3Reg = data.table(df3Reg)
setkey(df3Reg, district, year)

df4Appear = data.table(df4Appear)
setkey(df4Appear, district, year)

df4NotAppear = data.table(df4NotAppear)
setkey(df4NotAppear, district, year)

df4Reg = df4NotAppear[df4Appear[df3Reg]]

df4Reg[is.na(df4Reg$appearNum),]$appearNum = 0
df4Reg[is.na(df4Reg$notAppearNum),]$notAppearNum = 0


## Regression --------

# Parties appreared before

summary(pglm(appearNum ~ extremeNumber + neighborExtreme + interval + year,
             df4Reg, effect = "twoways", model = "within", index = "district",
             family = "poisson"))

# Parties not appreared before

summary(pglm(notAppearNum ~ extremeNumber + neighborExtreme + interval + year,
             df4Reg, effect = "twoways", model = "within", index = "district",
             family = "poisson"))


## Answer 4 --------

# Regression (1)

#                 Estimate Std. error t value Pr(> t)
# extremeNumber    0.070022   0.050574   1.385   0.166
# neighborExtreme -0.105052   0.072810  -1.443   0.149
# interval        -0.044950   0.042391  -1.060   0.289
# year             0.002929   0.004412   0.664   0.507

# All coefficients, in the regression of likelihood of (1) new political parties being founded in a district, that have contested an election in a neighboring district in any previous election period, are not statistically significant with p-value > 0.1. 


# Regression (2)

#                 Estimate Std. error t value Pr(> t)
# extremeNumber   0.076876   0.022641   3.395 0.000685 ***
# neighborExtreme 0.070561   0.032430   2.176 0.029572 *  
# interval        0.136454   0.018487   7.381 1.57e-13 ***
# year            0.023974   0.001943  12.336  < 2e-16 ***

# All coefficients, in the regression of likelihood of (2) new political parties being founded in a district that have not contested an election in a neighboring district in any previous election period., are statistically significant. 

# Therefore, we can conclude that it the process of political organization is diffusing, not the content of a specific political party (whether the new parties that appear in a district are the same parties that have appeared in neighboring districts in the past).

# We can also find out that the extreme weather in current election period, extreme weather in the neighbors both have a positive impact on the foundation of the parties that never appear anywhere before.
