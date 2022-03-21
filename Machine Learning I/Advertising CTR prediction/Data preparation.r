Labs <- scan(file="ProjectTrainingData.csv",what="xx",sep=",",nlines=1)
Data <- matrix(
            scan(file="ProjectTrainingData.csv",what="xx",sep=",",skip=1),
            ncol=length(Labs),byrow=T)
colnames(Data) <- Labs

# How many unique values in each variable?

apply(Data,2,FUN=function(x){length(unique(x))})

# Pareto of the number of observations for each category of C14

tmp <- sort(as.numeric(table(Data[,'C14'])),decreasing=T)
plot(tmp,ylab="Count")
title("Pareto of the number of observations
      for each category of C14")
