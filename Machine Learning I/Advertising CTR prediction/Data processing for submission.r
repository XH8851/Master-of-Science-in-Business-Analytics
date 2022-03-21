
# Read in the submission file with correct data types
Data <- read.table("ProjectSubmission-TeamX.csv",colClasses=c("character","numeric"),header=T,sep=",")

# Your code here puts the probabilities in Data[[2]]

# Round to 10 digits accuracy and prevent scientific notation.
# This converts Data[[2]] to strings.
Data[[2]] <- format(round(Data[[2]],10), scientific = FALSE)

# Write out the data in the correct format.
write.table(Data,file="ProjectSubmission-TeamX-Output.csv",quote=F,sep=",",
            row.names=F,col.names=c("id","P(click)"))
