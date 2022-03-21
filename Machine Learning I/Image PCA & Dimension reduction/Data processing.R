setwd("...")

library(dplyr)
library(stats)
library(rsvd)

# Read in the data ---------

# Note: Change Filename once the data is finalized
FileName <- "SelfieImageData-Final.csv"
Labs <- scan(file=FileName,what="xx",nlines=1,sep="|")
DataAsChars <- matrix(scan(file=FileName,what="xx",sep="|",skip=1),byrow=T,ncol=length(Labs))
colnames(DataAsChars) <- Labs
dim(DataAsChars)
# size in memory in MBs
as.double(object.size(DataAsChars)/1024/1024)

ImgData <- matrix(as.integer(DataAsChars[,-1]),nrow=nrow(DataAsChars))
colnames(ImgData) <- Labs[-1]
rownames(ImgData) <- DataAsChars[,1]
# size in memory in MBs
as.double(object.size(ImgData)/1024/1024)

ImgData0=ImgData

# Take a look
ImgData[1:20,1:8]

# Free up some memory just in case
remove(DataAsChars)

# Average Face ---------

par(mfcol=c(1,1),pty="s")

ImgDataMean=colMeans(ImgData0)
Img <- matrix(ImgDataMean,byrow=T,ncol=sqrt(ncol(ImgData)))
Img <- apply(Img,2,rev)
par(pty="s",mfrow=c(1,1))
image(z=t(Img),col = grey.colors(255),useRaster=T)
title('XHAN65: Average Face')
Sys.sleep(1)

rm(ImgDataMean)
rm(Img)

# PCA --------------

ImgData=sweep(ImgData0,2,apply(ImgData0,2,mean))
#ImgData=sweep(ImgData,2,apply(ImgData,2,length),'/')


X=ImgData%*%t(ImgData)
#Sigma <- var(X)
SDecomp <- eigen(X)

n=52
values=SDecomp$values/(n-1)

ImgData1=sweep(ImgData,2,norm(t(ImgData)%*%SDecomp$vectors,type="2"),'/')
vectors=t(ImgData1)%*%SDecomp$vectors
vectors=sweep(vectors, 2, sqrt(colSums(vectors**2)),'/')

sum(cumsum(values)/sum(values) < 0.85) + 1


# Scree plot ------------------

par(mfcol=c(1,1),pty="s")
plot(values, ylab="Eigenvalues",xlab='Component Number',xlim=c(1,30))
title('XHAN65: Scree Plot')


# Your (reconstructed) Face using 20 dimensions --------------

PCompTrain20d <- ImgData%*%vectors[,1:20]
dim(PCompTrain20d)
ReconTrain20d <- PCompTrain20d%*%t(vectors[,1:20])

par(mfcol=c(1,1),pty="s")
Img <- matrix(ReconTrain20d['Xiao Han ',],byrow=T,ncol=sqrt(ncol(ImgData)))
Img <- apply(Img,2,rev)
par(pty="s",mfrow=c(1,1))
image(z=t(Img),col = grey.colors(255),useRaster=T)
title('XHAN65: My Face 20D')
Sys.sleep(1)


# Eigenface for eigenvector 8 --------------------------------------

PCompTrain8d <- ImgData%*%vectors[,8]
dim(PCompTrain8d)
ReconTrain8d <- PCompTrain8d%*%t(vectors[,8])

par(mfcol=c(1,1),pty="s")
vec=vectors[,8]*255
Img <- matrix(vec,byrow=T,ncol=sqrt(ncol(ImgData)))
Img <- apply(Img,2,rev)
par(pty="s",mfrow=c(1,1))
image(z=t(Img),col = grey.colors(255),useRaster=T)
title('XHAN65: Eigenface 8')
Sys.sleep(1)


# Wearing Glasses -------------------------

for (i in c(1:20)){
PCompTrain8d <- ImgData%*%vectors[,i]

vec=vectors[,i]*255
Img <- matrix(vec,byrow=T,ncol=sqrt(ncol(ImgData)))
Img <- apply(Img,2,rev)
par(pty="s",mfrow=c(1,1))
image(z=t(Img),col = grey.colors(255),useRaster=T)
Sys.sleep(1)
}

par(mfcol=c(4,5),pty="s")
for(i in c(1:20)){
  PCompTrainid <- ImgData%*%vectors[,i]
  dim(PCompTrainid)
  ReconTrainid <- PCompTrainid%*%t(vectors[,i])
  
  Img <- matrix(ReconTrainid['emma zhang ',],byrow=T,ncol=sqrt(ncol(ImgData)))
  Img <- apply(Img,2,rev)
  par(pty="s",mfrow=c(1,1))
  image(z=t(Img),col = grey.colors(255),useRaster=T)
  Sys.sleep(1)
}

for (i in c(1:20)){
  PCompTrain8d <- ImgData%*%vectors[,i]
    
  vec=vectors[,i]*255
  Img <- matrix(vec,byrow=T,ncol=sqrt(ncol(ImgData)))
  Img <- apply(Img,2,rev)
  par(pty="s",mfrow=c(1,1))
  image(z=t(Img),col = grey.colors(255),useRaster=T)
  Sys.sleep(1)
}

values[0]
