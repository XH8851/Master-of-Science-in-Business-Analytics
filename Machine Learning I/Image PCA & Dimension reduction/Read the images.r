
# Read in the data

# Note: Change Filename once the data is finalized
FileName <- "SelfieImageData-Development.csv"
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

# Take a look
ImgData[1:8,1:8]

# Free up some memory just in case
remove(DataAsChars)

# Show each Image
for(whImg in 1:nrow(ImgData)) {
  Img <- matrix(ImgData[whImg,],byrow=T,ncol=sqrt(ncol(ImgData)))
  Img <- apply(Img,2,rev)
  par(pty="s",mfrow=c(1,1))
  image(z=t(Img),col = grey.colors(255),useRaster=T)
  Sys.sleep(1)
}
