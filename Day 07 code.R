wkdr<-getwd()
wkdr
#tell R where to look for files
filelocation<-paste(wkdr,"/day 07/",sep="")
#list the files in this location
files<-list.files(filelocation)
files
#"data.1.csv" "data.2.csv" "data.3.csv" "data.4.csv" "data.5.csv" "data.6.csv"
#make an empty vector
slope<-rep(NA,length(files))
length(grep("DNA.",list.files(figures)))
slope
file.rename(data)
#NA NA NA NA NA NA
#make a loop
for(i in 1:length(files)) {
  d<-read.csv(paste(filelocation,files[i],sep=""))
  m<-lm(d$waterlevel~d$DNA.concentration)
  slope[i]<-m$coefficients[2]
}
m$coefficients
#(Intercept) d$DNA.concentration 
#-45.030234            6.160328 
slope
#1.98588320  0.97949616  0.37602833  0.61066898 -0.01240866  6.16032773
dir.create(paste(filelocation,"/figure/",sep=""))
figures<-paste(wkdr,"/figures/",sep="")

pdf(paste(figures,"slope.hist.pdf",sep=""))
hist(slope,breaks=seq(from=(-1),to=7,1),col="red")
dev.off()

dir.create("analyses")

for(i in 1:length(files)) {
  d<-read.csv(paste(filelocation,files[i],sep=""))
  pdf(paste(figures,"DNA vs H2O-level",i,".pdf",sep=""))
  plot(d$waterlevel,d$DNA.concentration)
  dev.off()
}
slopes<-data.frame(cbind(files,slope))
outputfolder<-paste(wkdr,"/analyses/",sep="")
write.csv(slopes,file=paste(outputfolder,"data slopes2.csv",sep=""),row.names = FALSE)

d7c<-paste(wkdr,"/day 07c/",sep="")

name<-list.files(d7c)

trudat<-name[grep(".car.",list.files(d7c))]

slop<-rep(NA,length(trudat))

for(i in 1:length(trudat)){
  e<-read.csv(paste(d7c,trudat[i],sep=""))
  n<-lm(e$waterlevel~e$DNA.concentration)
  slop[i]<-n$coefficients[2]
}


