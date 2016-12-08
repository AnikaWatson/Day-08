#Hi folks, please feel free to comment or make changes!
#what did you do differently?
#Where do I have typos?
#etc.
#Thanks and enjoy :)
#
#first let's figure out what the working directory is
wkdr<-getwd()

#now we can tell R where to look for the data files
filelocation<-paste(wkdr,"/day 07/",sep="")

#then let's list the files in this location
files<-list.files(filelocation)
files
#"data.1.csv" "data.2.csv" "data.3.csv" "data.4.csv" "data.5.csv" "data.6.csv"

#now we will need to make an empty vector the same length as the number of files in the "day 07" location
slope<-rep(NA,length(files))
slope
#NA NA NA NA NA NA

#we're ready to make a loop which will save the slope of a linear model of each set of data in the empty vector "slope"
#here "i" is the counter going from one to the length of "files"
for(i in 1:length(files)) {
  #with each new "i" the loop will read a new file in the "files" path and assign this new data frame to the object "d"
  d<-read.csv(paste(filelocation,files[i],sep=""))
  #now the loop will run a linear model on the components of "d"
  m<-lm(d$waterlevel~d$DNA.concentration)
  #and save the slope information from the linear model to the position in the slope vector that corrosponds to the valie of the counter
  slope[i]<-m$coefficients[2]
}

#why m$coefficients and why [2]?
#what is m$coeficients?
m$coefficients #note that this c=gices the slope of the last ouput of the loop (data.6.csv) only!
#(Intercept) d$DNA.concentration 
#-45.030234            6.160328 
#not sure about this but i think the first column gives the intercept and the other column gives the slope such that you can use this information to construct a line in y=mx+b form
#according to this assumption we want the second value of m, which I am taking to be the slope

#what exactly is this slope thing anyway?
slope
#1.98588320  0.97949616  0.37602833  0.61066898 -0.01240866  6.16032773

#now let's make an output folder for our analyses
dir.create("outputs")
#we can define an object as the path to this folder to make things easier in the future
outputfolder<-paste(wkdr,"/outputs/",sep="")

#I want to visualize this data so let's make a figures folder within the outputs folder to store the figures we plot
#first we create the folder
dir.create(paste(outputfolder,"/figures/",sep=""))
#then name the path
figures<-paste(outputfolder,"/figures/",sep="")

#I wonder if the slope follow a normal distribution, let's see
#we can make a pdf and save it in our new folder
pdf(paste(figures,"slope.hist.pdf",sep=""))
#down here we define the plot
hist(slope,breaks=seq(from=(-1),to=7,1),col="red")
#and here we tell R that we are done with this "pdf" command
dev.off()

#now let's get out the big guns and make a plot of the DNA found versus the water level of each dataset
#in a loop of course
#lazy
#this loop plots each graph and saves it in the "figures" folder we just created
for(i in 1:length(files)) {
  d<-read.csv(paste(filelocation,files[i],sep=""))
  pdf(paste(figures,"DNA vs H2O-level",i,".pdf",sep=""))
  plot(d$waterlevel,d$DNA.concentration)
  dev.off()
}

#now we need to write a csv file with comparing each dataset to it's corrosponding slope value
#to do so we may begin by column binding the file names to the slope values and then making this into a data frame
#let's call this data frame "slopes"
slopes<-data.frame(cbind(files,slope))

#now let's write a csv file of this data frame and save it in the output folder
write.csv(slopes,file=paste(outputfolder,"data slopes2.csv",sep=""),row.names = FALSE)
#the "rownames=FALSE" business merely prevents R from adding an extra column of rownames (which were not very exciting, merely 1:6)

#this part was just for fun, I copied the folder "day 07" and added the word "car" randomly to the middle of three of the values
#then I decided that only the datasets containing the word "car" in their title were worthy of calculation and decided to only compute the slopes of these values
#now to make R do the dirty work!
#first I'll define an object "d7c" as the path to the copied "day 07" folder
d7c<-paste(wkdr,"/day 07c/",sep="")

#now I will define an object "name" as the names of the files in this folder
name<-list.files(d7c)
name
# "data.1car.csv" "data.2.csv"    "data.3.csv"    "data.4car.csv" "data.5.csv"    "data.car6.csv"
#here you see how I have messed with the original filenames

#now I will define the "trudat" (for true data, not what you're thinking!) object as the location in the list of files within the folder where the "worthy" data resides
trudat<-name[grep(".car.",list.files(d7c))]
  #let's dissect this:
  #first I find the locations within the folder of the "good" data
  grep(".car.",list.files(d7c))
  #1 4 6
  #then I define an object as onlt the components of the folder that I am interested in
  trudat<-name[grep(".car.",list.files(d7c))]
  trudat 
  # "data.1car.csv" "data.4car.csv" "data.car6.csv"

#now I'll make a new empty slope vector with a slightly different name than before
slop<-rep(NA,length(trudat))
#NA NA NA

#now I'll run a loop like before using trudat as my input because it has only the "true" data
for(i in 1:length(trudat)){
  e<-read.csv(paste(d7c,trudat[i],sep=""))
  n<-lm(e$waterlevel~e$DNA.concentration)
  slop[i]<-n$coefficients[2]
}
#let's see what "slop is
slop
#1.985883 0.610669 6.160328
#does this match the corrosponding values of slope?
slope
#1.98588320  0.97949616  0.37602833  0.61066898 -0.01240866  6.16032773
#sure does!