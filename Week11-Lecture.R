#read in all but the first row
ames <- scan("~/grad school/DS710/ds710practice/AmesHousing.csv", 
                 what=character(), #this reads everything in as character - not what we want.
                 sep=",",
                 skip=1,
                 nlines=3000)

#make a matrix of that.
ames.matrix = t(matrix(ames, nr=82))

#read in the first row
ames.header <- scan("~/grad school/DS710/ds710practice/AmesHousing.csv", 
             what=character(), 
             sep=",",
             nlines=1)
#set the colnames
colnames(ames.matrix) <- ames.header
head(ames.matrix)

##################################
#Tracking Progress
##################################
install.packages("beepr")
library(beepr)


#this does linear regression many times
p.val = numeric(1000)
for(i in 1:1000){
  x = runif(1000)
  y = x + rnorm(1000)
  model = lm(y~x)
  p.val[i] = summary(model)$coeff[2,4]
  if(i %% 100 == 0){
    print(i)
    flush.console() #flushes the console so we can see what loop we're on
  }
}
# call alarm() when done for audible notification
alarm() #this doesn't work here, so try beep

beep()


###############################
#more tricks
##############################

#get the types of columns you have
getColTypes <-  function(fname){sapply(read.csv(fname, header=T, nrows = 1), class)} 

colTypes <- getColTypes("~/grad school/DS710/ds710practice/Cars2005.csv")


#then pass that to the scan (this still doesn't seem to work)
cars <- scan("~/grad school/DS710/ds710practice/Cars2005.csv", 
             what=list(colTypes), 
             sep=",",
             skip=1,
             nlines=3000)
cars.df <- as.data.frame(cars)
head(cars.df)
