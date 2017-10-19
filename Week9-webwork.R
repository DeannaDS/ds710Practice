setwd("C:/Users/Deanna/Documents/grad school/DS710/ds710Practice")
boom <- read.csv("babyboom.csv")
head(boom)
summary(boom$Sex)



#My approach
clean_sex <- function(inWord){
  outWord = NA
  if (inWord == 'boy'| inWord == 'Male' | inWord == 'M'){
    outWord =  'M'
  }
  if (inWord == 'female'| inWord == 'girl'| inWord == 'F'){
    outWord = 'F'
  }
  return (outWord)
  
}

cleanedSex <- sapply(boom$Sex, clean_sex)
boom

#Webworks answer
SexToFM <- function(x){
  y <- rep.int(NA, length(x))
  y[ x == "M" ] <- "M"
  y[ x == "Male" ] <- "M"
  y[ x == "boy" ] <- "M"
  y[ x == "F" ] <- "F"
  y[ x == "female" ] <- "F"
  y[ x == "girl" ] <- "F"
  y
} # end of SexToFM function

cleanedSex <- SexToFM(boom$Sex)
boom

#another approach
SexToFM2 <- function(x){
  female = c("F", "female", "girl")
  male = c('M', 'Male', 'boy')
  y <- rep.int(NA, length(x))
  y[ x %in% male ] <- "M"
  y[ x %in% female ] <- "F"
  y
} # end of SexToFM function

cleanedSex <- SexToFM2(boom$Sex)
boom

#bind the raw and cleaned together
cleaned_sex_values <- SexToFM2(boom$Sex)
cbind(as.character(boom$Sex),cleaned_sex_values)

#update teh dataframe
boom$Sex <- cleaned_sex_values

#check out the time variable
summary(boom$Time)
typeof(boom$Time)

#this doesn't work
Time2 = as.numeric(boom$Time)
Time2

#this is how to convert without loss of info, other than converting non numbers to NA
Time3 <- as.numeric(levels(boom$Time))[boom$Time]
Time3

cbind(as.character(boom$Time),Time3)
boom$Time = Time3

#look at unrealistic numbers for time
summary(boom$Time)
tooLate = boom[which(boom$Time > 2459), ]
boom$Time[which(boom$Time > 2459)] = NA

#find times with decimals
decTimes <- boom[which(   ( trunc(boom$Time) == boom$Time ) == F   ), ]
decTimes

#find minutes more than 59
minPlus <- boom[which(boom$Time %% 100 > 59),]
minPlus

#set it to NA
boom$Time[which(boom$Time %% 100 > 59)] = NA
boom$Time[14]

summary(boom$Weight)
lowWeight <- boom[which(boom$Weight < 1000), ]
lowWeight

#set these values to NA?
boom$Weight[which(boom$Weight < 1000)] = NA

#look for NA's in Time
summary(boom$Time)
which( is.na( boom$Time ) )

Time3 = boom$Time[which (!is.na(boom$Time))]
Time3
sd(Time3)

sd(boom$Time, na.rm=T)


mydata2 = data.frame(boom$Time,boom$Sex,boom$Weight)
mydata2

boom
mydata3 <- mydata2[complete.cases(mydata2),]
mydata3

#this gets the actual row count, as opposed to looking at the index
dim(mydata3)

#create a column to indicate if the baby was born in the morning - my solution
myData3 <- within(mydata3, born.in.morning <- mydata3$boom.Time < 1200)
myData3

#webworks solution
mydata3$morning<-with(mydata3, Time < 1200)


sort(myData3$boom.Time)

timeOrder = order(myData3$boom.Time)
myData3[timeOrder,]
