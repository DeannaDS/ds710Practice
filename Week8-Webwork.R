x = matrix( c(1,3,2, 6,NA,4), nr = 3 )
x
apply(x, 2, max)

#apply with removing NA
apply(x, 2, max, na.rm=T)


#get the min by row
apply(x, 1, min, na.rm=T)

?sort()

#sort by columns
apply(x,2,sort)

?tapply()


ames <- read.csv("ds710Practice/AmesHousing.csv")
head(ames$Land.Slope)

#get median sales price by land.slope
tapply(ames$SalePrice, ames$Land.Slope, median)

#subset the 45-54 columns
subAmes <- ames[, 45:54] 

#create a function for pearson skew, allow additional functions to be passed in with ...
getPearsonSkew <- function(x, ...){
  #returns a skewness coeffecient for a vector x
  #positive numbers = right skew
  #negative numbers = left skew
  return(3*(mean(x, ...) - median(x, ...)) / sd(x, ...))
  
}

#set up vector y
y = c( 1, 1, 2, 10 )

#test the pearson skew function
getPearsonSkew(y, na.rm=T)

apply(subAmes, 2, getPearsonSkew, na.rm=T)

#note that you reference columns positionally, and need to look at column types to see if you need to do as.numeric()
houseScore<-function(x){
  score = as.numeric(x[48])/1000
  if(as.numeric(x[82]) < 200000){ score = score + 5 }
  else if(as.numeric(x[82]) < 300000){ score = score + 3 }
  if(x[14] == "NoRidge"){ score = score + 3 }
  if(x[14] == "NridgHt"){ score = score + 2 }
  return(score)
} # end of function HouseScore

scores = apply(ames, 1, houseScore)

which.max(scores)
