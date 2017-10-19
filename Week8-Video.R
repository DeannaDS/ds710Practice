#pull the manual page for apply()
?apply()

#create a matrix
x = matrix(0:5, nr=3)
x

#sum by rows
apply(x, 1, sum)

#sum by columns
apply(x, 2, sum)

#get the dimensions of your data
dim(x)


#what proportion of each column are zeros

#create a function to determine zero

prop0 <- function(x){
  # Finds the proportion of 0's in a vector
  return(length(which(x == 0))/length(x))
} #end of prop0

#apply it to our matrix
apply(x, 2, prop0)

#load the diamonds dataset
mydata <- read.table("ds710Practice/diamonds.txt", header = T)
attach(mydata)
head(mydata)

equalityTest <- function(x, alpha=.05){
  #uses a chi-squared GOF test to test whether x could be a random sample from a population in which each category is equally likely
  #alpha = significance level
  #get counts from x
  counts = table(x)
  #do the chisq test
  myTest <- chisq.test(counts)
  
  #extract the p-value 
  pval <- myTest$p.value
  
  #print the results
  if(pval < alpha) {
    print("not plausible")
  } else{
    print("plausible")
  }
} #end equalityTest


equalityTest(Clarity)
equalityTest(Clarity, .01)
