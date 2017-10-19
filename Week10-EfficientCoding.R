install.packages('microbenchmark')
library(microbenchmark)
x = runif(50)

MySqrt <- function(x){
  #finds square root of vector x using a for loop
  for (value in x){
    sqrt(value)
  }
}

microbenchmark(
  sqrt(x),
  MySqrt(x)
  
)

#get a boxplot of microbenchmarking results
timings = microbenchmark(
  sqrt(x),
  MySqrt(x)
  
)

boxplot(timings, las=1)

#by default, timings using a log of y, because right skew is common. You can turn that off by passing log=F to the boxplot

#call runif without parens to get the signature, note that it's calling c code under the hood
runif

#dnorm function - gets probability density function

#write your own dnorm function
my.dnorm <- function(x, mu, sigma){
  (1/(sigma*sqrt(2*pi))) * exp(-(x-mu)^2/(2*sigma^2))
}

#timing comparison
system.time(for(i in 1:100000) dnorm(1.5, 0, 1))
system.time(for(i in 1:100000) my.dnorm(1.5, 0, 1))

timings = microbenchmark(
  dnorm(1.5, 0, 1),
  my.dnorm(1.5, 0, 1)
  
)
#compare results
boxplot(timings)

#extract rows of matrix x to determine if a row has 1 in it.

FindOnes <- function(x){
  #extracts rows of x with 1's in columns 10-12
  y = NULL
  num.rows = dim(x)[1]
  for (i in 1:num.rows){
    num.ones = length(which(x[i, 10:12] == 1))
    if(num.ones>0){
      y = rbind(y, x[i, ])
    } #end if there are ones
  }#end iteratation over rows in x
  return(y)
  
} #end function FindOnes

cars <- read.csv("~/grad school/DS710/ds710practice/cars2005.csv")

head(cars)
#use profiling
#call the garbage collection
gc()

#give the profiler a file
Rprof("FindOnes.txt")
#run your command
y = FindOnes(cars)
#stop the profiling
Rprof(NULL)

#view the summary
summaryRprof("FindOnes.txt")

#look at the top row to figure out your slowest bit of code.


# a more efficient version
FindOnes.Fast <- function(x){
  #extract rows of x with 1's in columns 10-12
  x = x
  j = 1
  num.rows = dim(x)[1]
  for (i in 1:num.rows){
    num.ones = length(which(x[i, 10:12] == 1))
    if(num.ones > 0){
      y[j,] = x[i,]
      j = j + 1
    }
  }
  return(y[1:(j-1), ])
}

#compare
timings = microbenchmark(
  FindOnes(cars),
  FindOnes.Fast(cars)
  
)
#compare results
boxplot(timings)


x <- runif(100)
microbenchmark(
  x ^ (1 / 2),
  exp(log(x) / 2)
)

#run basic math functions
microbenchmark(
  x + 5,
  x - 5,
  x * 5,
  x / 5,
  x ^ 5
  
)

#run it on integers
y = runif(100, 0, 10^12)
microbenchmark(
  y + 5,
  y - 5,
  y * 5,
  y / 5,
  y ^ 5
  
)


f <- function(x) NULL

s3 <- function(x) UseMethod("s3")
s3.integer <- f

A <- setClass("A", representation(a = "list"))
setGeneric("s4", function(x) standardGeneric("s4"))
setMethod(s4, "A", f)

B <- setRefClass("B", methods = list(rc = f))

a <- A()
b <- B$new()

microbenchmark(
  fun = f(),
  S3 = s3(1L),
  S4 = s4(a),
  RC = b$rc()
)

#variables can be reassigned willy-nilly
a <- 1
f <- function() {
  g <- function() {
    print(a)
    assign("a", 2, envir = parent.frame())
    print(a)
    a <- 3
    print(a)
  }
  g()
}
f()

#benchmarking to show how parens impact performance
f <- function(x, y) {
  (x + y) ^ 2
}

random_env <- function(parent = globalenv()) {
  letter_list <- setNames(as.list(runif(26)), LETTERS)
  list2env(letter_list, envir = new.env(parent = parent))
}
set_env <- function(f, e) {
  environment(f) <- e
  f
}
f2 <- set_env(f, random_env())
f3 <- set_env(f, random_env(environment(f2)))
f4 <- set_env(f, random_env(environment(f3)))

f3

microbenchmark(
  f(1, 2),
  f2(1, 2),
  f3(1, 2),
  f4(1, 2),
  times = 10000
)

#the number of arguments passed in to a function impacts performance
f0 <- function() NULL
f1 <- function(a = 1) NULL
f2 <- function(a = 1, b = 1) NULL
f3 <- function(a = 1, b = 2, c = 3) NULL
f4 <- function(a = 1, b = 2, c = 4, d = 4) NULL
f5 <- function(a = 1, b = 2, c = 4, d = 4, e = 5) NULL
microbenchmark(f0(), f1(), f2(), f3(), f4(), f5(), times = 10000)


#look at differences in performance on different ways to extract a value from a vector
microbenchmark(
  "[32, 11]"      = mtcars[32, 11],
  "$carb[32]"     = mtcars$carb[32],
  "[[c(11, 32)]]" = mtcars[[c(11, 32)]],
  "[[11]][32]"    = mtcars[[11]][32],
  ".subset2"      = .subset2(mtcars, 11)[32]
)

#compare some built-in functions, which are surprisingly slow
squish_ife <- function(x, a, b) {
  ifelse(x <= a, a, ifelse(x >= b, b, x))
}
squish_p <- function(x, a, b) {
  pmax(pmin(x, b), a)
}
squish_in_place <- function(x, a, b) {
  x[x <= a] <- a
  x[x >= b] <- b
  x
}

x <- runif(100, -1.5, 1.5)
microbenchmark(
  squish_ife      = squish_ife(x, -1, 1),
  squish_p        = squish_p(x, -1, 1),
  squish_in_place = squish_in_place(x, -1, 1),
  unit = "us"
)


