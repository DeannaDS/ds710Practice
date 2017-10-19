
#powers of 3 as a for loop
x = numeric(10)
x[1] = 3
for (i in 2:10){
  print(paste(i-1, ":", x[i-1]))
  x[i] = 3 * x[i-1]
  
}

x

#powers of 3 as a vector calculation
my.ints = 1:10
x=3^my.ints
x


#benchmarking both as a function
Powers.of.3.for.loop <- function(){
  x = numeric(10)
  x[1] = 3
  for(i in 2:10){
    x[i] = 3*x[i-1]
  }
  return(x)
}
Powers.of.3.vector <- function(){
  my.ints = 1:10
  x = 3^my.ints
  return(x)
}
microbenchmark( Powers.of.3.for.loop(), Powers.of.3.vector() )

#load ames.csv
ames <- read.csv("~/grad school/DS710/ds710practice/AmesHousing.csv")
#convert it to a matrix
amesMatrix = as.matrix(ames)
#get the number of columns
cols <- ncol(amesMatrix)
cols
#get the number of rows
nrow(amesMatrix)

#create an empty matrix for holding our answers
y = matrix( , nr = 2930, nc=82)

#pseudocode to get houses with the qualities we want
#for( each row of ames2 ){
#Check whether at least one feature is Excellent
#if so, copy that row of ames2 into the next empty row of y
#}


#create a variable to hold the number of rows of amesMatrix, using the function dim
num.rows = dim(amesMatrix)[1]

#create a variable called quality, containing the values of the columns we care about
quality <-  amesMatrix[ i, c(32,42,55) ]

quality <-  quality[!is.na(quality)]

at.least.one.Excellent <- any(quality == 'Ex')

#final code to get rows with excellent
ames2 = as.matrix(ames)
j = 1
y = matrix( , nr = 2930, nc=82 )
num.rows = dim(ames2)[1]
for(i in 1:num.rows){
  quality=ames2[ i, c(32,42,55) ]
  quality = quality[ !is.na(quality) ]
  at.least.one.Excellent = any(quality == "Ex")
  if( at.least.one.Excellent ){
    y[ j, ] = ames2[ i, ]
    j = j+1
  }
}
ames_subset = y[ 1: (j-1), ]
num.rows <- dim(ames_subset)[1]
num.rows
