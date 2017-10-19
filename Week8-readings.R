install.packages("matlab")
library(matlab)
(magic4 <- magic(4))

#sum by rows
apply(magic4, 1, sum) #1 = rows, 2 would equal columns, sum is the function to apply

#sum by columns
apply(magic4, 2, sum)

#convert rows to strings
apply(magic4, 1, toString)

#convert columans to strings
apply(magic4, 2, toString)

#set up a dataframe
(baldwins <- data.frame(
  name             = c("Alec", "Daniel", "Billy", "Stephen"),
  date_of_birth    = c(
    "1958-Apr-03", "1960-Oct-05", "1963-Feb-21", "1966-May-12"
  ),
  n_spouses        = c(2, 3, 1, 1),
  n_children       = c(1, 5, 3, 2),
  stringsAsFactors = FALSE
))


#make the rows strings
apply(baldwins, 1, toString)

#make columns strings
apply(baldwins, 2, toString)

#use sapply to find the ranges of the dataframe
sapply(baldwins, range)

#use multiple argument list supply to pass in multiple vectors and names of vectors
prime_factors <- list(
  two   = 2,
  three = 3,
  four  = c(2, 2),
  five  = 5,
  six   = c(2, 3),
  seven = 7,
  eight = c(2, 2, 2),
  nine  = c(3, 3),
  ten   = c(2, 5)
)

msg <- function(name, factors)
{
  ifelse(
    length(factors) == 1,
    paste(name, "is prime"),
    paste(name, "has factors", toString(factors))
  )
}
mapply(msg, names(prime_factors), prime_factors)

####instant vectorization####
#this function expects a scalar input
baby_gender_report <- function(gender)
{
  switch(
    gender,
    male   = "It's a boy!",
    female = "It's a girl!",
    "Um..."
  )
}

#If we pass a vector into the function, it will throw an error:
  genders <- c("male", "female", "other")
baby_gender_report(genders)

#While it is theoretically possible to do a complete rewrite of a function that is inherently vectorized, it is easier to use the Vectorize function:

vectorized_baby_gender_report <- Vectorize(baby_gender_report)
vectorized_baby_gender_report(genders)

#so basically, this applies a scalar function (single value) to each value in a vector


############
#split-apply-combine
#let's us split a dataset, apply a function and combine it again
##############
(frogger_scores <- data.frame(
  player = rep(c("Tom", "Dick", "Harry"), times = c(2, 5, 3)),
  score  = round(rlnorm(10, 8), -1)
))
frogger_scores

#calculate the mean scores for each player (split by player, apply a mean function, combine again)
with(frogger_scores, tapply(score, player, mean))
#this is the same as a group-by operation in sql - get score, grouped by player, apply mean!