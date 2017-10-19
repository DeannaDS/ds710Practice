install.packages("learningr")
library(learningr)
library(stringr)

#get the English Monarchs data
data("english_monarchs")
head(english_monarchs)

#look for commas in the domain - this denotes multiple kingdoms (fixed means not a regex)
multiple_kingdoms <- str_detect(english_monarchs$domain, fixed(","))

#str_detect returns a logical vector, and we can use that to pull back the name and domain columns of the rows we want
english_monarchs[multiple_kingdoms, c("name", "domain")]

#look for either a comma or the word and, using a regular expression, in the name column
multiple_rulers <- str_detect(english_monarchs$name, ",|and")
#pull back those rows
english_monarchs$name[multiple_rulers & !is.na(multiple_rulers)]

#split the names into a list of names
individual_rulers <- str_split(english_monarchs$name, ", | and ")
#pull back those rows with more than 1 ruler
head(individual_rulers[sapply(individual_rulers, length) > 1])

#count the instances of old english characters that stand for th
th <- c("th", "π", "ώ")

sapply(th, function(th) {sum(str_count(english_monarchs$name, th))})

#replace all instances with th
english_monarchs$new_name <- str_replace_all(english_monarchs$name, "[πώ]", "th")
english_monarchs$new_name

#example of using str_replace_all and regex to clean genders
gender <- c(
  "MALE", "Male", "male", "M", "FEMALE",
  "Female", "female", "f", NA
)
clean_gender <- str_replace(
  gender,
  ignore.case("^m(ale)?$"),
  "Male"
)
(clean_gender <- str_replace(
  clean_gender,
  ignore.case("^f(emale)?$"),
  "Female"
))


#############################
#Manipulating Data Frames
#############################

#add a column to the dataframe, based on a calculation of existing columns
english_monarchs$length.of.reign.years <- with(
  english_monarchs,
  end.of.reign - start.of.reign
)

#similar to above, but returns the entire dataframe
english_monarchs <- within(
  english_monarchs,
  {
    length.of.reign.years <- end.of.reign - start.of.reign
  }
)
head(english_monarchs)

#within can be used to change multiple columns at once
english_monarchs <- within(
  english_monarchs,
  {
    length.of.reign.years <- end.of.reign - start.of.reign
    reign.was.more.than.30.years <- length.of.reign.years > 30
  }
)
head(english_monarchs)

#the plyr package has a mutate function to do this, too
library(plyr)
english_monarchs <- mutate(
  english_monarchs,
  length.of.reign.years        = end.of.reign - start.of.reign,
  reign.was.more.than.30.years = length.of.reign.years > 30
)
head(english_monarchs)


######################################
#dealing with missing values
#####################################
data("deer_endocranial_volume", package = "learningr")
#complete cases just pulls back a logical vector of rows with no missing values
has_all_measurements <- complete.cases(deer_endocranial_volume)
#use the logical vector to get the columns for these rows
deer_endocranial_volume[has_all_measurements, ]

#you can use the shortcut na.omit to do the same thing
na.omit(deer_endocranial_volume)

#na.fail lets you throw an error if there is any missing data
na.fail(deer_endocranial_volume)


#####################################
#Converting between wide and long form
####################################

#wide format = data for each deer in a row, each variable in a column
deer_wide <- deer_endocranial_volume[, 1:5] #dropping the na columns
head(deer_wide)

#long form, a row for each measurement with a factor column explaining it
#use teh reshape2 package
library(reshape2)
deer_long <-  melt(deer_wide, id.vars="SkullID")
head(deer_long)

#could also do it by passing in the measure vars instead of the ID vars (useful if you have lots of ids and few measures)
melt(deer_wide, measure.vars = c("VolCT", "VolBead", "VolLWH", "VolFinarelli"))

#dcast converts back to wide, in this case alpha ordered by skullid
deer_wide_again = dcast(deer_long, SkullID ~ variable)
head(deer_wide_again)


############################################
# Using SQL
############################################
install.packages("sqldf")
library(sqldf)
#get a subset of the data with native R code
subset(
  deer_endocranial_volume,
  VolCT > 400 | VolCT2 > 400,
  c(VolCT, VolCT2)
)

#get a subset of the data, using SQL
query <-   'SELECT VolCT, VolCT2 FROM deer_endocranial_volume WHERE VolCT > 400 OR VolCT2 > 400'
sqldf(query)


######################################
#sorting
######################################

#simple example of sort function
x <- c(2, 32, 4, 16, 8)
sort(x)
sort(x, decreasing = TRUE)

#sort by alpha
sort(c("I", "shot", "the", "city", "sheriff"))

#ordering - order creates a vector of the sort order of items in a list
order(x)
#you can then use that vector to sort something
x[order(x)]

#it's the same as sorting
identical(sort(x), x[order(x)])

#useful for sorting data frames, where you can't use sort directly
year_order <- order(english_monarchs$start.of.reign)
english_monarchs[year_order, ]

#arrange from plyr package is easier:
arrange(english_monarchs, start.of.reign)


#rank gives a few ways of dealing with ties
(x <- sample(3, 7, replace = TRUE))
rank(x)
rank(x, ties.method = "first")

###########################################
#functional Programming
#########################################
#negate function accepts a predicate (function that returns a logical vector) and returns the opposite
ct2 <- deer_endocranial_volume$VolCT2  #for convenience of typing
isnt.na <- Negate(is.na)
identical(isnt.na(ct2), !is.na(ct2))

#filter takes a logical vector and an input vector and returns only those values where the function returns true
Filter(isnt.na, ct2)

#position returns the first index where the logical vector is true
Position(isnt.na, ct2)

#find returns the first VALUE where the logical vector is true
Find(isnt.na, ct2)

#map applies a function, element-wise to its inputs. It's a wrapper for mapply with simplify = FALSE

get_volume <- function(ct, bead, lwh, finarelli, ct2, bead2, lwh2)
{
  #If there is a second measurement, take the average
  if(!is.na(ct2))
  {
    ct <- (ct + ct2) / 2
    bead <- (bead + bead2) / 2
    lwh <- (lwh + lwh2) / 2
  }
  #Divide lwh by 4 to bring it in line with other measurements
  c(ct = ct, bead = bead, lwh.4 = lwh / 4, finarelli = finarelli)
}

measurements_by_deer <- with(
  deer_endocranial_volume,
  Map(
    get_volume,
    VolCT,
    VolBead,
    VolLWH,
    VolFinarelli,
    VolCT2,
    VolBead2,
    VolLWH2
  )
)
head(measurements_by_deer)


#Reduce turns a binary function into one that accepts multple inputs:
#example
Reduce("+", list(1,2,3,4))

#binary function that calculates the parallel max of 2 inputs
pmax2 <- function(x, y) ifelse(x >= y, x, y)

#use reduce to allow for many inputs (like pmax in base R)
Reduce(pmax2, measurements_by_deer)

#do not use for calculating the mean - because it repeatedly calls the function!
