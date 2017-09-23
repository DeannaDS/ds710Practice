#test for proportions
#In a survey of 100 people, 61 support raising the minimum wage:
prop.test(61, 100, p=.5, alternative = "greater")

cars <- read.csv("ds710practice/Cars2005.csv", header=TRUE)

#do a test test on the price
t.test(cars$Price, mu=24721.86, alternative="less")

#do a 2 sample t test
t.test(cars$Price ~ cars$Doors, alternative="two.sided")

emp <- read.csv("ds710practice/Unemployment_rates.csv", header=TRUE)

head(emp)

#do a paired t test
t.test(emp$Rate_2013, emp$Rate_2014, paired=T, alternative = "greater")

#one sample test for proportion
#get the total cars that have cruise (what we're testing)
numCruise <- length(which(cars$Cruise == 1))

#pass in the thing we're testing, the population size (n), 
#the proportion we're testing against (.5 - for majority) & the alternative hypothesis (that it's more than half)
prop.test(numCruise, n=length(cars$Cruise), p=.5, alternative="greater")

#Two-sample test for difference of proportions
twoWithCruise <- length(which(cars$Doors == 2 & cars$Cruise == 1))
allTwo <- length(which(cars$Doors == 2))
fourWithCruise <- length(which(cars$Doors == 4 & cars$Cruise == 1))
allFour <- length(which(cars$Doors == 4))

#do a prop test with 2 vectors
prop.test(c(twoWithCruise, fourWithCruise), n=c(allTwo, allFour), alternative = "two.sided")


#chi-squared goodness of fit

#get a vector of the counts by type
typeCount <- summary(cars$Type)
typeCount

#make a vector based on the hypothesized proportions. (This should be in the same order as the summary vector)
prop2014 <- c(.022, .073, .118, .761, .026)

#multiple the hypothesized vector by the total sample size
#none of the results should be less than 5 to successfully use this test
prop2014 * length(cars$Type)

#perfrom the test
chisq.test(typeCount, p=prop2014)

#do the same for cylinders. It's a number, so we need to use the table() function to get counts or use as.factor(Cylinder)
cylCount <- table(cars$Cylinder)
cylCount

#since we expect equal proportions, we don't have to pass in a second vector

chisq.test(cylCount)

attach(cars)

#linear regression with r
plot(Mileage, Price)
#no curved pattern to indicate linear model is innappropriate

model <- lm(Price ~ Mileage) #y then x
model
#plot the line
abline(model, col="red", lwd=2)

#predict the price of a car with 20000 miles
predict(model, list(Mileage = 20000))

logPrice = log(Price)
model2 <- lm(logPrice ~ Mileage)
plot(model2)

summary(model2)

#get the summary as a variable
summ <- summary(model2)
#extract a bit of the summary
summ$coefficients
summ$coefficients[1,1]


#detach the cars dataset
detach(cars)
model <- lm(cars$Price ~ cars$Mileage) #y then x
#predict the price of a car with 20000 miles
predict(model, list(Mileage = 20000))
