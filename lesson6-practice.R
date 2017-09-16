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
