library("ggpubr")
library("gridExtra")
library("pwr")
install.packages("pwr")

# Read csv file  into work space
nycairbnb<-read.csv("/Users/brindamunuswamy/Desktop/MIS500/airbnb.csv")

#Study the data set
dim(nycairbnb)
#Summary of data set
summary(nycairbnb)

# finding NAs in data set
is.na(nycairbnb$Review.Scores.Rating)
sum(is.na(nycairbnb$Review.Scores.Rating))

# omitting NAs fromm data set
nycairbnb <- na.omit(nycairbnb)
#verification of NAs after omitting 
sum(is.na(nycairbnb$Review.Scores.Rating))


# power analysis 
nycairbnbbrooklyn <- subset(nycairbnb, Property.Type == "Apartment"& Neighbourhood == "Brooklyn")
nycairbnbmanhattan <- subset(nycairbnb, Property.Type == "Apartment"& Neighbourhood == "Manhattan")
dim(nycairbnbbrooklyn)
dim(nycairbnbmanhattan)
sdbklyn <- sd(nycairbnbbrooklyn$Review.Scores.Rating)
sdmnhtn <- sd(nycairbnbmanhattan$Review.Scores.Rating)
# View(nycairbnbbrooklyn)
#pooled standard deviation, which is the square root of the average of the two standard deviations.
psd <- sqrt(((sdbklyn^2)+(sdmnhtn^2))/2)
meanbklyn<-mean(nycairbnbbrooklyn$Review.Scores.Rating)
meanmhtn <-mean(nycairbnbmanhattan$Review.Scores.Rating)
cohen<- (meanbklyn-meanmhtn)/psd
pwr.t.test(n=NULL,d=cohen,sig.level=0.05,power=0.90,type="two.sample",alternative = "two.sided")


# deriving subset of data set
nycairbnb <- subset(nycairbnb, Property.Type == "Apartment"& (Neighbourhood == "Manhattan"|Neighbourhood == "Brooklyn"))

#study final subset of data set
dim(nycairbnb)
summary(nycairbnb)
View(nycairbnb)



# boxplots shows difference but cant tell if they are statistically significant or not
ggplot(nycairbnb,aes(Neighbourhood,Review.Scores.Rating))+geom_boxplot()

# histograms to show similar skewness between sample distribution
p1<-ggplot(nycairbnb,aes(Review.Scores.Rating))+geom_histogram(fill="white",color="grey30")+facet_wrap(~ Neighbourhood)
p2<-ggplot(nycairbnb,aes(Review.Scores.Rating))+geom_histogram(fill="grey",color="grey30")+facet_wrap(~ Neighbourhood)+scale_x_log10()
grid.arrange(p1,p2,nrow=2)


# F test to check variances 
var.test(Review.Scores.Rating~Neighbourhood, data = nycairbnb)
# two sample t test for 2 tailed test
t.test(Review.Scores.Rating~Neighbourhood, data = nycairbnb,var.equal=FALSE,conf.level=0.95)
# critical value is quantile of the standard normal distribution 
qt(0.025,15223)


# t test of log transformed data 
t.test(log(Review.Scores.Rating)~Neighbourhood, data = nycairbnb)
# wilcox test 
wilcox.test(Review.Scores.Rating~Neighbourhood, data = nycairbnb)



# two sample t test for 1 tailed test (right)
t.test(Review.Scores.Rating~Neighbourhood, data = nycairbnb, alternative = "greater")
# two sample t test for 1 tailed test (left)
t.test(Review.Scores.Rating~Neighbourhood, data = nycairbnb, alternative = "less")




# not used
par(mfrow=c(2,2))
ggplot(nycairbnb,aes(Review.Scores.Rating))+geom_histogram(fill="white",color="grey30")+facet_wrap(~ Neighbourhood)
ggplot(nycairbnb,aes(Review.Scores.Rating))+geom_histogram(fill="grey",color="grey30")+facet_wrap(~ Neighbourhood)+scale_x_log10()