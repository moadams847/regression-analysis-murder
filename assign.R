#### question 1--------------------------
### set working directory
setwd("E:/Data Analytics/Assignment")

#### load data
data_set<- read.csv("Auto.csv", 
                stringsAsFactors = FALSE, header = T)

data_df<-data_set
head(data_df) #### peak at the first 5 observation

##### question 2 ---------------------
str(data_df) ### the struct of the data

summary(data_df) ### summary statistics

## Univariate Visualization

#### histogram ----------------
# load the data
# data(data_df)

# create histograms for each attribute
par(mfrow=c(1,5))
for(i in 1:5) {
  hist(data_df[,i], main=names(data_df)[i])
}
## all features in the data set are skewed

#### density plots-------------

# load dataset
# data(data_df)

# create a panel of simpler density plots by attribute
par(mfrow=c(1,5))
for(i in 1:5) {
  plot(density(data_df[,i]), main=names(data_df)[i])
}
### one feature has biomodal
## the rest of the features are skewed
## for algoruthms to perform well it will be good to transform the features


###  Box And Whisker Plots--------------
# load dataset
data(data_df)
# Create separate boxplots for each attribute
par(mfrow=c(1,5))
for(i in 1:5) {
  boxplot(data_df[,i], main=names(data_df)[i])
}
##### it clear from the the boxplot that population is quite normal
### and rest of the features are all skewed

#### Missing Plot-------------
### the presense of missing values can have a  negeative impage on a model

# load packages
install.packages('Amelia')
library(Amelia)
# load dataset
data(data_df)
# create a missing map
missmap(data_df, col=c("black", "grey"), legend=FALSE)

#### the data is clean, there are no missing values
### otherwise going forward we would of to impute the missing values.


##### question 3 and 4----------------------------------------------------

###questions to be answered using regression analysis in this studey------------------
### Q1 how does illeteracy rate influence murder
#### Q2 how does income also influence murder

#### hypothesis to be tested from regression analysis using t-test--------------------

### for illteracy
### null hypothesis--- Illiteracy has no relatiosnhip with murder
### alternate hypothesis--- illiteracy has a relationship with murder

#### for income
### null hypothesis--- income has no relatiosnhip with murder
### alternate hypothesis--- income has a relationship with murder

#### why I THINK it is an interesting question----------------------------
#### using correlation and correlation plot


#### Correlation Plot-----------------------------------
#### I would have to take a subset of the data to get my questions answered.

var=c('Murder','Income','Illiteracy')
data_df_one<-data_df[var]

# load package
library(corrplot)

# load the data
data(data_df_one)
# calculate correlations
correlations <- cor(data_df_one[,1:3])

# create correlation plot
corrplot(correlations, method="circle")

### the bigger the circle the higher the correlation and vice versa
#### the circles tells us that income is negatively correlated with murder.
### also illiteracy rate is positively correlated with murder.

###### Scatterplot Matrix---------------------------------

# load the data
data(data_df_one)
# pair-wise scatterplots of all 4 attributes
pairs(data_df_one)

#### there is a downward sloping relationship beween murder and income indicating 
### a negative relationship between murder and income

#### also there is a positve relationship between illiteracy and murder, ie
#### indicating a positive realtionship

##### fitting the regression model------------------------------
fit<-lm(Murder~Illiteracy + Income,data=data_df_one)
summary(fit) ### summary of fit

##### checking model assumptions-----------------------------------------
par(mfrow=c(2,2))
plot(fit)


##### normaliy assumptions is quite ok given the Q-Q plot
#### constantance of varance(Homoscedasticity) has also been met
#### given the residuals vs fitted plot

## LINEARITY-------------------------------------------------
install.packages('car')
library(car)
crPlots(fit)
## linearity assumptions met
## since the broken lines are quite straight


###### test statistic, slope, pvale, intercept and R squared
summary(fit) 

##### confidence interval--------------------------------
confint(fit)
#### since both the intercept and income confidence interval contains zero
## it means they are insignificant
### and only  illteracy is significant

##### conclusion
##illiteracy----  ## holding all other factors costant it is expected the a unit increase in
### illteracy level will cause a increase in murder by 4.51 according to the model

##### income---- ## holding all other factors costant it is expected the a dollar increase in
### income will cause a increase in murder by 0.00057 according to the model

#### only variable that explains the model is income but intercept and illteracy are all
### insignificant

qf(0.05, 1, 47, lower.tail = F)
#### overall, the F-statistic when compared to F-critical
### tells us tha the entire 
##### model is significant

