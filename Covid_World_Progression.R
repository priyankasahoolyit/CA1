# Created a new repository called CA1 in GitHub.
# Then created a new project in R.
# Get the current working directory
getwd()
setwd("C:\\Users\\deepa\\Documents\\R\\CA1")
#  and reading it as a dataframe
# File "covid.csv" downloaded from blackboard, 
# storing the data file into the working directory and reading it as data frame called as "covid_data".
# Covid data set holds lots of variables that contain an empty space.
# Hence, replaced each empty space/missing content with NA.

covid_data <- read.csv("covid.csv", na = "") # Reading covid.csv file
covid_data [covid_data == ""] <- NA #Assigning blank spaces with NA
head(covid_data, n = 15) # Display the first 15 records of the dataframe
class(covid_data) # Confirm the class of covid_data
str(covid_data) # Check the structure of data frame
nrow(covid_data) #count the number of rows within the covid data frame

#------------------------ Data Preparation ------------------------------------#

# Structure displays that there are total 84529 observations and 59 variables in the Covid dataset.
# The datetime field is converted to a date variable in mm/dd/yyyy format from chr variable.

converted_date <- as.Date(covid_data$date, "%d/%m/%Y") # datetime variable is converted to date type and formatted
converted_date
str(converted_date)

#Counting the total number of `NA` values to see, 
#how many null values was there in the entire dataset
sum(is.na(covid_data$Count))

#Viewing the records with NA
na_records <- covid_data[!complete.cases(covid_data),]
na_records
#----------------------------------------------------------------------------
#Research Question 1: Effect of the vaccine on New cases
# H0 : There is no correlation between people_fully_vaccinated and new_cases
# H1 : There is correlation between people_fully_vaccinated and new_cases

# Analysing the variables used in each variable
# people_fully_vaccinated = continuous interval variable
# new_cases = continuous interval variable
#----------------------------------------------------

covid_subset <- subset(covid_data, select = c(people_fully_vaccinated, new_cases))
covid_subset

#Using mice library to display NA values and its count
install.packages("mice")
library(mice)
md.pattern(covid_subset)

# Installed VIM package and displayed the missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(covid_subset, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

#----------------------------------------------------


#Check whether the variables you are using for the hypothesis test are normally
#distributed or not. Do this visually and using a relevant statistical analysis test. Then
#decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset)
install.packages("ggplot2")
library(ggplot2)
options(scipen = 999)
ggplot(covid_subset, aes(x=people_fully_vaccinated,y=new_cases))+ geom_point(col="lightblue", size=3)



#plot(people_fully_vaccinated, new_cases, pch = 9, col= "lightblue",
  #   main = "comparision of people_fully_vaccinated with new_cases",
  #   xlab = "people_fully_vaccinated",
   #  ylab = "new_cases")


#Visual analysis seems to indicate the data normally distributed
#Summarize the
tapply(people_fully_vaccinated, new_cases, median)

#------------------------ Data Analysis ------------------------------------#


#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 

#Is people_fully_vaccinated normally distributed?
qqnorm(people_fully_vaccinated)
# Add line that represents normal distribution
qqline(people_fully_vaccinated, col = "red")
# people_fully_vaccinated appears not to be normally distributed


#Is new_cases normally distributed?
qqnorm(new_cases)
# Add line that represents normal distribution
qqline(new_cases, col = "red")
# new_cases appears not to be normally distributed



install.packages("psych")
library(psych)

covid_subset <- subset(covid_data, select = c(people_fully_vaccinated, new_cases))
covid_subset

pairs.panels(covid_subset,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   


# Need to decide a test for calulating p-value
#Pearson’s Correlation Coefficient 

??pearson

#---------------------------------------------------------------------------------------------------------------------------------------------
#Research Question 2: Does covid affect diabetic patients
# H0 : There is no correlation between new_deaths and diabetes_prevalence 
# H1 : There is correlation between new_deaths and diabetes_prevalence 

# Analysing the variables used in each variable
# new_deaths = continuous interval variable
# diabetes_prevalence = continuous interval variable
#----------------------------------------------------

covid_subset2 <- subset(covid_data, select = c(new_deaths, diabetes_prevalence))
covid_subset2

#Using mice library to display NA values and its count
install.packages("mice")
library(mice)
md.pattern(covid_subset2)

# Installed VIM package and displayed the missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(covid_subset2, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

#----------------------------------------------------


#Check whether the variables you are using for the hypothesis test are normally
#distributed or not. Do this visually and using a relevant statistical analysis test. Then
#decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset2)
install.packages("ggplot2")
library(ggplot2)
options(scipen = 999)
ggplot(covid_subset2, aes(x=new_deaths,y=diabetes_prevalence))+ geom_point(col="lightblue", size=3)



#plot(new_deaths, diabetes_prevalence, pch = 9, col= "lightblue",
#   main = "comparision of new_deaths with diabetes_prevalence",
#   xlab = "new_deaths",
#  ylab = "diabetes_prevalence")


#Visual analysis seems to indicate the data normally distributed
#Summarize the
tapply(new_deaths, diabetes_prevalence, median)

#------------------------ Data Analysis ------------------------------------#


#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 



#Is new_cases normally distributed?
qqnorm(new_deaths)
# Add line that represents normal distribution
qqline(new_deaths, col = "red")
# new_cases appears not to be normally distributed

#Is diabetes_prevalence normally distributed?
qqnorm(diabetes_prevalence)
# Add line that represents normal distribution
qqline(diabetes_prevalence, col = "red")
# diabetes_prevalence appears not to be normally distributed


install.packages("psych")
library(psych)

pairs.panels(covid_subset2,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   


#Pearson’s Correlation Coefficient 

??pearson



