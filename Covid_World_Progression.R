# First, we created a new repository called CA1 in GitHub.
# Then created a new project in R.
# Get the current working directory
# The working directory can be set if required

setwd("C:\\Users\\deepa\\Documents\\R\\CA1")
getwd()

# We will load some libraries that we are going to use during the research.
install.packages("ggplot2")   # Install package for Data visualization
install.packages("mice")      # Install mice package and displayed the missing values
install.packages("VIM")       # Install VIM package and displayed the missing values
install.packages("psych")     # Install psych package to find correlations between multiple variables
install.packages("magrittr")  # Install magrittr package are only needed the first time you use it
#install.packages("dplyr")     # Install dplyr package to decrease development time and improve readability of code
library(magrittr) # needs to be run every time you start R and want to use %>%
#library(dplyr)    # alternatively, this also loads %>%
library(psych)
library(VIM)
library(ggplot2)
library(mice)


# File "covid.csv" downloaded from blackboard, 
# storing the data file into the working directory and reading it as data frame called as "covid_data".
# Covid data set holds lots of variables that contain an empty space.
# Hence, replaced each empty space/missing content with NA.
# Also, converting the character type into factor for better analysis.

covid_data <- read.csv("covid.csv", na = "", header = T, stringsAsFactors = T) # Reading covid.csv file
covid_data [covid_data == ""] <- NA      # Assigning blank spaces with NA
head(covid_data, n = 15)                 # Display the first 15 records of the dataframe

class(covid_data)                        # Confirm the class of covid_data
str(covid_data)                          # Check the structure of data frame
nrow(covid_data)                         # Count the number of rows within the covid data frame 

#------------------------------ Data Preparation -----------------------------------------------#

# Structure displays that there are total 84529 observations and 59 variables in the Covid dataset.
# The datetime field is converted to a date variable in mm/dd/yyyy format from chr variable.

covid_data$date <- as.Date(covid_data$date)
#converted_date <- format(covid_data$date, "%d/%m/%Y") # datetime variable is converted to date type and formatted
str(covid_data$date)

# Getting the total number of `NA` values to see, how many null values were there in the entire dataset.
# Finding which columns contain `NA` values

sum(is.na(covid_data))                     # Count of `NA` is 2009585
names(which(sapply(covid_data, anyNA)))    # Almost all the variables contains `NA`, 
                                           # except `iso_code`, `location`, `date`

# Viewing the records with NA
na_records <- covid_data[!complete.cases(covid_data),]
na_records

# The variable `new_cases` are the counts of new confirmed cases of covid-19 reported daily, country wise. 
# So it will not be wrong to consider the null values as no new cases in the country on particular date, 
# hence replacing NA values with 0 for this column. 

# Similarly, the variable `people_fully_vaccinated` is total number of people who received all doses prescribed by the vaccination protocol. 
# The NA cases for this variable would be considered as if a country has number of people
# fully vaccinated is none then this column takes NA as value, can be replaced by 0 for analysis purpose.

covid_data$new_cases[is.na(covid_data$new_cases)] <- 0
covid_data$new_cases
covid_data$people_fully_vaccinated[is.na(covid_data$people_fully_vaccinated)] <- 0
covid_data$people_fully_vaccinated


#-------------------------------------- Hypothesis testing --------------------------------------#

# Research Question 1: Effect of the vaccine on New cases
# H0 : There is no correlation between people_fully_vaccinated and new_cases
# H1 : There is correlation between people_fully_vaccinated and new_cases

# Analyzing the variables used in null and alternate hypothesis
# people_fully_vaccinated = continuous interval variable
# new_cases = continuous interval variable
#------------------------------------------------------------------------------------------------#

# creating a subset of covid_data for convenient hypothesis testing

covid_subset <- subset(covid_data, select = c(iso_code, location, date, people_fully_vaccinated, new_cases))
str(covid_subset)
sum(is.na(covid_subset))

# Data cleaning for subset of data is now completed with no missing values. 
# A graph would be showing the total number of new cases per date, group by location. 
# We have to transform our dataset, so that it is grouped by date and location.

covid_new <- covid_subset %>% group_by(date, location) %>%
summarize(people_fully_vaccinated = sum(people_fully_vaccinated), new_cases = sum(new_cases)) 
head(covid_new)



#Using mice library to display NA values and its count
md.pattern(covid_subset)

# Using VIM library and displayed the missing values
missing_values <- aggr(covid_subset, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

#----------------------------------------------------


#Check whether the variables you are using for the hypothesis test are normally
#distributed or not. Do this visually and using a relevant statistical analysis test. Then
#decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset)
options(scipen = 999)
ggplot(covid_subset, aes(x=people_fully_vaccinated,y=new_cases))+ geom_point(col="lightblue", size=3)



plot(people_fully_vaccinated, new_cases, pch = 9, col= "lightblue",
     main = "comparision of people_fully_vaccinated with new_cases",
     xlab = "people_fully_vaccinated",
     ylab = "new_cases")


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

covid_subset1 <- subset(covid_subset, select = c(people_fully_vaccinated, new_cases))
str(covid_subset1)
sum(is.na(covid_subset1))

pairs.panels(covid_subset1,
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
#Spearman’s Correlation Coefficient (also use for ordinal data) 


corr1 <- cor.test(x=covid_subset1$people_fully_vaccinated, 
                 y=covid_subset1$new_cases, method = 'spearman')
corr1

corr2 <- cor.test(x=covid_subset1$people_fully_vaccinated, 
                 y=covid_subset1$new_cases, method = 'pearson')
corr2



#----------------------------------------- Research Question 2 ----------------------------------------------#

# Research Question 2: Older people are more likely to die from covid-19
# In  other words, death due to Covid is higher for those aged more than 70?
  
# H0: There is no correlation between death cases due to covid-19 and older people having age >= 70 
# H1: There is no correlation between death cases due to covid-19 and older people having age >= 70

# Analyzing the variables used in each variable
# total_deaths = continuous interval variable
# aged_70_older = categorical variable, represented as proportions
#--------------------------------------------------------------------------------------------------------------#



covid_data$total_deaths[is.na(covid_data$total_deaths)] <- 0
covid_data$total_deaths
covid_data$aged_70_older[is.na(covid_data$aged_70_older)] <- 0
covid_data$aged_70_older


covid_subset2 <- subset(covid_data, select = c(total_deaths, aged_70_older))
covid_subset2


#Using mice library to display NA values and its count
md.pattern(covid_subset2)

# Installed VIM package and displayed the missing values
missing_values <- aggr(covid_subset2, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

#----------------------------------------------------


#Check whether the variables you are using for the hypothesis test are normally
#distributed or not. Do this visually and using a relevant statistical analysis test. Then
#decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset2)

#options(scipen = 999)
ggplot(covid_subset2, aes(x=total_deaths,y=aged_70_older))+ geom_point(col="lightblue", size=3)



plot(new_deaths, diabetes_prevalence, pch = 9, col= "lightblue",
   main = "comparision of total_deaths with aged_70_older",
   xlab = "total_deaths",
   ylab = "aged_70_older")


#Visual analysis seems to indicate the data normally distributed
#Summarize the
tapply(total_deaths, aged_70_older, median)

#------------------------ Data Analysis ------------------------------------#


#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 


#Is new_cases normally distributed?
qqnorm(total_deaths)
# Add line that represents normal distribution
qqline(total_deaths, col = "red")
# total_deaths appears not to be normally distributed

#Is diabetes_prevalence normally distributed?
qqnorm(aged_70_older)
# Add line that represents normal distribution
qqline(aged_70_older, col = "red")
# diabetes_prevalence appears not to be normally distributed



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


# Need to decide a test for calculating p-value
# Here , we have dependent variable is continuous and normally distributed
# and Independent variable is categorical 
# Hence, Spearman test could be a good fit, b

??Whitney


wilcox.test(total_deaths~aged_70_older)  # Not working


corr1 <- cor.test(x=covid_subset2$total_deaths, 
                  y=covid_subset2$aged_70_older, method = 'spearman')
corr1



#-------------------------------------------- Research Question 3 -------------------------------------------------#
# Research Question 3: Does GDP per capita is impacted by population_density of a country

# H0 : GDP per capita is not impacted by population of a country 
# H1 : GDP per capita is impacted by population of a country 

# Analysing the variables used in each variable

# gdp_per_capita = categorical variable
# population_density = categorical variable
#---------------------------------------------------------------------------------------------------------------------#




covid_data$gdp_per_capita[is.na(covid_data$gdp_per_capita)] <- 0
covid_data$gdp_per_capita
covid_data$population_density[is.na(covid_data$population_density)] <- 0
covid_data$population_density


covid_subset3 <- subset(covid_data, select = c(gdp_per_capita, population_density))
covid_subset3


#Using mice library to display NA values and its count
md.pattern(covid_subset3)

# Installed VIM package and displayed the missing values
missing_values <- aggr(covid_subset3, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

#----------------------------------------------------


#Check whether the variables you are using for the hypothesis test are normally
#distributed or not. Do this visually and using a relevant statistical analysis test. Then
#decide on which statistical test you will use.

# ChecK linearity of the variables 

attach(covid_subset3)

#options(scipen = 999)
ggplot(covid_subset3, aes(x=gdp_per_capita,y=population_density))+ geom_point(col="lightblue", size=3)



plot(gdp_per_capita, population_density, pch = 9, col= "lightblue",
     main = "comparision of gdp_per_capita with population_density",
     xlab = "gdp_per_capita",
     ylab = "population_density")


#Visual analysis seems to indicate the data normally distributed
#Summarize the
tapply(gdp_per_capita, population_density, median)

#------------------------ Data Analysis ------------------------------------#


#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 


#Is gdp_per_capita normally distributed?
qqnorm(gdp_per_capita)
# Add line that represents normal distribution
qqline(gdp_per_capita, col = "red")
# gdp_per_capita appears not to be normally distributed

#Is population_density normally distributed?
qqnorm(population_density)
# Add line that represents normal distribution
qqline(population_density, col = "red")
# population_density appears not to be normally distributed



pairs.panels(covid_subset3,
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


# Need to decide a test for calculating p-value
# Here , we have dependent variable is continuous and normally distributed
# and Independent variable is categorical 
# Hence, Spearman test could be a good fit, b



corr1 <- cor.test(x=covid_subset3$gdp_per_capita, 
                  y=covid_subset3$population_density, method = 'spearman')
corr1


#======================Output ________________________________________________

#Spearman's rank correlation rho

#data:  covid_subset3$gdp_per_capita and covid_subset3$population_density
#S = 75994205022682, p-value < 0.00000000000000022
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
      rho 
#0.2450564 

#_______________________________________________________________






#-------------------------------------------- Research Question 4 -------------------------------------------------#
#Research Question 3: Does covid affect diabetic patients
      
# H0 : There is no correlation between total_deaths and diabetes_prevalence 
# H1 : There is correlation between total_deaths and diabetes_prevalence 

# Analysing the variables used in each variable
# total_deaths = continuous interval variable
# diabetes_prevalence = categorical variable
#-----------------------------------------------------------------------------------------------------------------#

      
      
      
      
      
            
#-------------------------------------------- Research Question 5 -------------------------------------------------#
#Research Question 3: Does covid affect cardiovasc_death_rate
      
# H0 : There is no correlation between total_deaths and cardiovasc_death_rate
# H1 : There is correlation between total_deaths and cardiovasc_death_rate 
      
# Analysing the variables used in each variable
# total_deaths = continuous interval variable
# cardiovasc_death_rate = categorical variable
#-------------------------------------------------------------------------------------------------------------------#
      
      
      
      
      
      
