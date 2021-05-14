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
install.packages("dplyr")     # Install dplyr package to decrease development time and improve readability of code
library(magrittr)             # needs to be run every time you start R and want to use %>%
library(dplyr)                # alternatively, this also loads %>%
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



# ------------------------------ Raw Data Visualization -----------------------------------------------#


Deaths<-aggregate(covid_data$total_deaths~covid_data$location,covid_data,FUN = max)
Deaths10<-Deaths[order(-Deaths$`covid_data$total_deaths`),][1:10,]

Deaths10

barplot(Deaths10$`covid_data$total_deaths`,names.arg = Deaths10$`covid_data$location`,
        main="Highest covid death cases",las=3,col="red")



#------------------------------ Data Preparation -----------------------------------------------#

# Structure displays that there are total 84529 observations and 59 variables in the Covid dataset.
# Already all the string variables are converted to Factors while reading the data into dataframe. 
# The `date` field is in "YYYY-mm-dd" format as char type
# converted to a `date` variable to date from char type.

covid_data$date <- as.Date(covid_data$date)
str(covid_data$date)



# ------------------------  Identifying the missing values---------------------------#

# Lets find out if there are any NA's in the data
# Using na.omit() to store any full rows into new_data frame

final_df<-na.omit(covid_data)
dim(final_df)
final_df

# It is observed that there are missing data in all the records, 
# so na.omit() function is dropping all the rows from the data frame.
# Hence, not an option to proceed with. 

# complete.cases() returns a vector with no missing values, can be swapped by using the `!`
# Using complete.cases() to show all complete rows store in complete_data
# and `!` complete_cases() for missing_data accordingly.
# Then using nrow() to show a total of all complete and missing rows

complete_data<-covid_data[complete.cases(covid_data),]
nrow(covid_data)
missing_data<-covid_data[!complete.cases(covid_data),]
nrow(covid_data)
missing_rows<-nrow(complete_data) -nrow(missing_data)
missing_rows

# Here as well its evident that the none of the rows are complete out of 84529 observations. 


# Now, getting the total number of `NA` values to see, how many null values were there in the entire dataset.
# Finding which columns contain `NA` values

sum(is.na(covid_data))                     # Count of `NA` is 2009585
names(which(sapply(covid_data, anyNA)))    # Almost all the variables contains `NA`, 



# -------------------------- Data Subsetting and Imputing ---------------------------------------------#

# Let's create a subset of covid_data, 
# considering the information required for further Hypothesis testing.

# Using the subset function to extract all records 
# from covid_data where age > 35 or age < 24. and only select the listed attributes


attach(covid_data)
names(covid_data)
covid_subset <-subset(covid_data, continent %in% c("Asia","Europe"), 
                      select = c(iso_code, 
                                 location, 
                                 date, 
                                 people_fully_vaccinated, 
                                 new_cases))
dim(covid_subset)


covid_data[(covid_data$location=="Asia"),2] <- "Asia"
covid_data[(covid_data$location=="Europe"),2] <- "Europe"
covid_data[(covid_data$location=="Africa"),2] <- "Africa"
covid_data[(covid_data$location=="North America"),2] <- "North America"
covid_data[(covid_data$location=="South America"),2] <- "South America"
covid_data[(covid_data$location=="Oceania"),2] <- "Oceania"
sum(is.na(covid_data$continent))

sum(is.na(covid_subset))


asia_new_cases <- subset(covid_data, continent %in% c("Asia"), 
                         select = c(iso_code, 
                                    location, 
                                    date,
                                    continent,
                                    people_fully_vaccinated, 
                                    new_cases))
dim(asia_new_cases)

europe_new_cases <- subset(covid_data, continent %in% c("Europe"), 
                         select = c(iso_code, 
                                    location, 
                                    date,
                                    continent,
                                    people_fully_vaccinated, 
                                    new_cases))
dim(europe_new_cases)


asis_vs_europe_new <- c(asia_new_cases$new_cases, europe_new_cases$new_cases)
dim(asis_vs_europe_new)


covid_subset <-subset(covid_data, continent %in% c("Asia","Europe"), 
                      select = c(iso_code, 
                                 location, 
                                 date, 
                                 people_fully_vaccinated, 
                                 new_cases))


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


# This test doesnt work on dicotomous variable
with(beavers_data, tapply(temp, activ, shapiro.test))

covid_subset1 <- subset(covid_subset, select = c(people_fully_vaccinated, new_cases))
str(covid_subset1)
sum(is.na(covid_subset1))


# This test doesnt work on dicotomous variable


my_sample<- covid_subset[sample(1:nrow(covid_subset), 100, replace = FALSE),]
my_sample

my_sample <- covid_subset1[sample(1:10, 10, replace=FALSE),]
my_sample


with(my_sample, tapply(people_fully_vaccinated, new_cases, shapiro.test))

#Error in FUN(X[[i]], ...) : sample size must be between 3 and 5000
# put a random sample of 5000 or less then do Shapiro test 



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
      
# use statistical methods to examine 
# the relationship between our variables of interest
      
      
# creating a subset
names(covid_data)      
covid_subset4 <- subset(covid_data, select = c(iso_code, location, date, total_deaths, diabetes_prevalence))
str(covid_subset4)
sum(is.na(covid_subset4))  #18038
dim(covid_subset4)         # 84529     5
head(covid_subset4)                 
      
# Check for missing data
incomplete_data <- covid_subset5[!complete.cases(covid_subset4),]
incomplete_data
nrow(incomplete_data)      #16377
      
# Install and use mice package to show
# missing var in  the manager dataframe
install.packages("mice")
library(mice)
md.pattern(covid_subset4)  
         
# Visualise the data for missing Vars
# Use VIM Package to show missing vars

install.packages("VIM")
library(VIM)
missing_values <- aggr(covid_subset4, prop = FALSE, numbers = TRUE)      
     
# Analysing the variables 
# total_deaths = continuous interval variable
# diabetes_prevalence = categorical nominal variable 


#Check whether the variables you are using for the hypothesis test are normally
#distributed or not. Do this visually and using a relevant statistical analysis test. 
#Then decide on which statistical test you will use.      

# ChecK linearity of the variables 
attach(covid_subset4)
plot(total_deaths, diabetes_prevalence, pch = 9, col= "lightblue", 
     main = "comparision of total_deaths with diabetes_prevalence",
     xlab = "total_deaths",
     ylab = "diabetes_prevalence") 

      
#Visual analysis seems to indicate the data normally distributed
#Summarize 
tapply(total_deaths, diabetes_prevalence, median)

# visualise the normality of the variables
opar = par(no.readonly = TRUE)
#arrange the plots in 1 rows by 2 cols
par(mfrow = c(1,2))

hist(total_deaths, col = "red", main = "dist of total_deaths" , xlab = "total_deaths")
hist(diabetes_prevalence, col = "red", main = "dist of diabetes_prevalence")
par = opar       
      
       
#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 

#Is total_deaths normally distributed?
with (covid_subset4, {qqnorm (total_deaths,
                              main = "Normal QQ-plot of total_deaths data",
                              xlab = "Theoritical Quantiles",
                              ylab = "Samles Quantiles")})

# Add line that represents normal distribution
qqline(total_deaths, col = "red")
# total_deaths appears not to be normally distributed

#Is mpg normally distributed?
with (covid_subset4, {qqnorm (diabetes_prevalence,
                              main = "Normal QQ-plot of weight data",
                              xlab = "Theoritical Quantiles",
                              ylab = "Samles Quantiles")})
# Add line that represents normal distribution
qqline(diabetes_prevalence, col = "red")
# diabetes_prevalence appears not to be normally distributed      
      
     
# create a normal QQ-plot of weight and mpg values
# we can examine the linear correlation
# between both variables
with (covid_subset4, {qqplot (total_deaths, diabetes_prevalence,
                              main = "comparing total_deaths and cardiovasc_death_rate",
                              xlab = "total_deaths",
                              ylab = "cardiovasc_death_rate")})

# we can run the formal test of normality
#provided through the widely used shapiro-wilks test

my_sample<-covid_subset4[sample(1:nrow(covid_subset4), 3, replace = FALSE),]
my_sample

normality_test <- shapiro.test(my_sample$total_deaths)
normality_test$p.value

#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is 
# not normally distributed

# In this example p-value= 0.02473123
# 0.02473123 > 0.05 (False)
# therefore the var wt is normally distributed

# normality test for mpg
normality_test <- shapiro.test(my_sample$diabetes_prevalence)
normality_test$p.value
# the p-value  = 0.7922128
# 0.7922128 > 0.05 (True)
# therefore the var diabetes_prevalence is normally distributed

# Both vars are normally distributed
# both Continuous
# test = pearson

# dependent var = mpg
# Independent var = wt

install.packages("psych")
library(psych)


my_sample1<-covid_subset4[sample(1:nrow(covid_subset4), 10000, replace = FALSE),]
my_sample1

pairs.panels(my_sample1,
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




cor.test(total_deaths, diabetes_prevalence, method = "pearson")  # not working
wilcox.test(total_deaths~diabetes_prevalence)  # not working

t.test(x = total_deaths, alternative = "greater")
#One Sample t-test

#data:  total_deaths
#t = 45.665, df = 72834, p-value < 2.2e-16
#alternative hypothesis: true mean is greater than 0
#95 percent confidence interval:
#   21945.48      Inf
#sample estimates:
#   mean of x 
#22765.51 




t.test(x = diabetes_prevalence, alternative = "greater")

#One Sample t-test

#data:  cardiovasc_death_rate
#t = 602.51, df = 77065, p-value < 2.2e-16
#alternative hypothesis: true mean is greater than 0
#95 percent confidence interval:
#   257.043     Inf
#sample estimates:
#   mean of x 
#257.7466 


# pearson correlation = -0.8676594
# p-value = 2.2e-16
# cut off = 0.05 

# 2.2e-16 < 0.05  (true)
# reject null/accept alternative
#hence reject H0 and accept H1
# there is significant       
      
      
      
      
      
      
      
      
      
      
      
      
      
            
#-------------------------------------------- Research Question 5 -------------------------------------------------#
#Research Question 3: Does covid affect cardiovasc_death_rate
      
# H0 : There is no correlation between total_deaths and cardiovasc_death_rate
# H1 : There is correlation between total_deaths and cardiovasc_death_rate 
      
# Analysing the variables used in each variable
# total_deaths = continuous interval variable
# cardiovasc_death_rate = categorical variable
#-------------------------------------------------------------------------------------------------------------------#
      
      
# use statistical methods to examine 
# the relationship between our variables of interest

      
# creating a subset
names(covid_data)      
covid_subset5 <- subset(covid_data, select = c(iso_code, location, date, total_deaths, cardiovasc_death_rate))
str(covid_subset5)
sum(is.na(covid_subset5))  #19157
dim(covid_subset5)         # 84529     5
head(covid_subset5)      

# Check for missing data
incomplete_data <- covid_subset5[!complete.cases(covid_subset5),]
incomplete_data
nrow(incomplete_data)      #16750
      
      
# Install and use mice package to show
# missing var in  the manager dataframe
install.packages("mice")
library(mice)
md.pattern(covid_subset5)      
      
# Visualise the data for missing Vars
# Use VIM Package to show missing vars

install.packages("VIM")
library(VIM)
missing_values <- aggr(covid_subset5, prop = FALSE, numbers = TRUE)
      

# Analysing the variables 
# total_deaths = continuous interval variable
# cardiovasc_death_rate = categorical nominal variable 


#Check whether the variables you are using for the hypothesis test are normally
#distributed or not. Do this visually and using a relevant statistical analysis test. Then
#decide on which statistical test you will use.      

# ChecK linearity of the variables 
attach(covid_subset5)
plot(total_deaths, cardiovasc_death_rate, pch = 9, col= "lightblue", 
     main = "comparision of total_deaths with cardiovasc_death_rate",
     xlab = "total_deaths",
     ylab = "cardiovasc_death_rate")

#Visual analysis seems to indicate the data normally distributed
#Summarize 
tapply(total_deaths, cardiovasc_death_rate, median)

# visualise the normality of the variables
opar = par(no.readonly = TRUE)
#arrange the plots in 1 rows by 2 cols
par(mfrow = c(1,2))

hist(total_deaths, col = "red", main = "dist of total_deaths" , xlab = "total_deaths")
hist(cardiovasc_death_rate, col = "red", main = "dist of cardiovasc_death_rate")
par = opar  
      
      
#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 

#Is total_deaths normally distributed?
with (covid_subset5, {qqnorm (total_deaths,
                     main = "Normal QQ-plot of total_deaths data",
                     xlab = "Theoritical Quantiles",
                     ylab = "Samles Quantiles")})

# Add line that represents normal distribution
qqline(total_deaths, col = "red")
# total_deaths appears not to be normally distributed

#Is mpg normally distributed?
with (covid_subset5, {qqnorm (cardiovasc_death_rate,
                     main = "Normal QQ-plot of weight data",
                     xlab = "Theoritical Quantiles",
                     ylab = "Samles Quantiles")})
# Add line that represents normal distribution
qqline(cardiovasc_death_rate, col = "red")
# cardiovasc_death_rate appears not to be normally distributed



# create a normal QQ-plot of weight and mpg values
# we can examine the linear correlation
# between both variables
with (covid_subset5, {qqplot (total_deaths, cardiovasc_death_rate,
                     main = "comparing total_deaths and cardiovasc_death_rate",
                     xlab = "total_deaths",
                     ylab = "cardiovasc_death_rate")})

# we can run the formal test of normality
#provided through the widely used shapiro-wilks test

normality_test <- shapiro.test(total_deaths)
normality_test$p.value

#p-value tells us  the chance  that the sample 
# comes form a normal distribution
# if p < 0.05 the variable is 
# not normally distributed

# In this example p-value= 0.09265499
# 0.09265499 > 0.05 (True)
# therefore the var wt is normally distributed

# normality test for mpg
normality_test <- shapiro.test(cardiovasc_death_rate)
normality_test$p.value
# the p-value  = 0.1228814
# 0.1228814 > 0.05 (True)
# therefore the var mpg is normally distributed

# Both vars are normally distributed
# both Continuous
# test = pearson

# dependent var = mpg
# Independent var = wt

install.packages("psych")
library(psych)


pairs.panels(covid_subset5,
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




cor.test(total_deaths, cardiovasc_death_rate, method = "pearson")  # not working
wilcox.test(total_deaths~cardiovasc_death_rate)  # not working

t.test(x = total_deaths, alternative = "greater")
#One Sample t-test

#data:  total_deaths
#t = 45.665, df = 72834, p-value < 2.2e-16
#alternative hypothesis: true mean is greater than 0
#95 percent confidence interval:
#   21945.48      Inf
#sample estimates:
#   mean of x 
#22765.51 




t.test(x = cardiovasc_death_rate, alternative = "greater")

#One Sample t-test

#data:  cardiovasc_death_rate
#t = 602.51, df = 77065, p-value < 2.2e-16
#alternative hypothesis: true mean is greater than 0
#95 percent confidence interval:
#   257.043     Inf
#sample estimates:
#   mean of x 
#257.7466 


# pearson correlation = -0.8676594
# p-value = 2.2e-16
# cut off = 0.05 

# 2.2e-16 < 0.05  (true)
# reject null/accept alternative
#hence reject H0 and accept H1
# there is significant 



 

#======================================================= The End =======================================================#
names(covid_data)      
covid_subset10 <- subset(covid_data, select = c(population, life_expectancy, human_development_index, diabetes_prevalence, cardiovasc_death_rate))

sample <-covid_subset10[sample(1:nrow(covid_subset10), 10000, replace = FALSE),]
sample

pairs.panels(sample,
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

      
names(covid_data)      
covid_subset11 <- subset(covid_data, select = c(total_cases,total_deaths, stringency_index))

sample <-covid_subset11[sample(1:nrow(covid_subset11), 10000, replace = FALSE),]
sample

pairs.panels(sample,
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
      
      
      
      
