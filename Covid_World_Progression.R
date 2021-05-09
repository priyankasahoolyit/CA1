# Created a new repository called CA1 in GitHub.
# Then created a new project in R.
# Get the current working directory
getwd()

#  and reading it as a dataframe
# File "covid.csv" downloaded from blackboard, 
# storing the data file into the working directory and reading it as data frame called as "covid_data".
# Covid data set holds lots of variables that contain an empty space.
# Hence, replaced each empty space/missing content with NA.

covid_data <- read.csv("covid.csv", na = "") # Reading covid.csv file
covid_data [covid_data == ""] <- NA #Assigning blank spaces with NA
head(covid_data, n = 15) # Display the first 10 records of the dataframe
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

#Using mice library to display NA values and its count
install.packages("mice")
library(mice)
md.pattern(covid_data)

# Installed VIM package and displayed the missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(covid_data, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)


#------------------------ Data Analysis ------------------------------------#
