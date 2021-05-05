# get the current working directory
getwd()

# Storing the data file into the working directory and reading it as a dataframe
covid_data <- read.csv("covid.csv")
# display the first 10 records of the dataframe
head(covid_data)
# Check the data type
class(covid_data)
# Check the structure of data frame
str(covid_data)

# Structure displays that there are total 84529 observations and 59 variables in the Covid dataset.



# --------------------------------------------------------------------------------------
# Data Preparation
# Cleaning the data
# --------------------------------------------------------------------------------------