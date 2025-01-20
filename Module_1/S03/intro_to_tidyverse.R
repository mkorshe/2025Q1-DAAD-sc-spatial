#####################################################
######### INTRODUCTION TO R WITH TIDYVERSE #########
####################################################

### Recommended textbook: R for Data Science by Hadley Wickham and Garrett Grolemund
#https://r4ds.hadley.nz/

###Also good lecture notes: https://evalsp21.classes.andrewheiss.com/projects/01_lab/slides/01_lab.html#1 

##To run command put the cursor on the row with the command and press Ctrl Enter

#1. Step 1: Preparation
#You need to install packages only once, but load them (with library() command) every time
#you launch R
#install.packages('tidyverse')
#install.packages('nycflights13') ##Data for this workshop
#install.packages('stargazer') ##
#install.packages('haven')
#install.packages('lubridate)
library(tidyverse) #A set of packages
library(haven) #To load .dta files
library(readr) #To read and write csv files
library(nycflights13) ##Data for this workshop
library(stargazer) #Summary statistics tables (and regression tables)

#Setting working directory to the folder where your data is located
#setwd('pathtoyourfolder')
#Or Session - Set working directory - Choose directory


#2. Step 2: Loading data
#shortcut keys: arrow = Alt  - 
?read_csv
flights <-  read_csv('3.Introduction to Tidyverse/flights.csv') 
##Difference between read_csv, read_delim and read_csv2
##Explore some options
#You can save files in a similar way, using write_csv instead of read_csv
write_csv(flights, 'flights_new.csv')

##Step 3. Exploring data ###
data('flights')
?flights
View(flights) #view dataset in a separate pane
#Alternatively, just click on the name of the dataset in the Environment pane
head(flights, 100) #print first 100 observations
#View only first 100 observations in a a separate pane
#Useful when dataset is too large and it is slow to open
##The function on the inside is performed first, the function on the outside works
#on the outcome of the function on the inside
View(head(flights, 100)) 
names(flights) #Column names
unique(flights$year) #Look at all the unique values for a given variable
length(unique(flights$carrier)) #How many unique values does a variable have?
#(Use $ to select a column from a dataframe)
glimpse(flights)
stargazer(as.data.frame(flights), type = 'text')
#For categorical variables - number of observations per category, arranged from most to least popular
#Shortcut keys: |>  = Ctrl Shift M
# Why use |> (pipe)? For clarity
#e.g.
# https://evalsp21.classes.andrewheiss.com/projects/01_lab/slides/01_lab.html#116 
#In some cases you will see |>  and in some %>%, they do the same thing

flights |> count(carrier) |> arrange(desc(n))
##Alternatively
flights |> count(carrier, sort = TRUE)
#this is equivalent to:
arrange(count(flights, carrier), desc(n))
#but easier to read

### Step 4. Data manipulation
###### Selecting only relevant columns ####
flights_dep <- flights |> select(year, month, day, starts_with('dep'))
# In a same way to starts_with() you can use ends_with() and contains()
flights_no_time <- flights |> select(everything(), -hour, -minute, -time_hour)
remove(flights_no_time, flights_dep)

#Selct variables from dep_time to arr_delay
flights_selected2 <- flights |> select(dep_time:arr_delay)
# Select all variables except those between dep_time and arr_delay

##Select all columns that are characters
flights |> 
  select(where(is.character))


### Renaming columns
flights |> rename(destination = dest) #New name then old name

###Filtering only relevant observations ####
flights_december<- flights |> filter(month == 12)


# == exactly equal
# != not equal 
#>=more or equal
# <= less or equal
#> more than
#<less than

#What if we are interested in several months that are not in a row?
#Creating vector

months <- c(1, 3,4, 12)

flights_selected <- flights |> filter(month %in% months)

#Filtering on several conditions
flights_selected <- flights |> filter(month %in% months & origin == 'JFK')

# Use | for or and ! for ! eg 
#flights_selected <- flights |> filter(month %in% months & !(origin == 'JFK') )


#### Adding new variables
flights_selected <- flights_selected |> mutate(delayed = ifelse(arr_delay>0,1,0),
                                       time_gained = dep_delay - arr_delay,
                                       .before = 1)

##.before = 1 means that we want to put these variables before the first variable in the dataset
## Similiarly, you can also use .after 
# . indicates that this is the parameter of the function and not the name of the new variable that 
#you are creating
## You can also refer to variable names rather than position (e.g. after = day)
flights_selected |> mutate(delayed = ifelse(arr_delay>0,1,0),
                            time_gained = dep_delay - arr_delay,
                            .keep = 'used')
#Only keeps variables that were used in calculations
#More options:
?mutate
#You can also use across() to apply the same function to multiple variables

flights_selected <- flights_selected |> mutate(across(starts_with('dep'), \(x) round(x)))

##Removing duplicate rows
flights_selected <- flights_selected |> distinct()

# Find all unique origin and destination pairs
flights_selected |> 
  distinct(origin, dest)

##Same, but keep all the columns
flights |> 
  distinct(origin, dest, .keep_all = TRUE)


###Merging data
###Loading additional dataset
## this dataset shows weather condition at each day 
data("weather")
glimpse(weather)
glimpse(flights)

### We can see that these datasets have several variables in common. 
#e.g. year, month, day, hour, origin
#So, we can join these datasets based on this variables ('keys')
### Different types of joins
?left_join 
#right_join
#full_join
#inner_join
?anti_join
#semi_join

combined_data <- left_join(flights_selected, weather, by = c('origin', 'year', 'month', 'day', 'hour', 'time_hour'))

## Step 5. Summarizing data
##You can group by multiple variables too, just list them in group_by!
?group_by
data_by_month <- combined_data |> group_by(month) |> summarize(mean_delay = mean(arr_delay, na.rm = TRUE),
                                                       median_delay = median(arr_delay, na.rm = TRUE),
                                                       mean_temp = mean(temp, na.rm = TRUE),
                                                       numb_flights = n(),
                                                       numb_delayed = sum(delayed, na.rm = TRUE),
                                                       prop_delayed = numb_delayed/numb_flights
                                                       )

### OR easier way to just use .by argument in summarize
combined_data |> summarize(mean_delay = mean(arr_delay, na.rm = TRUE),
                            median_delay = median(arr_delay, na.rm = TRUE),
                            mean_temp = mean(temp, na.rm = TRUE),
                            numb_flights = n(),
                            numb_delayed = sum(delayed, na.rm = TRUE),
                            prop_delayed = numb_delayed/numb_flights,
                            .by = month)

## You can also use slice functions with group_by to select a sample of each group
#slice_head
#slice_tail
#slice_min
#slice_max
?slice_sample

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1) 

#You can also group by several variables simultaneously simply by listing them separated by comma
#e.g. group_by(month, carrier)

#In addition to summarizing, group_by can be used to create new variables in the datasets
#At the end you need to ungroup to continue further data manipulations as before

####Let's say we want to know for each flight whether it had more delay than
#the average delay of the flights of that airline or less.

#First, we group by airline and calculate the mean delay for each airline (carrier)
#This value should be the same for all flights performed by the same airline

combined_data <- combined_data |> group_by(carrier) |> mutate(mean_car_delay = mean(arr_delay, na.rm = TRUE)) |> ungroup()


##Then, to calculate by how much each particular flight was delayed more or less than the average flight
#for that airline in the same way as we did before, by using the mutate() function
combined_data <- combined_data |> mutate(delay_over_mean = arr_delay-mean_car_delay)

###### Why use tidyverse rather than base R?
##Many things could be done by both, but tidyverse usually is more easily interpretable, easier to undertand, etc.
## e.g. (Source: https://marcyshieh.com/ps811-materials/base-r-vs-tidyverse.html#comparing-base-r-vs.-tidyverse)
## Extracting data: 
#Selecting top rowa
flights_short <- flights[1:100, ] #base R
flights_short <- flights |> top_n(100) #Tidyverse
##Looking at existing variables




###### Exercises #####

##1) Why doesn’t the following work, and what does the error mean?

flights |> 
  select(tailnum) |> 
  arrange(arr_delay)


### 2) what does function any_of do and when can it be useful?

## 3) Which flights traveled the farthest distance? Which traveled the least distance?

## 4) How many flights arrived more than two hours late but didn’t leave late?

## 5) Explore function num_range(). When it could be useful?

##6) Explore what function relocate() does. What alternative command can you use instead of relocate?

##7) In which months the arrival delays are highest and lowest? 

##8) Select only flights that left when the temperature was below average

##9) Create the speed variable, that measures the speed of each flight




