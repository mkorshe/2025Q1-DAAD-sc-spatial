
##Install packages if you haven't already
#install.packages('tidyverse')
#install.packages('lubridate')
#install.packages('nycflights13')

#Loading libraries
library(tidyverse)
library(lubridate)
library(nycflights13)

#### Data types
## Boolean or logical
TRUE
FALSE
T
F
1 == 1
3<2
0.1!=999

##Can convert numbers to boolean
as.logical(1)
as.logical(0)
as.logical(222)
##Non-zero numbers convert to TRUE
as.logical('0')
as.logical('abc')
##Charachters not converted to boolean

#### Numeric (or integer or number - any number)
1+1
1+"1"
0.2
1/2
9i ##Complex numbers

###Characters
'Hello!'
"Привіт!"
"TRUE"
"1111"
"1+1"
'1'+'1'

##Convert to other datatype
as.numeric('1111')
as.logical('TRUE')
as.character(999)

##Determining data type
class(TRUE)
class(1.2)
class(8)
class('hello!')

##Assigning variables
a <- 1
b <- 2
a+b
a <- TRUE
a+b
#Matrix and vectors operations
#Important to know that vectors and matrices operations
#do not work the same way as in linear algebra
##Vector recycling 
a <- c(1,2,3,4)
b <- c(1,2,3,4,5,6,7,8)
c <- c(1,2,3,4,5)
#Vectors are recycled - 
#when one fits into another exactly n times you 
#don't even get a warning (e.g. b is twice as long as a)
a+b
a+c
#Same for multiplication and division
a*b
a*c

## Operations with matrices also work elementwise
#Rather than how you are used to from mathematics class
A <- matrix(c(1,2,3,4,5,6), nrow = 2)
B <- matrix(c(1,1,1,1,1,1), nrow = 2)
C <- matrix(c(1,2,3,4,5,6), nrow = 3)
# Elements are filled by column, then row
#to change it: use byrow = TRUE argument
A
B
C
A+B# element-wise addition
A*C #Matrices do not recycle like vectors
A*B #element-wise multiplication
#If you want to do matrix multiplication you need to use special command
A %*% B #impossible to multiply since dimensions do not match
A %*% C


## Lists
##Lists are different from vectors in that you can
#put different data types in the list 
my_list <- list(1,"hello", TRUE, c(1,2,3), 
                tibble(x = c(1:3),y = c('a', 'b','c')),
                list(2, 'apple', c(6,4,5)))

View(my_list)


#Subsetting vectors, matrices and lists
#Unlike Python here indexing starts at 1 rather than 0
a[1:5] #select elements from 1 to 5
A[2,1] #Row number then column number
A[,2] #Select all of column 2
my_list[[2]] #for lists you use double brackets
my_list[[6]][[2]] #you can subset objects within lists, within list
my_list[[4]][1]
my_list[[5]]['x'] #for dataframes you can subset with [] by variable name


### Working with dates
#lubridate package
today()
now()
#you can convert strings into dates type format
#You can have date written in any format
dmy(11032010)
mdy("January 1st, 2020")
ymd("2021-05-06")

class(ymd("2021-05-06"))

#If you need to add time you can also use hms
mdy_hms('June 12th, 1987, 15:06:22')

#You can also specify timezone, check helpfile
?ymd

#You can also combine several columns to make a date
View(flights)
flights <- flights  |>  mutate(dep_date_time = make_datetime(year, month, day, hour, minute))
View(flights)
#You can use this new date variable for plots, e.g. to show how something changed with time

#You can also extract individual components from the date variable
year(flights$dep_date_time)
month(flights$dep_date_time)
mday(flights$dep_date_time) #day of month
wday(flights$dep_date_time) #Day of the week

##Calculating duration
#you can calculate difference between 2 dates
#just using - e.g
today()- ymd('1990-01-01')
#you can also add days or years
today()+years(1)

#The answer is in days, to convert to seconds/years
as.duration(today()- ymd('1990-01-01'))
#You can also convert weeks, months, days, etc. to seconds
dweeks(3)
dmonths(1)
ddays(5)

##Working with strings 
my_string <- c('I like apples, oranges, and bananas.',
               'Today is Thursday.'
)
#str_length
str_length(my_string)
#Combining strings
str_c('I like apples, oranges, and bananas.',
      'Today is Thursday.')
#Alternatively you can use paste or paste0 commands if you prefer base R to tidyverse
paste('I like apples, oranges, and bananas.',
      'Today is Thursday.')
paste0('I like apples, oranges, and bananas.',
       'Today is Thursday.')
#What's the difference? Paste inserts a space between strings while paste 0 does not
#Alternatively you can specify your own separator using sep argument
#this works both for str_c, paste but not for paste0
paste('I like apples, oranges, and bananas.',
      'Today is Thursday.', sep = 'xxxxx')
paste0('I like apples, oranges, and bananas.',
       'Today is Thursday.', sep = 'xxxxx')
str_c('I like apples, oranges, and bananas.',
      'Today is Thursday.', sep = 'xxxxx')
#Warning, doesn't work if one of your arguments is NA
str_c('x', 'y', NA)
#Vector recycling is also applicable here!
str_c('This is my sentence:', my_string, 'The End.')
#To put all elements of a vector into a single string use collapse
str_c(my_string, collapse = ' ')

##Subsetting strings
str_sub(my_string, 1, 4) #Only select characters from 1 to 4 in each string
# converting all characters to lower case
#Useful eg in text analysis so that word starting from capital letter and lowercase letter
str_to_lower(my_string)
#similarly can use str_to_upper

#How many times can we see the certain pattern in our string
str_count(my_string, 'T')

#Replacing one string with another
str_replace(my_string, 'T', 'Y') #Only replaces the first instance
str_replace_all(my_string, 'T', 'Y') #Replaces all

#Split strings when there is a particular pattern
str_split(my_string, " ")
#n - maximum strings that we have at the end
str_split(my_string, " ", n = 3)

## Select part of the string
str_extract(my_string, 'T')

## Regular expressions
### What is regular expressions?
#They allow you to match broad types of patterns in the strings
#It is very useful in data cleaning, e.g. when you have a certain pattern 
#in your text and you want to extract only part of this text


#Some examples
# . - matches any charachter
str_extract_all(my_string, 'T.')

#Match strings that start with certain pattern
str_extract_all(my_string, "^T.")
#Only extracts first match, because second is not at the beginning of a string

#Similarly, extract end of the string with $
str_extract_all(my_string, "day.$")

#Special symbols and how to escape them
##Some symbols are special - they have a special meaning because they correspond to a certain
#commands.
#E.g. ., $, \, etc. to match them use \\ to escape
str_extract('$^12', '\\$\\^12')


#To match different data type
my_string2 <- c('One, 2, three, 4',
                'hello, hallo, hollo')

#Match any digit \d (use \\ to escape)
str_extract_all(my_string2, "\\d")
#Match spaces \s
str_extract_all(my_string2, "\\s")
#[]matches any symbols in the bracket
str_extract_all(my_string2, "h[aeo]llo")
#alternatively use () and |
str_extract_all(my_string2, "h(e|a|o)llo")
#[^]matches anything except symbols in the bracket
str_extract_all(my_string2, "h[^ao]llo")

## How to match a pattern of repeating characters?
# ? repeats 0 or 1 times
# + repeats 1 or more
# * repeats 0 or more time
str_extract_all('1111023', '1?')
str_extract_all('1111023', '1+')
str_extract_all('1111023', '1*')

#Also can specify how many times the pattern is repeating
str_extract_all('1111023','1{2}') #exactly 2 times
str_extract_all('1111023','1{2,}') #2 or more times
str_extract_all('1111023','1{2,3}') #between 2 and 3 times


#### Working with factors
# We will use forcats package which is part of the tidyverse
## Factors or categorical variables is data type that has several categories that repeat throught
#different observations
data(iris)
View(iris)
unique(iris$Species)
#Since you see levels at the bottom it means it is already coded as factor
#you can also check the variable type with the class comand
class(iris$Species)
## Let's recode it to the character data type which would be the default 
#data type that R loads text data in
iris$Species <- as.character(iris$Species)
unique(iris$Species)
#no longer have levels
class(iris$Species)
#How do we recode it back to factor?
iris$Species_factor <- factor(iris$Species)
unique(iris$Species_factor)

#You can also set the levels yourself in whatever order you want
#By default they are ranked in the alphabetical order
levels <- c('virginica', 'versicolor', 'setosa')
iris$Species_factor2 <- factor(iris$Species, levels = levels)
unique(iris$Species_factor2)
#if you specify the levels but one (or more) is missing in your levels vector
#it recodes all values that are not included in the levels vector to NA
levels2 <- c('virginica', 'versicolor')
iris$Species_factor3 <- factor(iris$Species, levels = levels2)
unique(iris$Species_factor3)
#If you want to include all categories but sort them in the order in which they
#appear in the data rather than alphabetically use unique command for levels
iris$Species_factor <- factor(iris$Species, levels = unique(iris$Species))
unique(iris$Species_factor)
#Alternatively use fct_inorder command
iris$Species_factor <- fct_inorder(iris$Species)

#You can always check levels with levels command
levels(iris$Species_factor)

#### Reordering existing factors by the value of some other variable
#using fct_reorder command
iris$Species_factor2 <- fct_reorder(iris$Species_factor, iris$Sepal.Width)
#The order has changed - now it is from the category with the smallest Sepal Width to the largest
levels(iris$Species_factor2)
#This is especially useful for plots.
#The ggplot plots categories in the order of levels.
#But aesthetically it is not always the best choice
ggplot(iris)+geom_col(aes(Species_factor, Sepal.Width))
#Now it is in ascending order - when we use the recoded variable
ggplot(iris)+geom_col(aes(Species_factor2, Sepal.Width))
#You can also use fct_reorder command directly inside the ggplot
ggplot(iris)+geom_col(aes(fct_reorder(Species_factor, Sepal.Width), Sepal.Width))


#When we want to reorder based on frequency you can 
#use special commands fct_infreq and fct_rev
?fct_infreq
?fct_rev
iris |> mutate(Species_factor = factor(Species)  |>
  fct_infreq() |> fct_rev()) |>
  ggplot(aes(Species_factor)) +
  geom_bar()

## For line plots fct_reorder2 works better as it reorders category in order of the lines
ggplot(iris)+geom_smooth(aes(Sepal.Width, Sepal.Length, 
                             color = fct_reorder2(Species_factor, Sepal.Width, Sepal.Length)))

##Changing levels labels
iris <- iris |> 
  mutate(Species_factor2 = fct_recode(Species_factor,
                                      'my label 1' ='versicolor',
                                      'second label' = 'virginica',
                                      'final label' = 'setosa' ))

unique(iris$Species_factor2)
## Some other useful commands from forcats package
#Counts the number of observation for each level
fct_count(iris$Species_factor)

#If you want to change the order of levels but not according to another variable
#But in arbitrary pattern use fct_relevel
#fct_relevel also allows you to use more complex commands
#and use together with lapply we discussed before
#Let's look at some of the examples in the help file
?fct_relevel 

#Example from help file
#Help file on this command has some useful examples
f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
fct_relevel(f)
fct_relevel(f, "a")
fct_relevel(f, "b", "a")
# Move to the third position
fct_relevel(f, "a", after = 2)
# Relevel with a function
fct_relevel(f, sort) #alphabetical order
fct_relevel(f, sample)
fct_relevel(f, rev)

df  <- forcats::gss_cat[, c("rincome", "denom")]
lapply(df, levels)

#after = Inf means that we want to put a given category at the end
df2 <- lapply(df, fct_relevel, "Don't know", after = Inf)
lapply(df2, levels)

#fct_drop - drops levels that are defined but unused
f <- factor(c("a", "b"), levels = c("a", "b", "c"))
f
fct_drop(f)
fct_drop(f, only = "a")

#fct_expand - add additional levels to the factor
fct_expand(f, "d", "e", "f")
#fct_explicit_na #Give explicit factor level to missing values
#(Looks better on plots, etc.)
f <- factor(c("a", "b", "c", "d", NA), levels = c("b", "c", "d", "a"))
f
fct_explicit_na(f, na_level = "(Missing)")
#fct_other - group multiple levels into "other" category
x <- factor(rep(LETTERS[1:9], times = c(40, 10, 5, 27, 1, 1, 1, 1, 1)))
x
fct_other(x, keep = c("A", "B")) 
fct_other(x, drop = c("A", "B"))

## See full list of forcats commands here: https://forcats.tidyverse.org/index.html
#in the cheatsheet



################ EXERCISES ##########

##Create a factor variable with months of the year, where levels are numeric but labels are in words


#Create a numeric vector of the first 10 multiples of 3. Add 2 to each element and multiply by 5

###Create a vector with names of 10 of your friends or relatives. Extract all names that have letter a in them.
#Then, extract all names that are longer than 4 characters. Finally, combine all names in a single string, separated by ", 


##Calculate how many days are left until your birthday. How many days since your last birthday?


##Create a list with a) character vector, b) a number, c) a 2x2 matrix with numbers from 1 to 4
##Subset a second element in the matrix