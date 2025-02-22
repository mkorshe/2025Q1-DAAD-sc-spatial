library(tidyverse)

### Loops### 
### For loops and while loops

#While loops

x <- 0

while (x<5) {
  print(x)
  x <- x+1
}
##Look at the environment - x has changed - loop not only prints
#But also applies changes to variables

##for loops

test <- c(2:11)

for (i in 1:length(test)){
  print(test[i])
}

for (i in test){
  print(i)
}

## Conditional loops

test <- c(0:10)

for(i in 1:length(test)){
  if (test[i] == 0){
    print(paste(test[i], "is zero"))
  }
  else if(test[i] %% 2 == 0){ # %% means what is the remainder (modulus) after division
    print(paste(test[i], 'is even'))
  }
  else{
    print(paste(test[i], 'is odd'))
  }
}

## Creating functions
#When you have long code which you plan to use repeatedly,
#it may be useful to write a function which you would reuse
#many times.

divide <- function(x,y){
  print(x/y)
}

divide(8,2)
#you can set a default parameter in a function
divide_by_two <- function(x,y = 2){
  print(x/y)
}

divide_by_two(8)
divide_by_two(8,4)
## You can turn loops to functions
odd_even <- function(test){
  for(i in 1:length(test)){
    if (test[i] == 0){
      print(paste(test[i], " is zero"))
    }
    else if(test[i] %% 2 == 0){ # %% means what is the remainder after division
      print(paste(test[i], 'is even'))
    }
    else{
      print(paste(test[i], 'is odd'))
    }
  }
}
odd_even(test)
new_vector <- c(3, 17, 2, 6, 0, 9, 7,8)
odd_even(new_vector)

#### Apply and map functions
## Can be used instead of loops in many circumstances
#Also much faster than a loop
### Which to use apply or map?
#Depends on preferences
## Apply - base R, map - tidyverse

##Apply

#apply
?apply
A <- matrix(c(1,2,3,4,5,6), nrow = 2)

A

#1 - by row, 2 - by column
apply(A, 1, mean)
apply(A, 2, mean)
#if you wrote the same thing as a loop it would have been much longer/more complicated
#e.g.
for (i in 1:nrow(A)){
  print(mean(A[i,]))
}
#And would take longer to compute

#you can use apply with any function, even the ones which you created
apply(A,1, odd_even)
#But note that the output may be in different format, not a matrix anymore
#But regular apply does not work on the datasets
#Let's check with the real dataset
#This is a classic dataset on the charachteristics of different iris flowers
data(iris)
View(iris)
apply(iris, 2, mean)
#all NAs

#lapply
#l stands for l - expect list as an input
#Note that you don't need to specify 2 here
lapply(iris, mean)
#Warning - because not all columns are numeric
#You can also apply it to the list we made before
my_list <- list(1,"hello", TRUE, c(1,2,3), 
                tibble(x = c(1:3),y = c('a', 'b','c')),
                list(2, 'apple', c(6,4,5)))
my_list
lapply(my_list, mean)
#Here it does not calculate the mean of the dataframe which is one
#of the elements of the list, because data frame by itself is not numeric

#sapply
#Similar to lapply, but output is stored not as a list, but as a simplest data structure possible
#in this case numeric
output_sapply <- sapply(iris, mean)
class(output_sapply)
output_sapply
#vapply
#Same but returns output as a a prespecified data type
output_vapply <- vapply(iris, mean, c(1))
class(output_vapply)
output_vapply
#rapply
#Allows you to apply functions to subelements of lists (e.g. lists within lists)
my_list
rapply(my_list, mean)
#Here we calculate mean of each column of the dataframe which is one of the elements in the list
#Including each column of the dataframe and each element of the list which was an element of the list itself

#tapply
#tapply can be used to apply functions by some group
#e.g. in iris dataset we can calculate mean of all columns by species
#which is the last column
?tapply
tapply(iris$Sepal.Length, iris$Species, mean)

##Map (from purrr package), part of tidyverse
#Details available here: https://raw.githubusercontent.com/rstudio/cheatsheets/main/purrr.pdf
#Or check: Help - Cheatsheets - 'List manipulation with purrr'
# map - returns a list
iris |> map(mean) 
#map_df - returns a dataframe
iris |> map_df(mean)
#map_dbl - returns numeric vector
iris |> map_dbl(mean)
#map_chr - return character vector
iris |> map_chr(mean)
#map_lgl - returns a logical vector
iris |> map_lgl(is.numeric) 

#You can define function within the map
#to do it use ~
#And you can perform several maps separated by |> 
#e.g. imagine you want to do several regressions 
#each for a different 
#. - placeholder for dataset
iris |>
  (\(x) split(x, x$Species))() |>
  map(\(df) lm(Sepal.Length ~ Sepal.Width, data = df)) |>
  map(summary) |>
  map_dbl("r.squared")


#map2 - iterates over 2 vectors simultaneously
#They have to be of a same length
x <- c(1,2,3,4,5,6)
y <- c(7,8,9,10,11,12)
map2(x,y,sum)

#pmap - can apply to more than 2 variables
a <- c(0,999, 1, 7,5,67)
pmap(list(a,x,y),sum)



##### Exercises #####

#Create a function that converts miles into kilometers

### apply this function to a vector below using 1) loop 2) Apply 3) Map
miles <- c(300, 814.3, 0.2, 666)

#Write a function that takes a name as input (e.g., "John") and outputs "Hello, John!".


