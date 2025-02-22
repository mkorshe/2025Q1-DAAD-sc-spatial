## Data Visualization
##Cheatsheets: Help - Cheatsheets - Data Visualization with ggplot
#### ggplot package (from Tidyverse)
##The basic structure (see cheatsheet)
##Or 
#https://rstudio.github.io/cheatsheets/data-visualization.pdf

##Also useful book https://ggplot2-book.org/ 

library(tidyverse)
library(nycflights13)
library(viridis)
library(ggthemes) ## Different themes
##Different types of plots
data('flights')
data('weather')

months <- c(1, 3,4, 12)
flights_selected <- flights |> filter(month %in% months & origin == 'JFK')
flights_selected <- flights_selected |> mutate(delayed = ifelse(arr_delay>0,1,0),
                                               time_gained = dep_delay - arr_delay,
                                               .before = 1)

combined_data <- left_join(flights_selected, weather, by = c('origin', 'year', 'month', 'day', 'hour', 'time_hour'))
data_by_month <- combined_data |> group_by(month) |> summarize(mean_delay = mean(arr_delay, na.rm = TRUE),
                                                               median_delay = median(arr_delay, na.rm = TRUE),
                                                               mean_temp = mean(temp, na.rm = TRUE),
                                                               numb_flights = n(),
                                                               numb_delayed = sum(delayed, na.rm = TRUE),
                                                               prop_delayed = numb_delayed/numb_flights
)

combined_data <- combined_data |> 
  group_by(carrier) |> 
  mutate(mean_car_delay = mean(arr_delay, na.rm = TRUE)) |> ungroup()
combined_data <- combined_data |> mutate(delay_over_mean = arr_delay-mean_car_delay)


#Bar plot
##How many flights were done by each airline?
##For geom_bar you only need to provide it with one variable and it counts 
#the number of observations for each category automatically
ggplot(data = combined_data)+geom_bar(aes(x = carrier))

##If you already calculated the number of observations in each category yourself, e.g. as we did with 
#by month, use geom_col instead of geom_bar as it allows you to specify 
#both x and y variables
ggplot(data = data_by_month)+geom_col(aes(x = month, y = numb_flights))
### What went wrong here??
## R thinks that number of month is a continuous variable
## By using factor command we tell R that it is not continuous but categorical
#variable, i.e. it's numerical values do not matter
ggplot(data = data_by_month)+geom_col(aes(x =factor(month), y = numb_flights))

### To show the relationship between 2 variables
## E.g. Do planes get delayed more when the humidity is higher
#(However, remember, that this is just correlation rather than causal relationship)
###Let's 
#Points
ggplot(data = combined_data)+geom_point(aes(x = humid, y = dep_delay ))
#Smooth
ggplot(data = combined_data)+geom_smooth(aes(x = humid, y = dep_delay ))

## We can also combine 2 or more types of plots together
## We can also move aes() to the ggplot() command if both parts share the same 
#x and y variables
ggplot(data = combined_data, aes(x = humid, y = dep_delay ))+geom_point()+geom_smooth()
##Why it looks different - no clear trend?
#The scale of y variable is different - when we used geom_smooth we only needed 
#to display the average level of departure delay for each level of humidity
#therefore the y-axis went only up till 25
#when we plot points, we need to show all possible values (including outliers)
#THerefore, the trend is not that clearly visible

###plot with dates
##We can also plot time series data
ggplot(data = data_by_month)+geom_line(aes(x = month, y = mean_delay))
##However, the x-axis is not very informative
##We can change it to show the month names
flights_jan <- combined_data |> filter(month == 1)
ggplot(data = flights_jan)+geom_line(aes(x = time_hour, y = delay_over_mean))

flights_jan_1 <- combined_data |> filter(month == 1 & day == 1)
ggplot(data = flights_jan_1)+geom_line(aes(x = time_hour, y = delay_over_mean))

###Boxplots
##Boxplots are useful to show the distribution of a variable
ggplot(data = combined_data)+geom_boxplot(aes(x = factor(month), y = arr_delay))

### Customizing plots
#### Here we remove outliers, change transparency of the dots and add labels
##Customizing figures
ggplot(data = combined_data, aes(x = humid, y = dep_delay ))+
  geom_point(alpha = 0.3)+
  labs(x = 'Humidity', y = 'Delay')+ylim(0,25)
#Alpha parameter controls transparency
#Labs - labels
#Lims - limits to remove outliers
### The results look different then before - because previously
#all data was used to estimate the line and only then the plot was cut
#Here we remove outliers first, and then construct the line only based on the
#data without outliers

##Many other types of plots - check cheatsheet

##Coloring/filling/size by variable

ggplot(data = combined_data)+geom_smooth(aes(x = humid, y = dep_delay , color = factor(month)))
##Other options instead of color: 
#Shape
#Fill (when we have bars instead of line or points) 
#Alpha - transparency
#size
##Common mistake: put color outside of aes
#If you want to change color of all points independent of any condition - do it
ggplot(data = combined_data)+geom_smooth(aes(x = humid, y = dep_delay), color  = 'red')

##For bar charts use fill instead of color
ggplot(data = data_by_month)+geom_col(aes(x =factor(month), y = numb_flights, fill = factor(month)))
##or outside aes
ggplot(data = data_by_month)+geom_col(aes(x =factor(month), y = numb_flights), fill = 'red')

##Also instead of colors we can make separate plots for each month to examine whether relationship
#between humidity and departure delay 
#This is useful when we want to examine the heterogeneity by 2 variables simultaneously (eg carrier and month)
ggplot(data = combined_data)+geom_smooth(aes(x = humid, y = dep_delay , color = factor(carrier)))+facet_grid(~month)
## TO make figure more clear limited only to the TOP airline carriers
#Checking which carriers are most popular
combined_data |> count(carrier) |> arrange(desc(n))
##Let's select only 3 most popular carriers
carriers_top <- c('B6', 'DL', '9E')
combined_data_top <- combined_data |> filter(carrier %in% carriers_top)
#Updated plot
flights_plot <- ggplot(data = combined_data_top)+geom_smooth(aes(x = humid, y = dep_delay , color = factor(carrier)))+facet_grid(~month)
flights_plot
##We see that for 9E increased humidity is the biggest problem in all months
#while DL is the most consistent independently of humidity
##Scale_fill for bar plots, scale_color for points and lines
ggplot(data = data_by_month)+geom_col(aes(x =factor(month), y = numb_flights, fill = factor(month)))+scale_fill_viridis_d(name = 'Month')
##We can also use other color scales
ggplot(data = data_by_month)+geom_col(aes(x =factor(month), y = numb_flights, fill = factor(month)))+scale_fill_brewer(palette = 'Set1')
?scale_fill_brewer
###Themes
##We can change the appearance of the plot using themes
flights_plot + theme_minimal()
flights_plot + theme_light()
flights_plot_econ <- flights_plot + theme_economist()+theme(legend.position = 'none')+scale_color_economist()
?theme


ggsave('flights.png', flights_plot_econ)

###Exercises
#1) Create a scatter plot of distance (flight distance) vs. air_time (airtime in minutes).
#Color the points by the carrier column. 
##Custimize it by adding labels

#2) Plot a histogram of dep_delay for all flight. Change the color to skyblue. 
#Set the binwidth to 5.
#Only plot delays between -50 and 200 minutes

#3)Use the flights dataset to create a line plot of the average departure delay over time (by day).

#4) Create a heatmap showing the average dep_delay for each combination of hour and origin airport.
#Hint: use geom_tile()

#5) Create a boxplot of arr_delay grouped by the carrier.
#Limit the y-axis to delays between -50 and 200.
#Color the boxes by carrier.
#Add a title and clean axis labels.
