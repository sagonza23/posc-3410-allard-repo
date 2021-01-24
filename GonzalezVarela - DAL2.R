# Title: POSC-3410 Lab 1 ####
# Author: Sergio A. Gonzalez Varela
# Date: 01/14/2021

# Lesson 1 ####

install.packages("tidyverse")

# Lesson 2 ####

# Load tidyverse package
library(tidyverse)

## -- Attaching packages ---------------------------------- tidyverse 1.3.0 --

## v ggplot2 3.3.1     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0

## -- Conflicts ------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()

# Load data
data(mpg)

# Check the structure of mpg using str()
str(mpg)

# Answer below (using a comment) what type of data structure mpg is?
# Vector

# Create analysis data frame: We do this so we preserve a raw version of the dataframe to which we can refer as we make
mpg_df<-mpg

# Check the structure of mpg_df using str()
str(mpg_df)

# Scatter plot of displ x hwy
ggplot(mpg_df,aes(x=displ , y=hwy ))+
  geom_point()

# Scatter plot of displ x hwy by fuel type (fl)
ggplot(mpg_df,aes(x=displ , y=hwy, color=fl ) )+
  geom_point()

# On Your Own, Scatter plot of displ x hwy by class
ggplot(mpg_df,aes(x=displ , y=hwy, color=class ) )+
  geom_point()

## # A tibble: 5 x 11
##   manufacturer model   displ  year   cyl trans    drv     cty   hwy fl    class
##   <chr>        <chr>   <dbl> <int> <int> <chr>    <chr> <int> <int> <chr> <chr>
## 1 chevrolet    corvet~   5.7  1999     8 manual(~ r        16    26 p     2seat~
## 2 chevrolet    corvet~   5.7  1999     8 auto(l4) r        15    23 p     2seat~
## 3 chevrolet    corvet~   6.2  2008     8 manual(~ r        16    26 p     2seat~
## 4 chevrolet    corvet~   6.2  2008     8 auto(s6) r        15    25 p     2seat~
## 5 chevrolet    corvet~   7    2008     8 manual(~ r        15    24 p     2seat~

# Lesson 3 ####

?ggplot()

# Call help for filter
?filter()

# Call help for geom_bar()
?geom_bar()

# Call help for facet_wrap()
?facet_wrap()

# ggplot - data layer, assign it to mpg_data
mpg_data<-ggplot(data= mpg)

# Call mpg_data
mpg_data

# Describe the output you see in the console.
# A gray box

# Now, let's add an aesthetic layer to the plot.

# Put engine size on the x-axis, highway mileage on y-axis, and color by class.
mpg_plot<-mpg_data +
  geom_point(mapping =aes(x=displ , y=hwy, color=class))

# Call mpg_plot.
mpg_plot

# What do you see?
# A colored scatter plot

# Now, let's add an aesthetic layer to change the axis labels
mpg_axis <- mpg_plot+
  xlab("Engine Size in Liters")+
  ylab("Highway Mileage")+
  ggtitle("Engine Size and Mileage by Vehicle Class")

# Call mpg_axis
mpg_axis

# What do you see?
# Axis are now labeled

# Create different graphs for each class
mpg_wrap<-mpg_axis+
  facet_wrap(~class, nrow=2)

# Call mpg_wrap
mpg_wrap

# What do you see?

# Re-assign mpg_wrap by adding facet wrapping, change nrow to 3.

# What do you see?
# Graphs separated based on vehicle class

# Lesson 4 ####

# install nycflights13 package
install.packages("nycflights13")

# Load tidyverse package
library(tidyverse)

# Load nycflights13 package
library(nycflights13)

# Filter to keep only the flights that happened on 3-14.
filter(flights, month==3, day==14)

# Assign flights that happened on 3-14 to new variable:`Mar14`.
Mar14 <- filter(flights, month==3, day==14)

# Select flights that happened in either May or June
filter(flights, month==5|month==6)

# Filter flights  whose arrival was delayed over 60 minutes AND departed on time.
filter(flights, (arr_delay>60&dep_delay<1))

# Filter flights whose arrival was delayed over 60 minutes AND departure was delayed less than 15 minutes.
filter(flights, (arr_delay>60&dep_delay<15))

# Filter flights whose arrival was delayed over 60 minutes AND departure was delayed 15 minutes or more (i.e., remember to use your Greater Than or Equal To Operator).
filter(flights, (arr_delay>60&dep_delay>=15))

# Filter flights whose destination is Greenville-Spartanburg International Airport (GSP)
filter(flights, (dest == "GSP"))

# Filter flights that were in the air over 3 hours (180 mins)
filter(flights, (air_time>180))

# Effects of NAs on Logical Operations
NA==99
## [1] NA

NA>3
## [1] NA

NA<3
## [1] NA

# Number of Wins for Clemson Football in the 2021 season
Clemson <- NA

# Number of Wins for Ohio State football in the 2021 season
OSU <- NA

# Do Clemson and OSU have the same number of wins
Clemson==OSU
## [1] NA

# Interpret this result using a comment your script file.
# The wins of Clemson and OSU are not available, so it is unclear if they have the same amount of wins.

# use is.na(varName) to find whether it is NA. This command returns a logical vector of either TRUE or FALSE. See the example below.
is.na(Clemson)
## [1] TRUE

is.na(OSU)
## [1] TRUE

# Now let's see how it works on a data frame.
is.na(flights)

# Describe what you see.
# A data frame of values, but it all says false.

# Let's see what happens when we combine filter() and is.na(). We will search for NAs in arrival delay.
filter(flights,is.na(arr_delay))

# We will search for observations with NO NAs in arrival delay.
filter(flights,!is.na(arr_delay))

# In the real world, we often want to know how many observations are in each subset data frame. We will do this by calling nrow() outside of the filter commands.

# Number of observations in flights data set with missing data for arrival delay.
nrow(filter(flights,is.na(arr_delay)))
## [1] 9430

# Number of observations in flights data set with NO missing data for arrival delay.
nrow(filter(flights,!is.na(arr_delay)))
## [1] 327346

# On your own, number of observations in flights data set with missing data for departure delay.
nrow(filter(flights,is.na(dep_delay)))

# On your own, number of observations in flights data set with no missing data for departure delay.
nrow(filter(flights,!is.na(dep_delay)))

# In the real world, we often will filter out NAs across several variables and assign the resulting data frame to a new variable.
flights_df<-filter(flights, (!is.na(arr_delay)& !is.na(dep_delay)))

# Call flights_df
flights_df

# On your own, filter out NAs across year, month, dep_delay, arr_delay, carrier, flight, air_time. Assign the resulting data frame to a new variable: flights_df
flights_df<-filter(flights, (is.na(year) & is.na(month) & is.na(dep_delay) & is.na(arr_delay) & is.na(carrier) & is.na(flight) & is.na(air_time)))

# Call flights_df
flights_df

# Create sample dataframe  using sequence (use the help command to learn more about sequence)
sample<-sequence(10, from=10L, by=-1L )

sample_df <-as_tibble(sample)

# Call sample_df
sample_df

# Describe what the results that were returned.
# A column of numbers going from 1-10. Also numbered 1-10.

# Arrange from 1 to 10.
arrange(sample_df, value)

# Describe what the results that were returned.
# The column of numbers goes in ascending order according to the number list. I.e. Row 1 with value 1. Row 2 with value two.

# Create sample dataframe with values from 1:100  using sequence.
sample<-sequence(100, from=1L, by=1L )

sample_df <-as_tibble(sample)

# Call sample_df
sample_df

# Describe what the results that were returned.
# 100 rows of numbers 1-100 in ascending order.

# Arrange sample_df$value in descending order using the syntax, arrange(df, desc(varible))
arrange(sample_df, desc(sample_df$value))

# Describe what the results that were returned.
# The 100 rows are now descending from 100 to 1.

# What happens when we use arrange on a real life data frame (i.e., one that has missing values).

# Arrange flights by arrival delay and assign to arrange_df
arrange_df<-arrange(flights, arr_delay)

# Call the last six rows of  (call help on tail() to find out more)
tail(arrange_df)

# Lets try arranging data on your own.

# Arrange flights to find the most delayed flights.
arrange(flights, desc(dep_delay))

# Arrange the flights that left earliest.
arrange(flights, desc(arr_delay))

# Example use of select
select(flights, year, month, day, carrier, flight)

# Select call columns between year and arrival delay
select(flights, year:arr_delay)

# Select all columns except tailnum
select(flights,-tailnum)

# Select all columns between dep_delay and time_hour.
select(flights, dep_delay:time_hour)

# Select the following columns: year, month, day, dep_delay, arr_delay, dest, distance. Assign to flights_df.
flights_df<-select(flights, -dep_time, -sched_dep_time, -arr_time, -sched_arr_time, -carrier, -flight, -origin, -air_time, -hour, -minute, -time_hour)

# call flights_df
flights_df

# Check the column names
names(sample_df)

# Rename sample_df$value as sample_df$count
rename(sample_df, count = value)

# Rename sample_df$value as sample_df$count. Assign to sample_df
sample_df <-rename(sample_df, count = value)

# To be completed on your own.

# Check column names in flights
names(flights)

# Rename arr_delay as`arrival_delay`. Assignt to flights.
flights<-rename(flights_df, arrival_delay = arr_delay)

# Rename dep_delay as`departure_delay`. Assign to flights.
flights<-rename(flights_df, departure_delay = dep_delay)

# Call flights
flights

# Create dataframe with fewer variables:  flights_small
flights_small <-select(flights,
                       year:day,
                       ends_with("delay"),
                       distance,
                       air_time,
                       dest
                       )

# Create new column: avg_speed.
mutate(flights_small,
       speed = distance/air_time*60)

# You can also use mutate to change existing variables.
# Change destination to factor
mutate(flights_small,
       dest =factor(dest))

# Create column`gain`using formula`dep_delay`-`arr_delay`.
mutate(flights_small,
       gain = dep_delay - arr_delay)

# Create new column for arrival delay in hours (i.e., divide by 60)
mutate(flights_small,
       arrival_delay = arr_delay/60)

# Create new column for departure delay in hours (i.e., divide by 60)
mutate(flights_small,
       departure_delay = dep_delay/60)

# Change year to character
mutate(flights_small,
       character = year)

# Create new character variable using paste0 (call help to learn more and check the hint below) to combine year, month, and day (yyyy/m/d).
(yyyy/m/d)<-paste0(year, month, day, collapse = NULL, recycle0 = FALSE)

# Verse 1 as string
almaMater1<-"Where the Blue Ridge yawns its greatness; Where the Tigers play; Here the sons of dear Old Clemson, Reign supreme alway."

# Verse 2 as string
almaMater2<-"We will dream of great conquests For our past is grand, And her sons have fought and conquered Every foreign land."

# Verse 3 as string
almaMater3 <- "Where the mountains smile in grandeur O’er the hill and dale; Here the Tiger lair is nestling Swept by storm and gale."

# Verse 4 as string
almaMater4 <- "We are brothers strong in manhood For we work and strive; and our Alma Mater reigneth Forever in our lives."

# Chorus as string
almaMaterChorus <- "Dear Old Clemson, we will triumph And with all our might That the Tiger’s roar may echo O’er the mountain height."

# Combine vectors to create almaMater
almaMater<-paste0(almaMater1," ", almaMater2," ", almaMater3," ", almaMater4," ", almaMaterChorus)

# print almaMater to console.
almaMater
"Where the Blue Ridge yawns its greatness; Where the Tigers play; Here the sons of dear Old Clemson, Reign supreme alway. We will dream of great conquests For our past is grand, And her sons have fought and conquered Every foreign land. Where the mountains smile in grandeur O’er the hill and dale; Here the Tiger lair is nestling Swept by storm and gale. We are brothers strong in manhood For we work and strive; and our Alma Mater reigneth Forever in our lives. Dear Old Clemson, we will triumph And with all our might That the Tiger’s roar may echo O’er the mountain height."

# Use summarise to calculate avg (mean) arrival delay.
summarise(flights, avgArrDelay=mean(arr_delay, na.rm=TRUE))

# Use group_by and summarise to calculate avg arrival delay for all the carriers.

# Group flights by carrier: flights_carrier
flights_carrier <-group_by(flights, carrier)

#summarise average delay
summarise(flights_carrier, avgArrDelay=mean(arr_delay, na.rm=TRUE))

# Summarise standard deviation of distance to destinations
summarise(flights, )

# Create new data frame: not_cancelled
not_cancelled <-filter(flights,!is.na(dep_delay)& !is.na(arr_delay))

# Group by month, mean dep_delay
not_cancelled_year<-group_by(not_cancelled, month)

# Group by carrer, summarise mean dep_delay and mean arrival delay
not_cancelled_year<-group_by(not_cancelled, carrier )
summarize(not_cancelled_year, avgArrDelay= mean(arr_delay, na.rm=TRUE))
summarize(not_cancelled_year, avgDepDelay= mean(dep_delay, na.rm=TRUE))

# Group by carrier, summarise sd of distance traveled
not_cancelled_year<-group_by(not_cancelled, carrier )
summarize(not_cancelled_year, distance)
summarize(not_cancelled_year, )
# A tibble: 16 x 1

# Filter for origin=="LGA". When do the first flights depart? when do they arrive?
filter(not_cancelled_year, origin == "LGA")
# 517 o'clock
# 830 o'clock

# Lesson 5 ####

# Load nycflights13 packages
library(nycflights13)

# Load data
data("flights")

# Filter the data for November and December flights, count number of flights that arrived later than 59 minutes. Which destination airports are worst?
filter(flights, month == 11 | month == 12 & arr_delay>60)

# Filter, assign new varName
flights_extract<-filter(flights, month == 11|month == 12 & arr_delay>60)

# Group by destination
flights_extract <-group_by(flights_extract,dest )

# Count
flights_extract <-count(flights_extract)

# Rename
flights_extract <-rename(flights_extract, number = n )

# Arrange
flights_extract <-arrange(flights_extract,desc(number))

# Keep only top 6 rows
flights_extract <-head(flights_extract, n=6)

# Make ggplot bar graph
ggplot(flights_extract,aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity")

# Now look at the same operations with pipes.
flights%>%
  filter(month==11|month==12&arr_delay>60)%>%
  group_by(dest)%>%
  count()%>%
  rename(number=n)%>%
  arrange(desc(number))%>%
  head(6)%>%
  ggplot(aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity")

# Much simpler, right?

# Filter the flights data to keep only May and June flights
flights %>% 
  filter(month == 5 | month == 6)

# Filter the flights data to keep only April
flights %>% 
  filter(month == 4)

# Filter the flights data to keep only Delta flights
flights %>%
  filter(carrier == "Delta")

# Which airport had the most flights from it to NYC's 3 airports? Filter flights data to keep only flights that arrived on-time or early, group by origin airport, arrange in descending order.
flights %>%
  filter(arr_delay<=0 & dest=="JFK" | dest == "LGA" | dest == "EWR") %>%
  group_by(origin) %>%
  arrange(desc(arr_delay))

# Filter flights to keep only flights that departed JFK and arrived at ATL. Keep only origin, destination, arrival delay,  departure delay,and carrier. Create a ggplot scatterplot showing the relationship between departure delay and  arrival delay. Add carrier as color.
flights %>%
  filter(origin == "JFK" & dest=="ATL") %>%
  group_by(origin, dest, arr_delay, dep_delay, carrier) %>%
  count() %>%
  head(6)%>%
  ggplot(aes(x=reorder(dep_delay, -arr_delay), y=arr_delay, color=carrier))+
  geom_bar(stat="identity")

# To which airports did the most flights from NYC go? Make a ggplot visualization.
flights %>%
  filter(origin == "JFK" | origin == "ERW" | origin == "LGA") %>%
  group_by(dest) %>%
  count() %>%
  rename(number=n) %>%
  head(6)%>%
  ggplot(aes(x=reorder(dest, -number), y=number))+
  geom_bar(stat="identity")

# Which carrier had the shortest mean arrival delay?
flights %>%
  filter(arr_delay) %>%
  group_by(carrier) %>%
  count() %>%
  rename(number=n) %>%
  arrange(desc(arr_delay))
  head(6)%>%
  ggplot(aes(x=reorder(carrier, -number), y=number))+
  geom_bar(stat="identity")
  

# Which carrier experienced the most cancelled flights?
flights %>% flights %>%
  filter(arr_delay) %>%
  group_by(carrier) %>%
  count() %>%
  rename(number=n) %>%
  arrange(desc(number))
  head(6)%>%
  ggplot(aes(x=reorder(carrier, -number), y=number))+
  geom_bar(stat="identity")

# On average, which carrier flew fastest.
flights %>%
  filter(arr_delay) %>%
  group_by(carrier) %>%
  count() %>%
  rename(number=n) %>%
  arrange(desc(arr_delay))
  head(6)%>%
  ggplot(aes(x=reorder(carrier, -number), y=number))+
  geom_bar(stat="identity")
  
