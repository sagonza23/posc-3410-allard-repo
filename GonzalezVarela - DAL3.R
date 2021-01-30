# Title: POSC-3410 Lab 3 ####
# Author: Sergio A. Gonzalez Varela
# Date: 01/27/2021

# Lesson 1 ####

library(tidyverse)

# Read Zillow_Rentals_Index_Zip.csv
read_csv('Zillow_Rentals_Index_Zip.csv')

Zillow_rentals_raw<- read_csv('Zillow_Rentals_Index_Zip.csv')

# Practice on your own#### 
# Read `gtd_raw.csv` into R and assing it to gtd_raw.

# DAL Activity ####

# Load the package 'tidyverse' using library()
library(tidyverse)

# Import the 'gtd_raw.csv' file as assign it to 'gtd_raw' ####
library(readr)
gtd_raw <- read_csv("gtd_raw.csv")
View(gtd_raw)

# Create Analysis data frame by assinging gtd_raw to gtd_df 
gtd_df <- gtd_raw

# View gtd_df in a separate window in this pane using the following command. 
View(gtd_df)

# Check structure of gtd_df
str(gtd_df)

# Select the columns we will need to make our visualizations - List these in the prompt. 
gtd_df <- gtd_df %>% 
  select(eventid, iyear, imonth, iday, summary, country, country_txt, region, region_txt, provstate, city, crit1, crit2, crit3, doubtterr, success, suicide, attacktype1, attacktype1_txt, attacktype2, attacktype2_txt, attacktype3, attacktype3_txt, targtype1, targsubtype1_txt, targtype2, targtype2_txt, targtype3, targtype3_txt, gname, gname2, gname3, nperps, nperpcap, motive, nkill, nkillus, nkillter, nwound, nwoundus, nwoundte, property, propextent, propextent_txt, propvalue, nhostkid, nhostkidus, nhours, ndays, ransom, ransomamt, ransompaid, hostkidoutcome, hostkidoutcome_txt, nreleased)

# Check structure of gtd_df
str(gtd_df)

# QuestionA: What do the rows (be careful and specific) and columns represent in this data set? Code a text string describing what you see to `Answer1` ####
AnswerA <- "The rows represent incidents. The columns represent the characteristics of incidents. We can also call them variables."

# QuestionB: What years does the data set span? Code the first year and last year in the numeric string ####
#HINT: Call summary(gtd_df$iyear)

summary(gtd_df$iyear)

AnswerB <- c(1970, 2018)

# Create Visualization of Number of Terrorist Attacks per Year - Fill in the blanks on the Bar Chart #### 
gtd_df %>% 
  filter(crit1 == 1 & crit2 == 1 & crit3 == 1) %>% 
  group_by(iyear) %>%  
  count() %>% 
  rename(`Number of Attacks` = n, Year = iyear) %>% 
  ggplot(aes(x= Year, y=`Number of Attacks`))+ 
  geom_bar(stat="identity")

# Question 1: What trends do we see? Does anything look peculiar? Code a text string describing what you see to `Answer1`####
Answer1 <- "An increase from 1970 to 1990s, a missing data set, a drop-off in attacks, and then another increase again between the 2000s and the 2010s."

# Use a table to identify how many terrorist attacks there were per year 
gtd_df %>% 
  filter(crit1 == 1 & crit2 == 1 & crit3 == 1) %>% 
  group_by(iyear) %>% 
  count() %>%  
  View()

# Question 2: What year shows no terrorist attacks? Why are we missing it. (HINT: refer to the GTD Code Book). Code your answer as a text string called 'Answer2'.  ####
Answer2 <- "1993 has no terrorist attacks. The data was lost and attempts to recreate it were not able to fully recapture the stats, so the year is left empty."

# Question 3: In how many incidents is there doubt as to whether the incident is terrorism? Code answer as numeric vector.####
# View the code book to find which variable tells us if there is doubt whether the indicident is terorism. We will need to group and count. 
gtd_df$doubtterr

# Code goes here
gtd_df %>%
  group_by(doubtterr) %>%
  count()

# Write your answer.
Answer3<-c(31060)

# Question 4: What percent of the total number of incidents in the data frame is there doubt about whether it is terrorism. Show both code and assign answer to a numeric vector? ####
gtd_df %>%
  group_by(doubtterr) %>%
  count()

Answer4<- c(16)

# Question 5:  What were the top 3 years in terms of number of terrorist attacks.Show both code and answer. Assign a numeric vector to Answer5. ####
gtd_df$'Number of Attacks'

gtd_df %>% 
 filter(crit1 == 1 & crit2 == 1 & crit3 == 1) %>% # A filter command goes here.
 group_by(iyear) %>% # A tidyerse command goes here. Which one? 
 count() %>% # A tidyerse command goes here. Which one? 
  arrange()

Answer5 <- c(2014, 2015, 2016) # 3 numeric elements go into this vector. 


# Explore Relationship between Number of Incidents and Number of Casualties, Types of Casualities #### 
# Code a dataframe with year, number of incidents, variables for casualties: `gtd_casualties_df`
gtd_casualties_df <- gtd_df %>% 
  filter(iyear >= 1970, crit1 == 1 & crit2 == 1 & crit3 == 1) %>% 
  group_by(iyear) %>%
  #Fill in the below blank with the correct tidyverse command.
  summarize(incidents = n(), casualities = (sum(nkill, na.rm = TRUE) + sum(nwound, na.rm = TRUE)), victims_killed = sum(nkill, na.rm = TRUE), victims_wounded = sum(nwound, na.rm = TRUE))


# Question 6: How many incidents, per year, did we exclude because they did not have casualty data? What percentage have we been missing per year? Answer this question by creating a dataframe with year, number of incidents missing data in nkill or nwound,  number of incidents, and calculate the percent per year that are missing. #####

# Create a dataframe with year and count of missing data. HINT: You will have to add conditions to filter. 
missing_casualties_df <- gtd_df %>% 
  filter(iyear >= 1970, crit1 == 1 & crit2 == 1 & crit3 == 1, is.na(nkill) & is.na(nwound)) %>% 
  group_by(iyear) %>% #What tidyverse command goes here? 
  count() %>% #What tidyverse command goes here? 
  rename(missing = n)

# Add total incidents per year to missing_casualties_data
missing_casualties_data <- left_join(missing_casualties_df, gtd_casualties_df, by = "iyear" ) 
# We will talk about joins next week. You get to see an early example. 

missing_casualties_df <- missing_casualties_data %>% 
  select(iyear:incidents) %>% 
  mutate(perc_missing = missing/incidents) # Fill in this blank with the correct formula. 

Answer6 <- missing_casualties_df

# You do not have to include this code in your data ####

# Create Example data frame
year <- seq(1990, 2020, 1)
rent <- rnorm(31, 1062, 400  )
rent<-sort(rent)
year_rent_df <- tibble(year,rent )

# Make a line plot #### 
year_rent_df %>% 
  ggplot(aes(x=year, y =rent)) + # Notice, we plot the year on the x-axis 
  geom_line() #The geometric layer is called 'geom_line'.

# DAL Activity ####

# Question7: What is the overall trend in the lethality of terrorist incidents? You need to answer this question with both a plot and a text description ####

# Create a data frame with year, number of incidents, number of casualties, number of victims killed, number of victims wounded, number of casualties / incident, number of victims killed /incident, number of victims wounded / incident. 
gtd_casualties_df <- gtd_casualties_df %>% 
  mutate(casualties_per_inc = casualities / incidents, victims_killed_per_inc = victims_killed / incidents, victims_wounded_per_inc = victims_wounded / incidents) 

# Create a line plot of victims_killed_per_inc by year
gtd_casualties_df %>% 
  rename(year = iyear) %>% 
  ggplot(aes(x=year)) + 
  geom_line(aes(y=victims_killed_per_inc), color = "red") 

# Create a line plot of victims_wounded_per_inc by year
gtd_casualties_df %>% 
  rename(year = iyear) %>% 
  ggplot(aes(x=year)) + 
  geom_line(aes(y=victims_wounded_per_inc), color = "darkred") 

# Create a plot that Combines all these lines
casualties_plot <- gtd_casualties_df %>% 
  rename(year = iyear) %>% 
  ggplot(aes(x=year))

# Add lines to casualties_plot
casualties_plot <- casualties_plot +
  geom_point(aes(y=victims_killed_per_inc, color = "Victims Killed per Incident")) + # Add the correct ggplot geometric layer. 
  geom_point(aes(y=victims_wounded_per_inc, color = "Victims Wounded per Incident"))
  # Add the correct ggplot geometric layer. 

# Add a legend 
colors <- c("Victims Killed per Incident" = "red", "Victims Wounded per Incident" = "darkred")
casualties_plot +
  scale_color_manual(values = colors )+
  labs(color = "",
       y="Number") +
  theme(legend.position = "bottom")

Answer7_plot <- "casualties_plot <- casualties_plot +
  geom_point(aes(y=victims_killed_per_inc, color = "Victims Killed per Incident")) +  
  geom_point(aes(y=victims_wounded_per_inc, color = "Victims Wounded per Incident"))"

Answer7 <- "The number of victims wounded and victims killed follow a similar plot. But in the late 90s and early 2000s, there were many more victims wounded per incident than victims killed per incident."

# Question8: What is the relationship between amount of ransom terrorists demanded and the actual amount of ransom paid. Do incidents in all regions follow the same pattern (i.e., add region as color or facet wrap, which one works better?)? Answer this question using a scatterplot and a text vector description? HINT: You may need to make 2 scatter plots (1 that includes all data and one that filters out outliers) ####
# Base Scatterplot 
gtd_df %>% 
  ggplot(aes(x=ransomamt, y=ransompaid))+
  geom_point()

# Color Approach 
# HINT: the format for scientific notation in R is 1e03 = 1000
gtd_df %>% 
  filter(ransompaid<1e08 & ransomamt < 2.5e08) %>% 
  ggplot(aes(x=ransomamt, y=ransompaid, color=region_txt))+
  geom_point()

# Color-region_txt

# Facet Wrap Approach 
gtd_df %>% 
  filter(ransompaid<1e08 & ransomamt < 2.5e08) %>% # What is the correct tidyverse command? 
 ggplot(aes(x=ransomamt, y=ransompaid))+
  geom_point()+ # What is the correct ggplot command? 
  facet_wrap(~region_txt, scales = "free", nrow=4)

Answer8_plot <- "gtd_df %>% 
  filter(ransompaid<1e08 & ransomamt < 2.5e08) %>% # What is the correct tidyverse command? 
 ggplot(aes(x=ransomamt, y=ransompaid))+
  geom_point()+ # What is the correct ggplot command? 
  facet_wrap(~region_txt, scales = "free", nrow=4)." 

Answer8<- "In South America and Western Europe, there is a positive correlation between the ransom amount and the amount paid. In Places like Central America & Carribean and Eastern Europe and Central Asia and Middle East a& North Africa and South Asia and Southeast Asia and East Asia and Australasia & Oceania and North America, there seems to be very little correlation between ransom amount and amaount paid."