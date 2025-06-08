library(tidyverse)
setwd('C:\\Users\\kylet\\Desktop\\KRC_Behav_ACC') #now with double back slashes!

#this code will take PAM or camera trap data files and plot presence of species

#-----------Part 1: PAM data---------------------------------------------------------------
#first the PAM data
pamdata = read.table('BirdNET_Results_Selection_Table.txt', sep = '\t', header = T)
#when using your own data, just change the file name

pamdata$Begin.Path[1] #view just the first value of the Begin.Path column

pamdata = pamdata %>%
  mutate(DateTime = sub(".*S(\\d{8}T\\d{6})\\..*", "\\1", Begin.Path), #here we extract the date and time from the file name
         DateTime = as.POSIXct(DateTime, format = "%Y%m%dT%H%M%S", tz = "UTC"), #change it to a datetime object
         RoundDate = floor_date(DateTime, unit = '1 hour')) #then round to the nearest hour for plotting
head(pamdata)

#plot histogram
ggplot(pamdata) +
  geom_histogram(aes(x = RoundDate)) +
  facet_wrap(~Common.Name, ncol = 2) +
  theme_classic()

#plot lines for each species
#first get the counts for each species at each hour interval
bird.count = pamdata %>%
  group_by(Common.Name, RoundDate) %>%
  summarise(Count = n())
head(data.count)

#make the plot
ggplot(bird.count) +
  geom_line(aes(x = RoundDate, y = Count, color = Common.Name)) +
  theme_classic() #a standard theme to make the figure look pretty

#ok that's not easy to read
#let's make a regression line and 'smooth' the data
ggplot(bird.count) +
  stat_smooth(aes(x = RoundDate, y = Count, color = Common.Name), method = 'loess', span = 2, se = F) +
  #to turn off the shaded region for the SE, just set se = F
  theme_classic()

#we can also add the raw data in the background
ggplot(bird.count) +
  #first add the raw data
  geom_point(aes(x = RoundDate, y = Count, color = Common.Name), size = 5, alpha = 0.7) +
  #then add the lines
  stat_smooth(aes(x = RoundDate, y = Count, color = Common.Name), method = 'loess', span = 2, se = F) +
  theme_classic()

#we can also split the plot into panels for each species
ggplot(bird.count) +
  geom_point(aes(x = RoundDate, y = Count, color = Common.Name), size = 3, alpha = 0.7,) +
  stat_smooth(aes(x = RoundDate, y = Count, color = Common.Name), method = 'loess', span = 2, se = F) +
  facet_wrap(~Common.Name, ncol = 2) + #this splits it into panels by Common.Name, ncol specifies the column number
  theme_classic() + theme(legend.position = 'none')


#--------Part 2: camera trap data---------------------------------------------
#now camera trap data
camera.data = read.csv("TimeLapseData.csv", header = T)
head(camera.data)

camera.data = camera.data %>%
  mutate(DateTime = ymd_hms(DateTime),
         RoundDate = floor_date(DateTime, unit = 'hour'))
str(camera.data) #check the structure of the data to make sure it makes sense
#if the times are off by an hour or so run the following code
#just remove the #

#camera.data = camera.data %>%
#  mutate(DateTime = DateTime + hours(2))

#make a histogram
ggplot(camera.data) +
  geom_histogram(aes(x = RoundDate)) +
  facet_wrap(~Species, ncol = 2) +
  theme_classic()

#the data includes blank images so we can filter those out
#method 1: apply a filter then use %>% to plot the data
filter(camera.data, Species != '') %>% #first we filter out the blank species
  ggplot() + #since we are using the filtered data we don't need to specify the dataframe inside the ggplot
  geom_histogram(mapping = aes(x = RoundDate)) +
  facet_wrap(~Species, ncol = 2) +
  theme_classic()

#method 2: apply a filter within ggplot
ggplot(subset(camera.data, Species != '')) + #here we use the subset() command list the data we want to subset then the filter
  geom_histogram(mapping = aes(x = RoundDate)) +
  facet_wrap(~Species, ncol = 2) +
  theme_classic()
#these two methods create the same plot

#you can also just apply the filter to the data once
camera.data.filter = camera.data %>%
  filter(Species != '')

#plot lines for each species
#first get the counts for each species
camera.count = camera.data.filter %>%
  group_by(Species, RoundDate) %>%
  summarise(Count = n())
head(camera.count)

#make a line graph
ggplot(camera.count) +
  geom_line(aes(x = RoundDate, y = Count, color = Species)) +
  theme_classic() #a standard theme to make the figure look pretty

#split figure into panels for each species
ggplot(camera.count) +
  geom_line(aes(x = RoundDate, y = Count, color = Species)) +
  facet_wrap(~Species, ncol = 2) + #split the panels
  theme_classic() 


#make a smooth line plot
ggplot(camera.count) +
  geom_point(aes(x = RoundDate, y = Count, color = Species), size = 5, alpha = 0.7) +
  #then add the lines
  stat_smooth(aes(x = RoundDate, y = Count, color = Species), method = 'loess', se = F) +
  theme_classic()

#split the plot into panels for each species
ggplot(camera.count) +
  geom_point(aes(x = RoundDate, y = Count, color = Species), size = 3, alpha = 0.7,) +
  stat_smooth(aes(x = RoundDate, y = Count, color = Species), method = 'loess', span = 2, se = F) +
  facet_wrap(~Species, ncol = 2) + #this splits it into panels by Common.Name, ncol specifies the column number
  theme_classic() + theme(legend.position = 'none')
