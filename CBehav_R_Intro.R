packages = c("tidyverse", "lme4", "bioacoustics", "warbleR", "Rraven",
             "tuneR", "seewave", "soundgen", "TTR", "behavr", "ggetho",
             "zeitgebr", "sleepr", "scales", "chron", "sp", "raster",
             "move", "ggmap", "mapproj", "lattice", "RColorBrewer", 
             "adehabitatHR", "ks", "XML", "patchwork",
             "viridis",  "cowplot", "rgeos", "ohun", "pracma", "digiRhythm")
lapply(packages, FUN = install.packages, character.only = TRUE)

#load in packages
library(chron)
library(sp)
library(raster)
library(rgeos)
library(move)
library(RColorBrewer)
library(viridis)
library(lattice)
library(adehabitatHR)
library(ks)
library(tidyverse)
library(XML)
library(Rraven)
library(warbleR)
library(tidyverse)
library(cowplot)
library(soundgen)
library(ohun)

#another method
library(chron); library(sp); library(raster) ; library(rgeos); library(move)
library(RColorBrewer); library(viridis); library(lattice); library(adehabitatHR)
library(ks); library(tidyverse); library(XML); library(Rraven)
library(warbleR); library(cowplot); library(soundgen); library(ohun)

#yet another method
lapply(packages, FUN = library, character.only = TRUE)

#---STOP!!-----
#only run the next part if you need to update R and it's packages
##update R
install.packages("installr")
library(installr)

updateR()

#set the path to install packages
#location can be checked in Global Options -> General
.libPaths("C:/Program Files/R/R-4.5.3/library") #change the path to the correct version of R
library(tools)
update.packages()

#--Data Frames--------------------------------------------------------------#
#first set your working directory
#like most things in R, there's a multitude of methods
#Method 1: coding
setwd('C:\\Users\\klfiaa\\OneDrive\\Rwork')
#check your working directory
getwd()
#Method 2: Global Options
#click Tools -> Global Options -> General and navigate to the location you want
#for example I set mine to my OneDrive so I can access it across devices

#load in a data set
dice = read.csv('dicerolls.csv',header = T) 

#view the first few rows
head(dice)

#view the type of data in each column
str(dice)
#the data is stored as an integer

#find the min, max, mean, and standard devidation
min(dice$Roll)
max(dice$Roll)
mean(dice$Roll)
sd(dice$Roll)

#get counts of each value
dice.sum = dice %>%
  #first need to group by a value to get the counts for each value
  group_by(Roll) %>%
  summarise(count = n())

#plot the distribution of the counts
ggplot(dice) + 
  geom_histogram(aes(x = Roll), binwidth = 1, fill = 'white', color = 'black') +
  scale_x_continuous(breaks = seq(1,20,1))

#test if distribution of values significantly different than expected
chisq.test(dice$Roll)

#another example
#load in the first dataset
lervik = read.csv('Lervik.csv',header=T) 
head(lervik)
str(lervik)

#change the DateTime column to a different format
lervik$DateTime = ymd_hms(lervik$DateTime)
lervik$Location = 'Lervik'

#another way to do the same thing
lervik2 = lervik %>%
  #mutate here lets us change the data
  #if the column exists it will overwrite the data in that column
  mutate(DateTime = ymd_hms(DateTime),
         #if the column doesn't exist, it will be added
         Location = 'Lervik')
head(lervik)
head(lervik2)
str(lervik)
str(lervik2)
#the date and time is now in a format R recognizes for date times

#repeat with the other 2 datasets
#you can load the data and change it all at once
navrean = read.csv('Navrean.csv',header=T) %>%
  mutate(DateTime = ymd_hms(DateTime),
         Location = 'Navrean')
head(navrean)
okno = read.csv('Okno.csv',header=T) %>%
  mutate(DateTime = ymd_hms(DateTime),
         Location = 'Okno')
head(okno)

#merge the 3 data sets into a new dataset called 'temps'
#this works when all the datasets have the exact same column names
temps = rbind(lervik, navrean, okno) 

#get a list of the locations in the dataset
unique(temps$Location)

#check the end times for each location
temps %>%
  group_by(Location) %>%
  summarise(min = min(DateTime),
            max = max(DateTime))

#filter the data so that the beginning and end times match 
temps_filter = temps %>% 
  filter(between(DateTime, ymd_hms('2025-03-20 00:00:00'), ymd_hms('2025-06-24 23:59:59')))

#calculate the mean daily temp for each location
mean.temps = temps_filter %>%
  mutate(Day = as_date(DateTime)) %>%
  group_by(Location,Day) %>%
  summarise(mean = mean(Temp), sd = sd(Temp))

#plotting
#for help with plotting with ggplot see https://r-graphics.org/
#pdf available at: https://drive.google.com/file/d/1j4agCXSdYz0iGKFdjA1PxJx6KvL92flt/view?usp=sharing
ggplot(mean.temps, aes(x = Day, y = mean, color = Location)) +
  geom_line(linewidth = 1) +
  labs(title = 'Mean Water Temperature', x = 'Date', y = 'Temperature') 

#we can also add the standard deviation by using geom_ribbon
ggplot(mean.temps) +
  geom_ribbon(aes(x = Day, ymin = (mean - sd), ymax = (mean + sd), fill = Location), alpha = 0.1) +  
  geom_line(aes(x = Day, y = mean, color = Location), linewidth = 1) +
  #adjust the x-axis scale by changing the breaks and labels
  scale_x_datetime(breaks = date_breaks("14 days"),
                   labels = date_format("%d %b")) +
  #same with y-axis
  scale_y_continuous(labels = seq(0,25,5), limits = c(0,25)) +
  labs(title = 'Mean Water Temperature', x = 'Date', y = 'Temperature') 

#you can modify the appearance using themes
#see ?ggplot2::theme
ggplot(mean.temps) +
  geom_ribbon(aes(x = Day, ymin = (mean - sd), ymax = (mean + sd), fill = Location), alpha = 0.1) +  
  geom_line(aes(x = Day, y = mean, color = Location), linewidth = 1) +
  #adjust the x-axis scale by changing the breaks and labels
  scale_x_datetime(breaks = date_breaks("14 days"),
                   labels = date_format("%d %b")) +
  #same with y-axis
  scale_y_continuous(labels = seq(0,25,5), limits = c(0,25)) +
  labs(title = 'Mean Water Temperature', x = 'Date', y = 'Temperature') +
  theme_classic()

#sometimes you may want the data in different columns
# you can switch it from 'long' to 'wide'
mean.temps.wide = mean.temps %>%
  select(!sd) %>% #the sd column is not needed so we will remove it
  pivot_wider(names_from = Location, values_from = mean)

#then you can plot one line for each dataset
ggplot(mean.temps.wide) +
  geom_line(aes(x = Day, y = Lervik), color = 'red', linewidth = 1) +
  geom_line(aes(x = Day, y = Okno), color = 'blue', linewidth = 1) +
  geom_line(aes(x = Day, y = Navrean), color = 'green', linewidth = 1) +
  scale_x_datetime(breaks = date_breaks("14 days"),
                   labels = date_format("%d %b")) +
  scale_y_continuous(labels = seq(0,25,5), limits = c(0,25)) +
  labs(title = 'Mean Water Temperature', x = 'Date', y = 'Temperature') +
  theme_classic()

#since the lines for Navrean and Okno overlap, we can also plot them separately
ggplot(mean.temps) +
  geom_ribbon(aes(x = Day, ymin = (mean - sd), ymax = (mean + sd), fill = Location), alpha = 0.1) +  
  geom_line(aes(x = Day, y = mean, color = Location), linewidth = 1) +
  facet_wrap(~Location, ncol = 1) +
  scale_x_datetime(breaks = date_breaks("14 days"),
                   labels = date_format("%d %b")) +
  scale_y_continuous(labels = seq(0,25,5), limits = c(0,25)) +
  labs(title = 'Mean Water Temperature', x = 'Date', y = 'Temperature') +
  theme_classic()

#another way to do that is to do a filter then link to ggplot
#you can also save a plot to the environment by naming it
#this allows for more control over individual plots if needed
okno.plot = mean.temps %>%
  filter(Location == 'Okno') %>%
  ggplot() +
  geom_ribbon(aes(x = Day, ymin = (mean - sd), ymax = (mean + sd)),
              fill = 'blue', alpha = 0.1) +
  geom_line(aes(x = Day, y = mean), color = 'blue', linewidth = 1) +
  scale_x_datetime(breaks = date_breaks("14 days"),
                   labels = date_format("%d %b")) +
  scale_y_continuous(labels = seq(0,25,5), limits = c(0,25)) +
  labs(title = 'Okno', x = 'Date', y = 'Temperature') +
  theme_classic()

navrean.plot = mean.temps %>%
  filter(Location == 'Navrean') %>%
  ggplot() + 
  geom_ribbon(aes(x = Day, ymin = (mean - sd), ymax = (mean + sd)),
              fill = 'green', alpha = 0.1) +  
  geom_line(aes(x = Day, y = mean), color = 'green', linewidth = 1) +
  scale_x_datetime(breaks = date_breaks("14 days"),
                   labels = date_format("%d %b")) +
  scale_y_continuous(labels = seq(0,25,5), limits = c(0,25)) +
  labs(title = 'Navrean', x = 'Date', y = 'Temperature') +
  theme_classic()

lervik.plot = mean.temps %>%
  filter(Location == 'Lervik') %>%
  ggplot() + 
  geom_ribbon(aes(x = Day, ymin = (mean - sd), ymax = (mean + sd)),
              fill = 'red', alpha = 0.1) +  
  geom_line(aes(x = Day, y = mean), color = 'red', linewidth = 1) +
  scale_x_datetime(breaks = date_breaks("14 days"),
                   labels = date_format("%d %b")) +
  scale_y_continuous(labels = seq(0,25,5), limits = c(0,25)) +
  labs(title = 'Lervik', x = 'Date', y = 'Temperature') +
  theme_classic()

#then you can merge them together into one plot
fig1 = navrean.plot / okno.plot / lervik.plot + plot_layout(axes = "collect")
fig1

#save the figure to your working directory for publication
#create png, jpg, tiff files
ggsave('Fig1.png', fig1, width = 1800, height = 1800, units = 'px', dpi = 300)
#can also create vectorized graphics which retain resolution when scaled
#an eps file
ggsave('Fig1.eps', fig1, width = 6, height = 12, units = "in", dpi = 600)
#a pdf file
ggsave('Fig1a.pdf', fig1, width = 8, height = 11, units = "in")
