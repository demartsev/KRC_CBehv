#Camera trap data analysis
#by Kyle Finn
#kyletfinn@gmail.com
#camera trap data from TimeLapse practice data set available at https://saul.cpsc.ucalgary.ca/timelapse/
#hourly weather data for Kalahari Research Centre for 7-9 June 2023 downloaded from the ERA5-Land Hourly dataset 
#hosted by Copernicus Climate Change Service https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview

#First load the packages you will need for this exercise
#to load multiple packages at once combine multiple instances of library() with a semicolon (;)
library(tidyverse) ; library(lubridate)

#-----Step 1: loading the data-------------------------------------
#load in the data using read.csv()
#to let R know that the first line contains our column names we add "header = T" inside the parentheses
#when loading data name the dataframe something intuitive but unique
station1 = read.csv("TimelapseData-station1.csv", header=T)

#it's always a good idea to check that the data loaded correctly
#sometimes text files have different separators (or sometimes extra commas) making R confused
#so will view the first few lines of the data frame
head(station1)

#good, everything looks like what we expect it to look like

#-----Step 2: data manipulation-------------------------------------
#now we will start to do some data manipulation
#first let's change some of the column names
#we will use the rename() from dplyr
#the syntax is rename(data, NewName = OldName), for more info search for rename in the help tab or run help(rename)
#remember to always name a dataframe or else R will just print the results in the bottom console
#since we are manipulating the station1 dataframe we will just keep the same name for the dataframe
station1 = rename(station1, Station = RootFolder)
station1 = rename(station1, Session = RelativePath)

#now let's remove rows where no animals were photographed
#let's do a filter to filter out the rows without images
#when using filter you need to have 2 equal signs, this also allows for other expressions
#like >=,<=,or != which are greater than or equal to, less than or equal to, or not equal to (respectively)
#so we are going to filter where the DeleteFlag column is false to keep the rows with animals seen
station1 = filter(station1, DeleteFlag == 'false')

#now let's add a column showing that an animal was "active" at the time when it's picture was taken
#we will use another dplyr function called mutate()
#this function is probably the one I use the most because it's so versitile to add new columns or to make calculations
#basically what mutate does is create a new column in the dataframe or modifies an existing column
#if the column already exists in the data it will replace the data in the column
#otherwise it creates a new column
station1 = mutate(station1, Active = 1) #here we are making a new column "Active" and setting the value equal to 1
head(station1)

#there's still an extra column "X" so let's remove that using select()
#you can either do select(Station, File, Session, etc) 
#or simply select(!X) which says select all the columns except X
station1 = dplyr::select(station1, !c(X, DeleteFlag))

#if you have multiple animals per image
#add a column called comment to your template
#write the species in this column, export as csv 
#import into R and run this
station1a = station1 %>%
  #this next part will add new columns from text in a column separated by a space 
  #the syntax is (column_name, delim = "what separates the text", 
  #               names = c("new_column_name1", "new_column_name2", etc),
  #               too_few = this tells R what to do if there's not 2 words in the column
  #                         here we are saying if nothing when starting the function, fill with NA)
  separate_wider_delim(Comment, delim = " ", 
                       names = c("Species1", "Species2"), 
                       too_few = 'align_start') %>%
  #no we filter to remove rows where there are blanks in the Species2 column
  #is.na(column_name) will find NAs, adding the ! tells it to search for NOT NA)
  filter(!is.na(Species2)) %>%
  #replaces the Species with Species2
  mutate(Species = Species2)
#then bind the two dataframes back together and arrange them
station1b= bind_rows(station1, station1a) %>%
  arrange(RootFolder,DateTime)
  
#ok now let's import data from the other camera stations
#we're doing do it a bit faster now using a tidyverse code called pipes
#pipes "%>%" allow you to string multiple lines of code together and run all at once
station2 = read.csv("TimelapseData-station2.csv", header=T) %>% #first load the data as station2 and add the first pipe
  rename(Station = RootFolder,        #now we can just rename the columns without having toe say station2 because it's linked to the read.csv function
         Session = RelativePath) %>%  #and we can add multiple renames within the parentheses 
  filter(DeleteFlag == 'false') %>%   #then filter out the rows without animals
  mutate(Active = 1) %>%              #add the Active column setting the value to 1  
  dplyr::select(!c(X, DeleteFlag))                          #drop the extra blank column
#see now we have done the same as above but more efficiently

#and repeat for the 3rd station
station3 = read.csv("TimelapseData-station3.csv", header=T) %>% 
  rename(Station = RootFolder,  
         Session = RelativePath) %>%
  filter(DeleteFlag == 'false') %>% 
  mutate(Active = 1) %>%            
  dplyr::select(!c(X, DeleteFlag)) 
#now we can join the 3 dataframes together for analysis
cameradata = rbind(station1,station2,station3)

#like anything in R there's multiple ways to do things
#we could have loaded each of the dataframes first, and then joined them
#then filtered out the delete rows, changed column names, added the Active column etc
#but sometimes if the data is very large it may be easier to manipulate each dataframe separately to avoid errors


#lastly let's check the format of our dataset using str()
str(cameradata)
#this shows us what the data type is for each column
#notice that the DateTime column is "chr" or character
#we need to change the format of the data and time so that R can understand and plot it
#we will change it to what is called a POSIXct format using the mutate() function
#we are manipulating the same data so we keep the name of the dataframe ('cameradata = ')
#then tell R to use cameradata and then do the functions after each pipe ('cameradata %>%')
cameradata = cameradata %>%  
  mutate(DateTime = ymd_hms(DateTime))   
#here the ymd_hms() function from the lubridate package tells R the format of the date and time is year month day hours minutes seconds and is located in DateTime column
#let's check to make sure the format changed
str(cameradata)
#great the DateTime column is now POSIXct

#-----Step 3: analysis-------------------------------------
#let's try some simple activity analysis
#let's see if animals are consistantly active at a certain hour across days
#but we have multiple images of some animals so that could affect our analysis
#let's remove those extra images and keep just 1 image (ie data row) for each species per hour
#we will do two different ways to solve this problem

#Option 1
#to avoid changing the original dataframe we will call it 'cameradata1'
#then tell R to use cameradata and then do the functions after each pipe
cameradata1 = cameradata %>%
  filter(ImageNum == 1) #the easiest way is just to filter and take the first image
                        
#alternatively if you have thousands of images and don't want to add image number in TimeLapse you can do a work around
#we will call this cameradata2 so we can check that cameradata1 and cameradata2 have the same number of rows
cameradata2 = cameradata %>%
  #now we will add a new column with mutate
  #do floor_date() which rounds down the time to the hour in which the image was taken
  #note you can do floor_date for seconds, minutes, or intervals 5 minutes, 30 minutes, etc. see help(floor_date)
  mutate(Hour = floor_date(DateTime, 'hour')) %>% 
  #now we will remove a few columns so that some rows are duplicates
  #as before the ! inside select() says to remove this row, but since we have multiple rows we make a list inside the 'c()' function
  dplyr::select(!c(File, DateTime, ImageNum)) %>%
  #now we tell R to remove the duplicated rows
  distinct()

#now check to make sure the two dataframes have the same number of observations

#ok now let's make a bar graph of when the different species were active within a 24 hour period
#let's use the cameradata2 since the time is already rounded to the hour in which an image was taken
#first we need to remove the date part of the Hour column
cameradata = cameradata2 %>%
  #good ol' mutate again
  mutate(Hour = hour(Hour)) #the easiest way is just use the hour() function to extract the hour
#let's check the format of the dataframe
str(cameradata)
#the hour() function converts it back to a number so it is no longer a POSIXct format
#this is acceptable for our figure purposes but see below if you want to keep it in POSIXct for a figure

#ok now lets plot the data
ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge')

#hmm.. we expect values between 0 and 1, so there's still a few duplicate counts 
#lets fix that
cameradata = cameradata %>%
  dplyr::select(!Session) %>% #we remove the Session column because some animals are recorded in the same hour for each session
  distinct() #then take just unique rows

#then replot
ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge')

#the plot is a bit messy (and there's still 2 species with 2 counts), let's make it better
#with ggplot to modify a plot you just add new arguments with a + sign
ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge') +
  facet_grid(~Station) +  #we split the data into panels by the Location (Station) of the camera trap
  scale_x_continuous("Hour", limits = c(0,24), breaks = seq(0,24,2)) + #then change the x-axis
  #first set the label as "Hour", then the min and max with limits, and then the increments with a sequence inside breaks
  scale_y_continuous("Active", limits = c(0,1), breaks = seq(0,1,1)) #repeat for the y-axis
 
#we could instead facet it by species
ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge') +
  facet_grid(~Species) +  #now split by species
  scale_x_continuous("Hour", limits = c(0,24), breaks = seq(0,24,2)) + 
  scale_y_continuous("Active", limits = c(0,1), breaks = seq(0,1,1)) 

#better but the x-axis is hard to read, lets rotate the labels with a theme
#themes let you change the overall appearance of a graph 
ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge') +
  facet_grid(~Station) +  
  scale_x_continuous("Hour", limits = c(0,24), breaks = seq(0,24,2)) + 
  scale_y_continuous("Active", limits = c(0,1), breaks = seq(0,1,1)) +
  theme(axis.text.x = element_text(angle = 90)) 
#here the theme just changes the text of the x-axis, specifically adjusting the angle of the text by 90 degrees
#when making figures I often use one standard theme for all of them so they are consistant
#so I just make my own R object called "mytheme" with the custom ggplot theme I want to use
mytheme<- theme_light() + 
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), axis.title=element_text(size=6),
        panel.grid = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour="black", linewidth = 0.25), 
        axis.text = element_text(size = 6), 
        axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(size = 6), 
        axis.ticks = element_line(colour = "black"))

ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge') +
  facet_grid(~Station) +  
  scale_x_continuous("Hour", limits = c(0,24), breaks = seq(0,24,2)) + 
  scale_y_continuous("Active", limits = c(0,1), breaks = seq(0,1,1)) +
  mytheme 

#there's some annoying white space above and below the bars, lets remove that 
ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge') +
  facet_grid(~Station) +  
  scale_x_continuous("Hour", limits = c(0,24), breaks = seq(0,24,2)) + 
  scale_y_continuous("Active", limits = c(0,1), breaks = seq(0,1,1)) +
  coord_cartesian(expand = FALSE) + mytheme

#to save a figure there's a few options
#the ggsave() function gives you the most control over the resolution which is very important for publications
#but you need to name your figure in R
Fig1 = ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge') +
  facet_grid(~Station) +  
  scale_x_continuous("Hour", limits = c(0,24), breaks = seq(0,24,2)) + 
  scale_y_continuous("Active", limits = c(0,1), breaks = seq(0,1,1)) +
  coord_cartesian(expand = FALSE) + mytheme
#now we save it using ggsave('save_file_name.png/jpg', object to save, units = metric or pixels, dimensions and resolution)
#a minimum resolution of 300 is recommended
#when changing the dimensions and resolution you will need to adjust the text size in the theme with element_text(size = ?)
ggsave('Fig1.png', Fig1, units = "px", width=2000, height=2000, dpi=600)

#naming your figure also allows you to add other functions to it 
#so let's change mytheme and add it to the figure
mytheme2<- theme_light() + 
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), axis.title=element_text(size=6),
        legend.title = element_blank(), legend.text = element_text(size = 4),
        legend.key.width = unit(5, 'mm'), legend.key.height = unit(3, 'mm'), 
        legend.background = element_blank(), 
        legend.box.background = element_rect(color = 'black', linewidth = 0.1), 
        legend.box.margin = margin(t = 0.1, l = 0.1),
        panel.grid = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour="black", linewidth = 0.25), 
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 6), strip.text.x = element_text(size = 6), 
        axis.ticks = element_line(colour = "black"))
Fig1a = Fig1 + mytheme2
#to view your figure just type in what it is called
Fig1a
#also see how the legend has changed
#we added some extra functions to the theme to change the legend
ggsave('Fig1a.png', Fig1a, units = "px", width=2000, height=2000, dpi=600)

#---------alternative plot---------------------------
#sometimes you need the X-axis in POSIXct to display the hours properly
#so we now will convert the hour column to a HH:MM format for the figure x-axis
cameradata = cameradata2 %>%
  mutate(Hour = as.POSIXct(format(Hour, "%H:%M"), format = '%H:%M', tz = 'UTC'))
#ok this works, we now have a generic date with the time in HH:MM format
#but we now have some duplicate records for some hours, so we will do the distinct() function again
cameradata = cameradata %>%
  dplyr::select(!Session) %>% #we need to remove the session column 
  distinct() #then take just the unique rows

ggplot(cameradata, aes(x = Hour, fill = Species)) + geom_bar(position='dodge') +
  facet_grid(~Station) +
  scale_x_datetime("Hour", date_labels = "%H:%M", date_breaks = "2 hour") +
  scale_y_continuous("Active", limits = c(0,1), breaks = seq(0,1,1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#--------climate stuff-------
#now let's add some climate data to see if air temp affects activity
#read in the temp data
temp = read.csv('KMP_temp.csv', header = T) %>%
  mutate(DateTime = ymd_hms(DateTime), #convert the date to a POSIXct format
         DateTime = DateTime %m+% days(368)) %>% #here we're adding a year to the date so it matched with our camera dates
  dplyr::select(!X) #drop the X column
head(temp)
str(temp)

#we have temperature for every hour for a 24 hour period
#but our camera trap data is missing some hours, so we need to expand the dataframe to include the missing hours
#set the minimum and maximum date to the current system date
cameradata3 = cameradata %>%
  #we will use the complete() function to fill in the missing hours for each Station
  #we do a sequence starting at 00:00:00 and ending at 23:00:00, increasing by 1 hour
  #the fill argument tells R to at a 0 to the Active column when adding the extra rows
  #!!NOTE: here you will have to change the dates to correspond to the 10th when the lecture occurs!!
  complete(Station, Hour = seq(min(ymd_hms("2024-06-10 00:00:00")), max(ymd_hms("2024-06-10 23:00:00 UTC")), by = '1 hour'), fill = list(Active = 0)) %>%
  #then you can sort the data so that the hours increase within each station
  arrange(Station,Hour)
head(cameradata3)

#now lets join the cameradata to the temp data
cameratemp = cameradata3 %>%
  left_join(temp, join_by(Hour == DateTime))

#let's do a simple regression to see if temperature affects activity
#linear regression is lm(dependent variable (what is being affected) ~ independent variables (potential factors affecting it), data)
mod1 = lm(Active ~ Tair, cameratemp)
#once the model is built then we do summary() on the model to get the statistical output
summary(mod1) 

#note that a linear model is not the best method for binary data (1s and 0s)
#a negative binomial is better, and you need to use other models and packages for that
#for example using the glmmTMB package
library(glmmTMB)
model = glmmTMB(Active ~ Tair,
                family ="nbinom2", data = cameratemp)
summary(model)
#see now the p-value is no longer significant

