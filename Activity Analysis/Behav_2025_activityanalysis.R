#---------------------------------------------------------------------------------------------#
#----------------------------Activity Analysis using Accelerometers---------------------------#
#----------------------------Collective Behaviour Field Course 2025---------------------------#
#--------------------------------------by: Kyle Finn------------------------------------------#
#-----------------------------------kyletfinn@gmail.com---------------------------------------#
#---------------------------------------------------------------------------------------------#

#this analysis uses the data from accelerometers given to students
#it uses code modified from Finn et al. 2025 Ultradian rhythms of activity in a wild subterranean rodent
#see that code and dataset for an example of a real analysis on how temperature affects activity of molerats
#https://doi.org/10.1098/rsbl.2024.0401 and data at https://datadryad.org/dataset/doi:10.5061/dryad.7pvmcvf2w

#this analysis uses the rethomics framework
#see https://rethomics.github.io/index.html for more details and examples
setwd("C:/Users/....") #you can manually set your working directory here if you have not done it in settings

#load in the packages we need the ; here allows you to load all these at once instead of individually
library(tidyverse) ; library(TTR) ; library(behavr) ; library(ggetho) ; library(zeitgebr) ; library(sleepr) ; library(scales)


#note that raw accelerometer data files are recorded at 25hz (25 recordings/second) 
#data taken over 2 weeks can have over 11.5 million rows and be almost 2GB in size!
#our pike data recorded for 1 year is 6-7GB each
#so for very large files like this you need a computer with quite a lot of ram (minimum 16gb, preferably 32gb+)
#or you will likely have to run each file one by one, save as a csv file, and clear the RStudio memory before doing the next

#_______________________________Section I: Analyze Student ACC Data________________________________________

#----------------------------Part I: Load and Modify Data for Analysis-------------------------------------
#-----Step 1: view files-----------------------------
#to view all the files in your working directory you can list them
#this helps with loading the next file without having to look in the folder
list.files(pattern = "\\.csv$", full.names = T)

#-----Step 2: load data and view-----------------------------
#load in student ACC data
#we create a new data frame called acc.data by reading the file name in our working directory
#since the file is separated by tabs (not commas) read.table is better
acc.data = read.table('5_test_S3.csv', sep = '\t', header = T) %>%  #remember to change the file name if doing multiple files
  mutate(Timestamp = ymd_hms(Timestamp, tz = 'UTC')) #this makes the Timestamp column a date and time object
                                        #tz sets the time zone, otherwise the times may be off by an hour or two
#even though our files are relatively short, it still takes a lot of memory to work with them

#let's look at the first few lines to see if the file loaded as expected
head(acc.data)

#let's do some quick summary stats
min(acc.data$Timestamp) #start date and time
max(acc.data$Timestamp) #end date and time

#let's also do a quick view of what the data looks like
LiftFinder = filter(acc.data, between(Timestamp, ymd_hms("2025-05-13 12:55:00"), ymd_hms("2025-05-13 13:05:00")))
ggplot(LiftFinder) +
  geom_line(aes(x = Timestamp, y = accX), color = "red") +  
  geom_line(aes(x = Timestamp, y = accY), color = "blue") +
  geom_line(aes(x = Timestamp, y = accZ), color = "black") +
  scale_x_datetime("Date", date_breaks = "1 sec", guide = guide_axis(angle = 90)) + 
  scale_y_continuous("Acceleration (g)", limit = c(-4,4))

#-----Step 3: calculate OBDA-----------------------------
#-----Step 3a: OBDA function-----------------------------
#to process the data we will calculate something called ODBA
#this is overall dynamic body acceleration, a value which can tell us if an animal is moving or not
#we then calculated a mean ODBA from this raw data using the following function
#which takes the mean XYZ acceleration values over 2 second windows to calculate ODBA

#we will build a custom function to calculate the ODBA
fun_ODBA<-function(X,Y,Z){ 
  
  #Calculate baseline running mean for 2 second interval (50 data points)
  accXSmooth<-TTR::runMean(X,50) #the 'TTR::' specifies the function is in the TTR package
  accYSmooth<-TTR::runMean(Y,50)
  accZSmooth<-TTR::runMean(Z,50)
  
  #Subtracting baseline from X,Y,Z and make all values positive (absolute values)
  accX_DBA<-abs(X-accXSmooth)
  accY_DBA<-abs(Y-accYSmooth)
  accZ_DBA<-abs(Z-accZSmooth)
  
  #Add transformed values from X,Y and Z together
  accX_DBA+accY_DBA+accZ_DBA
}

#-----Step 3b: get OBDA values-----------------------------
#run the function on our dataset to calculate an OBDA for each row
acc.data = acc.data %>%  #again we create a new dataframe called acc.data, using the old acc.data 
  #in otherwords we are just modifying the acc.data dataframe
  mutate(ODBA = fun_ODBA(accX, accY, accZ)) #makes a new column which runs the ODBA function with X, Y, Z equal to accX/Y/Z
head(acc.data, n = 50) #view the first 50 lines to see if it makes sense

#-----Step 3c: time to every 2 seconds-----------------------------
#since the ODBA values overlap we will remove the extra values
#we will only keep every 50th row
#then create a file for each animal
ODBA = acc.data %>% 
  mutate(AnimalID = "5") %>% #adds AnimalID as an identifier
  #!!! remember to change the AnimalID for each animal !!!
  drop_na(ODBA) %>% #remove the first 49 rows with NA in ODBA column, technically removes every row with an NA value in the given column
  slice(which(row_number() %% 50 == 1)) %>% #now starting with the 1st line keep every 50th row
  select(AnimalID, Timestamp, accX, accY, accZ, ODBA) #keep only these columns 
head(ODBA) #once again check the first few rows to see if it makes sense, it is every 2 seconds so that's good
str(ODBA)
#now save the data as a csv file, changing the AnimalID to match the data you loaded in
write.csv(ODBA, file = "5_ODBA.csv", row.names = F) #the row.names = F prevents R from adding a column with unique values
#!!! remember to change the file name here to  match line 94!!!
#repeat from Step 2 for each individual, changing the AnimalID in line 94 and line 102

#-----Step 4: create dataset for analysis-----------------------------
#-----Step 4a: merge all files into one-----------------------------
#now that we have created all the ODBA files for each 'animal' we bind them into a single data file
#first view the files you have to get the file names
list.files(pattern = "\\.csv$", full.names = T)

# first load in each animal's ODBA file
acc1 = read.csv("1_ODBA.csv")
acc2 = read.csv("2_ODBA.csv")
acc3 = read.csv("3_ODBA.csv")
acc4 = read.csv("4_ODBA.csv")
ODBA = read.csv("5_ODBA.csv")
acc6 = read.csv("6_ODBA.csv")
acc7 = read.csv("7_ODBA.csv")
acc8 = read.csv("8_ODBA.csv")
acc9 = read.csv("9_ODBA.csv")
acc10 = read.csv("10_ODBA.csv")
acc11 = read.csv("11_ODBA.csv")
acc12 = read.csv("12_ODBA.csv")
acc13 = read.csv("13_ODBA.csv")
acc14 = read.csv("14_ODBA.csv")
acc15 = read.csv("15_ODBA.csv")
acc16 = read.csv("16_ODBA.csv")

#...and in the R bind them 
ODBA = rbind(acc1, acc2, acc3, acc4, acc5, acc6, acc7, acc8, 
             acc9, acc10, acc12, acc13, acc14, acc15, acc16) #rbind joins datasets which have the same number and spelling of columns
  #note that if columns are spelled slightly differently then it wont work (Animal.ID vs AnimalID)
ODBA$AnimalID = as.numeric(ODBA$AnimalID)  
head(ODBA)
str(ODBA) #this checks the type of data stored in each column
#you may need to check which of the times did not load properly, this will do that for you
which(is.na(ODBA$Timestamp))

#some more summary stats of the data
unique(ODBA$AnimalID) #this will list the animal IDs in the dataframe
length(unique(ODBA$AnimalID)) #this will count the number of unique animal IDs in the dataframe

#-----Step 4b: determine ODBA threshold-----------------------------
#to know when animals are active or not we need to view all individuals and determine what the ODBA threshold is
ggplot(ODBA) + #here we're using the ODBA data
  geom_line(aes(x = Timestamp, y = ODBA)) + #we are adding lines using the date and time for x and ODBA values for y
  facet_wrap( ~ AnimalID, scales = "free_x") + #this splits the figure into panels, one for each animal ID
  geom_hline(yintercept = 0.15, color = 'red') + #I've set a horizontal line at 0.15, a value I've used before
  scale_y_continuous(breaks = seq(0,15,2), expand = c(0,0)) + #set the y scale
  scale_x_datetime("Date", date_breaks = "4 hour", labels = date_format('%H:%M\n%d\n%b'), expand = c(0,0)) + #set the x scale
  theme_light() #a basic theme built into ggplot
#for more info on how to make pretty figures using ggplot see https://r-graphics.org

#-----Step 4c: join with metadata and modify-----------------------------
#now add the threshold and set ODBA > 0.15 as a column called Active = 1, and ODBA < 0.15 as Active = 0
#plus a few other modifications while we are at it
ODBA = ODBA %>%
  group_by(AnimalID) %>% #groups the data by animal so that when calculating the time from start it can do it for each animal
  mutate(Timestamp = ymd_hms(Timestamp), #first change the Timestamp to a date and time object 
         Time = hms::as_hms(Timestamp), #change the time as well
         Active = ifelse(ODBA > 0.15,1,0), #makes a new column Active in the data where if ODBA > 0.15 assigns a value of 1, otherwise assigns 0
         ActiveTF = as.logical(Active)) #makes T/F column for Active
head(ODBA) #view the data to make sure it still looks okay

#we will load in some meta data for each device and join to the ODBA dataset
metadata = read.csv('Activity_metadata.csv', header = T) %>%
  mutate(StartTime = ymd_hms(StartTime),
         LiftDateTime = ymd_hms(LiftDateTime),
         LiftTime = hms::as_hms(LiftTime))
str(metadata) #check the struture of the data to make sure the dates and times are correctly read

#now join to the metadata to the ODBA data 
#this is so we can calculate the time in seconds from midnight for each row
#this time in seconds is needed for analysis
ODBA = ODBA %>%
  left_join(metadata) %>%
         mutate(DataStart = as.numeric(difftime(Timestamp, LiftDateTime, units = 'secs')), #calculate the number of seconds after the lift time
         #difftime here subtracts the lift time from the time stamp
         Lifttime.s = seconds(LiftTime), #make a new column with the lift time in seconds from midnight
         t = round(as.numeric(DataStart + Lifttime.s))) %>% #now we calculate the time (in seconds) from start
         filter(DataStart > 0) #now we filter so we only have data after the DataStart time
#now we select just the columns we want for analysis
#we can call this ODBA_short so you can see that there are fewer columns
ODBA_short = ODBA %>%
         select(AnimalID, Timestamp, ODBA, Time, Active, ActiveTF, t) #select just the columns we want for analysis
#note that you can also link this to the above code as well like this:
#ODBA = ODBA %>%
#  left_join(metadata) %>%
#  mutate(DataStart = as.numeric(difftime(Timestamp, LiftDateTime, units = 'secs')), 
#         Lifttime.s = seconds(LiftTime), 
#         t = round(as.numeric(DataStart + Lifttime.s))) %>% 
#  filter(DataStart > 0) %>% 
#  select(AnimalID, Timestamp, ODBA, Time, Active, ActiveTF, t)

#now we finally have a dataset we can analyze!

#----------------------------Part II: Activity Bout Analysis-------------------------------------

#----Step 1: building a behavr table---------------------------
#a behavr data table links our data (ODBA) with info for each animal (metadata)
#a behavr table is necessary for further rethomics analysis
#we have already loaded the metadata, so we just need to join it with ODBA data

#but first set the Animal ID to a factor for better order when plotting  
metadata$AnimalID = factor(metadata$AnimalID) 
ODBA$AnimalID = factor(ODBA$AnimalID)

str(ODBA)
#change the format of the files from data.frame to data.table, using the same key
setDT(metadata, key = "AnimalID") #setDT() changes the format to data.table
setDT(ODBA, key = "AnimalID")
ODBA = behavr(ODBA, metadata) #now we link them together in a behavr object for plotting
summary(ODBA) #check it is a behavr table
ODBA #notice how the meta data is listed and the first and last 5 rows of the data are printed

#lets make some summary statistics of the data
ACCduration = ODBA %>%
  group_by(AnimalID) %>% #we have to group by animal ID or else all the data is averaged together
  #by grouping it calculates values (here max time) for each animal
  summarise(max = max(t)) %>% #gets duration of data recording for each animal (in seconds)
  mutate(days = max/86400) #divide by number of seconds in a day to get number of days
#get summary of duration of the accelerometer data
min(ACCduration$days) 
max(ACCduration$days) 
mean(ACCduration$days)
sd(ACCduration$days)


#calculate a mean ODBA over 10 minute periods
ODBA.10min = ODBA %>%
  group_by(AnimalID) %>% #grouping by AnimalID ensures we do the running mean for each and correctly slice later on
  mutate(ODBA = TTR::runMean(ODBA,300))  %>% #calculate the mean ODBA over 300 rows (600 seconds in 10 minutes, and divide by 2 since our data is every 2 seconds)
  drop_na(ODBA) %>% #removes every row with an NA value in the given column, so that we can...
  slice(which(row_number() %% 300 == 1)) %>% #keep the first row and then every 300th row (ie 10 minutes later)
  mutate(Active = ifelse(ODBA > 0.15,1,0), #makes a new column Active in the data where if ODBA > 0.15 assigns a value of 1, otherwise assigns 0
         Timestamp = floor_date(Timestamp, '10 minutes'), #since each collar ran at slightly different times we round down to 10 minute intervals
         Time = hms::as_hms(Timestamp)) %>%
  select(AnimalID, Timestamp, Time, t, ODBA, Active)

#we first calculate a mean ODBA value over 30 minute windows
#then assign Active Scores (1/0) depending on if ODBA is greater or less than 0.15
ODBA.30min = ODBA.10min %>%
  group_by(AnimalID) %>% #grouping by AnimalID ensures we do the running mean for each and correctly slice later on
  mutate(ODBA = TTR::runMean(ODBA,3))  %>% #this calculates the mean ODBA over 3 rows since our data is every 10 minutes
  drop_na(ODBA) %>% #removes every row with an NA value in the given column, so that we can...
  slice(which(row_number() %% 3 == 1)) %>% #keep the first row and then every 3rd row (30 minutes later)
  mutate(Active = ifelse(ODBA > 0.15,1,0), #reclassify active/inactive based on new mean ODBA
         ActiveTF = as.logical(Active),
         Timestamp = floor_date(Timestamp, '30 minutes')) %>%
  select(AnimalID, Timestamp, ODBA, Active, ActiveTF, t)
setDT(ODBA.30min, key = "AnimalID")
ODBA.30min = behavr(ODBA.30min, metadata) 

#rethomics can quickly find bouts of activity using bout_analysis()
bout_active = bout_analysis(ActiveTF, ODBA.30min)
#Each row in the data describes a bout
#The bout can take the values Active = TRUE or Active = FALSE (an active or inactive bout, respectively)
#t is the time from onset of the bout (in seconds)
#duration is length of the bout (in seconds)

#now remove rows of inactivity and any mistakes where activity duration is 0
bout_active = bout_active %>% filter(ActiveTF == T) %>% filter(duration > 0)


#create a histogram of activity bouts when activity averaged over 30 minute intervals
bout_active = bout_active %>%
  mutate(Minutes = duration/60) 
histotheme<- theme_light() + 
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 8),
        panel.border = element_rect(color = "black"), panel.grid = element_blank(),
        strip.text = element_text(size = 6, color = 'black'),
        strip.background = element_rect(color = "black",fill = "grey90"))
histo = ggplot(bout_active) + geom_histogram(aes(x=Minutes), bins = 15) +
  facet_wrap(~AnimalID, ncol = 4, scales = "free") + 
  scale_x_continuous("Duration of Activity Bout (minutes)") + 
  scale_y_continuous("Count", breaks = seq(0,100,20)) +
  histotheme
##since we called the figure histo, to see the figure we must 'run' histo
histo

#you can save figures by specifying the dimensions and resolution
#this helps for making figures for publication that will not appear pixelated
ggsave('Figure1.png', histo, width = 1800, height = 1800, units = c('px'), dpi = 300)

#we can summarize the bouts for each animal
#this does the same as a dplyr summarise(across()) using data.table syntax
bout.summary = bout_active[,
                           .(n_bouts = .N,
                             total_active = sum(Minutes/60),
                             mean_bout_length = mean(Minutes),
                             sd_bout_length = sd(Minutes)),
                           by=AnimalID]

#create a Table of the summary for each animal
#note that we used the raw 2 second data to create the 30-min windows, and the values may change slightly
bout.summary = bout.summary %>%
  left_join(ACCduration, by = 'AnimalID') %>%
  mutate(bout_per_day = n_bouts/days,
         hours_per_day = total_active/days) %>%
  dplyr::select(AnimalID, n_bouts,days, mean_bout_length, sd_bout_length, bout_per_day,hours_per_day)

#now we can calculate the overall means
mean(bout.summary$hours_per_day) 
sd(bout.summary$hours_per_day) 
mean(bout.summary$bout_per_day) 
sd(bout.summary$bout_per_day) 
mean(bout_active$Minutes) 
sd(bout_active$Minutes) 
median(bout_active$Minutes)
max(bout_active$Minutes) 


#----------------------------Part III: Circadian Rhythm Analysis-------------------------------------
#-------------Step 1: timing of activity----------------------------------------
#one way to visualize when animals are active is to plot the proportion of time spent active
#daily activity (or any 1/0 logical value) is automatically summarized into 30 minute windows using stat_pop_etho()
#first summarize activity for all individuals combined into one figure
ggetho(ODBA, aes(y=ActiveTF), time_wrap = hours(24)) +
  stat_pop_etho() +
  stat_ld_annotations(phase = hours(5.8), l_duration = hours(12.9)) + #this line adds the black/white bars at the bottom
  scale_y_continuous("Proportion of Time Active", limits = c(0,1))

#then faceted by individual
ggetho(ODBA, aes(y=ActiveTF), time_wrap = hours(24)) +
  stat_pop_etho() + stat_ld_annotations(phase = hours(5.8), l_duration = hours(12.9)) +
  facet_wrap(~AnimalID, ncol = 4) +
  scale_y_continuous("Proportion of Time Active", limits = c(0,1))

#-------------Step 2: double plotted actogram----------------------------------------
#another way to visualize activity over multiple days is an actogram
#this is basically a histogram of activity showing the frequency of activity per hour
#plot the double plotted actogram (actogram over 2 days)

#first we will make a custom theme for ggplot
#this bit here allows us to modify the appearance of our figure so that we can standardise across figures
actogramtheme<- theme_light() + 
  theme(axis.title.x = element_text(size=16), axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        panel.border = element_rect(color = "black"), 
        panel.grid.major.x = element_line(color = 'grey'), panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(), panel.spacing.x=unit(1, "lines"),
        strip.text = element_text(size = 8, color = "black"), 
        strip.background = element_rect(color = "black",fill = "grey90"))

#create Figure S2
#note that we are using the ggetho command from rethomics
#this works like ggplot but is specifically for behavr tables and can take typical ggplot commands
actogram = ggetho(ODBA, aes(x = t, z = Active), multiplot = 2) + #multiplot = 2 plots the actogram over 48 hours
  geom_rect(aes(xmin=hours(-0.3), xmax=hours(5.85), ymin=0, ymax=Inf), fill = 'grey') + #here we add grey bars for night time
  geom_rect(aes(xmin=hours(18.7), xmax=hours(29.85), ymin=0, ymax=Inf), fill = 'grey') +
  geom_rect(aes(xmin=hours(42.7), xmax=hours(48.3), ymin=0, ymax=Inf), fill = 'grey') +
  stat_bar_tile_etho() + ylab("Day") + xlab("Time (hours)") + 
  scale_x_hours(expand = c(0,0)) +
  facet_wrap( ~ AnimalID, ncol = 4) + #this breaks the graph into panels for each individual 
  actogramtheme #now we add the theme
actogram 
ggsave("Actogram.png", actogram, units = "px", width=1800, height=1800, dpi=300) 


#if you want a specific individual then you can do a subset
actoTE10F7 = ggetho(subset(ODBA, AnimalID == "TE10F007"), aes(x = t, z = Active), multiplot = 2) + 
  geom_rect(aes(xmin=hours(-0.3), xmax=hours(5.85), ymin=0, ymax=Inf), fill = 'grey') +
  geom_rect(aes(xmin=hours(18.7), xmax=hours(29.85), ymin=0, ymax=Inf), fill = 'grey') +
  geom_rect(aes(xmin=hours(42.7), xmax=hours(48.3), ymin=0, ymax=Inf), fill = 'grey') +
  stat_bar_tile_etho() + ylab("Day") + xlab("Time (hours)") + 
  scale_x_hours(expand = c(0,0)) +
  actogramtheme 
actoTE10F7

#a filter does the same thing
actoTE10F7.v2 = ODBA %>%
  filter(AnimalID == 'TE10F007') %>%
  ggetho(aes(x = t, z = Active), multiplot = 2) + 
  geom_rect(aes(xmin=hours(-0.3), xmax=hours(5.85), ymin=0, ymax=Inf), fill = 'grey') +
  geom_rect(aes(xmin=hours(18.7), xmax=hours(29.85), ymin=0, ymax=Inf), fill = 'grey') +
  geom_rect(aes(xmin=hours(42.7), xmax=hours(48.3), ymin=0, ymax=Inf), fill = 'grey') +
  stat_bar_tile_etho() + ylab("Day") + xlab("Time (hours)") + 
  scale_x_hours(expand = c(0,0)) +
  actogramtheme 
actoTE10F7.v2



#----------------------------Part IV: Assessing Temperature Effects-------------------------------------
#-----------Step 1: load temperature data----------------
temp = read.csv('KMP_temp.csv', header = T) %>%
  mutate(DateTime = ymd_hms(DateTime), #convert the date to a POSIXct format
         DateTime = DateTime + days(730),  #here we're adding 2 years to the date so it matched with our dates
         Day = day(DateTime),
         Hour = factor(hour(DateTime))) %>%
  dplyr::select(!X) #drop the X column

#calculate mean daily temp over the whole time period
mean.temp = temp %>%
  group_by(Day) %>%
  summarise(mean = mean(Tair), sd = sd(Tair), min = min(Tair), max = max(Tair))

#to make the figure we need to get the mean temp for each hour
temp.hourmeans <- temp %>%
  mutate(Hour = hour(floor_date(Timestamp, unit = '60 mins'))) %>% #here we round down each time to the nearest hour using floor_date()
  group_by(Hour)

#a custom theme for temp figure
temptheme = theme_light() + 
  theme(axis.title = element_text(size=16), axis.text = element_text(size = 14),
        panel.border = element_rect(color = "black"), panel.grid = element_blank())
str(temp.hourmeans)
str(temp)

#make a figure of the temperature per hour
Fig2b = ggplot(temp) + 
  geom_boxplot(mapping = aes(x = factor(Hour), y = Tair), notch = F, outliers = F, fill = '#D55E00') +
  scale_x_discrete("Time (hour)", breaks = function(x){x[c(TRUE, FALSE)]}) +
  scale_y_continuous(expression("Temperature  "(degree~C)), breaks = seq(0,30,5), limits = c(0,30)) + 
  temptheme
Fig2b 


#join ODBA data with the temperature data
ODBA.temp = ODBA %>% left_join(temp, by = join_by('Timestamp' == 'DateTime')) 
#syntax left_join(table_to_be_joined, by = join_by(columnA = columnB))
#if the two datasets have the same column names, it will automatically link them
#if you don't have shared column names you need to rename them or specify which are equal to each other like we did above
#for example

#do a simple regression to see if Activity is affected by temperature
mod1 = lm(Active ~ Tair, data = ODBA.temp)
summary(mod1)

