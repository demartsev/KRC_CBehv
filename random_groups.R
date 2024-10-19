#### this script makes X random groups from a list of names

#set working derctory
setwd("C:/Users/vdemartsev/ownCloud - vdemartsev@ab.mpg.de@owncloud.gwdg.de/SA_field_course/2024/aplicants")
#read the list of names to R
names <- read.csv("list_of_aplicants.csv")

#define the number of groups###
i <- 4

#create groups of equal size by sampling the names without replacement
groups <- split(sample(nrow(names), nrow(names), replace = F), as.factor(1:i)) 

#save group participant list
names_list <- lapply(groups, function(x) names[x,])


library(gplots)

#set the number of rows and columns of the plot panels
 
options(device = "windows")
dev.new(width=30, height=20) 
par(mfcol = c(3,2))
#print the generated teams
for (g in 1:i){
textplot(c(paste("Team", g) , names_list[[g]]), halign="center",
         valign="center", cex = 2, mar = c(1, 1, 1 ,1))
  Sys.sleep(1) }
