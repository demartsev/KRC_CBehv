#### this script makes X random groups from a list of names ####
#### The code is made to run on Windows machine


#Create a dataframe with all the names/items that you want to assign to groups 
names <- data.frame(c("Frodo", "Gandalf", "Sam", "Merry", "Pippin", "Aragorn", 
                        "Legolas", "Gimli", "Boromir", "Sauron", "Gollum", "Bilbo"))
#change the name of the column for easy referencing
colnames(names) <- "Name"

#define the number of groups that you need###
i <- 4

#create groups of equal size by sampling the names without replacement
groups <- split(sample(nrow(names), nrow(names), replace = F), as.factor(1:i)) 

#save group participant list
names_list <- lapply(groups, function(x) names[x,])

## Plotting the groups ##

library(gplots)

#define the display parameters
 options(device = "windows")
dev.new(width=30, height=20) 

#set the number of rows and columns of the plot panels
par(mfcol = c(2,2))

#print the generated teams with a slight delay between the teams
for (g in 1:i){
textplot(c(paste("Team", g) , names_list[[g]]), halign="center",
         valign="center", cex = 2, mar = c(1, 1, 1 ,1))
  Sys.sleep(1) }
