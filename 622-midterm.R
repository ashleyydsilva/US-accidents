#################### Midterm ######################
#Installing packages 
install.packages("ggplot2")
install.packages("maps")
install.packages("mapproj")
install.packages("lubridate")
install.packages("RcolorBrewer")
install.packages("treemap")

#Loading all the libraries
library(ggplot2)
library(maps)
library(mapproj)
library(lubridate)
library(RcolorBrewer)
library(treemap)

#Reading the dataset and assigning a variable to it
  usa1 <- read.csv("/Users/ashley/Desktop/EM-622/midterm/usa_cleandata.csv")

#Viewing the dataset
  View(usa1)

#Looking at the structure of the dataset
  str(usa1)

#dropping column that got introduced while deleting cells in excel
#I deleted rows from 10k-end in excel as my R couldn't process such large dataset

#dropping column X.1
  usa2 <- usa1[,!names(usa1) %in% c("X.1")]

#some columns 
#dropping blank values in select columns
  usa3 <- usa2[!(usa2$Sunrise_Sunset==""),]
  unique(usa3$Sunrise_Sunset)

  View(usa3)
#changing column names for better readability
  usa <-  usa3 %>%
  rename("Distance" = `Distance.mi.`, "Temperature" = `Temperature.F.`, "Humidity" = `Humidity...`, 
         "Pressure" = `Pressure.in.`, "Visibility" = `Visibility.mi.`, "Wind_Speed" = `Wind_Speed.mph.`,"Wind_Chill"=`Wind_Chill.F.`,"Precipitation"= `Precipitation.in.`)
  View(usa)
  str(usa)
  
  

####################### Geographic heat map ####################### 
###Severity of accidents based on the county geo location
#checking for unique values in column Severity  
  unique(usa$Severity)
#Creating a new color pallette using Rbrewer
  newpalette <- brewer.pal(5, "Reds")
#Creating t new column to hold the values 
  usa$colorBuckets <- as.numeric(cut(usa$Severity,c(0,1,2,3,4)))
#Creating the legend text
  leg.txt <- c("2","3","4")
#printing the head of usa
#head(usa)
#assigning variable to the created color pallette
  usa$colorCode <- newpalette[usa$colorBuckets]
#plotting the geographic heatmap
  map(database = "county",col= usa$colorCode, fill = TRUE,resolution= 0, lty=0,projection = "polyconic")
  map(database = "state",col= "black", fill = FALSE, add=TRUE, lty=1,lwd= 0.2 ,projection = "polyconic")
#modifying the legend and title aspects
  legend("bottomright", legend = leg.txt, horiz= FALSE,fill = newpalette, title = "Severity")
  title(main = "Severity of accidents based on the county geo location", 
      sub = "(illustraion of heat map)",
      font.main = 2 , cex.main = 1.3 , col.main = "black",
      font.sub = 7, cex.sub = 1) 




####################### Scatter plot ####################### 

myplot <- ggplot(data = usa, aes(x=Sunrise_Sunset, y = Distance)) + 
#adding jitter plot
  geom_jitter(mapping=aes(colour = Severity,size =Distance), alpha = 0.8) +
#Using facet wrap on the timezone 
  facet_wrap(~Timezone) +
#changing the legend title aspects
  theme(legend.title = element_text(colour = "black", size = 10, face 
                                    = "bold"),legend.text = element_text(color = "black"))+
#adding the main title 
  ggtitle ("Distance of road affected w.r.t Severity and Time of day") + 
#changing the main title aspects
  theme(plot.title=element_text(face = "bold")) + 
#Changing x & y axis labels
  labs(x = "Day/Night") + labs(y = "Distance of road affected")
#Viewing the plot
  myplot 

#####Extra
  myplot1 <- ggplot(data=usa) + geom_bar(aes(x=Sunrise_Sunset))+ facet_wrap(~Severity)
  myplot1


  
####################### Bar plot ####################### 
#to find out the breakdown of Severity of accidents for specific timezones w.r.t to Day/Night
  
#finding out the unique values in the column Sunrise_Sunset
  unique(usa$Sunrise_Sunset)
#using ggplot and facet wraping Sunrise_Sunset
  myplot1 <- ggplot(data=usa) + geom_bar(aes(x=Severity,y=..count..,fill = Timezone)) + facet_wrap(~Sunrise_Sunset)+
#giving a title to the graph
  ggtitle("Severity of accidents for specific timezones w.r.t to Day/Night")+
#changing the theme of the title
  theme(plot.title=element_text(face = "bold")) + 
#Changing x & y axis labels
  labs(y = "Frequency")
#Viewing the plot
  myplot1
  
   
  
####################### Table heatmap ####################### 
#to find statewise distributions of various weather aspects 
#finding out unique values in the column State  
  unique(usa$State)
#Removing the duplicate values from state column 
  unique_state <- usa[!duplicated(usa$State), ]
#Viewing the top 50 states 
  head(unique_state$State,50)
#Creating a matrix to feed in the table heat map 
#setting the row names as State
  row.names(unique_state) <- unique_state$State
#Selecting the columns and number of rows for the matrix 
  usa_heatmap <- unique_state[1:50,24:30]
#Viewing the created data to feed in the matrix
  View(usa_heatmap)
  head(usa_heatmap)
#Assigning variable to the matrix
  usa_heatmap_matrix <- data.matrix(usa_heatmap)
#Viewing the matrix
  View(usa_heatmap_matrix)
#plotting the table heatmap
  usa_heatmap <- heatmap(usa_heatmap_matrix, Rowv = NA,Colv = NA,margins = c(9,3),
                       col = brewer.pal(9,"Reds"), scale="column",
                       main = "Statewise Heatmap of various Weather attributes")




####################### Treemap ####################### 
#loading treemap library
  library(treemap)
  unique(usa$Weather_Condition)
#data(usa)
  treemap_data <- data.frame(usa %>% count(State))
  colnames(treemap_data)[2] <- "Frequency"
#Viewing the new data
  View(treemap_data)
#plotting treemap using the created data   
  usa2 <- transform(treemap_data)
#Viewing the data
  View(usa2)  
#plotting the treemap
  treemap_plot <- treemap(usa,index = c("State"),vSize ="Frequency")
#some error in the plotting as it did show labels and title the first time.


