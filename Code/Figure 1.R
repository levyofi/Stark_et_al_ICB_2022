###Main Code for analyzing and creating Figure 1###
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library("plotrix")
##Uploading databases:
shade_temp_data <- read.csv(file.choose())
open_temp_data <- read.csv(file.choose())
##Calculating mean and sd for each month of the year for open and shaded areas:
shade_temp_mean <- aggregate(shade_temp_data[, 1], list(shade_temp_data$Month), mean)
shade_temp_sd <- aggregate(shade_temp_data[, 1], list(shade_temp_data$Month), sd)
shade_data_merged <- merge(shade_temp_mean, shade_temp_sd, by = c("Group.1"))
names(shade_data_merged)[1] <- "Month"
names(shade_data_merged)[2] <- "Mean Temperatures under shade (small bush)"
names(shade_data_merged)[3] <- "Standard error of temps under shade (small bush)"

open_temp_mean <- aggregate(open_temp_data[, 1], list(open_temp_data$Month), mean)
open_temp_sd <- aggregate(open_temp_data[, 1], list(open_temp_data$Month), sd)
open_data_merged <- merge(open_temp_mean, open_temp_sd, by = c("Group.1"))
names(open_data_merged)[1] <- "Month"
names(open_data_merged)[2] <- "Mean Temperatures in the open field"
names(open_data_merged)[3] <- "Standard error of temps in the open field"
###Merge and save new databases:
Final_data_merged <- merge(shade_data_merged, open_data_merged, by = c("Month"))
write.csv(Final_data_merged, file = paste("combined_temps",today(), "csv", sep="." ), row.names = FALSE)
#####Statistics#####
library(ggplot2)
library(dplyr)
#Analysis of variance for two factors - Two-Way ANOVA#
combined_temps <- read.csv(file.choose()) # upload dataset
str(combined_temps) # check that the structure of the table fits the Anova
Two_way_anova <- aov(Mean_Temp ~ Microhabitat*Season, data = combined_temps) # run Two-Way Anova without fall and spring
summary(Two_way_anova) # Check results
##Creating a table with factors, means and standard deviation##
data_summary <- group_by(combined_temps, Microhabitat, Season) %>% 
  summarise(mean=mean(Mean_Temp), sd=sd(Mean_Temp)) %>%
  arrange(desc(mean))
print(data_summary)
###Comparing all the combinations of means by Tukey's test###
tukey <- TukeyHSD(Two_way_anova)
print(tukey)
####Figure of comparisons between ground temperature in the shade and the open across the seasons####
combined_temps <- read.csv(file.choose()) ### upload `combined_temps` dataset from Data folder###
combined_temps$Month <- factor(combined_temps$Month, levels=unique(combined_temps$Month))
months <- c("January", "February", "March","April","May","June","July","August","September","October","November","December")

tiff(file="Figure 1.tiff", width=2500, height=1800, res=300, compression="lzw")
o <- ggplot(combined_temps, aes(Month,Mean_Temp)) +
  geom_point(aes(color = Microhabitat), size=5) +
  geom_line(aes(color = Microhabitat, group=Microhabitat))+  
  geom_errorbar(aes(ymin=Mean_Temp-SD, ymax=Mean_Temp+SD, colour=Microhabitat), width=0.4, alpha=0.9, size=1.3)+
  labs(x = "Month",
       y = "Tempearture (C)")+theme_bw()+
  theme(axis.text=element_text(face='bold', size=15),
        axis.text.x=element_text(face='bold', size=13),
        axis.text.y=element_text(face='bold', size=15)
        ,axis.title.x=element_text(face='bold', size=15),
        axis.title.y=element_text(face='bold', size=15, vjust = 1))+scale_color_manual(values = c("orange","Black"))+
  scale_x_discrete(limits =c("1", "2", "3","4", "5", "6", "7","8", "9", "10","11", "12"),
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar",
                              "4" = "Apr", "5" = "May", "6" = "Jun", "7" = "Jul",
                              "8" = "Aug", "9" = "Sep", "10" = "Oct",
                              "11" = "Nov", "12" = "Dec"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
o
dev.off()



