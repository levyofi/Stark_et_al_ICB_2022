##################################Figure of ground temperature around the year######################
combined_temps <- read.csv(choose.files())
combined_temps$Month <- factor(combined_temps$Month, levels=unique(combined_temps$Month))
months <- c("January", "February", "March","April","May","June","July","August","September","October","November","December")
o <- ggplot(combined_temps, aes(Month,mean_temp)) +
  geom_point(aes(color = Microhabitat), size=5) + 
  geom_line(aes(color = Microhabitat, group=Microhabitat))+  
  geom_errorbar(aes(ymin=mean_temp-sd, ymax=mean_temp+sd, colour=Microhabitat), width=0.4, alpha=0.9, size=1.3)+
  labs(x = "Month",
       y = "Tempearture (C)")+theme_bw()+
  theme(axis.text=element_text(face='bold', size=15), 
        axis.text.x=element_text(face='bold', size=13),
        axis.text.y=element_text(face='bold', size=15)
        ,axis.title.x=element_text(face='bold', size=15),
        axis.title.y=element_text(face='bold', size=15, vjust = 1))+scale_color_manual(values = c("orange","Black"))+
  scale_x_discrete(limits =c("1", "2", "3","4", "5", "6", "7","8", "9", "10","11", "12"),
                   labels = c("1" = "January", "2" = "Feburary", "3" = "March",
                              "4" = "April", "5" = "May", "6" = "June", "7" = "July",
                              "8" = "August", "9" = "Spetember", "10" = "October",
                              "11" = "November", "12" = "December"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
o
