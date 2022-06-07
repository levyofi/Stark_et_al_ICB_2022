##################################Figure of ground temperature around the year######################
combined_temps <- read.csv("Data/combined_temps_final.csv") ### upload `combined_temps` dataset from Data folder###
combined_temps$Month <- factor(combined_temps$Month, levels=unique(combined_temps$Month))
months <- c("January", "February", "March","April","May","June","July","August","September","October","November","December")

tiff(file="Figure 1.tiff", width=2500, height=1800, res=300, compression="lzw")
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
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar",
                              "4" = "Apr", "5" = "May", "6" = "Jun", "7" = "Jul",
                              "8" = "Aug", "9" = "Sep", "10" = "Oct",
                              "11" = "Nov", "12" = "Dec"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
o
dev.off()
