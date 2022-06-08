combined_temps <- read.csv("Data/combined_temps_for_figure_1.csv") # upload dataset
combined_temps <- combined_temps[combined_temps$Season %in% c("Summer", "Winter"),]
str(combined_temps) # check that the structure of the table fits the Anova

library(nlme)
gls.model <- gls(mean_temp ~ Microhabitat*Season, data = combined_temps, weights = varIdent(form =~1|Microhabitat*Season)) 
summary(gls.model)

e = residuals(gls.model, type="n")
plot(e)
boxplot(e~combined_temps$Season+combined_temps$Microhabitat)
plot(e~combined_temps$mean_temp)
