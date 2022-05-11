rescale <- function(x, to, from) {
  x.temp = (x-from[1])/(from[2]-from[1])
  print(x.temp)
  return(to[1] + x.temp*(to[2]-to[1]))
}

Data_bc <- read.csv("Fixed_maps/Rock_and_Veg_Cover_Results.2022-01-24.csv",header=T)
#Convert season to from character to a factor: 
Data_bc$Season <- as.factor(Data_bc$Season)
Data_bc$BC = Data_bc$Mass..in.grams./Data_bc$SVL
Data_bc = Data_bc[Data_bc$SVL>35,]
Data_bc = na.omit(Data_bc)
Data_bc = Data_bc[Data_bc$ID!="GS30",]
Data_bc$logsvl = log(Data_bc$SVL)
Data_bc$logmass = log(Data_bc$Mass..in.grams.)
#The residual index of condition (Ri) -https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5349178/
lm_model = lm(logmass~logsvl, data=Data_bc)
#lm_model = lm(logmass~logsvl+Season, data=Data_bc)
summary(lm_model)
Data_bc$Ri = residuals(lm_model)
#The scaled mass index (Mi) of condition
pearson = cor(Data_bc$logmass, Data_bc$logsvl, method="pearson")
mean_svl = mean(Data_bc$SVL)
bSMA = 2.7555/pearson
Data_bc$Mi = Data_bc$Mass..in.grams. * (mean_svl/Data_bc$SVL)^bSMA #Mi = M × (SVL0/SVL)bSMA

Data_bc = Data_bc[Data_bc$Season %in% c("Summer", "Winter"),]
nrow(Data_bc[Data_bc$Season=="Winter",])
nrow(Data_bc[Data_bc$Season=="Summer",])
nrow(Data_bc[Data_bc$Location=="Mishmar River",])
nrow(Data_bc[Data_bc$Location!="Mishmar River",])

library(ggplot2)
library(R2jags)
load("Fixed_maps/Mi_model_jagsfit.RData")

original.fveg <- sort(runif(100, min(Data_bc$fveg90), max(Data_bc$fveg90)))
fveg <-(original.fveg - mean(Data_bc$fveg90))/(2*sd(Data_bc$fveg90)) 
original.frock <- sort(runif(100, min(Data_bc$frock10), max(Data_bc$frock10)))
frock <-(original.frock - mean(Data_bc$frock10))/(2*sd(Data_bc$frock10)) 
svl=0 #mean svl for the model (scaled!!)

iseason=2; ifrock=3; ifveg=4; isvl = 5; ifvegrock=6
# X[i,2]<-equals(season[i], 2)				
# X[i,3]<-(rock[i]-mean(rock[]))/(2*sd(rock[]))
# X[i,4]<-(veg[i]-mean(veg[]))/(2*sd(veg[]))
# X[i,5]<-(svl[i]-mean(svl[]))/(2*sd(svl[]))
# X[i,6]<-X[i,3]*X[i,4]

beta.season = jagsfit$BUGSoutput$mean$alpha[2]
beta.frock= jagsfit$BUGSoutput$mean$alpha[3]
beta.fveg= jagsfit$BUGSoutput$mean$alpha[4]
beta.svl = jagsfit$BUGSoutput$mean$alpha[5]
beta.fveg.frock = jagsfit$BUGSoutput$mean$alpha[6]

n.sims = 2000
start_sim = 4000
# Create matrices to contain prediction for each MCMC iteration

#the effect of each factor, based on the avergae of the other factors (zero in the scaled variables)
mean.Predicted.Mi.fveg =  jagsfit$BUGSoutput$mean$alpha_mean+ jagsfit$BUGSoutput$mean$alpha[ifveg]*fveg
mean.Predicted.Mi.frock = jagsfit$BUGSoutput$mean$alpha_mean+ jagsfit$BUGSoutput$mean$alpha[ifrock]*frock
mean.Predicted.Mi.fveg.winter = jagsfit$BUGSoutput$mean$alpha_mean+ jagsfit$BUGSoutput$mean$alpha[iseason]+ jagsfit$BUGSoutput$mean$alpha[ifveg]*fveg
mean.Predicted.Mi.frock.winter = jagsfit$BUGSoutput$mean$alpha_mean + jagsfit$BUGSoutput$mean$alpha[iseason]+ jagsfit$BUGSoutput$mean$alpha[ifrock]*frock

Predicted.Mi.fveg = array(dim=c(100, n.sims))
Predicted.Mi.frock = array(dim=c(100, n.sims))
Predicted.Mi.fveg.winter = array(dim=c(100, n.sims))
Predicted.Mi.frock.winter = array(dim=c(100, n.sims))

# Predicted.Mi.average_svl_for_frockveg_interaction = array(dim=c(100, 100))
# Predicted.Mi.average_svl_for_frockveg_interaction.winter = array(dim=c(100, 100))

# Fill in these vectors: this is clumsy, but it works
for(i in 1:100) {
  k=1
  for(j in (start_sim+1:n.sims)) {
    #print(j)
    Predicted.Mi.fveg[i,k] = jagsfit$BUGSoutput$sims.list$alpha_mean[j,1]+ jagsfit$BUGSoutput$sims.list$alpha[j,ifveg]*fveg[i]
    Predicted.Mi.frock[i,k] = jagsfit$BUGSoutput$sims.list$alpha_mean[j,1]+ jagsfit$BUGSoutput$sims.list$alpha[j,ifrock]*frock[i]
    Predicted.Mi.fveg.winter[i,k] = jagsfit$BUGSoutput$sims.list$alpha_mean[j,1]+ jagsfit$BUGSoutput$sims.list$alpha[j,ifveg]*fveg[i] + jagsfit$BUGSoutput$sims.list$alpha[j,iseason]
    Predicted.Mi.frock.winter[i,k] = jagsfit$BUGSoutput$sims.list$alpha_mean[j,1]+ jagsfit$BUGSoutput$sims.list$alpha[j,ifrock]*frock[i] + jagsfit$BUGSoutput$sims.list$alpha[j,iseason]
    k=k+1
    # for (k in 1:100){
    #   Predicted.Mi.average_svl_for_frockveg_interaction[i,k,j] = jagsfit$BUGSoutput$sims.list$alpha[j,1]+ jagsfit$BUGSoutput$sims.list$alpha[j,ifveg]*fveg[k] + jagsfit$BUGSoutput$sims.list$alpha[j,ifrock]*frock[i] + jagsfit$BUGSoutput$sims.list$alpha[j,ifvegrock]*frock[i]*fveg[k]
    #   Predicted.Mi.average_svl_for_frockveg_interaction.winter[i,k,j] = jagsfit$BUGSoutput$sims.list$alpha[j,1]+ jagsfit$BUGSoutput$sims.list$alpha[j,ifveg]*fveg[k] + jagsfit$BUGSoutput$sims.list$alpha[j,ifrock]*frock[i] + jagsfit$BUGSoutput$sims.list$alpha[j,ifvegrock]*frock[i]*fveg[k] + jagsfit$BUGSoutput$sims.list$alpha[j,iseason]
    # }
  }    
}

LCB.summer.fveg <-apply(Predicted.Mi.fveg, 1, quantile, prob=0.025)
UCB.summer.fveg <-apply(Predicted.Mi.fveg, 1, quantile, prob=0.975)
LCB.summer.frock <-apply(Predicted.Mi.frock, 1, quantile, prob=0.025)
UCB.summer.frock<-apply(Predicted.Mi.frock, 1, quantile, prob=0.975)

LCB.winter.fveg <-apply(Predicted.Mi.fveg.winter, 1, quantile, prob=0.025)
UCB.winter.fveg <-apply(Predicted.Mi.fveg.winter, 1, quantile, prob=0.975)
LCB.winter.frock <-apply(Predicted.Mi.frock.winter, 1, quantile, prob=0.025)
UCB.winter.frock<-apply(Predicted.Mi.frock.winter, 1, quantile, prob=0.975)

tiff(file="Figure 3.tiff", width=1600, height=2000, res=300, compression="lzw")
par(mfcol=c(2,1), cex=1.1)
par(mar=c(2,4,1,2))
#par(bty="l")
#fveg
plot(c(0,max(original.fveg)), c(0,4), type="n", xlab="", ylab="", xaxt="n", yaxt="n")
points(original.fveg, LCB.summer.fveg, col="orange", type="l", lwd=2, lty=2)
points(original.fveg, UCB.summer.fveg, col="orange", type="l", lwd=2, lty=2)
points(original.fveg, mean.Predicted.Mi.fveg, col =	"orange", type = "l", lwd = 2)
points(original.fveg, LCB.winter.fveg, col="black", type="l", lwd=2, lty=2)
points(original.fveg, UCB.winter.fveg, col="black", type="l", lwd=2, lty=2)
points(original.fveg, mean.Predicted.Mi.fveg.winter, col =	"black", type = "l", lwd = 2)
points(Data_bc$fveg90, Data_bc$Mi, col=ifelse(Data_bc$Season=="Summer", "orange","black"), pch=19)
text(0.01,3.7,"A", cex=1.2)
axis(1, at = seq(0, 0.10, 0.01), cex.axis=0.8, lwd=0, line=-1, labels = seq(0, 0.10, 0.01)*100)
axis(1, at = seq(0, 0.10, 0.005), labels=F, tcl=-0.2)
axis(2, at = seq(0, 4, 0.5), cex.axis=0.8, lwd=0, line=-0.5, las=2)
axis(2, at = seq(0, 4, 0.25), labels=F, tcl=-0.2)
#mtext(side=2, "Mi", line=1.6)
mtext(side=1, "Vegetation cover (%)", line=1, cex=1.3)
par(mar=c(2,4,1,2))
#par(bty="l")
#fveg
plot(c(0,max(original.frock)), c(0,4), type="n", xlab="", ylab="", xaxt="n", yaxt="n")
points(original.frock, LCB.summer.frock, col="orange", type="l", lwd=2, lty=2)
points(original.frock, UCB.summer.frock, col="orange", type="l", lwd=2, lty=2)
points(original.frock, mean.Predicted.Mi.frock, col =	"orange", type = "l", lwd = 2)
points(original.frock, LCB.winter.frock, col="black", type="l", lwd=2, lty=2)
points(original.frock, UCB.winter.frock, col="black", type="l", lwd=2, lty=2)
points(original.frock, mean.Predicted.Mi.frock.winter, col =	"black", type = "l", lwd = 2)
points(Data_bc$frock10, Data_bc$Mi, col=ifelse(Data_bc$Season=="Summer", "orange","black"), pch=19)
text(rescale(0.01, to = c(0,max(original.frock)), from = c(0,max(original.fveg))),3.7,"B", cex=1.2)
axis(1, at = seq(0, 0.40, 0.05), cex.axis=0.8, lwd=0, line=-1, labels = seq(0, 0.40, 0.05)*100)
axis(1, at = seq(0, 0.40, 0.025), labels=F, tcl=-0.2)
axis(2, at = seq(0, 4, 0.5), cex.axis=0.8, lwd=0, line=-0.5, las=2)
axis(2, at = seq(0, 4, 0.25), labels=F, tcl=-0.2)
mtext(side=2, "Scaled Mass Index (Mi)", line=1.6, at = 4.5)
mtext(side=1, "Rock cover (%)", line=1, cex=1.3)
dev.off()

# Figure 4 - the synergic effect of vegetation and rock cover
library(plotly) 
data_3d_original_scale = expand.grid(original.frock=original.frock, original.fveg=original.fveg)
data_3d = expand.grid(frock=frock, fveg=fveg)
data_3d$predictions = jagsfit$BUGSoutput$mean$alpha_mean+ jagsfit$BUGSoutput$mean$alpha[ifveg]*data_3d$fveg+ jagsfit$BUGSoutput$mean$alpha[ifrock]*data_3d$frock + jagsfit$BUGSoutput$mean$alpha[ifvegrock]*data_3d$frock*data_3d$fveg
data_3d = cbind(data_3d, data_3d_original_scale)
library(reshape2)
plot_matrix <- t(acast(data_3d, original.fveg~original.frock, value.var="predictions"))
plot_matrix

library(plot3D)
tiff(file="Figure 4.tiff", width=2000, height=2000, res=300, compression="lzw")

plot3D::persp3D(x = 100*as.numeric(colnames(plot_matrix)), 
                y = 100*as.numeric(rownames(plot_matrix)), 
                z = plot_matrix,
                xlab = "Vegetation cover (%)",
                ylab = "Rock cover (%)",
                zlab = "Scaled Mass Index (Mi)",
                ticktype ='detailed',  cex.lab=1.2,cex.axis=1,
                theta=50, phi=25, expand=0.65, colkey = list(length = 0.6, width = 1, shift = 0,
                                                             cex.axis = 0.8, cex.clab = 0.85))
points3D(Data_bc$fveg90*100, Data_bc$frock10*100, Data_bc$Mi, col = "red", size = 30, add=T)
dev.off()
