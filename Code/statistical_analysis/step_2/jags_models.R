library(R2jags)
Data_bc = read.csv(file = "Data/lizards_Mi_data_Appendix_S1.csv", header=T)
Data_bc = na.omit(Data_bc)
Data_bc %>% count(Location, Season, sort = TRUE)

svl<-Data_bc$SVL
season = as.numeric(Data_bc$Season)
veg = Data_bc$fveg90*100
rock = Data_bc$frock10*100
location = as.numeric(as.factor(Data_bc$Location))
N<-nrow(Data_bc)
Mi = Data_bc$Mi
id<-1:N

jags.data <- list("Mi","svl","season", "veg", "location","rock","N")
jags.params <- c("Loc", "alpha", "tau", "mu", "e.obs", "delta", "p.val")
jags.inits <- function(){
  list("tau"= 1, "tau.loc"=1, "delta"=0, "alpha"=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), "Loc"=c(0,0))	
}

model.file="../Mi model JAGS.txt"
#=============#
# using jags  #
#=============#
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=500000, model.file=model.file, n.thin=300 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
options(width=300, max.print=300000)
sink("model jags result 1.txt")			
print(jagsfit, digits=3)
sink(NULL)			


jags.inits <- function(){
  list("tau"= 1, "delta"=0, "alpha"=c(0,0,0,0,0,0,0,0,0,0,0,0,0), "Loc"=c(0,0))	
}

model.file="../Mi model JAGS simp1.txt"
#=============#
# using jags  #
#=============#
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=300000, model.file=model.file, n.thin=300 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
options(width=300, max.print=300000)
sink("model jags result simp1.txt")			
print(jagsfit, digits=3)
sink(NULL)			


jags.inits <- function(){
  list("tau"= 1, "delta"=0, "alpha"=c(0,0,0,0,0,0,0,0,0,0,0,0), "Loc"=c(0,0))	
}

model.file="../Mi model JAGS simp2.txt"
#=============#
# using jags  #
#=============#
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=400000, model.file=model.file, n.thin=300 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
options(width=300, max.print=300000)
sink("model jags result simp2.txt")			
print(jagsfit, digits=3)
sink(NULL)			


jags.inits <- function(){
  list("tau"= 1, "delta"=0, "alpha"=c(0,0,0,0,0,0,0,0,0,0,0), "Loc"=c(0,0))	
}

model.file="../Mi model JAGS simp3.txt"
#=============#
# using jags  #
#=============#
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=400000, model.file=model.file, n.thin=300 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
options(width=300, max.print=300000)
sink("model jags result simp3.txt")			
print(jagsfit, digits=3)
sink(NULL)			



jags.inits <- function(){
  list("tau"= 1, "delta"=0, "alpha"=c(0,0,0,0,0,0,0,0,0,0), "Loc"=c(0,0))	
}

model.file="../Mi model JAGS simp4.txt"
#=============#
# using jags  #
#=============#
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=400000, model.file=model.file, n.thin=300 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
options(width=300, max.print=300000)
sink("model jags result simp4.txt")			
print(jagsfit, digits=3)
sink(NULL)			


jags.inits <- function(){
  list("tau"= 1, "delta"=0, "alpha"=c(0,0,0,0,0,0,0,0,0), "Loc"=c(0,0))	
}
jags.params <- c("Loc", "alpha", "tau", "delta", "p.val")

model.file="../Mi model JAGS simp5.txt"
#=============#
# using jags  #
#=============#
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=400000, model.file=model.file, n.thin=300 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
options(width=300, max.print=300000)
sink("model jags result simp5.txt")			
print(jagsfit, digits=3)
sink(NULL)			

jags.inits <- function(){
  list("tau"= 1, "delta"=0, "alpha"=c(0,0,0,0,0,0,0,0), "Loc"=c(0,0))	
}
jags.params <- c("Loc", "alpha", "tau", "delta", "p.val")

model.file="../Mi model JAGS simp6.txt"
#=============#
# using jags  #
#=============#
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=400000, model.file=model.file, n.thin=300 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
options(width=300, max.print=300000)
sink("model jags result simp6.txt")			
print(jagsfit, digits=3)
sink(NULL)			


model.file="../Mi model JAGS simp7.txt"
#=============#
# using jags  #
#=============#
jags.inits <- function(){
  list("tau"= 1, "delta"=0, "alpha"=c(0,0,0,0,0,0,0), "Loc"=c(0,0))	
}
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=400000, model.file=model.file, n.thin=300 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
options(width=300, max.print=300000)
sink("model jags result simp7.txt")			
print(jagsfit, digits=3)
sink(NULL)			

model.file="Mi model JAGS simp8.txt"
#=============#
# using jags  #
#=============#
jags.data <- list("Mi","svl","season", "veg", "location","rock","N")
jags.params <- c("alpha_mean", "div_veg_rock","Loc", "alpha", "tau", "delta", "p.val", "beta.veg", "beta.rock", "beta.svl", "beta.season", "beta.rockveg")
jags.inits <- function(){
  list("tau"= 1, "delta"=0, "alpha"=c(0,0,0.000001,0.00001,0,0), "Loc"=c(0,0))	
}
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params, 
                n.iter=100000, model.file=model.file, n.thin=100 )

#save(jagsfit, file="model no ssvs with no autocorr.RData")
#jagsfit = recompile(jagsfit)
jagsfit = update(jagsfit, n.iter=200000, model.file=model.file, n.thin=100)
options(width=300, max.print=300000)
sink("Fixed_maps/model jags result simp8.txt")			
print(jagsfit, digits=4)
sink(NULL)			
save(jagsfit, file = "Fixed_maps/Mi_model_jagsfit.RData")

load("Fixed_maps/Mi_model_jagsfit.RData")
p.vals = jagsfit$BUGSoutput$mean$p.val
pseason=2-p.vals[2]
prock=p.vals[3]
pveg=p.vals[4]
psvl=2-p.vals[5]
pvegrock= p.vals[6]

div_veg_rock = jagsfit$BUGSoutput$sims.list$div_veg_rock

traceplot(jagsfit, varnames=c("alpha"))
