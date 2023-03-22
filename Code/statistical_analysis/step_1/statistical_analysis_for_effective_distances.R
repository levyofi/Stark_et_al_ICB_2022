#Install and load Packages:
library(MASS)
library(lubridate)
library(ggplot2)
library(rayshader)
library(dplyr)
library(car)
library(constants)
library("lme4")
library(Epi)
library(emmeans)

Data_bc <- read.csv("Data/lizards_Mi_data_Appendix_S1.csv")

Data_bc$logsvl = log(Data_bc$SVL)
Data_bc$logmass = log(Data_bc$mass)

#calculate bSMA
lm_model = lm(logmass~logsvl, data=Data_bc)
summary(lm_model)
pearson = cor(Data_bc$logmass, Data_bc$logsvl, method="pearson")
mean_svl = mean(Data_bc$SVL)
bSMA = 2.6764/pearson

#The scaled mass index (Mi) of condition
Data_bc$Mi = Data_bc$mass * (mean_svl/Data_bc$SVL)^bSMA #Mi = M × (SVL0/SVL)bSMA



BC_var = Data_bc$Mi
library(nlme)
require(MuMIn)


get_all_models = function(response_column){#Compare between model with location as fixed effect vs location as random effect

  #initiate column names
  veg_columns = c("fveg_std10")
  rock_columns = c("frock_std10")
  for (i in 2:10){
    veg_columns[i] = paste0("fveg_std",i*10)
    rock_columns[i] = paste0("frock_std",i*10)
    
  }
  
  #initiate results arrays
  formulas = c()
  var_veg = c()
  var_rock = c()
  
  #create formulas for all combinations
  i=1
  for (cveg in veg_columns){
    for (crock in rock_columns){
      formula = as.formula(paste0(response_column,"~SVL_std*Season*", paste0(cveg,"*",crock))) #as.formula(paste0("BC~SVL_scaled*Season*", paste0(cveg,"*",crock), "+(1|Location)"))
      formulas = append(formulas, formula)
      var_veg = append(var_veg, as.formula(paste0("~",cveg)))
      var_rock = append(var_rock, as.formula(paste0("~",crock)))
      i=i+1
    }
  }
  
  #run all models and collect AICs
  AICs = c()
  var_formulas = c()
  models = list()
  for (i in 1:length(formulas)){
    var_form = "" # to save best variance structure
    
    #creating df and attaching is needed to avoid error in dredge
    df = data.frame(current_formula = paste(formulas[[i]][2], formulas[[i]][3], sep="~"))
    attach(df)
    
    #run every combination of variance structure
    model = lme(as.formula(current_formula), random=~1|Location, data = Data_bc)
    model_var_rock = lme(as.formula(current_formula), random=~1|Location, data = Data_bc, weights = varExp(form=var_rock[[i]]))
    model_var_veg = lme(as.formula(current_formula), random=~1|Location, data = Data_bc, weights = varExp(form=var_veg[[i]]))
    
    #get AIC of different variance structures
    aic = AIC(model)
    aic_var_rock = AIC(model_var_rock)
    aic_var_veg = AIC(model_var_veg)
    
    #get the model with the lowest AIC
    if (aic_var_rock < min(aic, aic_var_veg)){
      model = model_var_rock
      var_form = var_rock[[i]]
    } else {
      if (aic_var_veg < min(aic, aic_var_rock)) {
        model = model_var_veg
        var_form = var_veg[[i]]
      }
    }
    
    #do variable selection using dregde
    model = update(model, method="ML")
    d=dredge(model)
    m = get.models(d, subset = 1)[[1]]
    
    #log the AIC, formula and model
    AICs = append(AICs, AICc(m))
    var_formulas = append(var_formulas, var_form)
    models = append(models, summary(m))
    detach(df)
  }
  return(list(AICs = AICs, models=models, formulas = formulas, var_formulas = var_formulas))
}

#create columns of standarized values of fveg and frock
for (i in 1:10){
  v = Data_bc[,paste0("fveg",i*10)]
  r = Data_bc[,paste0("frock",i*10)]
  Data_bc[,paste0("fveg_std",i*10)] = (v-mean(v))/(2*sd(v))
  Data_bc[,paste0("frock_std",i*10)] = (r-mean(r))/(2*sd(r))
}
#create standarized SVL column
Data_bc$SVL_std = (Data_bc$SVL-mean(Data_bc$SVL))/(2*sd(Data_bc$SVL)) 

#get all models
Mi_models =  get_all_models("Mi")

#find index of best model
best_model_Mi = which(abs(Mi_models[["AICs"]]-min(Mi_models[["AICs"]]))<0.0001)[1]

#get formula and variance structure of best model
f = Mi_models[["formulas"]][[best_model_Mi]]
fvar = Mi_models[["var_formulas"]][[best_model_Mi]]

#for the results section - AICs of the different model
paste(round(mean(Data_bc$SVL),2), "±", round(sd(Data_bc$SVL),2))
paste(round(mean(Data_bc$Mass..in.grams.),2), "±", round(sd(Data_bc$Mass..in.grams.),2))
paste(round(mean(Data_bc$Mi),2), "±", round(sd(Data_bc$Mi),2))
aics = Mi_models[["AICs"]]
range(aics)[2] - range(aics)[1]
worst_model_Mi = which(abs(Mi_models[["AICs"]]-max(Mi_models[["AICs"]]))<0.0001)[1]
f = Mi_models[["formulas"]][[worst_model_Mi]]
fvar = Mi_models[["var_formulas"]][[worst_model_Mi]]
almost_best_model_Mi = which(abs(Mi_models[["AICs"]]-min(Mi_models[["AICs"]]))<2)
