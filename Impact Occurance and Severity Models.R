setwd ("C:/Users/AndrewT/OneDrive - CAAS (Environmental Services) Ltd/Desktop/AndyPhD/PhD_Data_R/WAW")
##calls and runs the full script
source("WAW_DataCleaning17_19.R")


library(tidyr)
VOS <- readOGR(dsn ="C://Users//AndrewT//OneDrive - CAAS (Environmental Services) Ltd//Desktop//AndyPhD//PhD_Data_R//WAW//Visitor Movement")
plot(VOS)
VOS_Data <- data.frame(VOS)
modeldata_VOS <- merge(modeldata, VOS_Data, by.x = "Visitor.Unique.ID", by.y = "Name")
modeldata_VOS <- as.data.frame(modeldata_VOS)
format(modeldata_VOS)
modeldata_VOS$Year <- modeldata_VOS$Year.y


hist1 <- hist(modeldata_VOS$Prop.Imp.HM)
hist2 <- hist(modeldata_VOS$ObservableImpacts)

##Factors or catagorical variables
nrow(modeldata_VOS)

modeldata_VOS$ObservableImpacts <- as.factor(modeldata_VOS$ObservableImpacts)
summary(modeldata_VOS$ObservableImpacts)
nlevels(modeldata_VOS$ObservableImpacts)

modeldata_VOS$Site.Type <- as.factor(modeldata_VOS$Site.Type)
summary(modeldata_VOS$Site.Type)
nlevels(modeldata_VOS$Site.Type)

modeldata_VOS$Group.Type <- as.factor(modeldata_VOS$Group.Type)
summary(modeldata_VOS$Group.Type)
nlevels(modeldata_VOS$Group.Type)


table(modeldata_VOS$Site.Type, modeldata_VOS$ObservableImpacts)
table(modeldata_VOS$Group.Type, modeldata_VOS$ObservableImpacts)
table(modeldata_VOS$Group.Type, modeldata_VOS$Site.Type)

table(modeldata_VOS$Group.Type)


table(modeldata_VOS$Year, modeldata_VOS$Site.Type)
table(modeldata_VOS$Site.Type, modeldata_VOS$Year)
table(modeldata_VOS$Year, modeldata_VOS$Site.Type)

modeldata_VOS$ImpSeverity <- (modeldata_VOS$highsImps)/(modeldata_VOS$highsImps+modeldata_VOS$totalLowImp)
ggplot(modeldata_VOS, aes(x=Site.Type, y=ImpSeverity)) + 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2))


table(modeldata_VOS$Site.Type, modeldata_VOS$highsImps)
table(modeldata_VOS$Site.Type, modeldata_VOS$highsImps)



## Looking at which varibles still contain NAs
list <- c()
for (i in names(modeldata_VOS)){
  list[i] <-length(which(is.na(modeldata_VOS[,i])))
}
print(list) ## all variables should be zero 


##Quick look at model dataframe
##Factors
for (i in names(Filter(is.factor, modeldata_VOS))) {
  plot(modeldata_VOS[,i],
       main = paste(i))
}
## Numeric variables
for (i in names(Filter(is.numeric, modeldata_VOS))) {
  hist(modeldata_VOS[,i],
       breaks = 3000,
       main = paste(i),
       xlab = paste(i))
}

##Scaling
##dput(names(modeldata))
cont_vars <- c("Group.Size", "totalActivities", "totalimpacts", "prop.Act.HM", "Prop.Imp.HM")
##cont_vars <- c("Group.Size", "totalActivities", "totalimpacts", "Impact_Index")
for (i in cont_vars){
  modeldata_VOS[, i] <- c(scale(modeldata_VOS[,i]))
}


#############################Maximal model
##install.packages("blme")
library(blme)

library(ggplot2)
ggplot(data = modeldata_VOS, aes(x = Group.Type, y = ObservableImpacts)) +
  geom_point()


modeldata_VOS$Site.Type <- relevel(modeldata_VOS$Site.Type, ref = "Built Infrastructure")

#####################
###no opt

modsBINOMIAL <- NULL
modsBINOMIAL$max <- glmer(ObservableImpacts ~ Group.Type + Group.Size + totalActivities + 
                            prop.Act.HM + Site.Type + Year + totalActivities:prop.Act.HM + 
                            Group.Size:prop.Act.HM + Group.Size:totalActivities + (1 | Site.Name), 
                          family = binomial,  control = glmerControl(optimizer = c("bobyqa"), optCtrl = list(maxfun = 2e4)), 
                          data=modeldata_VOS)


model <- glmer(ObservableImpacts ~ Group.Type + Mode.of.Transport + Group.Size + totalActivities +
                 prop.Act.HM + Site.Type + totalActivities:prop.Act.HM +
                 Group.Size:prop.Act.HM + Group.Size:totalActivities + (1 | Site.Name:Year) + (1 | Year),
               family = binomial,
               data=modeldata_VOS)

library(sp)
library(raster)
##install.packages("dfoptim")
library(dfoptim)
##install.packages("afex")
library(afex)
##install.packages("optimx")
library(optimx)
##install.packages("parallel")
library(parallel)
library(lme4)
library(Matrix)
diff_optims <- allFit(model, maxfun = 1e5, parallel = 'multicore', ncpus = detectCores())
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)

convergence_results <- lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
working_indices <- sapply(convergence_results, is.null)

if(sum(working_indices)==0){
  print("No algorithms from allFit converged.")
  print("You may still be able to use the results, but proceed with extreme caution.")
  first_fit <- NULL
} else {
  first_fit <- diff_optims[working_indices][[1]]
}
first_fit

dredgeImpactObservedMax_YearAdded_withmode <- dredge(modsBINOMIAL$Mode, trace = 2)
summary(modsBINOMIAL$maxMode)

largemixedgroup <- modeldata_VOS %>% filter(Group.Type == "MixedOver18LargeGroup" )
largemixedgroup_transportcount <- largemixedgroup %>% count(Mode.of.Transport)

options(na.action = "na.fail")
dredgeImpactObservedMax_YearAdded <- dredge(modsBINOMIAL$max, trace = 2)
options(na.action = "na.omit")


modsBINOMIAL$actualBESTequivelant <- glmer(ObservableImpacts ~ Group.Type + Group.Size + totalActivities + 
                                             prop.Act.HM + Site.Type + totalActivities:prop.Act.HM +  (1 | Site.Name), 
                                           family = binomial,  control = glmerControl(optimizer = c("bobyqa"), optCtrl = list(maxfun = 2e4)), 
                                           data=modeldata_VOS)


#get.models(dredge(modsBINOMIAL$max), subset=TRUE)
AICc <- sapply(modsBINOMIAL, AICc)
AICtable(AICc)


##totalimpacts*prop.Act.HM
quantile(modeldata_VOS$prop.Act.HM, probs = c(.25, .5, .75))
Activity_Intensity_ProportionHM_Catagories <- c(-0.387143, -0.387143, -0.387143)
plot(modeldata_VOS$prop.Act.HM, modeldata_VOSp$Prop.Imp.HM)
plot_model(modsBINOMIAL$actualBESTequivelant, type = "pred", terms = c("totalActivities", "prop.Act.HM"))

#the best model from the dredge is as follows
# modsBINOMIAL$best <- glmer(ObservableImpacts ~ Group.Size + totalActivities + prop.Act.HM + totalActivities:prop.Act.HM + Group.Size:totalActivities + (1 | Site.Name) + (1 | Year), family = binomial,  control = glmerControl(optimizer = c("bobyqa"), optCtrl = list(maxfun = 2e4)), nAGQ = 10, data=modeldata_VOS)
# modsBINOMIAL$equivelant <- glmer(ObservableImpacts ~ Group.Size + totalActivities + prop.Act.HM + totalActivities:prop.Act.HM + (1 | Site.Name) + (1 | Year), family = binomial,  control = glmerControl(optimizer = c("bobyqa"), optCtrl = list(maxfun = 2e4)), nAGQ = 10, data=modeldata_VOS)
# modsBINOMIAL$new <- glmer(ObservableImpacts ~ Group.Type + Site.Type + totalActivities + prop.Act.HM + totalActivities*prop.Act.HM + (1 | Site.Name) + (1 | Year), family = binomial,  control = glmerControl(optimizer = c("bobyqa"), optCtrl = list(maxfun = 2e4)), nAGQ = 10, data=modeldata_VOS)
modsBINOMIAL$actualBEST <- glmer(ObservableImpacts ~ Group.Size + Group.Type + Site.Type + totalActivities + prop.Act.HM + totalActivities:prop.Act.HM + Group.Size:totalActivities + (1 | Site.Name), family = binomial,  control = glmerControl(optimizer = c("bobyqa"), optCtrl = list(maxfun = 2e4)), data=modeldata_VOS)
#modsBINOMIAL$actualBESTequivelant <- glmer(ObservableImpacts ~ Group.Size + Group.Type + Site.Type + totalActivities + prop.Act.HM + totalActivities:prop.Act.HM + (1 | Site.Name) + (1 | Year), family = binomial,  control = glmerControl(optimizer = c("bobyqa"), optCtrl = list(maxfun = 2e4)), nAGQ = 10, data=modeldata_VOS)


AICc <- sapply(modsBINOMIAL, AICc)
AICtable(AICc)
logLik(modsBINOMIAL$actualBESTequivelant)

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
plot_model(modsBINOMIAL$actualBESTequivelant, show.values = TRUE, value.offset = .4)
plot_model(modsBINOMIAL$actualBESTequivelant, type = "pred")

tab_model(modsBINOMIAL$actualBEST, show.se = TRUE)
summary(modsBINOMIAL$actualBEST)
tab_model(modsBINOMIAL$actualBESTequivelant, show.se = TRUE)
summary(modsBINOMIAL$max)
summary(modsBINOMIAL$actualBESTequivelant)

cat(coef(summary(modsBINOMIAL$actualBEST))[,"z value"])
fixef(modsBINOMIAL$actualBESTequivelant)

summary(modsBINOMIAL$mod29)
##assumtion testing
plot(modsBINOMIAL$actualBESTequivelant)
plot(residuals(modsBINOMIAL$actualBESTequivelant))
qqline((residuals(modsBINOMIAL$actualBESTequivelant)), col = "steelblue", lwd = 2)
hist(residuals(modsBINOMIAL$actualBESTequivelant))
max(residuals(modsBINOMIAL$actualBESTequivelant))
?residuals
qplot((residuals(modsBINOMIAL$actualBESTequivelant)),
      geom = "histogram",
      bins = 10) +
  labs(title = "Histogram of residuals",
       x = "residual")
?qplot
rand<-data.frame(ranef(modsBINOMIAL$actualBESTequivelant))
str(ranef(modsBINOMIAL$actualBESTequivelant))
hist(rand$condval)
overdisp <- deviance(modsBINOMIAL$actualBESTequivelant)/df.residual(modsBINOMIAL$actualBESTequivelant)


plot(modsBINOMIAL$max)

plot(residuals(modsBINOMIAL$max))
##plot(residuals(modsBINOMIAL$mod21))


qqnorm(residuals(modsBINOMIAL$max))
qqline((residuals(modsBINOMIAL$max)), col = "steelblue", lwd = 2)


qplot((residuals(modsBINOMIAL$max)),
      geom = "histogram",
      bins = 10) +
  labs(title = "Histogram of residuals",
       x = "residual")
?qplot


hist(residuals(modsBINOMIAL$max))
max(residuals(modsBINOMIAL$max))
?residuals

rand<-data.frame(ranef(modsBINOMIAL$max))
str(ranef(modsBINOMIAL$max))

hist(rand$condval)

vif(modsBINOMIAL$max)
#vif(modsBINOMIAL$best)
#vif(modsBINOMIAL$mod21)

########################################
#########################################


#######################

modeldata_VOS_imp <- filter(modeldata_VOS, ObservableImpacts == 1)
##modeldata_VOS_imp$Site.Type <- relevel(modeldata_VOS_imp$Site.Type, ref = "Highly Managed")
hist2 <- hist(modeldata_VOS_imp$Prop.Imp.HM)
nrow(modeldata_VOS_imp)

##modeldata_VOS_imp$prop.Act.HM <- logit(modeldata_VOS_imp$prop.Act.HM)
sum_high_and_mod_simps <- sum(modeldata_VOS_imp$highsImps)
sum_totalimps <- sum(modeldata_VOS_imp$totalimpacts)
sum_high_and_mod_simps/sum_totalimps
sum(modeldata_VOS_imp$totalLowImp)/sum_totalimps
sum_high_and_mod_simps+sum(modeldata_VOS_imp$totalLowImp)
sum_totalimps
sum_high_and_mod_simps
##install.packages("nlme")
##install.packages("lme4")
##install.packages("Matrix")
##library(nlme)
##library(Matrix)
##library(lme4)


############################

########
modsBinary <- NULL

##########################
model <- glmer(cbind(highsImps, totalLowImp) ~ totalimpacts + Group.Type + Group.Size + totalActivities + prop.Act.HM + Site.Type + 
                 totalActivities:prop.Act.HM + Group.Size:prop.Act.HM + Group.Size:totalActivities + totalimpacts:Group.Size + 
                 totalimpacts:prop.Act.HM + totalimpacts:totalActivities + totalActivities:prop.Act.HM:totalimpacts + 
                 Site.Type + (1 | Site.Name), family = binomial, modeldata_VOS_imp)


modsBinary$max <- glmer(cbind(highsImps, totalLowImp) ~ totalimpacts + Group.Type + Group.Size + totalActivities + prop.Act.HM + Site.Type + Year +
                          totalActivities:prop.Act.HM + Group.Size:prop.Act.HM + Group.Size:totalActivities + totalimpacts:Group.Size + 
                          totalimpacts:prop.Act.HM + totalimpacts:totalActivities + totalActivities:prop.Act.HM:totalimpacts + 
                          (1 | Site.Name), family = binomial, control = glmerControl(optimizer = "bobyqa" , optCtrl = list(maxfun = 1e5)), 
                        modeldata_VOS_imp)

##na.action = "na.fail"
#library(MuMIn)

options(na.action = "na.fail")
dredge_binomial_severity <- dredge(modsBinary$max, trace = 2)
dredge(modsBinary$max, trace = 2)
#options(na.action = "na.omit")

##write.csv(dredge_binomial_severity,"C:\\Users\\AndrewT\\OneDrive - CAAS (Environmental Services) Ltd/Desktop\\AndyPhD\\PhD_Data_R\\dredge_binomial_severity.csv", row.names = FALSE)


library(HDInterval)
library(mcmcOutput)
library(wiqid)

#get.models(dredge(modsBinary$max), subset=TRUE)
AICc <- sapply(modsBinary, AICc)
AICtable(AICc)

modsBinary$best_withYear <- glmer(cbind(highsImps, totalLowImp) ~ totalimpacts + totalActivities + prop.Act.HM + Site.Type + Year +
                                    totalActivities:prop.Act.HM + totalimpacts:prop.Act.HM + totalimpacts:totalActivities + 
                                    totalActivities:prop.Act.HM:totalimpacts + (1 | Site.Name),
                                  family = binomial, control = glmerControl(optimizer = "bobyqa" , optCtrl = list(maxfun = 1e5)), 
                                  modeldata_VOS_imp)
AICc(modsBinary$best_withYear)
summary(modsBinary$best_withYear)
plot_model(modsBinary$best_withYear, show.values = TRUE, value.offset = .4)
plot_model(modsBinary$best_withYear, type = "pred")

tab_model(modsBinary$best_withYear, show.se = TRUE)

##modeldata_VOS$ImpSeverity <- (modeldata_VOS$highsImps)/(modeldata_VOS$highsImps+modeldata_VOS$totalLowImp)
ggplot(modeldata_VOS_imp, aes(x=Site.Type, y=ImpSeverity)) + 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2))

ggplot(modeldata_VOS_imp, aes(x=Year, y=ImpSeverity)) + 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2))



AICc(modsBinary$test)

AICc(modsBinary$best)
summary(modsBinary$best)
plot_model(modsBinary$best, show.values = TRUE, value.offset = .4)
plot_model(modsBinary$best, type = "pred")

tab_model(modsBinary$best, show.se = TRUE)

## Graphing interactions
##totalimpacts*prop.Act.HM
quantile(modeldata_VOS_imp$totalimpacts, probs = c(.25, .5, .75))
ImpactRichness_Catagories <- c(0.8420889, 0.8420889, 2.1717031)
plot(modeldata_VOS_imp$prop.Act.HM, modeldata_VOS_imp$Prop.Imp.HM)
plot_model(modsBinary$best_withYear, type = "pred", terms = c("totalimpacts", "prop.Act.HM"))

##Total Activities*Proportion of Activities High or Moderate
quantile(modeldata_VOS_imp$totalActivities, probs = c(.25, .5, .75))
ActivityRichness_Catagories <- c(-0.1313627,  0.6411282,  1.4136191)
plot(modeldata_VOS_imp$prop.Act.HM, modeldata_VOS_imp$Prop.Imp.HM)
plot_model(modsBinary$best_withYear, type = "pred", terms = c("totalActivities", "prop.Act.HM"))

##Total Activities*totalimpacts
quantile(modeldata_VOS_imp$totalimpacts, probs = c(.25, .5, .75))
ImpactRichness_Catagories <- c(0.8420889, 0.8420889, 2.1717031)
plot(modeldata_VOS_imp$totalimpacts, modeldata_VOS_imp$totalimpacts)
plot(modeldata_VOS_imp$prop.Act.HM, modeldata_VOS_imp$Prop.Imp.HM)
plot_model(modsBinary$best_withYear, type = "pred", terms = c("totalActivities", "totalimpacts", "prop.Act.HM"))

################### allFit code


diff_optims <- allFit(model, maxfun = 1e5, parallel = 'multicore', ncpus = detectCores())
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)

convergence_results <- lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
working_indices <- sapply(convergence_results, is.null)

if(sum(working_indices)==0){
  print("No algorithms from allFit converged.")
  print("You may still be able to use the results, but proceed with extreme caution.")
  first_fit <- NULL
} else {
  first_fit <- diff_optims[working_indices][[1]]
}
first_fit
############################
