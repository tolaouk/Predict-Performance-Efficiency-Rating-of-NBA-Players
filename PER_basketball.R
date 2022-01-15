
## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------
library(mltools)
library(data.table)
library(corrplot)
library(here)
library(dplyr)
library(ALSM)

main_df = read.csv(here("nba2021_advanced.csv"), sep=',')
#write.csv(main_df,"C:\\Users\\JS Home\\Documents\\MSDS\\MATH 6357\\M6357Project\\marketing_campaign_fixed.csv",sep=',',row.names = FALSE)


data = main_df[!(duplicated(main_df$Player)),] #delete entries for separate teams
summary(data$MP)
summary(data$G)
boxplot(data$MP)

# filter data to players with >200 mins played
bymp = data[(data$MP>=200),]
bymp$Player

#obtain the stats and rename rows for analysis
stat = bymp[,c("PER","WS", "USG.","TS.", "Age", "FTr", "X3PAr")]
row.names(stat)=bymp$Player

#correlation matrix & correlation plots
round(cor(stat[]),3)
pairs(stat)
summary(stat$PER)
summary(stat)

corrplot(cor(stat[]))
model = lm(PER~., data=stat)

vif(model)
# since all VIF are <4, no need to adjust for multicollinearity

#scatterplots for all variables against PER
plot(stat[,2],stat[,1],ylab="PER",xlab="Win Shares",main="PER vs. Win Shares")
plot(stat[,3],stat[,1],ylab="PER",xlab="Usage",main="PER vs. Usage")
plot(stat[,4],stat[,1],ylab="PER",xlab="True Shooting %",main="PER vs. TS%")
plot(stat[,5],stat[,1],ylab="PER",xlab="Age (Years)",main="PER vs. Age")
plot(stat[,6],stat[,1],ylab="PER",xlab="Free Throw Frequency",main="PER vs. FT Frequency")
plot(stat[,7],stat[,1],ylab="PER",xlab="Three Point Frequency",main="PER vs. Three Point Frequency")


## ---------------------------------------------------------------------------------------------------------------
# library to show names on boxplots
library(car)
# full model
per.lm = lm(PER~., data=stat)
summary(per.lm)
anova(per.lm)

#show boxplots for each variable
Boxplot(stat$PER, main="PER",id=list(labels=rownames(stat),n=2, location = "u"))
Boxplot(stat$WS, main="WS",id=list(labels=rownames(stat),n=1))
Boxplot(stat$USG., main="USG", id=list(labels=rownames(stat),n=2, location="u"))
Boxplot(stat$TS.,main="TS",id=list(labels=rownames(stat),avoid=T))
boxplot(stat$Age,main="Age")
Boxplot(stat$FTr,main="FTr",id=list(labels=rownames(stat),n=2))
boxplot(stat$X3PAr,main="X3")

## ---------------------------------------------------------------------------------------------------------------
# Full Model:
per.lm = lm(PER~., data=stat)
summary(per.lm)
## F-critical
qf(0.95, 6, 327)



# Reduced Model:
reduced.lm = lm(PER~WS+USG.+TS.+X3PAr, data=stat)
summary(reduced.lm)



anova(reduced.lm, per.lm)
# F-Critical
qf(0.95, 2, 327)



# Stepwise Regression:

start.model = lm(PER ~ 1, data = stat)

step.backward = step(per.lm, direction = "backward")
step.forward = step(start.model, direction = "forward", scope = formula(per.lm))
step.both = step(per.lm, direction = "both")

step.backward
step.forward
step.both

step.lm = lm(PER~ WS + USG. + TS. + X3PAr + Age, data=stat)
summary(step.lm)
anova(step.lm, per.lm)
# F-Critical
qf(0.95, 1, 327)


## ---------------------------------------------------------------------------------------------------------------


library(caret)
set.seed(12345)

#set up k-fold cross validation
train.control = trainControl(method="cv", number = 5)
# models with all, 5, and 4 predictors
modelall = train(PER~., data=stat, method="lm", trControl = train.control)
model4 = train(PER~WS+USG.+TS.+X3PAr, data=stat, method="lm", trControl = train.control)
model5 = train(PER~WS+USG.+TS.+X3PAr+FTr, data=stat, method="lm", trControl = train.control)
# show R-squared and RMSE for cross-validated models
print(modelall)
print(model4)
print(model5)

c(modelall$results$RMSE, model4$results$RMSE, model5$results$RMSE)

# anova for 4 predictors vs full model
model4pred = lm(PER~WS+USG.+TS.+X3PAr+FTr, data=stat)
anova(model4pred, per.lm)


## ---------------------------------------------------------------------------------------------------------------
# redisplay cross validated 4 predictor model results
print(model4)

# rebuild model with 4 predictors
model = lm(PER~WS+USG.+TS.+X3PAr, data=stat)

#deleted studentized residuals 
dtres = rstudent(model)

#t critical value
qt((0.05/(2*334)), (334-4-1))

plot(rstudent(model), type="o", xlab = "Index", main="Deleted Studentized Residuals", ylab="Deleted t-residuals")
# t* = 3.83
abline(h=3.836, col="red")
abline(h=-3.836, col="red")

# Show players exceeding t-critical value
dtres[dtres>=3.836]
# Willy Hernangomez        CJ McCollum    
#           3.943443           4.162823         

# Cook's distance plot
cd = cooks.distance(model)
plot(cd, type="o", xlab = "Index", main = "Cook's Distance")
#text(cd, labels=rownames(stat), cex=0.9, font=2)
# all cook's distances are very small, <<1, so likely not influential

# calculate DFFITS
dffit = dffits(model)
cutoff = 2*sqrt(6/334)
# cutoff ~ 0.26726
plot(dffit, type="o", xlab="Index", main = "DFFITS")
# show players that exceed DFFITS
dffit[dffit>.267|dffit<(-.267)]
#text(dffit, labels=rownames(stat), cex=0.9, font=2)
abline(h=cutoff, col="red")
abline(h=-cutoff, col="red")
# lots of influential points 


## ---------------------------------------------------------------------------------------------------------------
# Fitted vs residuals plot
plot(predict(model, stat),residuals(model), main="Residual vs. Fits", xlab = "Fitted Values", ylab = "Residuals")
# sequence plot
plot(1:334,residuals(model), main="Sequence Plot", xlab = "Index", ylab = "Residuals",type="o")
abline(h=0, col="red")

# boxplot of residuals
boxplot(residuals(model))

# QQ-plot for normality
qqplot(predict(model, stat),residuals(model))
qqline(predict(model, stat),residuals(model))
