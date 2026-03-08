library(tidyverse)
library(skimr)
library(GGally)
library (dplyr)
library(ggplot2)
library(psych)
library(readxl)
library(car)
library(lmtest)
library(whitestrap)

### Import data set with set up dummy variables ###
FPA_FinalProject_RData <- read_excel("~/FPA_FinalProject_RData.xlsx")
view(FPA_FinalProject_RData)
glimpse(FPA_FinalProject_RData)
FPA_FinalProject_RData$`Total Rings (Y)`

###OLS Regression###
model <- lm(data = FPA_FinalProject_RData, `Total Rings (Y)` ~ `Career PPG (X1)`+`Career APG (X2)`+`Career RPG (X3)`+`Career SPG (X4)`+
              `Career BPG (X5)`+`Career FG% (X6)`+`Career 3PFG% (X7)`+`Career Points High(X8)`+`Career Assists High(X9)`+`Career Rebounds High(X10)`+
              `Playoff Appearances (X11)`+ `All NBA 1st Team selections (X12)`+`Career Wins(X13)`+ `Career Losses(X14)`+
              `All NBA Defensive Team Selections(X15)`+ `Seasons Played (X16)`+`All Star Selections (X17)`+`Total MVPs (X18)`+
              `Conference-East(Dummy)(X19)`+`Position-Forward(Dummy)(X20)`+`Position-Center(Dummy)(X21)`+`Time Period-Expansion(Dummy)(X22)`+
              `Time Period-Modern(Dummy)(X23)`)
summary(model)
model_residuals <- residuals(model)
sum(model_residuals)
### sum of residuals 0 (3.080869e-15) ###
### Significant variables are FG%, Wins, Losses, Eastern Conference ###

### DW Test (two tailed) ###
durbinWatsonTest(model)
### Dstat = 1.829056 ###
### alpha = 0.05, n = 75 k = 20 du = 1.027  dL = 2.315 ###
### Dstat = 1.829 is between dU dL so the test is inconclusive ###

### BP Test ###
bptest(model)
### BP Stat = 31.411 chi square_23_0.05= 35.172 ###
### BP Stat < chi square so we do not reject H0 ###

### White test ###
white_test(model)
### White_Stat: 16.28 p value: 0.000292 ###
### Chi square_22_0.05 = 33.924 ###
### Wstat < critical value so we do not reject H0 ###

### New model with only significant data ###
model2 =lm(data = FPA_FinalProject_RData, `Total Rings (Y)`~ `Career FG% (X6)`+`Career Wins(X13)`+`Career Losses(X14)`+`Conference-East(Dummy)(X19)`)
summary(model2)
model2_residuals <- residuals(model2)
sum(model2_residuals)
### Sum of Residuals = 0 (2.331468e-15) ###
### Playing in the eastern conference became largely more significant, as did the intercept and career wins and losses. 
### FG% became slightly less significant ###

### DW Test (two tailed) ###
durbinWatsonTest(model2)
### Dstat = 2.028797 dL = 1.51 dU = 1.74 N=75 K=4 ###
### Dstat> dU so we do not reject H0 ###

### BP Test ###
bptest(model2)
### BPstat = 18.685 chisquare_4_0.05 = 9.488 ###
### BPstat > chisquare so we reject H0, evidence of heteroscedasticity ###

### White Test ###
white_test(model2)
### WStat = 26.32 chisquare_3_0.05 = 7.815 ###
### Wstat > chi_square so we reject H0, more evidence of heteroscedasticity ###

### Normality/Histogram of dependent values ###
qplot(FPA_FinalProject_RData$`Total Rings (Y)`)
### Not normally distributed ###

### Residual plot vs fitted values ###
plot(model, which=1, col=c("darkblue"))
### points closely follow the curve of the line with few outliers ###
plot(model2,which=1, col=c("green"))
### point are more closely along the curve with the same few outliers ###


### Residual Analysis ###
FPA_FinalProject_RData$predicted <- predict(model)
FPA_FinalProject_RData$residuals <- residuals(model)
ggplot(FPA_FinalProject_RData, aes(x=`Career PPG (X1)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career APG (X2)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career RPG (X3)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career SPG (X4)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career BPG (X5)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career FG% (X6)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career 3PFG% (X7)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career Points High(X8)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career Assists High(X9)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career Rebounds High(X10)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Playoff Appearances (X11)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`All NBA 1st Team selections (X12)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career Wins(X13)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Career Losses(X14)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`All NBA Defensive Team Selections(X15)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Seasons Played (X16)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`All Star Selections (X17)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Total MVPs (X18)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Conference-East(Dummy)(X19)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Position-Forward(Dummy)(X20)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Position-Center(Dummy)(X21)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Time Period-Expansion(Dummy)(X22)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))

ggplot(FPA_FinalProject_RData, aes(x=`Time Period-Modern(Dummy)(X23)`, y=`Total Rings (Y)`)) +
  geom_smooth(method ="lm", se = FALSE, color="blue")+
  geom_segment(aes(xend=`Career PPG (X1)`, yend = predicted),alpha = 0.2)+ 
  geom_point(aes(color=abs(residuals), size=abs(residuals)))
