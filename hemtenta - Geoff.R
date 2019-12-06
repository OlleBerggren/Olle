
my.df = data.frame(sex,pitch)
xmdl = lm(pitch ~ sex, my.df)
summary()
require(tidyr)
library(tidyverse)
library(tibble)
library(smacof)
library(sandwich)
library(r2glmm)
library(psych)
library(MuMIn)
library(MASS)
library(lsr)
library(lmtest)
library(lmerTest)
library(lme4)
library(lattice)
library(influence.ME)
library(ggplot2)
library(dplyr)
library(car)
library(cAIC4)

setwd("~/Documents/Rdata")
source("GraphPlot.R")
openGraph()
data_cleaned <- spread(PAQ_Olle, var, value) #omvandla till brett format



data_cleaned11 <- data_cleaned [-1] # tar vort variabel id, vilken är deltagare

data_cleaned1_1 <- drop_na(data_cleaned11) # ta bort na
data_cleaned1 <- data_cleaned1_1[ , -c(1,11)] #ta bort age och sex

View(data_cleaned1)
summary(data_cleaned1)
describe(data_cleaned1)


anx_pca <- princomp(data_cleaned1[,-1], cor = TRUE)
summary(anx_pca)
pairs(anx_pca$scores[,1:3], ylim = c(-6, 4), xlim = c(-6, 4),
      panel = function(x,y, ...) {
        text(x, y, abbreviate(row.names(data_cleaned1)), 
             cex = 0.6)
        bvbox(cbind(x,y), add = TRUE)
      })

data_cleaned2=princomp(data_cleaned1,cor=TRUE)
summary(data_cleaned2, loadings=TRUE)
data_cleaned3=princomp(data_cleaned1,cor=FALSE)
summary(data_cleaned3, loadings=TRUE)

cor(data_cleaned1)                                         
plot(data_cleaned1) 

plot(data_cleaned2$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "l", main = "Scree diagram") 
 plot(log(data_cleaned2$sdev^2), xlab = "Component number",
                                    ylab = "log(Component variance)", type="l",
                                   main = "Log(eigenvalue) diagram")
 
 biplot(anx_pca, col = c("gray", "black"))
