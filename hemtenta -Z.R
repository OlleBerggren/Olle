Assignment 1

setwd("~/Documents/Rdata")
source("GraphPlot.R")
openGraph()

#ändra STAI id18, mindfulness 2=1.000, 1 - på income

str(hemtenta)
summary(hemtenta)

hemtenta_cleaned <- hemtenta %>% 	
  mutate(	
    STAI_trait = as.numeric(replace(STAI_trait, STAI_trait == 3.5, 35)))

hemtenta_cleaned %>% summary()
describe(hemtenta_cleaned)

hemtenta_cleaned %>% ggplot() + aes( x= age) + geom_histogram()
hemtenta_cleaned %>% ggplot() + aes( x= pain_cat) + geom_histogram()
hemtenta_cleaned %>% ggplot() + aes( x= cortisol_serum) + geom_histogram()
hemtenta_cleaned %>% ggplot() + aes( x= cortisol_saliva) + geom_histogram()
hemtenta_cleaned %>% ggplot() + aes( x= STAI_trait) + geom_histogram()
hemtenta_cleaned %>% ggplot() + aes( x= mindfulness) + geom_histogram()

mod1 <- lm( formula = pain ~ age + sex,
                                     data = hemtenta_cleaned )
print(mod1)
summary(mod1)
error_plotter(mod1, col = "blue")
mod2 <- lm( formula = pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva,
            data = hemtenta_cleaned )
summary(mod1)


#boxplot

summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared
anova(mod1, mod2)

confint( object = mod1)
confint( object = mod2)
confint( object = mod3)


standardCoefs(mod1 )
standardCoefs(mod2 )
standardCoefs(mod3)

residuals( object = mod1 )
rstandard( model = mod2 )
rstudent( model = mod2 )
hatvalues( model = mod1 )
hatvalues( model = mod2 )
cooks.distance( model = mod1 )
plot(x = mod1, which = 4)
plot(x = mod2, which = 4) # about 0.06, a lot less than 1
plot(x = mod1, which = 5)
plot(x = mod2, which = 5)
plot(x = mod1, which = 2)
plot(x = mod2, which = 2)
plot(x = mod2, which = 1)

hist( x = residuals( mod1 ),
           xlab = "Value of residual",
       main = "",
      breaks = 20)
hist( x = residuals( mod2 ),
      xlab = "Value of residual",
      main = "",
      breaks = 20)
describe(residuals(mod2)) #normaility looks good

lm( formula = pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva,
    data = hemtenta_cleaned, subset = -88 )

?shapiro.test

yhat.2 <- fitted.values( object = mod2 )
plot( x = yhat.2,
             y = hemtenta_cleaned$pain,
               xlab = "Fitted Values",
         ylab = "Observed Values" )

residualPlots( model = mod2 ) # linearity looks good
residualPlots( model = mod3 ) # linearity looks good



plot(x = mod2, which = 3)
ncvTest( mod2 )
bptest(mod2) #homoskedacity looks good
bptest(mod3) #homoskedacity looks good
coeftest( mod2, vcov= hccm ) 
coeftest( mod3, vcov= hccm )
lm.beta(mod3)
summary(mod3)

vif(mod2) #colinearity, ta bort cortisol_saliva
mod3 <- lm( formula = pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum,
            data = hemtenta_cleaned ) # cortisol saliva removed

vif(mod3)



AIC(mod1, mod3)
summary(anova(mod1, mod3))






