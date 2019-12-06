Assignment 2

mob1 <- lm(formula = pain ~ age + sex + STAI_trait + pain_cat + mindfulness +  cortisol_serum + weight + IQ + household_income,
           data = hemtenta_cleaned)



cooks.distance( model = mod1 )
plot(x = mob1, which = 4)

plot(x = mob1, which = 5)

plot(x = mob1, which = 2)

plot(x = mob1, which = 1)

rstandard( model = mob1 )

hist( x = residuals( mob1 ),
      xlab = "Value of residual",
      main = "",
      breaks = 20)

yhat.3 <- fitted.values( object = mob1 )
plot( x = yhat.3,
      y = hemtenta_cleaned$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values" )

residualPlots( model = mob1 )

plot(x = mob1, which = 3)


coeftest( mob1, vcov= hccm ) 




step( object = mob1)

mob1_1 <- lm(formula = pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + 
               weight, data = hemtenta_cleaned)

summary(mob1_1)
summary(mod3)
standardCoefs(mob1_1 )
confint( object = mob1_1)


cooks.distance( model = mob1_1 )

plot(x = mob1_1, which = 4) # about 0.06, a lot less than 1
plot(x = mob1_1, which = 3)
plot(x = mob1_1, which = 5)
plot(x = mob1_1, which = 2)
plot(x = mob1_1, which = 1)

hist( x = residuals( mob1_1 ),
      xlab = "Value of residual",
      main = "",
      breaks = 20)
describe(residuals(mob1_1)) #normaility looks good

yhat.2 <- fitted.values( object = mob1_1 )
plot( x = yhat.2,
      y = hemtenta_cleaned$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values" )

residualPlots( model = mob1_1 ) # linearity looks good

plot(x = mob1_1, which = 3)
ncvTest( mob1_1 )
bptest(mob1_1) #homoskedacity looks good


vif(mob1_1) #colinearity 

AIC(mob1, mob1_1)

AIC(mod3, mob1_1)
summary(anova(mod3, mob1_1))
anova(mob1, mob1_1)

pred_test <- predict(mod3, home_sample_2.csv)	
pred_test_back <- predict(mob1_1, home_sample_2.csv)
RSS_test = sum((home_sample_2.csv[,"pain"] - pred_test)^2)
RSS_test
RSS_test_back = sum((home_sample_2.csv[,"pain"] - pred_test_back)^2)
RSS_test_back
