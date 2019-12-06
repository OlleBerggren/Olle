#Assignment 3 - Z

stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

describe(home_sample_3.csv)
summary(home_sample_3.csv)
home_cleaned <- home_sample_3.csv %>% 	
  mutate(	
    sex = droplevels(replace(sex, sex == "Female", "female")))
summary(home_cleaned)

home_cleaned %>% ggplot() + aes( x= age) + geom_histogram()
home_cleaned %>% ggplot() + aes( x= pain_cat) + geom_histogram()
home_cleaned %>% ggplot() + aes( x= cortisol_serum) + geom_histogram()
home_cleaned %>% ggplot() + aes( x= cortisol_saliva) + geom_histogram()
home_cleaned %>% ggplot() + aes( x= STAI_trait) + geom_histogram()
home_cleaned %>% ggplot() + aes( x= mindfulness) + geom_histogram()

home_cleaned %>% 		
  ggplot() +		
  aes(y = pain, x = age) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

home_cleaned %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)
home_cleaned %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)
home_cleaned %>% 		
  ggplot() +		
  aes(y = pain, x = STAI_trait) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)
home_cleaned %>% 		
  ggplot() +		
  aes(y = pain, x = mindfulness) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

int_plot = home_cleaned %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)	
int_plot

mixed_mod1 = lmer( formula = pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum +
                (1|hospital),   data = home_cleaned )

summary(mixed_mod1)

influence_observation = influence(mixed_mod1, obs = T)$alt.fixed
influence_group = influence(mixed_mod1, group = "hospital")$alt.fixed

influence_observation

data_plot_inflience = as_tibble(influence_group) %>% 	
  gather(colnames(influence_group), value = coefficient, key = predictor)	

data_plot_inflience %>% 	
  ggplot() +	
  aes(x = 1, y = coefficient, group = predictor) +	
  geom_violin() +	
  facet_wrap( ~ predictor, scales = "free")
qqmath(mixed_mod1, id=0.05) #normality
qqmath(ranef(mixed_mod1))
plot(mixed_mod1, arg = "pearson") #linearity
# You should also look at the scatterplot of the residuals and the fixed predictors separately.

home_cleaned = home_cleaned %>% 	
  mutate(resid = residuals(mixed_mod1))
homosced_mod = lm(resid^2 ~ hospital, data = home_cleaned)	
summary(homosced_mod)
plot(x = mixed_mod1, which = 3)
pairs.panels(home_cleaned[,c("age", "sex", "STAI_trait", "pain_cat", 
        "mindfulness", "cortisol_serum" )], col = "red", lm = T)



describe(residuals(mixed_mod1)) #normaility looks good

sum(residuals(mixed_mod1)^2)
#linearity
#homoskedacity



vif(mixed_mod1) #colinearity looks good

mod3 <- lm( formula = pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum,
            data = hemtenta_cleaned )

sum(residuals(mod3)^2)
sum(residuals(mixed_mod1)^2)

AIC(mod3)		
cAIC(mixed_mod1)$caic
anova(mod3, mixed_mod1)

r2beta(mixed_mod1, method = "nsj", data = home_cleaned)
r.squaredGLMM(mixed_mod1)

stdCoef.merMod(mixed_mod1)
standardCoefs(mod3 )
confint( object = mixed_mod1)
summary(mod3)$adj.r.squared
summary(mixed_mod1)
describe(home_sample_4.csv)
summary(home_sample_4.csv)

RSS = sum((home_sample_4.csv$pain - predict(mixed_mod1))^2)	
RSS
mod_mean <- lm(pain ~ 1, data = home_sample_4.csv)
TSS = sum((home_sample_4.csv$pain - predict(mod_mean))^2)
TSS
R2 = 1-(RSS/TSS)
R2

mod_rnd_int = lmer(pain ~ cortisol_serum + (1|hospital), data = home_cleaned)
mod_rnd_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = home_cleaned)		

int_plot = mod_rnd_int %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)	
int_plot
int_plot+		
  xlim(-1, 50)+		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)

slope_plot = mod_rnd_slope %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE) +		
  xlim(-1, 50)+		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)		
slope_plot








