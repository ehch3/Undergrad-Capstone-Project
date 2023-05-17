# May 13, 2022
# Edmond Cheung
# BSAN ###

# I called the data "ins". The data used is the "insurance_data_clean_v1. 
# I exported a CSV file from Google Drive, and I named it "ins" below.
# Below, I ran a stepwise model selection function, then tried to whittle down
# - why the diagnostic plots for the model did not look good and fix them.


# Stepwise model section to determine which factors are relevant
fullmodel<- lm(charges ~ ., data = ins)
nullmodel<- lm(charges ~ 1, data = ins)
step(fullmodel, nullmodel, direction = "both")

lm2 <- lm(charges ~ region + age_range + children + age + bmi + smoker,
          data = ins)
summary(lm2)
plot(lm2$res ~ lm2$fit)
qqnorm(lm2$res)
qqline(lm2$res)

# The residuals vs. fitted plot as well as the QQ plot did not look good.

hist(ins$age)
hist(ins$bmi)
hist(ins$charges)

# On further inspection, the distribution of charges is skewed
# Tried logging charges. For good measure, also tried a square-root.

hist(ins$charges)
hist(log(ins$charges))
hist(sqrt(ins$charges))

lm3 <- lm(log(charges) ~ region + age_range + children + age + bmi + smoker,
          data = ins)
summary(lm3)

plot(lm3$fit ~ lm3$res)
qqnorm(lm3$res)
qqline(lm3$res)

# Logging the charges data results in "age_range" not being significant

fullmodel2<- lm(log(charges) ~ ., data = ins)
nullmodel2<- lm(log(charges) ~ 1, data = ins)
step(fullmodel, nullmodel, direction = "both")

# While model selection suggests keeping age_range, I chose to exclude it below
# - because it was not significant in lm3

summary(lm3)
lm4 <- lm(log(charges) ~ region + children + age + bmi + smoker, data = ins)
summary(lm4)
plot(lm4$res ~ lm4$fit)
qqnorm(lm4$res)
qqline(lm4$res)

#The diagnostic plots here for lm4 look comparatively better than the plots
# - for lm2, however the qqplot is effectively the same as lm3 where age_range 
# - was included. 

