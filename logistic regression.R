
#install.packages(c("gridExtra"))

require(data.table)
require(magrittr)
require(grid)
require(ggplot2)
require(gridExtra)


diabetes_dt = read.csv(file='C:/Users/hs_90/Documents/logistic_regression/diabetes data for R.csv', header=TRUE, sep=',') 
#colnames(diabetes_dt)

# need to tell R which variables are CATEGORICAL (wtih as.factor), as it will assume they're all continuous by default
gender = as.factor(diabetes_dt[,'gender'])
dm = as.factor(diabetes_dt[,'dm']) 
dm2 = factor(dm, exclude=NULL) # make new factor from the old one keeping the missing values

t = table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results

round(prop.table(t), digits=4) # using prop.table command to get the above in percentages (4 decimal places)
round(prop.table(t)*100, digits=2) # using prop.table command to get the above in percentages

table(dm2)

# for continuous variables use the 'summary' command to get variable stats
summary(diabetes_dt$chol)
summary(diabetes_dt$height)
summary(diabetes_dt$weight)

# turn table in to data.table
diabetes_dt = diabetes_dt %>% setDT()

diabetes_dt = diabetes_dt[, `:=` (height.si = height*0.0254, 
                    weight.si = weight*0.453592)]

diabetes_dt = diabetes_dt[, `:=` (bmi = weight.si/height.si^2)]

summary(diabetes_dt$bmi)

diabetes_dt = diabetes_dt[, `:=` (bmi_categorised = ifelse(bmi < 18.5, "underweight", 
                                              ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                                     ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                                            ifelse(bmi > 30, "obese", NA)))) )]

dm_by_bmi_category = table(diabetes_dt$bmi_categorised, diabetes_dt$dm, exclude=NULL)
round(prop.table(dm_by_bmi_category, margin = 1)*100, digits=2) 
# argument margin = 1 we can specify that the table gives us the row percentages. 2 would give us the column %

# cross tabulation: age groups vs. gender 
diabetes_dt = diabetes_dt[, `:=` (age_grouped = ifelse(age < 45, "under 45", 
                                             ifelse(age >= 45 & age <= 64, "45-64", 
                                                    ifelse(age > 64 & age <= 74, "65-74", 
                                                           ifelse(age >= 75, "75 or over", NA)))) )]

age_by_gender = table(diabetes_dt$age_grouped, diabetes_dt$gender, exclude=NULL)

round(prop.table(age_by_gender)*100, digits=1) # % of ALL


# Running a model with only one predictor
# To get the output, though, you need the "summary" command as well. 
# To do this, you need to make an R object out of the model.
m = glm(dm ~ 1, family=binomial (link=logit)) # "1" is just R's way of saying that there's only an intercept term in 
                                              # the model (no predictors yet)
summary(m)

# checking how R has interpreted the binary outcome "dm". R is modelling the log-odds of dm=1 (Yes)
table(m$y)

# We've already told R that gender is a factor (in its language), that is to say, 
# a categorical variable (in our language). That was the point of the "as.factor" commands earlier
m = glm(dm ~ gender, family=binomial (link=logit))
summary(m)

contrasts(gender) # Check how R has entered gender into the model.

age = diabetes_dt$age

m = glm(dm ~ age, family=binomial (link=logit))
# m = glm(dm ~ diabetes_dt$age, family=binomial (link=logit))
summary(m)

# It's straightforward to include age as a single term in the model, but we are assuming a linear relation with the 
# outcome. More precisely, this assumes that the relation between age and the 'log odds' of having diabetes is 
# linear. Is that reasonable? We need to check this. The easiest way is just to plot one against the other.

# cross tabulation of age and diabetes status
dm_by_age = table(age, dm) 

# output the frequencies of diabetes status by age 
freq_table = prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds = freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds = log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 



# to change the reference for the log-odds so you get odds for females divided by odds for males
# R, by default organises values (called levels) of categorical variables alphabetically. Check:
levels(gender)
# So by default, "female" is the first level. R will automatically set this as the reference group 
# in statistical analyses, such that the odds ratios of other groups will be displayed relative to this one.
# To redefine the reference group: 
gender = relevel(gender, ref='male')
levels(gender)


# glm command produces an R object with various useful elements, identified using the dollar symbol $, that can 
# be manipulated and exported. That's the reason why we store the glm output as an object, m. For instance, 
# to see the model's coefficients, type:
m$coefficients
# since this is a "log odds" ratio we exponentiate to get the odds 
exp(m$coefficients)


# WEEK 2 quiz
dm_location = table(diabetes_dt$dm, diabetes_dt$location)
prop.table(dm_location, margin = 2)

location = diabetes_dt$location

m = glm(dm ~ location, family=binomial (link=logit))
summary(m)


## WEEK 3: Notes 
summary(age)
hist(age)
d1 = density(age)
plot(d1, main='') # smooths out the noise using kernel smoothing which uses weighted average of neighbouring data (i.e. frequencies for ages just above and below each age)

summary(bmi)
hist(bmi, main='bmi')
d2 = density(bmi, na.rm=TRUE)
plot(d2, main='')

summary(diabetes_dt$hdl, main='hdl')
hist(diabetes_dt$hdl)
d3 = density(diabetes_dt$hdl, na.rm=TRUE)
plot(d3, main='')

summary(diabetes_dt$chol)
hist(diabetes_dt$chol, main='cholesterol')
d4 = density(diabetes_dt$chol, na.rm=TRUE)
plot(d4, main='')


# define the age variable (continuous) 
age <- diabetes_dt$age

# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by age 
dm_by_age_prop <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds_age <- dm_by_age_prop[, "yes"]/dm_by_age_prop[, "no"] 

# calculate the log odds 
logodds_age <- log(odds_age) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_age_prop), logodds_age) 


cor.test(x=diabetes_dt$chol,y=diabetes_dt$hdl,method='pearson') 

multiple_reg = glm(dm ~ age+gender+bmi, family=binomial (link=logit))
summary(multiple_reg)

exp(confint(multiple_reg))

# WEEK 3 quiz: 
insurance = as.factor(diabetes_dt$insurance) # turn into categorical
cholesterol = diabetes_dt$chol
multiple_reg2 = glm(dm ~ age+cholesterol+insurance, family=binomial (link=logit))
summary(multiple_reg2)

# odds ratio 
round(exp(multiple_reg2$coefficients),digits=2)

# WEEK 4: Notes
# McFadden's R-squared: the formula is 1 minus the maximised log-likelihood of the fitted model divided by the log-likelihood of a null model (1-lnL(M1)/lnL(Mnull))

fitted_model = glm(dm ~ age+cholesterol+insurance, family=binomial (link=logit))
null_model = glm(dm ~ 1, family=binomial (link=logit)) 

R_squared = 1-logLik(fitted_model)/logLik(null_model)
R_squared

# C-statistic 
# install.packages("DescTools") 
require(DescTools)
Cstat(fitted_model)

# Hosmer-Lemeshow statistic and test
#install.packages("ResourceSelection") 
require(ResourceSelection) 

# fitted_model$y  is the outcome variable we specified (dm); fitted(fitted_model) generates fitted values from the model.
HL = hoslem.test(x=fitted_model$y, y = fitted(fitted_model), g=10)
HL

# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 

# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"]) 

# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"])) 

# plot the ten ratios of observed:predicted cases, where a well-calibrated model would show ten points very near 1. 
#install.packages("generalhoslem") 
require(generalhoslem) 
logitgof(obs = fitted_model$y, exp = fitted(fitted_model), g = 10) 

# analyse table of deviance 
anova(fitted_model, test='Chisq')


# fit a model with age, BMI, cholesterol, HDL and blood pressure as predictors. Your task now is to fit a model with those predictors (try both systolic and diastolic BP) and apply backwards elimination to remove any that are not statistically significant - just use the conventional p<0.05 for this. 

#categorical
dm        = as.factor(diabetes_dt$dm) 
insurance = as.factor(diabetes_dt$insurance)# let's say 0=none, 1=gov, 2=private 
fh        = as.factor(diabetes_dt$fh)       # 1=FH, 0=no FH 
smoking   = as.factor(diabetes_dt$smoking)  # 1,2,3 
gender    = as.factor(diabetes_dt$gender) 
frame     = as.factor(diabetes_dt$frame) 
location  = as.factor(diabetes_dt$location) 

bmi       = diabetes_dt$bmi
chol      = diabetes_dt$chol  
hdl       = diabetes_dt$hdl 
ratio     = diabetes_dt$ratio
age       = diabetes_dt$age
systolic  = diabetes_dt$bp.1s
diastolic = diabetes_dt$bp.1d

model_1 = glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial (link = logit)) 
summary(model_1) 

anova(model_1, test='Chisq')

model_2 = glm(dm ~ age + bmi + chol + hdl, family = binomial (link = logit)) 
summary(model_2)

cor.test(systolic,hdl)
cor.test(systolic,bmi)
cor.test(systolic,chol)
cor.test(systolic,age)


model_3 = glm(dm ~ age + bmi + chol + hdl + systolic + diastolic + gender + location + frame + insurance + smoking, family = binomial (link = logit))
summary(model_3)

anova(model_3, test='Chisq')


