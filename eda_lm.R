#
# Hunter Schuler - Leon Yuan
# STAT 6336 - Final Project
# Southern Methodist University
# December 2022
# 

library(tidyverse)
library(ggpubr)
library(gridExtra)
library(groupdata2)
library(ROSE)
library(MASS)
library(interactions)
library(stats)


DF.MASTER = read.csv("insurance.csv")
df = DF.MASTER
test = read.csv('test.csv')
train = read.csv('train.csv')


test$sex = factor(test$sex, ordered = F)
train$sex = factor(train$sex, ordered = F)
test$smoker = factor(test$smoker, ordered = F)
train$smoker = factor(train$smoker, ordered = F)
test$region = factor(test$region, ordered = F)
train$region = factor(train$region, ordered = F)


# # Check for and remove duplicate data
# df[duplicated(df),]
# df = distinct(df)

# Check for class imbalance
ci.age = 
  ggplot(data = train) + 
  aes(x = age) + 
  geom_bar(stat = 'bin', binwidth = 1, fill = '#00BFC4') +
  labs(title = "Age (bin = 1)") +
  theme_dark()

ci.isFemale = 
  ggplot(data = as.data.frame(table(train$sex))) + 
  aes(x = Var1, y = Freq, fill = Var1) +
  ylim(0,900) +
  geom_bar(stat = 'identity') +
  labs(title = "Sex") +
  theme_dark() +
  theme(legend.position = 'none')

ci.bmi = 
  ggplot(data = train) + 
  aes(x = bmi) + 
  geom_bar(stat = 'bin', binwidth = .5, fill = '#00BFC4') +
  labs(title = "BMI (bin = 0.5)") +
  theme_dark()

ci.children = 
  ggplot(data = train) + 
  aes(x = children) + 
  geom_bar(stat = 'bin', binwidth = 1, fill = '#00BFC4') +
  labs(title = "Children") +
  theme_dark()

ci.smoker = 
  ggplot(data = as.data.frame(table(train$smoker))) + 
  aes(x = Var1, y = Freq, fill = Var1) +
  ylim(0,1100) +
  geom_bar(stat = 'identity') +
  labs(title = "Smoker", x = '') +
  theme_dark() +
  theme(legend.position = 'none')

ci.charges = 
  ggplot(data = train) + 
  aes(x = charges) + 
  geom_bar(stat = 'bin', binwidth = 1000, fill = '#00BFC4') +
  labs(title = "Charges (bin = 1000)") +
  theme_dark()

ci.region =
  ggplot(data = train) +
  aes(x = region, fill = region) + 
  geom_bar(stat = 'count') +
  labs(title = "Region") +
  theme_dark()

ggarrange(ci.age, ci.charges, ci.children, ci.smoker)
grid.arrange(arrangeGrob(ci.isFemale, ci.bmi, ncol = 2), ci.region, nrow = 2)


# Fix class imbalances
# Children
train$children = ifelse(train$children>0, 'yes', 'no')
test$children = ifelse(test$children>0,'yes','no')
train$children = factor(train$children, ordered = F)
test$children = factor(test$children, ordered = F)
ggplot(data = train) + 
  aes(x = children) + 
  geom_bar(stat = 'count', fill = c('#00BFC4', '#F8766D')) +
  labs(title = "Children") +
  theme_dark()

# Age
train = downsample(train, cat_col = 'age')
ggplot(data = train) + 
  aes(x = age) + 
  geom_bar(stat = 'bin', binwidth = 1, fill = '#00BFC4') +
  labs(title = "Age (bin = 1)") +
  theme_dark()

# Smoker
train = ovun.sample(smoker~., data = train, method = 'over')$data
ggplot(data = as.data.frame(table(train$smoker))) + 
  aes(x = Var1, y = Freq, fill = Var1) +
  ylim(0,700) +
  geom_bar(stat = 'identity') +
  labs(title = "Smoker", x = '') +
  theme_dark() +
  theme(legend.position = 'none')


# Check for covariance against charges
cv.age = 
  ggplot(data = train) + 
  geom_point(mapping = aes(x = age, y = charges), alpha = .5) +
  theme_dark()

cv.sex = 
  ggplot(data = train) + 
  aes(x = charges, y = sex) +
  geom_jitter(aes(color = sex)) +
  geom_boxplot(aes(fill = sex), width = 0.3)+
  theme_dark() +
  theme(legend.position = 'none')

cv.bmi = 
  ggplot(data = train) + 
  geom_point(mapping = aes(x = charges, y = bmi)) +
  theme_dark() +
  coord_flip()

cv.children = 
  ggplot(data = train) + 
  aes(x = charges, y = children) +
  geom_jitter(aes(color = children)) +
  geom_boxplot(width = 0.3, aes(fill = children)) +
  theme_dark() +
  theme(legend.position = 'none')
  
cv.smoker = 
  ggplot(data = train) + 
  aes(x = charges, y = smoker) +
  geom_jitter(aes(color = smoker)) +
  geom_boxplot(width = 0.3, aes(fill = smoker)) +
  theme_dark() +
  theme(legend.position = 'none')

cv.region =
  ggplot(data = train) +
  aes(x = charges, y = region) +
  geom_jitter(aes(color = region)) +
  geom_boxplot(width = 0.3, aes(fill = region)) +
  theme_dark() +
  theme(legend.position = 'none')

grid.arrange(arrangeGrob(cv.children, cv.age, ncol = 2), cv.smoker, nrow = 2)
grid.arrange(arrangeGrob(cv.sex, cv.bmi, ncol = 2), cv.region, nrow = 2)

ggplot(data = df) + 
  geom_point(mapping = aes(x = charges, y = bmi)) +
  theme_dark()
ggplot(data = df) + 
  geom_point(mapping = aes(x = charges, y = bmi)) +
  coord_flip() +
  theme_dark()




# Other interesting covariances
ggplot(data = train) + 
  aes(x = age, y = bmi) +
  geom_point(alpha = .5) +
  geom_smooth(method=lm, aes(color = 'red')) +
  theme_dark() + 
  theme(legend.position = 'none')

ggplot(data = train) + 
  aes(x = bmi, y = region) +
  geom_jitter(aes(color = region)) +
  geom_boxplot(aes(fill = region), width = 0.3) + 
  theme_dark() +
  theme(legend.position = 'none')

train %>%
  group_by(age, smoker) %>%
  summarize(count = n()) %>%
  ggplot() +
  aes(x = age, y = count, fill = smoker) +
  geom_bar(position = 'fill', stat = 'identity') +
  theme_dark() +
  labs(y = 'Percent for each age')

ggplot(data = df) +
  aes(x = bmi) +
  geom_bar(stat = 'bin', binwidth = 2, fill = '#00BFC4') +
  theme_dark()

# RMSE calculator
lmCompare = function(lm){
  lm.pred = predict.lm(lm, test)
  return = sqrt(sum((lm.pred-test$charges)^2)/nrow(test))
}

# Basic model
lm.basic = lm(charges ~., data = train)
summary(lm.basic)
lm.basic.summary = summary(lm.basic)
par(mfrow=c(2,2))
plot(lm.basic,which=1:4)
write.csv(lm.basic.summary$coefficients, 'basic_model.csv')
lm.basic.RMSE = lmCompare(lm.basic)

# Model interaction effects
lm.interact = lm(charges ~ (age + sex + bmi + children + smoker + region)^2, data = train)
summary(lm.interact)
interact_plot(lm.interact, pred = bmi, modx = smoker, plot.points = TRUE)

# Reduced linear model
lm.reduced = lm(charges ~ . + age*sex + age*children + age*region + sex*smoker + sex*region + bmi*children + bmi*smoker + bmi*region + children*region, data = train)

lm.reduced.compare = data.frame(cbind(predict.lm(lm.reduced,test), test$charges))
lm.reduced.compare$X3 = lm.reduced.compare$X1 - lm.reduced.compare$X2

ggplot(data = lm.reduced.compare)+
  aes(x = X1, y = X2)+
  geom_point() +
  geom_abline(slope = 1)+
  labs(x = 'Predicted', y = 'Actual')+
  theme_dark()
ggplot(data = lm.reduced.compare)+
  aes(x = X2, y = X3)+
  geom_abline(slope = 0, intercept = 0) +
  labs(x = 'Actual', y= 'Error') +
  geom_point() +
  theme_dark()
ggplot(data = lm.reduced.compare)+
  aes(x = X2, y = X1/X2)+
  geom_abline(slope = 0, intercept = 1) + 
  labs(x = 'Actual', y= 'Ratio (Predicted/Actual)') +
  geom_point() +
  theme_dark()

par(mfrow=c(2,2))
plot(lm.reduced,which=1:4)

#### SANDBOX #### 


