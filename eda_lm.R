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
library(scales)


DF.MASTER = read.csv("insurance.csv")
df = DF.MASTER


# Data preparation
df = df %>% mutate(sex = recode(sex, "male" = 0, "female" = 1))
names(df)[names(df) == "sex"] = "isFemale"
df = df %>% mutate(smoker = recode(smoker, "no" = 0, "yes" = 1))
names(df)[names(df) == "smoker"] = "isSmoker"
df$isNortheast = recode(df$region, "northeast" = 1, .default = 0)
df$isNorthwest = recode(df$region, "northwest" = 1, .default = 0)
df$isSoutheast = recode(df$region, "southeast" = 1, .default = 0)
df = df[,-6]
df$age = as.double(df$age)
df$isFemale = as.double(df$isFemale)
df$children = as.double(df$children)
df$isSmoker = as.double(df$isSmoker)
df$isNortheast = as.double(df$isNortheast)
df$isNorthwest = as.double(df$isNorthwest)
df$isSoutheast = as.double(df$isSoutheast)


# Check for na, NULL, etc.
df[is.na(df) == TRUE,]
df[is.null(df) == TRUE,]
df[is.nan(as.matrix(df)) == TRUE,]
df[is_empty(df) == TRUE,]

# Check for and remove duplicate data
df[duplicated(df),]
df = distinct(df)

# Check for class imbalance
ci.age = 
  ggplot(data = df) + 
  aes(x = age) + 
  geom_bar(stat = 'bin', binwidth = 1, fill = '#00BFC4') +
  labs(title = "Age (bin = 1)") +
  theme_dark()

ci.isFemale = 
  ggplot(data = as.data.frame(table(df$isFemale))) + 
  aes(x = Var1, y = Freq, fill = Var1) +
  ylim(0,900) +
  geom_bar(stat = 'identity') +
  labs(title = "isFemale", x = 'Sex (0 = Male, 1 = Female)') +
  theme_dark()

ci.bmi = 
  ggplot(data = df) + 
  aes(x = bmi) + 
  geom_bar(stat = 'bin', binwidth = .5, fill = '#00BFC4') +
  labs(title = "BMI (bin = 0.5)") +
  theme_dark()

ci.children = 
  ggplot(data = df) + 
  aes(x = children) + 
  geom_bar(stat = 'bin', binwidth = 1, fill = '#00BFC4') +
  labs(title = "Children") +
  theme_dark()

ci.smoker = 
  ggplot(data = as.data.frame(table(df$isSmoker))) + 
  aes(x = Var1, y = Freq, fill = Var1) +
  ylim(0,1100) +
  geom_bar(stat = 'identity') +
  labs(title = "isSmoker") +
  theme_dark() +
  theme(legend.position = 'none')

ci.charges = 
  ggplot(data = df) + 
  aes(x = charges) + 
  geom_bar(stat = 'bin', binwidth = 1000, fill = '#00BFC4') +
  labs(title = "Charges (bin = 1000)") +
  theme_dark()

ci.region =
  ggplot(data = DF.MASTER) +
  aes(x = region, fill = region) + 
  geom_bar(stat = 'count') +
  labs(title = "Region") +
  theme_dark()

ggarrange(ci.age, ci.charges, ci.children, ci.smoker)
grid.arrange(arrangeGrob(ci.isFemale, ci.bmi, ncol = 2), ci.region, nrow = 2)


# Fix class imbalances
# Children
df$children = ifelse(df$children>0, 1, 0)
ggplot(data = df) + 
  aes(x = children) + 
  geom_bar(stat = 'bin', binwidth = 1, fill = '#00BFC4') +
  labs(title = "Children") +
  theme_dark()

# Age
#df$age = as.factor(df$age)
df = downsample(df, cat_col = 'age')
ggplot(data = df) + 
  aes(x = age) + 
  geom_bar(stat = 'bin', binwidth = 1, fill = '#00BFC4') +
  labs(title = "Age (bin = 1)") +
  theme_dark()

# Smoker
df = ovun.sample(isSmoker~., data = df, method = 'over')$data
ggplot(data = as.data.frame(table(df$isSmoker))) + 
  aes(x = Var1, y = Freq, fill = Var1) +
  ylim(0,1100) +
  geom_bar(stat = 'identity') +
  labs(title = "isSmoker") +
  theme_dark() +
  theme(legend.position = 'none')


# Check for covariance against charges
cv.age = 
  ggplot(data = df) + 
  geom_point(mapping = aes(x = charges, y = age), alpha = .5) +
  theme_dark()

cv.isFemale = 
  ggplot(data = df) + 
  geom_jitter(mapping = aes(x = charges, y = isFemale, color = isFemale)) +
  ylim(-1, 2) +
  theme_dark() +
  theme(legend.position = 'none')

cv.bmi = 
  ggplot(data = df) + 
  geom_point(mapping = aes(x = charges, y = bmi)) +
  theme_dark() +
  coord_flip()
  
cv.children = 
  ggplot(data = df) + 
  geom_jitter(mapping = aes(x = charges, y = children, color = children)) +
  ylim(-1,2) +
  theme_dark() +
  theme(legend.position = 'none')

cv.smoker = 
  ggplot(data = df) + 
  geom_jitter(mapping = aes(x = charges, y = isSmoker, color = isSmoker)) +
  ylim(-1,2) +
  theme_dark() +
  theme(legend.position = 'none')
  
cv.region =
  ggplot(data = DF.MASTER) +
  geom_jitter(mapping = aes(x = charges, y = region, color = region)) +
  theme_dark()

grid.arrange(arrangeGrob(cv.children, cv.age, ncol = 2), cv.smoker, nrow = 2)
grid.arrange(arrangeGrob(cv.isFemale, cv.bmi, ncol = 2), cv.region, nrow = 2)

ggplot(data = df) + 
  geom_point(mapping = aes(x = charges, y = bmi)) +
  theme_dark()
ggplot(data = df) + 
  geom_point(mapping = aes(x = charges, y = bmi)) +
  coord_flip() +
  theme_dark()




# Other interesting covariances
ggplot(data = df) + 
  geom_point(mapping = aes(x = bmi, y = age), alpha = .5) +
  theme_dark()

ggplot(data = DF.MASTER) + 
  geom_jitter(mapping = aes(x = bmi, y = region, color = region)) +
  theme_dark() +
  theme(legend.position = 'none')

df %>%
  group_by(age, isSmoker) %>%
  summarize(count = n()) %>%
  ggplot() +
  aes(x = age, y = count, fill = isSmoker) +
  geom_bar(position = 'fill', stat = 'identity') +
  theme_dark() +
  theme(legend.position = 'none') +
  labs(y = 'percent')

ggplot(data = df) +
  aes(x = bmi) +
  geom_bar(stat = 'bin', binwidth = 2, fill = '#00BFC4') +
  theme_dark()

# Log-transform the charges data
df$charges = log(df$charges)
ggplot(data = df) + 
  aes(x = charges) + 
  geom_bar(stat = 'bin', binwidth = 0.05, fill = '#00BFC4') +
  labs(title = "Charges (bin = 0.05*ln)") +
  theme_dark()


# Split data into test and training set
set.seed(1126)
index = sample(nrow(df), round(0.80 * nrow(df)))
train = df[index,]
test  = anti_join(df, train)

# Interaction effects
df.lm = lm(charges ~., data = train)
df.lm = lm(charges ~(.)^2, data = train)
interact_plot(df.lm, pred = bmi, modx = isSmoker, plot.points = TRUE)

# Model reduction
reduce = summary(df.lm)
sigvars = rownames(reduce$coefficients[reduce$coefficients[,4] < 0.05,])
sigvars = sigvars[-1]
sigvars = paste(sigvars, collapse="+")
sigvars = as.formula(paste("charges ~ ", sigvars ,sep = ""))
df.lm = lm(sigvars, data = train)
summary(df.lm)
reduce = summary(df.lm)
sigvars = rownames(reduce$coefficients[reduce$coefficients[,4] < 0.05,])
sigvars = sigvars[-1]
sigvars = paste(sigvars, collapse="+")
sigvars = as.formula(paste("charges ~ ", sigvars ,sep = ""))
df.lm = lm(sigvars, data = train)
summary(df.lm)




#### SANDBOX #### 


