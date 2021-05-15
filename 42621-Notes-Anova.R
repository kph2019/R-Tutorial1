#Anova R Script ------------------

# Load Libraries -------------------

# install.packages("ggpubr")
install.packages("plotly")

library(dplyr)
library(devtools)
library(ggpubr)
library(car)
library(ggplot2)
library(easyGplot2)
library(plotly)



# Import Data ----------------

attach(insuranceDS)

#insuranceDS <- read.csv(file.choose())



# Random data snapshot ----------------

set.seed(1234) # set the starting # used to generate a sequence of random numbers - it ensures you get the same result if you start w/ the same seed each time you run the same process

dplyr::sample_n(insuranceDS, 10)

str(insuranceDS)

summary(insuranceDS)

# insuranceDS$ageN <- NULL # remove features not needed



# Convert numerical variables to Factors ---------------------

insuranceDS$age2 <- ifelse(insuranceDS$age < 25, "younger than 25", "25 and older")

insuranceDS$age2 <- as.factor(insuranceDS$ageN)

tail(insuranceDS$ageN)


insuranceDS$bmi2 <- ifelse(
                    insuranceDS$bmi < 18.5, "underweight", 
                       ifelse(insuranceDS$bmi >= 18.5 & insuranceDS$bmi <=24.9, "normal weight", 
                           ifelse(insuranceDS$bmi >= 25 & insuranceDS$bmi <=29.9, "overweight", "obese")))

insuranceDS$bmi2 <- as.factor(insuranceDS$bmi2)

tail(insuranceDS$bmi2)



# Categorical Variable Levels Snapshot -----------------

variable.names(insuranceDS)

levels(insuranceDS$sex)

levels(insuranceDS$children)

levels(insuranceDS$smoker)

levels(insuranceDS$region)

levels(insuranceDS$age2)

levels(insuranceDS$bmi2)

insuranceDS$age2 <- ordered(insuranceDS$age2, levels = c("younger than 25", "25 and older"))

insuranceDS$bmi2 <- ordered(insuranceDS$bmi2, levels = c("underweight", "normal weight", "overweight", "obese"))


# Distribution (Histogram) Visualization --------------------------------------

# quickly see if distribution is normal or skewed to left or skewed to the right! save you some time :)
# take the square root if the data is left skewed to make it normal
# take the log if the data is right skewed to make it normal

region_hist <- ggplot(insuranceDS, aes(x = charges, color = region)) + 
  geom_histogram(fill = "white", position = "dodge") + 
  theme(legend.position = "top")

ggplotly(region_hist)

sex_hist <- ggplot(insuranceDS, aes(x = charges, color = sex)) + 
  geom_histogram(fill = "white", position = "dodge") + 
  theme(legend.position = "top")

ggplotly(sex_hist)

children_hist <- ggplot(insuranceDS, aes(x = charges, color = children)) + 
  geom_histogram(fill = "white", position = "dodge") + 
  theme(legend.position = "top")

ggplotly(children_hist)

smoker_hist <- ggplot(insuranceDS, aes(x = charges, color = smoker)) + 
  geom_histogram(fill = "white", position = "dodge") + 
  theme(legend.position = "top")

ggplotly(smoker_hist)

age2_hist <- ggplot(insuranceDS, aes(x = charges, color = age2)) + 
  geom_histogram(fill = "white", position = "dodge") + 
  theme(legend.position = "top")

ggplotly(age2_hist)

bmi2_hist <- ggplot(insuranceDS, aes(x = charges, color = bmi2)) + 
  geom_histogram(fill = "white", position = "dodge") + 
  theme(legend.position = "top")

ggplotly(bmi2_hist)






# Summary Stats (Count, Mean, SD) -----------------

# how can I automate this!
# summary stats of outcome variable grouped by categorical predictor variable
# dplyr for group by with normal values
# aggregate method for group by w/ log values
# performing against normal and log values helps you see right away if means are the same or different. log and normal outputs should align. For example means between normal values and log values should both be significantly different or not significantly different 


# for (${1:insuranceDS} in insuranceDS) {
#   
# group_by(insuranceDS, region) %>%
#   summarise(count = n(),
#             mean = mean(charges, na.rm = TRUE),
#             sd = sd(charges, na.rm = TRUE))
# }

# statFunction <- function (x) {
#   
#   group_by(insuranceDS, x) %>%
#     summarise(count = n(), 
#               mean = mean(charges, na.rm = TRUE), 
#               sd = sd(charges, na.rm = TRUE))
# }
# 
# 
# apply(insuranceDS, 2, statFunction(x))


group_by(insuranceDS, region) %>%
    summarise(count = n(),
              mean = mean(charges, na.rm = TRUE),
              sd = sd(charges, na.rm = TRUE))

aggregate(log(charges)~region, insuranceDS, mean)



group_by(insuranceDS, sex) %>%
  summarise(count = n(),
            mean = mean(charges, na.rm = TRUE),
            sd = sd(charges, na.rm = TRUE))

aggregate(log(charges)~sex, insuranceDS, mean)


group_by(insuranceDS, children) %>%
  summarise(count = n(),
            mean = mean(charges, na.rm = TRUE),
            sd = sd(charges, na.rm = TRUE))

aggregate(log(charges)~children, insuranceDS, mean)


group_by(insuranceDS, smoker) %>%
  summarise(count = n(),
            mean = mean(charges, na.rm = TRUE),
            sd = sd(charges, na.rm = TRUE))

aggregate(log(charges)~smoker, insuranceDS, mean)


group_by(insuranceDS, age2) %>%
  summarise(count = n(),
            mean = mean(charges, na.rm = TRUE),
            sd = sd(charges, na.rm = TRUE))

aggregate(log(charges)~age2, insuranceDS, mean)


group_by(insuranceDS, bmi2) %>%
  summarise(count = n(),
            mean = mean(charges, na.rm = TRUE),
            sd = sd(charges, na.rm = TRUE))

aggregate(log(charges)~bmi2, insuranceDS, mean)





# Boxplot Data Visualizations ---------------------------

ggplotly(ggboxplot(insuranceDS, x = "region", y = "charges", color = "region", ylab = "charges", xlab = "region"))
boxplot(log(charges) ~ region, data = insuranceDS)

ggplotly(ggboxplot(insuranceDS, x = "sex", y = "charges", color = "sex", ylab = "charges", xlab = "sex"))
boxplot(log(charges) ~ sex, data = insuranceDS)

ggplotly(ggboxplot(insuranceDS, x = "children", y = "charges", color = "children", ylab = "charges", xlab = "children"))
boxplot(log(charges) ~ children, data = insuranceDS)

ggplotly(ggboxplot(insuranceDS, x = "smoker", y = "charges", color = "smoker", ylab = "charges", xlab = "smoker"))
boxplot(log(charges) ~ smoker, data = insuranceDS)

ggplotly(ggboxplot(insuranceDS, x = "age2", y = "charges", color = "age2", ylab = "charges", xlab = "age2"))
boxplot(log(charges) ~ age2, data = insuranceDS)

ggplotly(ggboxplot(insuranceDS, x = "bmi2", y = "charges", color = "bmi2", ylab = "charges", xlab = "bmi2"))
boxplot(log(charges) ~ bmi2, data = insuranceDS)

# ggline(insuranceDS,  x = "smoker", y = "charges", add = c("mean_se", "jitter"), order = c("no", "yes"), x = "smoker", y = "charges")



# Create Anova models (Region) --------------

# null hypothesis: there is no significant difference between sample means
# alt hypothesis: there is significant differences between sample means
# if p-value < significance level 0.05 then significant differences between sample means (but don’t know which groups are different from each other)
# if p-value > significance level 0.05 then no significant differences between sample mean
# if f-ratio <= 1 then no significant difference between the means
# if f-ratio > 1 then significant difference /variation between the means


region.aov <- aov(charges ~ region, data = insuranceDS) # build model and run model on data

summary(region.aov) # print the outcome of aov model



# Conduct Tukey HSD --------------

# Tukey multi pairwise-comparison / Tukey HSD (Tukey Honest Significant Differences)
# perform if the anova test is significant --- there are significant differences (f-ratio > 1 and p-value < 0.05)
# compares the difference between two means at a time across all sample means or means in the group / categorical variable … and it determines if the mean difference are significant or not
# which group has the p adj < significance level 0.05? the diff is telling as well

TukeyHSD(region.aov)



# Conduct Parametric Assumptions Test (Region) -------------------

# Check homogeneity of variances (Region) -----------------
# if outliers are detected then consider removing or running a non-parametric test because outliers can severely affect normality and homogeneity of variances. Therefore, parametric assumptions tests did not past and you cannot rely on the parametric test output.
# if you decide to remove outliers then take log of the outcome variable to ignore the outliers i.e. aggregate(log(charges)~smoker, insuranceDS, mean)
# leveneTest:
# if p-value < significance level 0.05 then significant differences in mean >> there’s no homogeneity of variances
# if p-value > significance level 0.05 then no significant differences between means >> there’s homogeneity of variances

plot(region.aov, 1)

leveneTest(charges ~ region, data = insuranceDS)



# Check Normality of data distribution (Region) ------------------------

# if all points fall approx along reference line then we can assume normality
# Shapiro-Wilk test
# check p-value
# if p-value < 0.05 then significant differences >> data not normally distributed
# if p-value > 0.05 then no significant differences >> data is normally distributed

plot(region.aov, 2)

region_aov_residuals <- residuals(object = region.aov) # extract residuals

shapiro.test(x = region_aov_residuals) # run test: 


res.aov <- aov(log(charges) ~ region, data = insuranceDS)
summary(res.aov)



# Non-Parametric Test: Kruskal-Wallis Rank Sum Test (Region) ----------------------------
# if p-value < 0.05 then significant differences in mean 
# if p-value > 0.05 then no significant differences in mean

kruskal.test(charges ~ region, data = insuranceDS)




# Anova (Sex) --------------

# null hypothesis: there is no significant difference between sample means
# alt hypothesis: there is significant differences between sample means
# if p-value < significance level 0.05 then significant differences between sample means (but don’t know which groups are different from each other)
# if p-value > significance level 0.05 then no significant differences between sample mean
# if f-ratio <= 1 then no significant difference between the means
# if f-ratio > 1 then significant difference /variation between the means


sex.aov <- aov(charges ~ sex, data = insuranceDS) # build model and run model on data

summary(sex.aov) # print the outcome of aov model

TukeyHSD(sex.aov)

sex.aov.log <- aov(log(charges) ~ sex, data = insuranceDS)

summary(sex.aov.log)

kruskal.test(charges ~ sex, data = insuranceDS)


# Anova (Sex) Observations ---------------------------
# Sample means of charges by sex are not statistically different so accept the null hypothesis
# Kruskal and anova w/ log of charges p-values are different why? They are both above significance level of 0.05 though.
# Kruskal test proves that that the data is not normal distributed and the sample means are not significantly different -- p-value > 0.05



# Anova (Children) --------------

# null hypothesis: there is no significant difference between sample means
# alt hypothesis: there is significant differences between sample means
# if p-value < significance level 0.05 then significant differences between sample means (but don’t know which groups are different from each other)
# if p-value > significance level 0.05 then no significant differences between sample mean
# if f-ratio <= 1 then no significant difference between the means
# if f-ratio > 1 then significant difference /variation between the means


children.aov <- aov(charges ~ children, data = insuranceDS) # build model and run model on data

summary(children.aov) # print the outcome of aov model

TukeyHSD(children.aov)

children.aov.log <- aov(log(charges) ~ children, data = insuranceDS)

summary(children.aov.log)

TukeyHSD(children.aov.log)

kruskal.test(charges ~ children, data = insuranceDS)


# Anova (Children) Observations ---------------------------
# Sample means of charges by children are  significantly different so reject the null hypothesis
# anova w/ normal values shows p-value < level of significance 0.05 so we should reject the null hypothesis (normal data)
# Tukey shows w/ a 95% CI, difference in means between charges by clients with 2 children and 0 children (normal data)
# anova w/ log values shows p-value < level of significance 0.05 so we should reject the null hypothesis (log data)
# Tukey shows w/ a 95% CI, difference in means between charges by clients with 2 children and 0 children, 3 children and 1 child, 2-0, 3-0 (log data)
# Kruskal test proves that that the sample means are  significantly different -- p-value < 0.05
# Kruskal and anova w/ log of charges p-values agree that sample means of charges by children are significantly different.




# Anova (Smoker) --------------

# null hypothesis: there is no significant difference between sample means
# alt hypothesis: there is significant differences between sample means
# if p-value < significance level 0.05 then significant differences between sample means (but don’t know which groups are different from each other)
# if p-value > significance level 0.05 then no significant differences between sample mean
# if f-ratio <= 1 then no significant difference between the means
# if f-ratio > 1 then significant difference /variation between the means


smoker.aov <- aov(charges ~ smoker, data = insuranceDS) # build model and run model on data

summary(smoker.aov) # print the outcome of aov model

TukeyHSD(smoker.aov)

smoker.aov.log <- aov(log(charges) ~ smoker, data = insuranceDS)

summary(smoker.aov.log)

TukeyHSD(smoker.aov.log)

kruskal.test(charges ~ smoker, data = insuranceDS)


# Anova (Smoker) Observations ---------------------------
# Sample means of charges by smoker are  significantly different so reject the null hypothesis
# anova w/ normal values shows p-value < level of significance 0.05 so we should reject the null hypothesis (normal data)
# Tukey shows w/ a 95% CI, difference in means between charges by clients who smoke (normal data)
# anova w/ log values shows p-value < level of significance 0.05 so we should reject the null hypothesis (log data)
# Tukey shows w/ a 95% CI, difference in means between charges by clients who smoke (log data) -- why does it show 0 p-value
# Kruskal test proves that that the sample means are  significantly different -- p-value < 0.05
# Kruskal and anova w/ log of charges p-values agree that sample means of charges by smoker type are significantly different.



# Anova (age2) --------------

# null hypothesis: there is no significant difference between sample means
# alt hypothesis: there is significant differences between sample means
# if p-value < significance level 0.05 then significant differences between sample means (but don’t know which groups are different from each other)
# if p-value > significance level 0.05 then no significant differences between sample mean
# if f-ratio <= 1 then no significant difference between the means
# if f-ratio > 1 then significant difference /variation between the means


age2.aov <- aov(charges ~ age2, data = insuranceDS) # build model and run model on data

summary(age2.aov) # print the outcome of aov model

TukeyHSD(age2.aov)

age2.aov.log <- aov(log(charges) ~ age2, data = insuranceDS)

summary(age2.aov.log)

TukeyHSD(age2.aov.log)

kruskal.test(charges ~ age2, data = insuranceDS)


# Anova (Age2) Observations ---------------------------
# Sample means of charges by Age2 are  significantly different so reject the null hypothesis
# anova w/ normal values shows p-value < level of significance 0.05 so we should reject the null hypothesis (normal data)
# Tukey shows w/ a 95% CI, difference in means between charges by 2 age categories (normal data)
# anova w/ log values shows p-value < level of significance 0.05 so we should reject the null hypothesis (log data)
# Tukey shows w/ a 95% CI, difference in means between charges by clients who smoke (log data) -- why does it show 0 p-value
# Kruskal test proves that that the sample means are  significantly different -- p-value < 0.05
# Kruskal and anova w/ log of charges p-values agree that sample means of charges by age2 are significantly different.


# Anova (bmi2) --------------

# null hypothesis: there is no significant difference between sample means
# alt hypothesis: there is significant differences between sample means
# if p-value < significance level 0.05 then significant differences between sample means (but don’t know which groups are different from each other)
# if p-value > significance level 0.05 then no significant differences between sample mean
# if f-ratio <= 1 then no significant difference between the means
# if f-ratio > 1 then significant difference /variation between the means


bmi2.aov <- aov(charges ~ bmi2, data = insuranceDS) # build model and run model on data

summary(bmi2.aov) # print the outcome of aov model

TukeyHSD(bmi2.aov)

bmi2.aov.log <- aov(log(charges) ~ bmi2, data = insuranceDS)

summary(bmi2.aov.log)

TukeyHSD(bmi2.aov.log)

kruskal.test(charges ~ bmi2, data = insuranceDS)


# Anova (bmi2) Observations ---------------------------
# Sample means of charges by bmi2 are  significantly different so reject the null hypothesis
# anova w/ normal values shows p-value < level of significance 0.05 so we should reject the null hypothesis (normal data)
# Tukey shows w/ a 95% CI, difference in means between charges by clients who are obese vs normal weight and obese vs overweight (normal data)
# anova w/ log values shows p-value < level of significance 0.05 so we should reject the null hypothesis (log data)
# Tukey shows w/ a 95% CI, difference in means between charges by clients who smoke (log data) 
# Kruskal test proves that that the sample means are  significantly different -- p-value < 0.05
# Kruskal and anova w/ log of charges p-values agree that sample means of charges by bmi2 are significantly different.































# Misc Notes --------------

# You have to analyze the situation / storytelling
# a) it may look like we should reject the null hypothesis but should we really?
# 1) Removed outliers by looking log of charges
# 2) now we can look at the P value -- p value is not giving you all of the data


# take the square root if the data is left skewed to make it normal
# take the log if the data is right skewed to make it normal

