# 04/28/2016
# Ryan R. Cupo
# BIOM 611 Final

# clears objects from working environment & initializes new
rm(list=ls())
objects()

# sets wokring directory
setwd("C:\\Users\\Ryan Cupo\\Documents\\Graduate School\\2016_Spring\\BIOM 611")

# load needed libraries
library(ggplot2)

# Q2
# loads dataset
# reads in c elegans dataset from filepath selected in pop up window
DataSet <- read.csv(file.choose(), header = TRUE)

# use summary function to check for Na values
summary(DataSet)
# there are no missing values

# find difference between nightime activity (activity_2) and daytime activity (activity_1)
DataSet$diff <- DataSet$activity_2 - DataSet$activity_1
# the difference is used with a directio vector because we DO care whether the activity increases or decreases

# set trt to factor type
DataSet$trt <- factor(DataSet$trt)

# plot distributions of activity by group for the first light cycle
# boxplot
ggplot(DataSet, aes(trt, activity_1)) + geom_boxplot(fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Daytime Activity") + ggtitle("Daytime Activity by Treatment Group")
# violin plot
ggplot(DataSet, aes(trt, activity_1)) + geom_violin(scale = "area", fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Daytime Activity") + ggtitle("Daytime Activity by Treatment Group")
# dotplot
ggplot(DataSet, aes(trt, activity_1)) + geom_dotplot(binaxis = "y", stackdir = "center", fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Daytime Activity") + ggtitle("Daytime Activity by Treatment Group")

# plot distributions of activity by group for the second light cycle
# boxplot
ggplot(DataSet, aes(trt, activity_2)) + geom_boxplot(fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Nighttime Activity") + ggtitle("Nighttime Activity by Treatment Group")
# violin plot
ggplot(DataSet, aes(trt, activity_2)) + geom_violin(scale = "area", fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Nighttime Activity") + ggtitle("Nighttime Activity by Treatment Group")
# dotplot
ggplot(DataSet, aes(trt, activity_2)) + geom_dotplot(binaxis = "y", stackdir = "center", fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Nighttime Activity") + ggtitle("Nighttime Activity by Treatment Group")

# plot distributions of acitivty difference by group
# boxplot
ggplot(DataSet, aes(trt, diff)) + geom_boxplot(fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Difference in Activity") + ggtitle("Difference in Activity by Treatment Group")
# violin plot
ggplot(DataSet, aes(trt, diff)) + geom_violin(scale = "area", fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Difference in Activity") + ggtitle("Difference in Activity by Treatment Group")
# dotplot
ggplot(DataSet, aes(trt, diff)) + geom_dotplot(binaxis = "y", stackdir = "center", fill = "darkmagenta", colour = "black") + xlab("Treatment Group") + ylab("Difference in Activity") + ggtitle("Difference in Activity by Treatment Group")

# subset data by group to test variance and normality
# trt=LL subset
DataSetLL <- subset(DataSet, DataSet$trt == "LL")
# trt=DD
DataSetDD <- subset(DataSet, DataSet$trt == "DD")
# trt=LD
DataSetLD <- subset(DataSet, DataSet$trt == "LD")

# qnorm and qqplot to check for variance
# LL
qqnorm(DataSetLL$diff, pch = 19)
qqline(DataSetLL$diff)
# LL is approximately normal
# DD
qqnorm(DataSetDD$diff, pch = 19)
qqline(DataSetDD$diff)
# DD is approximately normal
# LD
qqnorm(DataSetLD$diff, pch = 19)
qqline(DataSetLD$diff)
# LD is approximately normal, but is less normal than the others

# the null hypothesis (Ho) is that the difference in activity for the LD group is no different than the difference in activity for the LL or DD group
# the standard type I error rate of 0.05 is acceptable for this application and will be used
# the variance will be checked for each group
# get mean for each trt group
means <- with(DataSet, tapply(diff, trt, mean))
means
# get sd for each trt group
sds <- with(DataSet, tapply(diff, trt, sd))
sds
# get median of each trt group
medians <- with(DataSet, tapply(diff, trt, median))
medians
# based upon the difference in sd, the variance is not approximately equal, so we cannot use ANOVA, and therefore must use a nonparametric test

# We have more than two independent samples, it is approximately normal, and variance is not equal so we will use the Kruskal-Wallis test
with(DataSet, kruskal.test(diff, trt))

# also use a pariwise wilcoxon rank sum test to see if LD is significantly different from LL or DD
with(DataSet, pairwise.wilcox.test(diff, trt, paired = FALSE))






