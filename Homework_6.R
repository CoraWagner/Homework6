# Cora Sarzynski
# 04/01/2022
# Homework 6

install.packages("data.table")
install.packages("naivebayes")
install.packages("dplyr")
install.packages("ggplot2")
library(data.table)
library(naivebayes)
library(dplyr)
library(ggplot2)

# Data set:(Added more rows to increase training and testing group sizes)
# https://www.kaggle.com/datasets/saptarsi/naiveflu

# Resources:
# https://www.programiz.com/r/ifelse-function#:~:text=In%20R%2C%20the%20ifelse%20%28%29%20function%20is%20a,traditional%20if...else%20block%20is%20the%20ifelse%20%28%29%20function.
# https://stackoverflow.com/questions/47546658/logistic-regression-on-factor-error-in-evalfamilyinitialize-y-values-must
# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization#add-regression-lines
# https://www.statology.org/plot-logistic-regression-in-r/
# http://www.cookbook-r.com/Statistical_analysis/Logistic_regression/
# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization#add-regression-lines

# Read in data
data1 <- fread("C:\\Users\\ckwag\\Documents\\Spring 2022\\Advanced Data Science\\Homework 6\\flu.csv")

# Create training and testing datasets
indexes <- sample(2,nrow(data1), replace = TRUE, prob = c(0.8,0.2))
train <- data1[indexes == 1,]
test <- data1[indexes == 2,]

# Naive Bayes with Laplace smoothing
model <- naive_bayes(Flu ~ ., data = train, laplace = 1, usekernel = T)

p <- predict(model, test)
table(p, test$Flu)

# Plot Naive Bayes
ggplot(data1, aes(x=Fever)) +
  geom_density()

# Add column making Flu data binomial
data2 <- data1 %>% mutate(flu = ifelse(Flu == "No",0,1))

# Create new training and testing datasets
indexes <- sample(2,nrow(data2), replace = TRUE, prob = c(0.8,0.2))
train <- data2[indexes == 1,]
test <- data2[indexes == 2,]

# Logistic Regression
glm.fit <- glm(flu ~ Chills + Runny_nose + Head_ache + Fever, data = train, family = binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, test, type = "response")
glm.probs[1:4]

glm.pred <- ifelse(glm.probs > 0.5, "Flu", "Not Flu")
attach(test)
table(glm.pred, Flu)

# Plot Logistial Regression
ggplot(data2, aes(x=Fever, y=flu)) + geom_point(shape=1, position=position_jitter(width=.05,height=.05)) + 
   geom_smooth(method="auto", se=TRUE)
