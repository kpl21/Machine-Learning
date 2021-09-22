rm(list = ls(all = TRUE)) # make sure previous work is clear
ls()

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio, Metrics, rpart, rpart.plot, RColorBrewer, randomForest, neuralnet) 

# install.packages('caret', dependencies = TRUE)

# include knn()
library(class)
# include rmse()
library(Metrics) 
# include rpart()
library(rpart)
library(rpart.plot)
# include sample.split()
library(caTools)
library(caret)
library(dplyr)
library(randomForest)

library(neuralnet)


# load CSV
# https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
df <- import("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv")
RedWine <- df
colnames(RedWine)[1] = "fixedAcidity"
colnames(RedWine)[2] = "volatileAcidity"
colnames(RedWine)[3] = "citricAcid"
colnames(RedWine)[4] = "residualSugar"
colnames(RedWine)[6] = "freeSulfulDioxide"
colnames(RedWine)[7] = "totalSulfulDioxide"

RedWine_GB <- RedWine
for(i in 1:nrow(RedWine_GB)) {
  if (RedWine_GB[i,12] < 6) {
    RedWine_GB[i,12] = "bad"
  } else {
    RedWine_GB[i,12] = "good"
  }
}
# splitting into training adn testing data
set.seed(123)
# with the set.seed() we can insure that everytime we get same seperation
sample = sample.split(RedWine_GB$quality, SplitRatio = .80)

Rw_train = subset(RedWine_GB, sample == TRUE)
Rw_test = subset(RedWine_GB, sample == FALSE)

nn <- neuralnet(quality ~ ., data = Rw_train, hidden = 3, linear.output = FALSE, stepmax=1e7 )

plot(nn)

predict_testNN = compute(nn, Rw_test[, -12])
head(predict_testNN$net.result)
head(Rw_test[1, ])
p1 <- predict_testNN$net.result
# 1 => bad  0 => good
pred1 <- ifelse(p1[, 1] > 0.5, 'bad', 'good') 
tab1 <- table(pred1, Rw_test$quality) 
tab1 

acc_GB <- (tab1[1] + tab1[4]) / sum(tab1)
acc_GB

