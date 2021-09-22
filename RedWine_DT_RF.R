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
pacman::p_load(pacman, rio, Metrics, rpart, rpart.plot, RColorBrewer, randomForest) 

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

RedWine[, 12] <- as.factor(RedWine[, 12])
#RedWine_csv <- import("D:/MachineLearning_Project/winequality-red.csv")

### SECOND STEP Data Preprocessing !!!

# splitting into training adn testing data
set.seed(123)
# with the set.seed() we can insure that everytime we get same seperation
sample = sample.split(RedWine$quality, SplitRatio = .70)

Rw_train = subset(RedWine, sample == TRUE)
Rw_test = subset(RedWine, sample == FALSE)



### DECISION TREE algorithm !!!

# Training the Decision tree classifier
tree <- rpart(quality ~ ., data = Rw_train)

# Prediction
tree.quality.predicted <- predict(tree, Rw_test, type = 'class')

# Confusion matrix for evaluating the model
confusionMatrix(tree.quality.predicted, Rw_test$quality)

rpart.plot(tree)




### random forest algorithm !!!

rf <- randomForest(quality ~ ., data = Rw_train, mtry=4, ntree=2001, importance=TRUE)

rf
plot(rf)

result <- data.frame(Rw_test$quality, predict(rf, Rw_test[, 1:11], type = "response"))
result

plot(result)



rf2 <- randomForest(quality ~ ., data = Rw_train, mtry=4, ntree=601, importance=TRUE)

rf2

result2 <- data.frame(Rw_test$quality, predict(rf, Rw_test[, 1:11], type = "response"))
result2

plot(result2)
