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
pacman::p_load(pacman, rio, Metrics, rpart, rpart.plot, RColorBrewer) 

# include knn()
library(class)
# include rmse()
library(Metrics) 
# include rpart()
library(rpart)
library(rpart.plot)

# load CSV
# https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
RedWine_csv <- import("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv")
#RedWine_csv <- import("D:/MachineLearning_Project/winequality-red.csv")

### FIRST STEP Exploratory data analysis !!!

# check the dataset structure
head(RedWine_csv)
summary(RedWine_csv)
str(RedWine_csv)
hist(RedWine_csv$quality)

# check if there are missing data
any(is.na(RedWine_csv))
# the answer is FALSE which means there is not missing value.

# from the summary we notice that the Residual Sugar's range
# may have the most important outline questions

# use boxplot to create visualize graph
boxplot(RedWine_csv$`fixed acidity`)
boxplot(RedWine_csv$`residual sugar`)

# decide to use Z-scores to eliminate the outline data
# find absolute value of z-score for each value in each column
z_scores <- as.data.frame(sapply(RedWine_csv, function(RedWine_csv) (abs(RedWine_csv-mean(RedWine_csv))/sd(RedWine_csv))))
head(z_scores)

# only keep rows in dataframe with all z-scores less than absolute value of 3 
# NOL_RedWine means No Outlier
NOL_RedWine <- RedWine_csv[!rowSums(z_scores>3), ]
summary(NOL_RedWine)
str(NOL_RedWine)
# after eliminate the outlier , we have 1451 rows left
# which means we eliminate almost 9.26% datas

# Checking the correlation between attributes
cco <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
for ( i in 1:12) {
  cco[i] <- cor(NOL_RedWine[,i], NOL_RedWine[,12])
}
cco
cc <- c()
for (i in 1:12){cc[[i]] <- i}
plot(cc,cco, xlab="attribute", ylab="correlation coefficent")

# Checking the class imbalance
table(RedWine_csv$quality)

### SECOND STEP Data Preprocessing !!!

# Train and test sets split
ind <- sample(2, nrow(RedWine_csv), replace=TRUE, prob=c(0.8, 0.2))
ind
RedWine_trainData <- RedWine_csv[ind==1, ]
RedWine_testData <- RedWine_csv[ind==2, ]
str(RedWine_trainData)
str(RedWine_testData)

### 3rd STEP KNN algorithm !!!

# Empty variables
KnnTestPrediction <- list()
accuracy <- numeric()

# From k=1 to k=100...
for(k in 1:100){
  
  # KnnTestPrediction for each k
  KnnTestPrediction[[k]] <- knn(RedWine_trainData[, -12], RedWine_testData[, -12], RedWine_trainData$quality, k, prob=TRUE)
  
  # Accuracy for each k
  accuracy[k] <- sum(KnnTestPrediction[[k]]==RedWine_testData$quality)/length(RedWine_testData$quality)*100
  
}

plot(accuracy, type = "b", col = "dodgerblue", cex = 1, pch = 20, xlab = "k, number of neighbors", ylab = "Classification accuracy", main = "Accuracy vs Neighbors")

