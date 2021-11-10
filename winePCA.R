Whitewine <-read.csv(file="winequality-white.csv",head=TRUE,sep=";")
str(Whitewine)
library(dplyr)
summary(Whitewine)
Whitewine <- Whitewine %>%
 mutate(target = 'white')

Redwine <-read.csv(file="winequality-red.csv",head=TRUE,sep=",")
Redwine <- Redwine %>%
  mutate(target = 'red')

Whitewine <- Whitewine %>% slice(1:500)
Redwine <- Redwine%>% slice(1:500)

wine_data <- rbind(Whitewine, Redwine)
##################################################
str(dataset)


#target: 1 = white // 2 = red

dataset$target[dataset$target=="white"] <- "1"
dataset$target[dataset$target=="red"] <- "2"


dataset$quality = NULL


str(dataset)

dataset$target <- as.factor(dataset$target)

str(dataset)
#We call the libraries and set the training and the test

library(caTools)
set.seed(123)

split = sample.split(dataset$target, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature scaling. Here we remove the dependent variable
training_set[-12] = scale(training_set[-12])
test_set[-12] = scale(test_set[-12])

#We apply PCA
library(ggplot2)
library(lattice)
library(caret)
library(e1071)

#we create a new variable, and we need to use a function preProcess to do so

pca = preProcess(x = training_set[-12], method = 'pca', pcaComp = 2)
#Now we transform the training set into a new one with the new extracted features
training_set = predict(pca, training_set)
#we set the order
training_set = training_set[c(2, 3, 1)]

#now we do the same for our test set
test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]

#classification model
library(e1071)

classifier = svm(formula = target ~ .,#dependent variable
                 data = training_set, # this will be the new training set
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
# we exclude the dependent variable from the test set
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
#here we need to keep the dependent variable and the "y" we predicted for that value
cm = table(test_set[, 3], y_pred)
cm
###### Visualising the Training set results

#we need to indicate the PC of the set that we want to plot
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
#we create a grid set to separate the data in different regions according to the independent variables
grid_set = expand.grid(X1, X2)
#we add the names of the axes. We are working with PC1 and PC2 as the variables
colnames(grid_set) = c('PC1', 'PC2')

#we need to add the grid
y_grid = predict(classifier, newdata = grid_set)

#Now we plot it

plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
#here we add different conditions and the colour will appear accordingly
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'pink', ifelse(y_grid == 1, 'purple', 'yellow')))
#we use the conditions for the colours of the points
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'red', ifelse(set[, 3] == 1, 'blue', 'green')))

##### Visualising the Test set results

#we need to indicate the PC of the set that we want to plot
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

#we create a grid set to separate the data in different regions according to the independent variables
grid_set = expand.grid(X1, X2)
#we add the names of the axes. We are working with PC1 and PC2 as the variables
colnames(grid_set) = c('PC1', 'PC2')

#we need to define a new "y" variable that includes the colours

y_grid = predict(classifier, newdata = grid_set)

#we will exclude the dependent variable from the plot. Remember that this is not considered on the model 

plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
#here we add different conditions and the colour will appear accordingly
#the first argument is the condition itself, the second argument is the result when the condition is true 
#and the third argument is the result when the condition is not true. 
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
