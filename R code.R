library(tidyverse)
library(caTools)
library(ROSE)
library(rpart)
library(randomForest)
library(rpart.plot)
library(ROCR)
library(ggplot2)
library(corrplot)
library(caret)
library(glmnet)
library(pROC)
library(dplyr)
library(ClusterR) 
library(cluster)
library(clusterCrit)


data <- read.csv("Data.csv")
data <- subset(data, select = -Message)
str(data)
colSums(is.na(data))
summary(data)


ggplot(data, aes(x = Label)) +
  geom_bar(fill = "maroon") +
  labs(title = "Count of 'spam' and 'ham'") +
  xlab("Label") +
  ylab("Count") + theme_minimal()

ggplot(data, aes(x = Label, y = Text_Length, fill = Label)) +
  geom_boxplot() +
  labs(title = "Relationship between Text Length and Label", x = "Label", y = "Text Length") + theme_minimal()

ggplot(data, aes(x = Punctuation_Count, fill = Label)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Punctuation Count by Label", x = "Punctuation Count", y = "Count") + theme_minimal()

ggplot(data, aes(x = Word_Count, y = Text_Length, color = Label)) +
  geom_point() +
  labs(title = "Word Count vs. Text Length", x = "Word Count", y = "Text Length") + theme_minimal()

data$Label <- ifelse(data$Label == "spam", 1, 0)


set.seed(123)
split <- sample.split(data$Label, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)


train_data_balanced <- ovun.sample(Label ~ ., data = train_data, method = "over")$data
test_data_balanced <- ovun.sample(Label ~ ., data = test_data, method = "over")$data





logistic_model <- glm(Label ~ ., data = train_data_balanced, family = "binomial")
predictions <- predict(logistic_model, newdata = test_data_balanced, type = "response")
summary(logistic_model)

logistic_model2 <- glm(Label ~ . -and -can -in. -is -the -me -of -so -to -you, data = train_data_balanced, family = "binomial")
predictions <- predict(logistic_model2, newdata = test_data_balanced, type = "response")
summary(logistic_model2)

logistic_model3 <- glm(Label ~ . -and -can -in. -is -the -me -of -so -to -you -Avg_Word_Length -on, data = train_data_balanced, family = "binomial")
predictions <- predict(logistic_model3, newdata = test_data_balanced, type = "response")
summary(logistic_model3)

threshold <- 0.4
predicted_labels <- ifelse(predictions > threshold, 1, 0)
actual_labels <- test_data_balanced$Label

conf_matrix <- table(test_data_balanced$Label, predicted_labels)
conf_matrix

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
sensitivity
specifity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
specifity
F1_score <- 2 * (sensitivity+specifity)
F1_score

pred_test <- predict(logistic_model3, newdata=test_data_balanced, type ="response")
pred1 <- prediction(pred_test, test_data_balanced$Label)

perf1 <- performance(pred1, "tpr", "fpr")


plot(perf1, col="skyblue")

as.numeric(performance(pred1, "auc")@y.values)




cart_model <- rpart(Label ~ ., data = train_data_balanced, method = "class", cp=0.01)

prp(cart_model)
predictions <- predict(cart_model, newdata = test_data_balanced, type = "class")

conf_matrix <- table(test_data_balanced$Label, predictions)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", precision, "\n")
cat("specificity:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

pdf("decision_tree.pdf")
rpart.plot(cart_model, yesno=2, type=4, extra=101, under=TRUE, fallen.leaves=TRUE, tweak=1.2, compress=TRUE)
dev.off()

bestmtry <- tuneRF(train_data_balanced,train_data_balanced$Label,stepFactor = 1.2, improve = 0.01, trace=T, plot= T) 


penalty_matrix <- matrix(c(0,1 , 4,0),2,2)
penalty_matrix

rf_model <- randomForest(Label ~ ., data = train_data_balanced, mtry = 10, ntree = 200, type='class',parms=list(loss=PenaltyMatrix))
summary(rf_model)
rf_model
importance(rf_model)
varImpPlot(rf_model)
threshold <- 0.3

predictions <- predict(rf_model, newdata = test_data_balanced)
predicted_labels <- ifelse(predictions > threshold, 1, 0)

conf_matrix <- table(test_data_balanced$Label, predicted_labels)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", precision, "\n")
cat("specificity:", specificity, "\n")
cat("F1 Score:", f1_score, "\n")


data_new <- subset(data, select=-Label)

kmeans_result <- kmeans(data_new, centers = 2)

cluster_assignments <- kmeans_result$cluster

conf_matrix <- table(data$Label, cluster_assignments)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
Sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
specificity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (Sensitivity * specificity) / (Sensitivity + specificity)
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", Sensitivity, "\n")
cat("specificity:", specificity, "\n")
cat("F1 Score:", f1_score, "\n")



