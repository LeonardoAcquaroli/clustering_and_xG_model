library(dplyr) # data manipulation
library(purrr) # data manipulation
# plot
library(ggplot2)
library(ggsoccer) # to draw pitch
library(ggthemes) # for color-blindness
library(corrplot) # for correloagram
# utilities
library(doParallel) # for parallel
# ML
library(randomForest) # for RF
library(tree) # tree
library(rpart.plot) # tree plot
library(rpart) # tree
library(xgboost) # for boosting
library(caret) # for ML
library(tidymodels) # to tidy plot models summary 
library(pROC) # AUC-ROC
library(performance) # to check models
# process in parallel
cl = makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

# DESCRIPTIVE
## correlation
ggcorr(shots_matrix, method = c("everything", "pearson"), geom = 'circle', min_size = 5, max_size = 15, label = TRUE)
##boxplot
#par(mfrow = c(1, 1))
boxplot(shots$distance_to_goal, xlab = "Distance to goal", cex.lab = 1.5, col = "#00BF63")
boxplot(shots$angle, xlab = "Angle", cex.lab = 1.5, col = "#00BF63")
boxplot(shots$start_x, xlab = "X", cex.lab = 1.5, col = "#00BF63")
boxplot(shots$start_y, xlab = "Y", cex.lab = 1.5, col = "#00BF63")
# histograms for dummies
dummy_names = c("fromSmart_pass","fromCross","fromSave","fromDBL","head_OR_body","Goal","strongFoot")
percent_1 <- colMeans(shots[,dummy_names])
barplot(percent_1, ylim = c(0, 0.7), main = "Relative frequencies of binary variables", xlab = "Binary variables", ylab = "Percentage", names.arg = dummy_names, col = "#00BF63")
text(x = barplot(percent_1, plot = FALSE) - 0.6, y = percent_1 + 0.025, labels = paste0(round(percent_1*100, 2), "%"), pos = 4, cex = 0.8)
# drop the shots under the midfield line who probably are recording errors
# shots = shots |> filter(start_x > 20) # 21 observations dropped | already saved
# stacked histograms for shots and goal
goal_proportion = round( (sum(shots$Goal) / nrow(shots) * 100), 2 )
ggplot(data = shots, mapping = aes(x = factor(Goal), fill = factor(Goal))) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Proportion of goals", x = "Goal or No-goal", y = "Frequency") +
  scale_fill_manual(values = c("#5271FF", "#00BF63")) +
  theme(legend.position = "none") + 
  geom_text(mapping = aes(x = 2, y = 29500),
            label = paste("Percentage of goals over shots:", goal_proportion, "%"))

# evaluation functions
mse = function(predictions, data, y){
  residuals = (predictions - (data[, y]))
  mse = (1/nrow(data))*sum((residuals^2))
  return(mse)
}

class_evaluation <- function(predictions, data, y, proba_threshold = 0.5, print_table = FALSE){
  model_classes = ifelse(predictions >= proba_threshold, 1, 0) # model_classes are on the vertical (left) axes
  modelCM<-table(model_classes,data[, y])
  if (print_table){print(modelCM)}
  TP = modelCM[4]
  FP = modelCM[2]
  TN = modelCM[1]
  FN = modelCM[3]
  # ACCURACY
  accuracy = (TP+TN) / length(model_classes)
  # PRECISION
  precision = TP / (TP+FP)
  # RECALL
  recall = TP / (TP+FN) # or sensitivity
  return(data.frame(accuracy = accuracy, precision = precision, recall = recall))
}

# train test
set.seed(42)
training_index = createDataPartition(shots_matrix$Goal, p=0.7, list = FALSE) # index of the train set examples
train = shots_matrix[training_index,]
test = shots_matrix[-training_index,]

# LOGISTIC
logistic <-  glm(Goal ~ ., data = train, family = binomial(link = 'logit'))
summary(logistic)
fitted_logistic = fitted(logistic)
predicted_logistic = predict.glm(logistic, newdata = test, type = "response")
hist(fitted_logistic, breaks = 100)
hist(predicted_logistic, breaks = 100)

### diagnostics
check_model(logistic)
check_outliers(logistic)
### evaluation
#### roc-auc
roc_logistic <- roc(test$Goal, predicted_logistic) #roc-auc
auc_logistic <- round(auc(roc_logistic),4) #0.7825
logistic_optimal_threshold <- coords(roc_logistic, "best")$threshold

plot(roc_logistic, main = "Logistic: ROC Curve with Optimal Threshold", xlab = "False Positive Rate", ylab = "True Positive Rate")
points(roc_logistic$specificities[which.max(roc_logistic$sensitivities)],
       roc_logistic$sensitivities[which.max(roc_logistic$sensitivities)],
       col = "#00BF63", pch = 16, cex = 1.5)
text(roc_logistic$specificities[which.max(roc_logistic$sensitivities)] + 0.02,
     roc_logistic$sensitivities[which.max(roc_logistic$sensitivities)] - 0.07,
     paste0("Threshold = ", round(logistic_optimal_threshold, 2)), col = "#00BF63")
text(x = 0, y = 0.4, paste("AUC:", auc_logistic), col = "#5271FF")

#### errors
mse_logistic = mse(predicted_logistic, test, "Goal") # 0.08187195
logistic_evaluation = class_evaluation(predicted_logistic, test, proba_threshold = logistic_optimal_threshold, "Goal", print_table = T)

# TREE (features importance)
tree = rpart(Goal ~ ., data = train)
fitted_tree = predict(tree, newdata = train)
predicted_tree = predict(tree, newdata = test)
hist(fitted_tree)
hist(predicted_tree)
### plot with rpart
#### use a custom threshold to color the boxes
rpart.plot(tree, shadow.col = "gray") # choose the threshold to color in green or blue

### evaluation
#### roc-auc
roc_tree <- roc(test$Goal, predicted_tree) #roc-auc
auc_tree <- round(auc(roc_tree), 4) #0.7316
tree_optimal_threshold <- coords(roc_tree, "best")$threshold

plot(roc_logistic, main = "Decision tree: ROC Curve with Optimal Threshold", xlab = "False Positive Rate", ylab = "True Positive Rate")
points(roc_tree$specificities[which.max(roc_tree$sensitivities)],
       roc_tree$sensitivities[which.max(roc_tree$sensitivities)],
       col = "#00BF63", pch = 16, cex = 1.5)
text(roc_tree$specificities[which.max(roc_tree$sensitivities)] + 0.14,
     roc_tree$sensitivities[which.max(roc_tree$sensitivities)] - 0.08,
     paste0("Threshold = ", round(tree_optimal_threshold, 2)), col = "#00BF63")
text(x = 0.2, y = 0.4, paste("AUC:", auc_tree), col = "#5271FF")

#### errors
mse_tree = mse(predicted_tree, test, "Goal") # 0.08467176
tree_evaluation = class_evaluation(predicted_tree, test, proba_threshold = tree_optimal_threshold, "Goal", print_table = T)

# RANDOM FOREST
rForest = randomForest(as.factor(Goal) ~ ., data=train, ntree=500, mtry=round(sqrt(ncol(train)-1),0), importance=TRUE)
varImpPlot(rForest, main = "Random forest: variables importance")
fitted_rForest = as.numeric(as.character(rForest$predicted))
predicted_rForest = as.numeric(as.character(predict(rForest, test, type = "response")))
hist(fitted_rForest)
hist(predicted_rForest)

### evaluation
#### roc-auc
roc_rForest <- roc(test$Goal, predicted_rForest) #roc-auc
auc_rForest <- round(auc(roc_rForest),4) #0.5464 with 500 trees
rForest_optimal_threshold <- coords(roc_rForest, "best")$threshold

plot(roc_rForest, main = "ROC Curve with Optimal Threshold", xlab = "False Positive Rate", ylab = "True Positive Rate")
points(roc_rForest$specificities[which.max(roc_rForest$sensitivities)],
       roc_rForest$sensitivities[which.max(roc_rForest$sensitivities)],
       col = "#00BF63", pch = 16, cex = 1.5)
text(roc_rForest$specificities[which.max(roc_rForest$sensitivities)] + 0.02,
     roc_rForest$sensitivities[which.max(roc_rForest$sensitivities)] - 0.05,
     paste0("Threshold = ", round(rForest_optimal_threshold, 2)), col = "#00BF63")
text(x = 0.2, y = 0.4, paste("AUC:", auc_rForest), col = "#5271FF")

#### errors
mse_rForest = mse(predicted_rForest, test, "Goal") # 0.1018117
rForest_evaluation = class_evaluation(predicted_rForest, test, proba_threshold = rForest_optimal_threshold, "Goal", print_table = T)

# BOOSTING
dtrain <- xgb.DMatrix(data = as.matrix(train[,names(train) != "Goal"]), label = train$Goal)
# Set the parameters for the XGBoost model
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc"
)

# Train the XGBoost model
XGboost <- xgboost(params = params, data = dtrain, nrounds = 20000) # 19982 iterations to max AUC
dtest <- xgb.DMatrix(data = as.matrix(test[,names(test) != "Goal"]), label = test$Goal)
fitted_XGboost = predict(XGboost, dtrain, type = "response")
predicted_XGboost <- predict(XGboost, dtest)
hist(fitted_XGboost, breaks = 100)
hist(predicted_XGboost, breaks = 100)
# features importance
XGb_featuresImp = xgb.importance(model = XGboost)
XGb_featuresImp_to_plot = as.data.frame(cbind(Variable = XGb_featuresImp$Feature, Gain = XGb_featuresImp$Gain))
XGb_featuresImp_to_plot$Gain =as.numeric(XGb_featuresImp_to_plot$Gain)
ggplot(data = XGb_featuresImp, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Feature") +
  ylab("Gain") +
  ggtitle("XGboost: feature Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### evaluation
#### roc-auc
roc_XGboost <- roc(test$Goal, predicted_XGboost) #roc-auc
auc_value <- round(auc(roc_XGboost),4) #1
XGboost_optimal_threshold <- coords(roc_XGboost, "best")$threshold

plot(roc_XGboost, main = "ROC Curve with Optimal Threshold", xlab = "False Positive Rate", ylab = "True Positive Rate")
points(roc_XGboost$specificities[which.max(roc_XGboost$sensitivities)],
       roc_XGboost$sensitivities[which.max(roc_XGboost$sensitivities)],
       col = "#00BF63", pch = 16, cex = 1.5)
text(roc_XGboost$specificities[which.max(roc_XGboost$sensitivities)] + 0.02,
     roc_XGboost$sensitivities[which.max(roc_XGboost$sensitivities)] - 0.08,
     paste0("Threshold = ", round(XGboost_optimal_threshold, 2)), col = "#00BF63")
text(x = 0.2, y = 0.4, paste("AUC:", auc_rForest), col = "#5271FF")

#### errors
mse_XGboost = mse(predicted_XGboost, test, "Goal") # 0
XGboost_evaluation = class_evaluation(predicted_XGboost, test, proba_threshold = XGboost_optimal_threshold, "Goal", print_table = T)

##############################################################################################################
##############################################################################################################

# Add predicted xG to the dataframe
# And group by player to create a leaderboard

# add xG column
#xG = predict(logistic, shots_matrix, type = "response")
#try xgboost
dtot <- xgb.DMatrix(data = as.matrix(shots_matrix[,names(shots_matrix) != "Goal"]), label = shots_matrix$Goal)
xG_xgboost <- predict(XGboost, dtot)
shots_xG = cbind(shots, xG_xgboost = xG_xgboost)
# groupby
shots_xG |>
  group_by(playerId) |>
  summarise(sum_xG_xgboost = sum(xG_xgboost)) |>
  inner_join(players, by = c("playerId" = "wyId")) |>
  select( c("sum_xG_xgboost", "shortName") ) |>
  arrange(desc(sum_xG_xgboost))










