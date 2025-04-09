install.packages("SHAPforxgboost")
install.packages("here")
install.packages("data.table")
library(xgboost)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(caret)
library(pROC)
library(SHAPforxgboost)
library(ggplot2)
library(here)
library(data.table)
form_cs<-read.csv(file.choose())
skim(form_cs)
plot_missing(form_cs)
hist(form_cs$SES,breaks = 50)

#split pool
set.seed(50)
trains<-createDataPartition(
  y=form_cs$SES,
  p=0.8,
  list = F,
  times=1
)
trains2<-sample(trains,nrow(form_cs)*0.7)
valids<-setdiff(trains,trains2)

data_train<-form_cs[trains2,]
data_valid<-form_cs[valids,]
data_test<-form_cs[-trains,]

#Distribution of dependent variables after splitting
hist(data_train$SES,breaks = 50)
hist(data_valid$SES,breaks = 50)
hist(data_test$SES,breaks = 50)
#data preparation
colnames(form_cs)
dvfunc<-dummyVars(~.,data = data_train[,1:10],fullRank = T)
data_trainx<-predict(dvfunc,newdata=data_train[,1:10])
data_trainy<-data_train$SES

data_validx<-predict(dvfunc,newdata=data_valid[,1:10])
data_validy<-data_valid$SES

data_testx<-predict(dvfunc,newdata=data_test[,1:10])
data_testy<-data_test$SES

dtrain<-xgb.DMatrix(data = data_trainx,
                   label=data_trainy)

dvalid<-xgb.DMatrix(data = data_validx,
                   label=data_validy)

dtest<-xgb.DMatrix(data = data_testx,
                    label=data_testy)
watchlist<-list(train=dtrain,test=dvalid)
# Ten-fold cross-validation using caret
set.seed(123)
xgb_cv <- train(data_trainx, data_trainy, method = "xgbTree", trControl = trainControl(method = "cv", number = 10))
print(xgb_cv)
#training model
fit_xgb_reg<-xgb.train(
  data = dtrain,
  
  nrounds =100, 
  objective = "reg:squarederror", 
  eta = 0.3, 
  max_depth =1, 
  gamma = 0, 
  colsample_bytree = 0.8, 
  min_child_weight = 1, 
  subsample =1)
#model summary
fit_xgb_reg
#variable importance
importance_matrix<-xgb.importance(model = fit_xgb_reg)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")
#SHAP
xgb.plot.shap(data = data_trainx,
              model = fit_xgb_reg,
              top_n = 5)
shap_data <- copy(shap_values$shap_score)
shap_data[, BIAS := shap_values$BIAS0]
pred_mod <- predict(fit_xgb_reg, data_trainx, ntreelimit = 10)
shap_data[, `:=`(rowSum = round(rowSums(shap_data),6), pred_mod = round(pred_mod,6))]
rmarkdown::paged_table(shap_data[1:20,])
#Summary graph ( global feature importance )
#method 1
shap_values <- shap.values(xgb_model = fit_xgb_reg, X_train = data_trainx)
shap_values$mean_shap_score
shap.plot.summary.wrap1(model = fit_xgb_reg, X = data_trainx)
#method 2
shap_long <- shap.prep(xgb_model = fit_xgb_reg, X_train = data_trainx)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = data_trainx)
shap.plot.summary(shap_long)

#dependence graph
#g1 <- shap.plot.dependence(data_long = shap_long, x = 'PTCD', y = 'PTCD', color_feature = 'SES') + ggtitle("(A) SHAP values of Time trend vs. Time trend")
#g2 <- shap.plot.dependence(data_long = shap_long, x = 'PTCD', y = 'SES', color_feature = 'SES') +  ggtitle("(B) SHAP values of SES vs. Time trend")
#gridExtra::grid.arrange(g1, g2, ncol = 2)
fig_list <- lapply(names(shap_values$mean_shap_score)[10], 
                   shap.plot.dependence, data_long = shap_long)
gridExtra::grid.arrange(grobs = fig_list, ncol = 1)

# Create an empty data box to store the values of x-axis, y-axis and shap _ score.
output_data_combined <- data.frame()

# Loop each graph, extract the values of the x-axis, y-axis, and shap _ score, and add them to the data box.
for (i in seq_along(fig_list)) {
  # Output the index of the current loop
  cat("Processing figure", i, "\n")
  
  # View the data structure of the graph
  print(ggplot_build(fig_list[[i]])$data)
  
  # Extract x, y values
  x_col <- NULL
  y_col <- NULL
  
  # Try to extract x and y columns
  for (j in seq_along(ggplot_build(fig_list[[i]])$data)) {
    data_cols <- ggplot_build(fig_list[[i]])$data[[j]]
    if ("x" %in% names(data_cols) && "y" %in% names(data_cols)) {
      x_col <- data_cols$x
      y_col <- data_cols$y
      break
    }
  }
  
  # If x _ col and y _ col are still empty, try using other columns
  if (is.null(x_col) || is.null(y_col)) {
    for (j in seq_along(ggplot_build(fig_list[[i]])$data)) {
      data_cols <- ggplot_build(fig_list[[i]])$data[[j]]
      if ("group" %in% names(data_cols) && "colour" %in% names(data_cols)) {
        x_col <- data_cols$group
        y_col <- data_cols$colour
        break
      }
    }
  }
  
  # Creates a data box containing x, y values
  fig_data <- data.frame(x = x_col, y = y_col)
  
  # Output the current loop data box
  print(fig_data)
  
  # Add the data box to output _ data _ combined
  output_data_combined <- rbind(output_data_combined, fig_data)
}

# Save to CSV
write.csv(output_data_combined, "circle_financial.csv", row.names = FALSE)


# Create an empty data box to store the values of x-axis, y-axis and shap _ score
output_data_combined <- data.frame()

# Loop each graph, extract the values of the x-axis, y-axis, and shap _ score, and add them to the data box
for (i in seq_along(fig_list)) {
  fig_data <- ggplot_build(fig_list[[i]])$data[[4]]
  output_data_combined <- rbind(output_data_combined, fig_data)
}

# Save to CSV
write.csv(output_data_combined, "output_combined.csv", row.names = FALSE)



#interaction
shap_int <- shap.prep.interaction(xgb_mod = fit_xgb_reg, X_train = data_trainx)
shap_int <- predict(fit_xgb_reg, data_trainx, predinteraction = TRUE)
g3 <- shap.plot.dependence(data_long = shap_long,
                           data_int = shap_int,
                           x= "CR_residen", y = "NDVI", 
                           color_feature = "NDVI")
gridExtra::grid.arrange(g3)
#
plot_data <- shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 5, n_groups = 6)
shap.plot.force_plot(plot_data, zoom_in_location = 150, y_parent_limit = c(-0.3,0.3))
shap.plot.force_plot_bygroup(plot_data)

#prediction
#Training set prediction results
trainpred<-predict(fit_xgb_reg,
                   newdata = dtrain)
#Training set prediction error index
defaultSummary(data.frame(obs=data_train$SES,
                          pred=trainpred))
#Graphical training set prediction results
plot(x=data_train$SES,
     y=trainpred,
     sub="training set")
trainlinmod<-lm(trainpred~data_train$SES)
abline(trainlinmod,col="blue",lwd=2.5,lty="solid")
abline(a=0,b=1,col="red",lwd=2.5,lty="dashed")
legend("topleft",
       legend=c("Model","Base"),
              col = c("blue","red"),
              lwd=2.5,
              lty = c("solid","dashed"))
#Test set prediction results
testpred<-predict(fit_xgb_reg,
                   newdata = dtest)
#Test set prediction error index
defaultSummary(data.frame(obs=data_test$SES,
                          pred=testpred))
#Graphical test set prediction results
plot(x=data_test$SES,
     y=testpred,
     sub="test set")
testlinmod<-lm(testpred~data_test$SES)
abline(testlinmod,col="blue",lwd=2.5,lty="solid")
abline(a=0,b=1,col="red",lwd=2.5,lty="dashed")
legend("topleft",
       legend=c("Model","Base"),
       col = c("blue","red"),
       lwd=2.5,
       lty = c("solid","dashed"))       
#The prediction results of the training set and the test set are displayed in a centralized manner
predresult<-
  data.frame(obs=c(data_train$SES,data_test$SES),
             pred=c(trainpred,testpred),
             group=c(rep("Train",length(trainpred)),
                     rep("Test",length(testpred))))
ggplot(predresult,
       aes(x=obs,y=pred,fill=group,colour=group))+
  geom_point(shape=21,size=3)+
  geom_smooth(method = "lm",se=F)+
  geom_abline(intercept = 0,slope = 1)+
  theme_classic()+
  theme(legend.position = "bottom",
        plot.title = element_text((hjust=0.5)))
#Save to CSV
write.csv(importance_matrix, "importance.csv", row.names = FALSE)
  