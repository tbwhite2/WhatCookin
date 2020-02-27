library(ggplot2)
library(data.table)
library(magrittr)
library(randomForest)
source('data_shaping_funcs.R')

train_test = get_train_data() %>% 
  create_train_test(test_prop = .25, write = T)

cuisine_freq = table(train_test$train_dt_y)
new_wts = 1/(cuisine_freq/min(cuisine_freq))

rf_wts_1 = randomForest(x = train_test$train_dt_x,
                  y = train_test$train_dt_y,
                  ntree = 2500,
                  mtry = 10, 
                  classwt = new_wts)

sampsize=c('0'=100, '1'=50)

new_wts = log(cuisine_freq/min(cuisine_freq))
new_wts[is.infinite(new_wts)] = 1
new_wts[new_wts < 1] = 1
new_wts = 1/new_wts

rf_wts_2 = randomForest(x = train_test$train_dt_x,
                  y = train_test$train_dt_y,
                  ntree = 2500,
                  mtry = 10, 
                  classwt = new_wts)

predictions_1 = predict(rf_wts_1,train_test$test_dt_x)
predictions_2 = predict(rf_wts_2,train_test$test_dt_x)

confusion_calc = caret::confusionMatrix(data = predictions_2,
                                        reference = train_test$test_dt_y)
all_recall = confusion_calc$byClass[,'Sensitivity']
acc = confusion_calc$overall['Accuracy']
balanced_acc = mean(all_recall)

cat('raw accuracy : ', acc)
cat('balanced accuracy : ', balanced_acc)

output = data.table(actual = train_test$test_dt_y,
                    predicted = predictions)

output[,correct := actual == predicted]
group_accuracy = output[,.(accuracy = round(mean(correct),3)*100,
                           count_obs = .N), by = .(actual)]

group_accuracy[,actual := factor(actual,levels = group_accuracy$actual[order(-group_accuracy$accuracy)])]

# setorder(train_data_ingredient_sum,-frequency)

ggplot(group_accuracy, aes(x = actual, y = accuracy)) +
  geom_bar(stat = 'identity') + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90,
                                 vjust = .2))

ggplot(group_accuracy, aes(x = accuracy, y = count_obs)) + 
  geom_point()
