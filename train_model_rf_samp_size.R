library(ggplot2)
library(data.table)
library(magrittr)
library(randomForest)
source('data_shaping_funcs.R')

train_test = get_train_data() %>% 
  create_train_test(test_prop = .25, write = T)

cuisine_freq = table(train_test$train_dt_y)
samp_prop = .8
equal_samp_size = round(min(cuisine_freq)*samp_prop)

samp_vec = rep(equal_samp_size, length(cuisine_freq))
names(samp_vec) = names(cuisine_freq)

rf_samp_1 = randomForest(x = train_test$train_dt_x,
                        y = train_test$train_dt_y,
                        ntree = 2500,
                        mtry = 10,
                        sampsize = samp_vec,
                        replace = F)

cuisine_freq = table(train_test$train_dt_y)
samp_prop = .8
equal_samp_size = round(cuisine_freq*samp_prop)
new_sample_prop = log(equal_samp_size/min(equal_samp_size))
new_sample_prop[new_sample_prop<1]= 1
new_samp_vec = round(new_sample_prop*min(equal_samp_size))

rf_samp_2 = randomForest(x = train_test$train_dt_x,
                        y = train_test$train_dt_y,
                        ntree = 2500,
                        mtry = 10, 
                        sampsize = new_samp_vec,
                        replace = F)

predictions_1 = predict(rf_samp_1,train_test$test_dt_x)
predictions_2 = predict(rf_samp_2,train_test$test_dt_x)

confusion_calc = caret::confusionMatrix(data = predictions_2,
                                        reference = train_test$test_dt_y)
all_recall = confusion_calc$byClass[,'Sensitivity']
acc = confusion_calc$overall['Accuracy']
balanced_acc = mean(all_recall)

cat('raw accuracy : ', acc)
cat('balanced accuracy : ', balanced_acc)

output = data.table(actual = train_test$test_dt_y,
                    predicted = predictions_1)

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
