library(ggplot2)
library(data.table)
library(magrittr)

source('data_shaping_funcs.R')

train_test = get_train_data() %>% 
  create_train_test(test_prop = .25, write = T)

rf = randomForest::randomForest(x = train_test$train_data_x,
                                y = train_test$train_data_y)

predictions = predict(rf,train_test$test_dt_x)

output = data.table(actual = train_test$test_dt_y,
                    predicted = predictions)

nrow(output[actual == predicted])/nrow(output)
output[,correct := actual == predicted]
output[,round(mean(correct),3)*100]
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
