library(ggplot2)
library(data.table)
library(magrittr)

source('data_shaping_funcs.R')

train_data = get_train_data(top_ingredients = readRDS('.data/top_ingredients.RDS'))


train_data = jsonlite::read_json(path = './data/train.json',simplifyVector = T) %>% 
  setDT() %>% 
  expand_ingredients(min_frequency = 4) %>% 
  create_ingredient_features() %>% 
  summarize_to_recipe_level()
  
train_data[,cuisine := factor(cuisine)]

train_data[,validation_size := round(.N*.3),by = .(cuisine)]

validation_data = train_data[,lapply(.SD,sample,size = validation_size),
                             by = .(cuisine),.SDcols = 'id']
validation_data = merge(train_data, validation_data, by = c('id','cuisine'))

train_data = fsetdiff(train_data, validation_data)


rf = randomForest::randomForest(x = train_data_x,
                                y = train_data_y)

predictions = predict(rf,validation_data[,names(train_data)[!names(train_data) %in% c('id','cuisine')], with = F])

output = data.table(actual = validation_data$cuisine,
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
