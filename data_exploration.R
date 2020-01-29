library(ggplot2)
library(data.table)

train_data = jsonlite::read_json(path = './data/train.json',simplifyVector = T)
setDT(train_data)

#Find Frquencies of each cusine
train_data_cuisine_sum = train_data[,.(frequency = .N), by = .(cuisine)]
train_data_cuisine_sum[,cuisine := factor(cuisine,
                                          levels = train_data_cusine_sum$cuisine[order(-train_data_cuisine_sum$frequency)])]

cuisine_freq_plot = ggplot(data = train_data_cuisine_sum, aes(x = cuisine, y = frequency)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust = .2)) 

cuisine_freq_plot

ggsave(plot = cuisine_freq_plot,filename = "./plots/cuisine_freq_plot.png")

#Find Frequencies of each ingredient
train_data = train_data[,lapply(.SD,function(x){unlist(x)}),
                        .SDcols = 'ingredients',
                        by = .(id, cuisine)]

train_data_ingredient_sum = train_data[,.(frequency = .N), by = .(ingredients)]

train_data_ingredient_sum[,ingredients := factor(ingredients,
                                             levels = train_data_ingredient_sum$ingredients[order(-train_data_ingredient_sum$frequency)])]

setorder(train_data_ingredient_sum,-frequency)

ingredient_freq_plot = ggplot(data = train_data_ingredient_sum[1:10], aes(x = ingredients, y = frequency)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust = .2)) 

ingredient_freq_plot

ggsave(plot = ingredient_freq_plot,filename = "./plots/ingredient_freq_plot.png")