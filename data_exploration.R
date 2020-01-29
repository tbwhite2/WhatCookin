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

ggsave(plot = cuisine_freq_plot,filename = "cuisine_freq_plot.png")

#Find Frequencies of each ingredient
