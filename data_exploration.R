library(ggplot2)
library(data.table)

train_data = jsonlite::read_json(path = './data/train.json',simplifyVector = T)
setDT(train_data)

#Expand out ingredients
train_data = train_data[,lapply(.SD,function(x){unlist(x)}),
                        .SDcols = 'ingredients',
                        by = .(id, cuisine)]
#Duplications in ingredients exist, probably data error
train_data = unique(train_data)
#Find Frquencies of each cusine
train_data_cuisine_sum = train_data[,.(frequency = .N), by = .(cuisine)]
train_data_cuisine_sum[,cuisine := factor(cuisine,
                                          levels = train_data_cuisine_sum$cuisine[order(-train_data_cuisine_sum$frequency)])]

cuisine_freq_plot = ggplot(data = train_data_cuisine_sum, aes(x = cuisine, y = frequency)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust = .2)) 

cuisine_freq_plot

ggsave(plot = cuisine_freq_plot,filename = "./plots/cuisine_freq_plot.png")

#Find Frequencies of each ingredient

train_data_ingredient_sum = train_data[,.(frequency = .N), by = .(ingredients)]

train_data_ingredient_sum[,ingredients := factor(ingredients,
                                             levels = train_data_ingredient_sum$ingredients[order(-train_data_ingredient_sum$frequency)])]

setorder(train_data_ingredient_sum,-frequency)

ingredient_freq_plot = ggplot(data = train_data_ingredient_sum[1:10], 
                              aes(x = ingredients, y = frequency)) + 
  geom_bar(stat = 'identity') + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90,
                                 vjust = .2)) 

ingredient_freq_plot

ggsave(plot = ingredient_freq_plot,filename = "./plots/ingredient_freq_plot.png")

#investigate repeats
# words that are subsets of another 
# word = 'onion'

# train_data[,last_word_ingredient := get_last_word_v(ingredients)]
train_data[,count_ingredient := .N, by = .(ingredients)]

train_data = train_data[count_ingredient > 4]

create_token_dt = function(dt){
  
  tokens = quanteda::tokens(dt$ingredients,
                            remove_punct = T,
                            ngrams = 1:2,)

  tokens = quanteda::tokens_tolower(tokens)
  # tokens = quanteda::tokens_wordstem(tokens)
  
  original_word = rep(x = dt$ingredients,
                      unlist(lapply(tokens, length)))
  id = rep(x = dt$id,
           unlist(lapply(tokens, length)))
  
  data.table(ingredient_token = unlist(tokens),
             ingredients = original_word,
             id = id)
}

token_dt = create_token_dt(train_data)

token_dt[,term_frequency := .N, by = .(ingredient_token)]

token_dt[,ingredient_diversity := uniqueN(ingredients), by = .(ingredient_token)]

token_dt[,ingredient_no_space := stringr::str_replace_all(tolower(ingredients), ' ', '_')]

token_dt[ingredient_diversity == 1, ingredient_token := ingredient_no_space]

# thing = token_dt[ingredient_diversity == 1]
token_dt = unique(token_dt)

train_data = merge(token_dt, train_data, by = c('id', 'ingredients'), all = T)

train_data_sum = train_data[,.(count_in_cuisine = .N,
              term_frequency = sum(term_frequency)), by = .(ingredient_token, cuisine)]

train_data_sum[,total_in_cuisine := sum(count_in_cuisine), by = .(cuisine)]

train_data_sum[,cuisine_frequency := uniqueN(.SD$cuisine),by = .(ingredient_token)]

train_data_sum[,total_cuisines := uniqueN(cuisine)]

train_data_sum[,inverse_cuisine_frequency := log(total_cuisines/cuisine_frequency)]
# train_data[,inverse_cuisine_frequency := log(uniqueN(train_data$cuisine)/cuisine_frequency)]
train_data_sum[,TFICF := term_frequency*inverse_cuisine_frequency]
# train_data_tficf = unique(train_data[,c('TFICF','cuisine','ingredient_token'), with = F])
setorder(train_data_sum, cuisine, -TFICF)
thing = train_data_sum[,.SD[1:5], by = .(cuisine)]

ggplot(data = thing[cuisine %in% c('southern_us','chinese','french')], aes(x = ingredient_token, y = TFICF, fill = cuisine)) + 
  geom_bar(stat = 'identity') +
  facet_grid(cols = vars(cuisine),scales = 'free') +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90,
                                 vjust = .2)) 

TFICF = sort(train_data_sum$TFICF,decreasing = T)[1:1000]

ggplot(data = data.frame(TFICF = TFICF,
                         ind = seq_along(TFICF)),
       aes(x = ind, y = TFICF)) + 
  geom_line() + 
  geom_vline(xintercept = 80,colour = 'red') + 
  xlab('') + 
  theme_minimal()

cutoff_TFICF = TFICF[80]

top_terms = train_data_sum[TFICF >= cutoff_TFICF, unique(ingredient_token)]

token_dt = token_dt[ingredient_token %in% top_terms]
token_dt_ohe = dcast(token_dt, id ~ ingredient_token, fun = length)
sum(rowSums(token_dt_ohe[,-1,with = F]) == 0)
rm(token_dt)

train_data = train_data[,.(count_ingredient = uniqueN(ingredients)), by = .(id,cuisine)]

train_data = merge(train_data, token_dt_ohe, by = 'id')
train_data[,cuisine := factor(cuisine)]

train_data[,validation_size := round(.N*.3),by = .(cuisine)]

validation_data = train_data[,lapply(.SD,sample,size = validation_size), by = .(cuisine),.SDcols = 'id']
validation_data = merge(train_data, validation_data, by = c('id','cuisine'))

train_data = fsetdiff(train_data, validation_data)

train_data_x = train_data[,names(train_data)[!names(train_data) %in% c('id','cuisine')], with = F]
train_data_y = train_data$cuisine



rf = randomForest::randomForest(x = train_data_x, y = train_data_y)

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
# train_data
# IDF = log(10,000,000 / 1,000) = 4
# get_last_word = function(string){
#   split_string = strsplit(string, split = ' ')[[1]]
#   tail(split_string,1)
# }
# get_last_word_v = Vectorize(get_last_word)


# words that are very close in string dist

#ingredients that appear with the same cohorts