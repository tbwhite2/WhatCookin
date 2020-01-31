library(data.table)

expand_ingredients = function(dt, min_frequency){
  dt = dt[,lapply(.SD,function(x){unlist(x)}),
          .SDcols = 'ingredients',
          by = .(id, cuisine)]
  #Duplications in ingredients exist, probably data error
  dt = unique(dt)
  dt[,count_ingredient := .N, by = .(ingredients)]
  #4 is roughly 1 in 10000 - seems legit
  dt = dt[count_ingredient > min_frequency]
  dt[,count_ingredient := NULL]
  dt
}

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

create_ingredient_features = function(dt){
  token_dt = create_token_dt(dt)
  token_dt[,term_frequency := .N, by = .(ingredient_token)]
  token_dt[,ingredient_diversity := uniqueN(ingredients), by = .(ingredient_token)]
  token_dt[,ingredient_no_space := stringr::str_replace_all(tolower(ingredients), ' ', '_')]
  token_dt[ingredient_diversity == 1, ingredient_token := ingredient_no_space]
  token_dt = unique(token_dt)
  
  dt = merge(token_dt, dt, by = c('id', 'ingredients'), all = T)
  
  dt
}

create_TFICF = function(dt){
  dt_sum = dt[,.(term_frequency = sum(term_frequency)),
                              by = .(ingredient_token, cuisine)]
  dt_sum[,cuisine_frequency := uniqueN(.SD$cuisine),
                 by = .(ingredient_token)]
  dt_sum[,total_cuisines := uniqueN(cuisine)]
  dt_sum[,inverse_cuisine_frequency := log(total_cuisines/cuisine_frequency)]
  dt_sum[,TFICF := term_frequency*inverse_cuisine_frequency]
  dt_sum
}

one_hot_encode_ingredients = function(dt, cutoff_TFICF = 80){

  top_ingredients = find_top_ingredients(dt, cutoff_TFICF = cutoff_TFICF)

  saveRDS(top_ingredients,'./data/top_ingredients.RDS')

  dt_reduced = dt[ingredient_token %in% top_ingredients]
  dt_ohe = dcast(dt_reduced, id ~ ingredient_token, fun = length)
  
  if(sum(rowSums(dt_ohe[,-1,with = F]) == 0) > 0){
    warning('some recipes have no ingredients at this cutoff level')
  }
  
  dt_ohe
}

summarize_to_recipe_level = function(dt,...){
  dt_ohe = one_hot_encode_ingredients(dt,...)
  dt = create_summary_features(dt)
  dt = merge(dt, dt_ohe, by = 'id')
  
  dt
}

create_summary_features = function(dt){
  dt = dt[,.(count_ingredient = uniqueN(ingredients)), by = .(id,cuisine)]
  dt
}

find_top_ingredients = function(dt,cutoff_TFICF = 80){
  TFICF_dt = create_TFICF(dt)
  
  setorder(TFICF_dt, cuisine, -TFICF)
  
  cutoff_TFICF = sort(TFICF_dt$TFICF,decreasing = T)[cutoff_TFICF]
  TFICF_dt[TFICF >= cutoff_TFICF, unique(ingredient_token)]
  
}

create_train_test = function(dt, test_prop = .3,write = F){
  
  dt[,cuisine := factor(cuisine)]
  
  dt[,validation_size := round(.N*test_prop),by = .(cuisine)]
  
  test_dt = dt[,lapply(.SD,sample,size = validation_size),
                               by = .(cuisine),.SDcols = 'id']
  test_dt = merge(dt, test_dt, by = c('id','cuisine'))
  
  train_dt = fsetdiff(dt, test_dt)
  
  model_vars = names(train_dt)[!names(train_dt) %in% c('id','cuisine')]
  
  out = list(train_dt_x = train_dt[,model_vars, with = F],
             train_dt_y = train_dt$cuisine,
             test_dt_x = test_dt[,model_vars, with = F],
             test_dt_y = test_dt$cuisine)
  if(write){
    for(name in names(out)){
      write.csv(x = out[[name]],file = paste0('./data/',name,'.csv'))
    }
  }
  
  out

}

get_train_data = function(){
  train_data = jsonlite::read_json(path = './data/train.json',simplifyVector = T) %>% 
    setDT() %>% 
    expand_ingredients(min_frequency = 4) %>% 
    create_ingredient_features() %>% 
    summarize_to_recipe_level()
}