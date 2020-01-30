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
  train_data_sum = train_data[,.(term_frequency = sum(term_frequency)),
                              by = .(ingredient_token, cuisine)]
  train_data_sum[,cuisine_frequency := uniqueN(.SD$cuisine),
                 by = .(ingredient_token)]
  train_data_sum[,total_cuisines := uniqueN(cuisine)]
  train_data_sum[,inverse_cuisine_frequency := log(total_cuisines/cuisine_frequency)]
  train_data_sum[,TFICF := term_frequency*inverse_cuisine_frequency]
  train_data_sum
}

one_hot_encode_ingredients = function(dt, cutoff_TFICF = 80,top_ingredients = NA,save_top_ingredients = T){
  
  if(is.na(top_ingredients)){
    top_ingredients = find_top_ingredients(dt, cutoff_TFICF = cutoff_TFICF)
    saveRDS(top_ingredients)
  }
  
  dt_reduced = dt[ingredient_token %in% top_ingredients]
  dt_ohe = dcast(dt_reduced, id ~ ingredient_token, fun = length)
  
  if(sum(rowSums(dt_ohe[,-1,with = F]) == 0) > 0){
    warning('some recipes have no ingredients at this cutoff level')
  }
  
  dt_ohe
}

summarize_to_recipe_level = function(dt,...){
  dt_ohe = one_hot_encode_ingredients(dt)
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