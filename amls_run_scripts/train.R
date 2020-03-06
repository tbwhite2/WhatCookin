library(randomForest)
library(optparse)
message('libraries loaded')
options = list(
  make_option(c("-d", "--data_folder")),
  make_option(c("-i", "--inputs")),
  make_option(c("-m", "--min_samples_leaf")),
  make_option(c("-n", "--n_estimators")),
  make_option(c("-w", "--wts_method"))
)

opt_parser = OptionParser(option_list = options)
opt = parse_args(opt_parser)
message('script args parsed')

files = c('train_dt_x.csv',
          'train_dt_y.csv',
          'test_dt_x.csv',
          'test_dt_y.csv')

data_list = lapply(files, function(file){
  read.csv(file.path(opt$data_folder,opt$inputs,file))
})
message('data loaded')
names(data_list) = sub('(\\.).+', replacement = '',x = files)
message('min_samples_leaf:',opt$min_samples_leaf, class(opt$min_samples_leaf))
message('n_estimators:',opt$n_estimators, class(opt$n_estimators))

new_wts = rep(1,length(unique(data_list$train_dt_y$x)))

if(opt$wts_method != 'no_wts'){
  message('constructing class weights')
  cuisine_freq = table(data_list$train_dt_y$x)
  
  if(opt$wts_method == 'inv_freq'){
    new_wts = 1/(cuisine_freq/min(cuisine_freq))
  }
  
  if(opt$wts_method == 'inv_log_freq'){
    new_wts = log(cuisine_freq/min(cuisine_freq))
    new_wts[is.infinite(new_wts)] = 1
    new_wts[new_wts < 1] = 1
    new_wts = 1/new_wts
  }
  
}

rf = randomForest(x = data_list$train_dt_x,
                  y = data_list$train_dt_y$x,
                  nodesize = as.integer(opt$min_samples_leaf),
                  ntree = as.integer(opt$n_estimators),classwt = new_wts)
message('model fit')

pred = predict(rf, data_list$test_dt_x)
message('model predicted')

confusion_calc = caret::confusionMatrix(data = pred, reference = data_list$test_dt_y$x)
all_recall = confusion_calc$byClass[,'Sensitivity']
acc = confusion_calc$overall['Accuracy']
balanced_acc = mean(all_recall)
message('metrics created')

azuremlsdk::log_metric_to_run("Accuracy", acc)
azuremlsdk::log_metric_to_run("Balanced Accuracy", balanced_acc)
azuremlsdk::log_metric_to_run("Min Samples Leaf", opt$min_samples_leaf)
azuremlsdk::log_metric_to_run("N Estimators", opt$n_estimators)
message('metrics logged')

output_dir = "outputs"
if (!dir.exists(output_dir)){
  dir.create(output_dir)
}

saveRDS(rf, file = "./outputs/model.rds")
message("Model saved")