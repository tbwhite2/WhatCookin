library(reticulate)
# library(azuremlsdk) 
# loading the package causes some backend reticulate stuff to be changed - don't do it on the
# R server
use_condaenv('r-azureml', required = T)

ws = azuremlsdk::load_workspace_from_config()

ds = azuremlsdk::get_default_datastore(workspace = ws)

experiment_name <- "r_sdk"
exp <- azuremlsdk::experiment(ws, experiment_name)

cluster_name <- "cpucluster"
compute_target <- azuremlsdk::get_compute(ws, cluster_name = cluster_name)
if (is.null(compute_target)) {
  vm_size <- "STANDARD_D2_V2" 
  compute_target <- azuremlsdk::create_aml_compute(workspace = ws,
                                                   cluster_name = cluster_name,
                                                   vm_size = vm_size,
                                                   max_nodes = 2)
  
  azuremlsdk::wait_for_provisioning_completion(compute_target, show_output = TRUE)
}


est <- azuremlsdk::estimator(source_directory = './amls_run_scripts',
                 entry_script = "train.R",
                 script_params = list("--data_folder" = ds$as_mount(),
                                      "--inputs" = 'cooking_data_r',
                                      "--min_samples_leaf" = 10,
                                      "--n_estimators" = 100),
                 compute_target = compute_target
                 )

run <- azuremlsdk::submit_experiment(exp, est)

azuremlsdk::wait_for_run_completion(run, show_output = TRUE)

metrics <- azuremlsdk::get_run_metrics(run)
metrics

download_files_from_run(run, prefix="outputs/")
model <- readRDS("outputs/model.rds")
summary(model)

azuremlsdk::delete_compute(compute_target)



