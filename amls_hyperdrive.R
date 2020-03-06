library(reticulate)
# library(azuremlsdk) 
# loading the package causes some backend reticulate stuff to be changed - don't do it on the
# R server
script_folder = 'amls_run_scripts'
experiment_name = "r_sdk"
cluster_name = "cpucluster"
entry_script = 'train.R'
use_condaenv('r-azureml', required = T)

ws = azuremlsdk::load_workspace_from_config()

exp = azuremlsdk::experiment(ws, experiment_name)

compute_target = azuremlsdk::get_compute(ws, cluster_name = cluster_name)
if (is.null(compute_target)) {
  vm_size = "STANDARD_D2_V2" 
  compute_target = azuremlsdk::create_aml_compute(workspace = ws,
                                                   cluster_name = cluster_name,
                                                   vm_size = vm_size,
                                                   max_nodes = 2)
  
  azuremlsdk::wait_for_provisioning_completion(compute_target, show_output = TRUE)
}

ds = azuremlsdk::get_default_datastore(workspace = ws)

est = azuremlsdk::estimator(source_directory = script_folder,
                             entry_script = entry_script,
                             script_params = list("--data_folder" = ds$as_mount(),
                                                  "--inputs" = 'cooking_data_r'),
                             compute_target = compute_target)

param_sampling = azuremlsdk::random_parameter_sampling(list("--min_samples_leaf" = azuremlsdk::uniform(1, 15),
                                            "--n_estimators" = azuremlsdk::choice(c(500, 1000, 2000)),
                                            "--wts_method" = azuremlsdk::choice(c('no_wts', 'inv_freq','inv_log_freq'))
                                            ))

early_termination_policy = azuremlsdk::median_stopping_policy(evaluation_interval=1L,
                                                              delay_evaluation=5L)

hyperdrive_run_config = azuremlsdk::hyperdrive_config(hyperparameter_sampling = param_sampling, 
                              primary_metric_name = 'Balanced Accuracy',
                              primary_metric_goal = azuremlsdk::primary_metric_goal('MAXIMIZE'),
                              max_total_runs = 50,
                              max_concurrent_runs = 3,
                              estimator = est,
                              policy = early_termination_policy)

h_run = azuremlsdk::submit_experiment(experiment = exp, hyperdrive_run_config)

best_run = azuremlsdk::get_best_run_by_primary_metric(h_run)

