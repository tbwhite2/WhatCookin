library(reticulate)
# library(azuremlsdk) 
# loading the package causes some backend reticulate stuff to be changed - don't do it on the
# R server
target_path = 'tmp_data'

use_condaenv('r-azureml', required = T)

ws = azuremlsdk::load_workspace_from_config()

ds = azuremlsdk::get_default_datastore(workspace = ws)

azuremlsdk::download_from_datastore(ds,target_path = target_path)
