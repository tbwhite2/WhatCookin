library(reticulate)
# library(azuremlsdk) 
# loading the package causes some backend reticulate stuff to be changed - don't do it on the
# R server
use_condaenv('r-azureml', required = T)

ws = azuremlsdk::load_workspace_from_config()
# ws = azuremlsdk::get_workspace(name = 'MYWORKSPACENAME',
#                                subscription_id = 'MYSUBSCRIPTIONID',
#                                resource_group = 'MYRESOURCEGROUP')
# azuremlsdk::write_workspace_config(ws)