


config_pvenv <- function(venv_location, print_version = TRUE){
  reticulate::use_virtualenv(venv_location)
  reticulate::py_run_string('import pybmds')
  if(print_version){
      reticulate::py_run_string('print(pybmds.__version__)')
  }
}

