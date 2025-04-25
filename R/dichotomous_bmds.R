
#no multistage 1 - same as linear

dichotomous_bmds <- function(dose, n, incidence, recommend_best_model = TRUE,
                             bmr = 0.1, bmr_type = 'Extra', alpha = 0.05,
                             dataset_name = "Dichotomous Dataset"){

  #flag non-existent python env
  py <- reticulate::py
  py$doses=dose
  py$ns=n
  py$incidences=incidence
  py$name = dataset_name
  if(bmr_type == "Extra"|bmr_type == "ER"|bmr_type == "extra"){
    pybmds_bmr_type <- "pybmds.DichotomousRiskType.ExtraRisk"
  }
  else if (bmr_type == "Added"|bmr_type == "AR"|bmr_type == "added"){
    pybmds_bmr_type <- "pybmds.DichotomousRiskType.AddedRisk"
  }
  else{
    warning("Please choose added or extra for bmr_type")
  }
  global_settings <- paste0("session.add_default_models(global_settings = dict(bmr = ", bmr, ", bmr_type = ",
                            pybmds_bmr_type, ", alpha = ", alpha, "))")
  reticulate::py_run_string("dataset = pybmds.DichotomousDataset(name = name, doses=doses, ns=ns, incidences=incidences)")
  reticulate::py_run_string("session = pybmds.Session(dataset=dataset)")
  reticulate::py_run_string(global_settings)
  reticulate::py_run_string("session.execute()")
  reticulate::py_run_string("session.recommend()")
  reticulate::py_run_string("results=session.to_dict()")
  results = py$results
  results$recommender$results$recommended_model_index <- results$recommender$results$recommended_model_index + 1
  if(is.null(results$recommender$results$recommended_model_index)){
    warning("No viable models")
  }
  return(results)
}
