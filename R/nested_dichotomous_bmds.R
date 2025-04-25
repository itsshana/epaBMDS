nested_dichotomous_bmds <- function(doses, litter_ns, incidences, litter_covariates,
                                    recommend_best_model = TRUE,
                                    bmr = 0.1, bmr_type = 'Extra', alpha = 0.05){

  #flag non-existent python env
  py <- reticulate::py
  py$doses= doses
  py$litter_ns =litter_ns
  py$incidences=incidences
  py$litter_covariates = litter_covariates
  # if(bmr_type == "Extra"|bmr_type == "ER"|bmr_type == "extra"){
  #   pybmds_bmr_type <- "pybmds.DichotomousRiskType.ExtraRisk"
  # }
  # else if (bmr_type == "Added"|bmr_type == "AR"|bmr_type == "added"){
  #   pybmds_bmr_type <- "pybmds.DichotomousRiskType.AddedRisk"
  # }
  # else{
  #   warning("Please choose added or extra for bmr_type")
  # }
  # global_settings <- paste0("session.add_default_models(global_settings = dict(bmr = ", bmr, ", bmr_type = ",
  #                           pybmds_bmr_type, ", alpha = ", alpha, "))")
  reticulate::py_run_string("dataset = pybmds.NestedDichotomousDataset(
                            doses=doses, litter_ns=litter_ns, incidences=incidences,
                            litter_covariates=litter_covariates)")
  reticulate::py_run_string("session = pybmds.Session(dataset=dataset)")
  # reticulate::py_run_string(global_settings)

  reticulate::py_run_string("session.add_default_models()")
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




## Nested dichotomous
# Working with Andy to get "correct" object back from python

# nds <- data.frame(doses = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#                             25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
#                             50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
#                             100, 100, 100, 100, 100, 100, 100, 100, 100),
#                   litter_ns = c(16, 9, 15, 14, 13, 9, 10, 14, 10, 11,
#                                 14, 9, 14, 9, 13, 12, 10, 10, 11, 14,
#                                 11, 11, 14, 11, 10, 11, 10, 15, 7, 14,
#                                 11, 14, 12, 13, 12, 14, 11, 8, 10),
#                   incidences = c(1, 1, 2, 3, 3, 0, 2, 2, 1, 2,
#                                  4, 5, 6, 2, 6, 3, 1, 2, 4, 3,
#                                  4, 5, 5, 4, 5, 4, 5, 6, 2, 4,
#                                  6, 6, 8, 7, 8, 6, 6, 5, 4),
#                   litter_covariates = c(16, 9, 15, 14, 13, 9, 10, 14, 10, 11,
#                                         14, 9, 14, 9, 13, 12, 10, 10, 11, 14,
#                                         11, 11, 14, 11, 10, 11, 10, 15, 7, 14,
#                                         11, 14, 12, 13, 12, 14, 11, 8, 10))
#
# nd_bmds <- nested_dichotomous_bmds(doses = nds$doses,
#                                    litter_ns = nds$litter_ns,
#                                    incidences = nds$incidences,
#                                    litter_covariates = nds$litter_covariates)
#
# nd_bmds_table <- model_summary_table(nd_bmds)
