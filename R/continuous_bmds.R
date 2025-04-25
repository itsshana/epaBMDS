

#Summary data
continuous_bmds <- function(dose, n, mean, stdev, recommend_best_model = TRUE,
                            distribution = "normal", variance = "constant",
                            bmr = 1, bmr_type = 'SD', alpha = 0.05){

  #Initiate python's py object
  py <- reticulate::py

  #Add data to py
  py$doses=dose
  py$ns=n
  py$means=mean
  py$stdevs=stdev

  #Define BMR type
  if(bmr_type == "Standard"|bmr_type == "SD"|bmr_type == "sd"){
    pybmds_bmr_type <- "pybmds.ContinuousRiskType.StandardDeviation"
  }
  else if (bmr_type == "Relative"|bmr_type == "RD"|bmr_type == "rd"){
    pybmds_bmr_type <- "pybmds.ContinuousRiskType.RelativeDeviation"
  }
  else if (bmr_type == "Absolute"|bmr_type == "AD"|bmr_type == "ad"){
    pybmds_bmr_type <- "pybmds.ContinuousRiskType.AbsoluteDeviation"
  }
  else{
    warning("Please choose 'standard', 'relative' or 'absolute [deviation] for bmr_type")
  }

  #Assign distribution
  #Need to check if this is correct

  #Work in if lognormal available
  # dist_type <- ifelse(distribution == "Lognormal" |distribution == "log"|distribution == "L",
  #                     "lognormal", "normal")
  dist_type <- "normal"

  #not sure if working correctly
  var_type <- ifelse(variance == "nonconstant" |variance == "Nonconstant"|variance == "NC"|variance == "nc",
                      "_ncv", "")

  disttype <- paste0("pybmds.ContinuousDistType.",dist_type, var_type)

  #Bundle settings into one variable
  global_settings <- paste0("session.add_default_models(global_settings = dict(bmr = ", bmr, ", bmr_type = ",
                            pybmds_bmr_type, ", alpha = ", alpha, ", disttype = ", disttype,"))")

  #Run BMDS with python (pybmds)

  #Build data object
  #reticulate::py_run_string("dataset = bmds.ContinuousDataset(doses=doses, ns=ns, means = means, stdevs=stdevs)")
  reticulate::py_run_string("dataset = pybmds.ContinuousDataset(doses=doses, ns=ns, means=means, stdevs=stdevs)")
  #Start bmds session
  #reticulate::py_run_string("session = bmds.BMDS.latest_version(dataset=dataset)")
  reticulate::py_run_string("session = pybmds.Session(dataset=dataset)")
  #Assign global settings
  reticulate::py_run_string(global_settings)
  #Conduct DR and BMD modeling
  reticulate::py_run_string("session.execute()")
  #Recommend best model (default for now)
  if(recommend_best_model){
    reticulate::py_run_string("session.recommend()")

  }
  #Record results in python
  reticulate::py_run_string("results=session.to_dict()")
  #Export results to R
  results = py$results
  results$recommender$results$recommended_model_index <- results$recommender$results$recommended_model_index + 1

  if(is.null(results$recommender$results$recommended_model_index)){
    warning("No viable models")
  }
  cat(paste0("Test 1 (Dose Response): ", ifelse(results$models[[1]]$results$tests$p_values[1] < 0.0001,
                                                "< 0.0001",
                                                round(results$models[[1]]$results$tests$p_values[1],4))),"\n")
  cat(paste0("Test 2 (Homogeneity of variance): ", ifelse(results$models[[1]]$results$tests$p_values[2] < 0.0001,
                                                          "< 0.0001",
                                                          format(round(results$models[[1]]$results$tests$p_values[2],4), scientific = FALSE))),"\n")
  cat(paste0("Test 3 (Variance Model Selection): ", ifelse(results$models[[1]]$results$tests$p_values[3] < 0.0001,
                                                           "< 0.0001",
                                                           format(round(results$models[[1]]$results$tests$p_values[3],4), scientific = FALSE))))
  return(results)
}

#Individual data
continuous_bmds_indiv_data <- function(doses, responses, recommend_best_model = TRUE,
                            distribution = "normal", variance = "constant",
                            bmr = 1, bmr_type = 'SD', alpha = 0.05){

  #Initiate python's py object
  py <- reticulate::py

  #Add data to py
  py$doses=doses
  py$responses=responses

  #Define BMR type
  if(bmr_type == "Standard"|bmr_type == "SD"|bmr_type == "sd"){
    pybmds_bmr_type <- "pybmds.ContinuousRiskType.StandardDeviation"
  }
  else if (bmr_type == "Relative"|bmr_type == "RD"|bmr_type == "rd"){
    pybmds_bmr_type <- "pybmds.ContinuousRiskType.RelativeDeviation"
  }
  else if (bmr_type == "Absolute"|bmr_type == "AD"|bmr_type == "ad"){
    pybmds_bmr_type <- "pybmds.ContinuousRiskType.AbsoluteDeviation"
  }
  else{
    warning("Please choose 'standard', 'relative' or 'absolute [deviation] for bmr_type")
  }

  #Assign distribution
  #Need to check if this is correct

  #Work in if lognormal available
  # dist_type <- ifelse(distribution == "Lognormal" |distribution == "log"|distribution == "L",
  #                     "lognormal", "normal")
  dist_type <- "normal"

  #not sure if working correctly
  var_type <- ifelse(variance == "nonconstant" |variance == "Nonconstant"|variance == "NC"|variance == "nc",
                     "_ncv", "")

  disttype <- paste0("pybmds.ContinuousDistType.",dist_type, var_type)

  #Bundle settings into one variable
  global_settings <- paste0("session.add_default_models(global_settings = dict(bmr = ", bmr, ", bmr_type = ",
                            pybmds_bmr_type, ", alpha = ", alpha, ", disttype = ", disttype,"))")



  #Run BMDS with python (pybmds)

  #Build data object
  #reticulate::py_run_string("dataset = bmds.ContinuousDataset(doses=doses, ns=ns, means = means, stdevs=stdevs)")
  reticulate::py_run_string("dataset = pybmds.ContinuousIndividualDataset(doses=doses, responses=responses)")
  #Start bmds session
  #reticulate::py_run_string("session = bmds.BMDS.latest_version(dataset=dataset)")
  reticulate::py_run_string("session = pybmds.Session(dataset=dataset)")
  #Assign global settings
  reticulate::py_run_string(global_settings)
  #Conduct DR and BMD modeling
  reticulate::py_run_string("session.execute()")
  #Recommend best model (default for now)
  if(recommend_best_model){
    reticulate::py_run_string("session.recommend()")

  }
  #Record results in python
  reticulate::py_run_string("results=session.to_dict()")
  #Export results to R
  results = py$results
  results$recommender$results$recommended_model_index <- results$recommender$results$recommended_model_index + 1

  if(is.null(results$recommender$results$recommended_model_index)){
    warning("No viable models")
  }
  cat(paste0("Test 1 (Dose Response): ", ifelse(results$models[[1]]$results$tests$p_values[1] < 0.0001,
                                                "< 0.0001",
                                                round(results$models[[1]]$results$tests$p_values[1],4))),"\n")
  cat(paste0("Test 2 (Homogeneity of variance): ", ifelse(results$models[[1]]$results$tests$p_values[2] < 0.0001,
                                                          "< 0.0001",
                                                          format(round(results$models[[1]]$results$tests$p_values[2],4), scientific = FALSE))),"\n")
  cat(paste0("Test 3 (Variance Model Selection): ", ifelse(results$models[[1]]$results$tests$p_values[3] < 0.0001,
                                                           "< 0.0001",
                                                           format(round(results$models[[1]]$results$tests$p_values[3],4), scientific = FALSE))))

  # cat(paste0("Test 1 (Dose Response): ", ifelse(results$models[[1]]$results$tests$p_values[1] < 0.0001,
  #                                                 "< 0.0001",
  #                                                 round(results$models[[1]]$results$tests$p_values[1],4))))
  # cat(paste0("Test 2 (Homogeneity of variance): ", ifelse(results$models[[1]]$results$tests$p_values[2] < 0.0001,
  #                                                           "< 0.0001",
  #                                                           format(round(results$models[[1]]$results$tests$p_values[2],4), scientific = FALSE))))
  # cat(paste0("Test 3 (Variance Model Selection): ", ifelse(results$models[[1]]$results$tests$p_values[3] < 0.0001,
  #                                                            "< 0.0001",
  #                                                            format(round(results$models[[1]]$results$tests$p_values[3],4), scientific = FALSE))))

  return(results)
}


