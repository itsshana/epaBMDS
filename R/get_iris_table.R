get_iris_table <- function(bmds_output){

  data_type <- bmds_output$dataset$dtype
  model_names <- c()
  for(i in 1:length(bmds_output$models)){
    model_name <- bmds_output$models[[i]]$name
    model_names <- c(model_names, model_name)
  }
  iris_table <- data.frame("Model" = model_names, stringsAsFactors = FALSE)
  if(data_type == "C"){
    var_type <- bmds_output$models[[1]]$results$fit$dist
    if(var_type == 1){
      p_val_type <- "Test 2 (p-value)"
      iris_table$p_val <- bmds_output$models[[1]]$results$tests$p_values[2]
    }
    if(var_type ==2){
      p_val_type <- "Test 3 (p-value)"
      iris_table$p_val <- bmds_output$models[[1]]$results$tests$p_values[3]
    }
    names(iris_table)[2] <- p_val_type
  }
  for(i in 1:nrow(iris_table)){
    iris_table$BMD[i] <- bmds_output$models[[i]]$results$bmd
    iris_table$BMDL[i] <- bmds_output$models[[i]]$results$bmdl
    iris_table$gof_pval[i] <- bmds_output$models[[i]]$results$tests$p_values[4]
    iris_table$AIC[i] <- bmds_output$models[[i]]$results$fit$aic
    if(bmds_output$recommender$results$model_bin[i] == 0){
      iris_table$classification[i] <- "Viable - Alternate"
      iris_table$bmds_notes[i] <- bmds_output$recommender$results$model_notes[[i]][1]
    }
    # if(i == bmds_output$recommender$results$recommended_model_index & bmds_output$recommender$results$model_bin[i] == 0){
    #   iris_table$classification[i] <- "Viable - Recommended"
    #   iris_table$bmds_notes[i] <- c( bmds_output$recommender$results$recommended_model_variable,
    #                                  bmds_output$recommender$results$model_notes[[i]][1])
    # }
    if(bmds_output$recommender$results$model_bin[i] == 1){
      iris_table$classification[i] <- "Questionable"
      iris_table$bmds_notes[i] <- c(bmds_output$recommender$results$model_notes[[i]][1],
                                    bmds_output$recommender$results$model_notes[[i]][2])
    }
    if(bmds_output$recommender$results$model_bin[i] == 2){
      iris_table$classification[i] <- "Unusable"
      iris_table$bmds_notes[i] <- c(bmds_output$recommender$results$model_notes[[i]][1],
                                    bmds_output$recommender$results$model_notes[[i]][2],
                                    bmds_output$recommender$results$model_notes[[i]][3])

    }
  }
  return(iris_table)
}
