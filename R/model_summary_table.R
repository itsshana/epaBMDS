model_summary_table <- function(bmds_output){

  data_type <- bmds_output$dataset$dtype
  model_names <- c()
  for(i in 1:length(bmds_output$models)){
    model_name <- bmds_output$models[[i]]$name
    model_names <- c(model_names, model_name)
  }
  bmds_table <- data.frame("Model" = model_names, stringsAsFactors = FALSE)
  # if(data_type == "C"){
  #   var_type <- bmds_output$models[[1]]$results$fit$dist
  #   if(var_type == 1){
  #     p_val_type <- "Test 2 (p-value)"
  #     bmds_table$p_val <- bmds_output$models[[1]]$results$tests$p_values[2]
  #   }
  #   if(var_type ==2){
  #     p_val_type <- "Test 3 (p-value)"
  #     bmds_table$p_val <- bmds_output$models[[1]]$results$tests$p_values[3]
  #   }
  #   names(bmds_table)[2] <- p_val_type
  # }
  for(i in 1:nrow(bmds_table)){
    bmds_table$BMDL[i] <- bmds_output$models[[i]]$results$bmdl
    bmds_table$BMD[i] <- bmds_output$models[[i]]$results$bmd
    bmds_table$BMDU[i] <- bmds_output$models[[i]]$results$bmdu
    bmds_table$p_value[i] <- bmds_output$models[[i]]$results$tests$p_values[4]
    bmds_table$AIC[i] <- bmds_output$models[[i]]$results$fit$aic
    bmds_table$SR_control[i] <- bmds_output$models[[i]]$results$gof$residual[1]
    bmds_table$SR_BMD[i] <- bmds_output$models[[i]]$results$gof$roi
    if(bmds_output$recommender$results$model_bin[i] == 0){
      if(i == bmds_output$recommender$results$recommended_model_index){
        bmds_table$classification[i] <- "Recommended"
      }
      else{
        bmds_table$classification[i] <- "Viable - Alternate"
      }

      # bmds_table$bmds_notes[i] <- bmds_output$recommender$results$model_notes[[i]][1]
    }
    # if(i == bmds_output$recommender$results$recommended_model_index & bmds_output$recommender$results$model_bin[i] == 0){
    #   bmds_table$classification[i] <- "Viable - Recommended"
    #   bmds_table$bmds_notes[i] <- c( bmds_output$recommender$results$recommended_model_variable,
    #                                  bmds_output$recommender$results$model_notes[[i]][1])
    # }
    if(bmds_output$recommender$results$model_bin[i] == 1){
      bmds_table$classification[i] <- "Questionable"
      # bmds_table$bmds_notes[i] <- c(bmds_output$recommender$results$model_notes[[i]][1],
      #                               bmds_output$recommender$results$model_notes[[i]][2])
    }
    if(bmds_output$recommender$results$model_bin[i] == 2){
      bmds_table$classification[i] <- "Unusable"
      # bmds_table$bmds_notes[i] <- c(bmds_output$recommender$results$model_notes[[i]][1],
      #                               bmds_output$recommender$results$model_notes[[i]][2],
      #                               bmds_output$recommender$results$model_notes[[i]][3])

    }
  }
  return(bmds_table)
}
