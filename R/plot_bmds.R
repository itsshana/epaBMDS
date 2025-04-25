#library(ggplot2)


plot_bmds <- function(bmds_output, percentage = TRUE, log = FALSE,
                            dataset_name = "default",
                            dose_units = "mg/kg-day",
                            model = "Best",
                            run_notes = "All Dose Groups"){
  dtype <- bmds_output$dataset$dtype
  if(dtype == "D"){
    bmd_dataset <- data.frame(dose = bmds_output$dataset$doses,
                              mean = bmds_output$dataset$incidences/bmds_output$dataset$ns)
    y_label <- "% Incidence"
    bmd_dataset$ll <- bmd_dataset$mean - bmds_output$dataset$plotting$ll
    bmd_dataset$ul <- bmd_dataset$mean + bmds_output$dataset$plotting$ul

    if (dataset_name == "default"){
      dataset_name <- "Dichotomous Data"
    }

  }
  else if(dtype == "CI"){
    bmd_dataset <- data.frame(dose = bmds_output$dataset$doses,
                              mean = bmds_output$dataset$responses)
    y_label <- "Response"

    if (dataset_name == "default"){
      dataset_name <- "Continuous Individual Data"
    }
    bmd_dataset$ll <- 0
    bmd_dataset$ul <- 100
  }
  else if(dtype == "C"){
    bmd_dataset <- data.frame(dose = bmds_output$dataset$doses,
                              n = bmds_output$dataset$ns,
                              mean = bmds_output$dataset$means,
                              stdev = bmds_output$dataset$stdevs)
    y_label <- "Response"
    bmd_dataset$ll <- bmds_output$dataset$plotting$ll
    bmd_dataset$ul <- bmds_output$dataset$plotting$ul
    if (dataset_name == "default"){
      dataset_name <- "Continuous Data"
    }
    #var_type
  }
  # else {
  #   errorCondition("Please enter a valid BMDS object")
  # }


  if (model == "Best"){
    MI <- bmds_output$recommender$results$recommended_model_index
  }
  else {
    # mod <- get(paste0("bmds_output$models[[", model, "]]"))
    MI <- model
  }


  model_name <- bmds_output$models[[MI]]$name


  line_data <- data.frame(dr_x = bmds_output$models[[MI]]$results$plotting$dr_x,
                          dr_y = bmds_output$models[[MI]]$results$plotting$dr_y)
  bmd_data <- data.frame(bmd = bmds_output$models[[MI]]$results$bmd,
                         bmdl = bmds_output$models[[MI]]$results$bmdl,
                         bmdu = bmds_output$models[[MI]]$results$bmdu,
                         BMR = bmds_output$models[[MI]]$results$plotting$bmd_y)
  x_label <- paste0("Dose (", dose_units, ")")

  if(dtype == "CI"){
    ggplot() +
      geom_point(data = bmd_dataset, aes(y = mean, x = dose))+

      labs(title = paste0(dataset_name,": ", model_name, " Model"), y = y_label, x = x_label)+

      geom_line(data = line_data, aes(x = dr_x,y = dr_y), color = "blue") +

      geom_point(data = bmd_data, aes(y = BMR, x = bmd), color = "red", pch = 18, size = 6) +

      geom_errorbarh(data = bmd_data, aes(xmin = bmdl, xmax = bmdu, y = BMR),
                   height =.010, colour = "red", linewidth = 1.5)
  }

  else if (dtype == "C" | dtype == "D"){
    ggplot() +
      geom_point(data = bmd_dataset, aes(y = mean, x = dose))+

      labs(title = paste0(dataset_name,": ", model_name, " Model"), y = y_label, x = x_label)+

      geom_line(data = line_data, aes(x = dr_x,y = dr_y), color = "blue") +


      geom_errorbar(data = bmd_dataset, aes(ymin = ll, ymax = ul, x = dose)) +


      geom_errorbarh(data = bmd_data, aes(xmin = bmdl, xmax = bmdu, y = BMR),
                     height =.010, colour = "red", linewidth = 1.5) +


      geom_point(data = bmd_data, aes(y = BMR, x = bmd), color = "red", pch = 18, size = 6)
  }

}

