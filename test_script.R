
## This script is intended to showcase the "base" functions for the package
## and the examples reproduce the analyses for bmds online
## Ideal scenario: each assessment is a project, each endpoint gets a script
## Moving forward: create vignettes for usecases, discuss other features or
## scenarios (ie nested data, individual data, trend tests, model averaging)

## Note - I foresee the linkup of python to R as a potential hurdle but there
## are ways to circumvent this issue for demonstrative purposes



###The function below is a placeholder - need to have location as input
##warning message pops up
config_pvenv()

#not sure why this call needs to be made atm
library(ggplot2)

#Ideal workflow for assessments would be to download DR data AND experiment/
#endpoint metadata directly from HAWC; auto-convert SE to SD.
#When available, extract NOAEL/LOAEL


### Step 1: Define dichotomous DR dataset with three inputs:
## 1) dose = positive number; amount of chemical administered to each group
## 2) n = number of experimental subjects in each group
## 3) incidence = number of affected experimental subjects in each group


dds <- data.frame(dose = c(0, 10, 50, 150, 400), n = c(25, 25, 24, 24, 24),
                  incidence = c(0, 3, 7, 11, 15))


dds_bmds <- dichotomous_bmds(dose = dds$dose, n = dds$n, incidence = dds$incidence)

plot_bmds(dds_bmds)
plot_bmds(dds_bmds, model = 1)



cds <- data.frame(dose = c(0, 10, 50, 75, 100), n = c(20, 20, 20, 20, 20),
                  mean = c(6, 8, 13, 25, 30), stdev = c(4, 4.3, 3.8, 4.4, 3.7))

cds_bmds <- continuous_bmds(dose = cds$dose, n = cds$n, mean = cds$mean, stdev = cds$stdev)
plot_bmds(cds_bmds, model = 1)
plot_bmds(cds_bmds)

cds_bmds_ncv <- continuous_bmds(dose = cds$dose, n = cds$n, mean = cds$mean,
                                stdev = cds$stdev, variance = "NC")
plot_bmds(cds_bmds, model = 1)
plot_bmds(cds_bmds)

# Ends of errorbars do not show in continuous case?

#get summary tables
#need to fix notes

dds_iris <- get_iris_table(dds_bmds)
cds_iris <- get_iris_table(cds_bmds)
cds_ncv_iris <- get_iris_table(cds_bmds_ncv)

library(clipr)
my_data <- read_clip_tbl()
tab_nams <- names(my_data)
my_data <- read_clip_tbl()
names(my_data) <- tab_nams

test_bmds <- continuous_bmds(dose = my_data$Dose..mg.kg.d., n = my_data$n,
                             mean = my_data$Mean..g., stdev = my_data$SD, variance = "NC")
test_iris <- get_iris_table(test_bmds)

