### IR review of BMDS Online
library(ggplot2)

#### Recreate online examples

config_pvenv("C:/Users/SWHITE01/dev/bmds/venv")

### Dichotomous Example

## Data
#dds => dichotomous data set
dds <- data.frame(dose = c(0, 50, 100, 200, 400),
                  n = c(20, 20, 20, 20, 20),
                  incidence = c(0, 1, 2, 10, 19))

# Default option set (bmr = 0.1, type = Extra, conf. = 0.95)
# d_bmds => dichotomous bmd session
d_bmds <- dichotomous_bmds(dose = dds$dose,
                           n = dds$n,
                           incidence = dds$incidence)

d_bmds_table <- model_summary_table(d_bmds)
plot_bmds(d_bmds)

# Different option set
d_bmds_diff <- dichotomous_bmds(dose = dds$dose,
                                n = dds$n,
                                incidence = dds$incidence,
                                bmr = 0.05,
                                bmr_type = "Added",
                                alpha = 0.1)

d_bmds_table_diff <- model_summary_table(d_bmds_diff)
plot_bmds(d_bmds_diff,dataset_name = "Different Option Set")

## Continuous Example
#cds => continuous data set (summary)
cds <- data.frame(dose = c(0, 50, 100, 200, 400),
                  n = c(20, 20, 20, 20, 20),
                  mean = c(5.26, 5.76, 6.13, 8.24, 9.23),
                  stdev = c(2.23, 1.47, 2.47, 2.24, 1.56))

# Default option set (bmr = 1, type = SD, conf. = 0.95, variance = constant)
# c_bmds => continuous bmd session
c_bmds <- continuous_bmds(dose = cds$dose,
                          n = cds$n,
                          mean = cds$mean,
                          stdev = cds$stdev)

c_bmds_table <- model_summary_table(c_bmds)
plot_bmds(c_bmds)

c_bmds_diff <- continuous_bmds(dose = cds$dose,
                               n = cds$n,
                               mean = cds$mean,
                               stdev = cds$stdev,
                               bmr = 0.5,
                               bmr_type = "rd",
                               variance = "nc")

c_bmds_table_diff <- model_summary_table(c_bmds_diff)
plot_bmds(c_bmds_diff, dataset_name = "Different Option Set (Continuous)")

#cds_indiv => continuous data set (individual)
cds_indiv <- data.frame(doses =
                        c(0,0,0,0,0,0,0,0,0.1,0.1,0.1,0.1,0.1,0.1,1,1,
                        1,1,1,1,10,10,10,10,10,10,100,100,100,100,100,100,
                        300,300,300,300,300,300,500,500,500,500,500,500),
                        responses =
                        c(8.1079,9.3063,9.7431,9.7814,10.0517,10.6132,10.7509,
                        11.0567,9.1556,9.6821,9.8256,10.2095,10.2222,12.0382,
                        9.5661,9.7059,9.9905,10.2716,10.471,11.0602,8.8514,
                        10.0107,10.0854,10.5683,11.1394,11.4875,9.5427,9.7211,
                        9.8267,10.0231,10.1833,10.8685,11.368,13.5176,12.3168,
                        14.002,17.1186,13.6368,19.9572,20.1347,16.7743,20.0571,
                        15.1564,15.0368))

c_bmds_indiv <- continuous_bmds_indiv_data(doses = cds_indiv$doses,
                                           responses = cds_indiv$responses)
c_bmds_table_indiv <- model_summary_table(c_bmds_indiv)

c_bmds_indiv_ncv <- continuous_bmds_indiv_data(doses = cds_indiv$doses,
                                               responses = cds_indiv$responses,
                                               variance = "nc")
c_bmds_table_indiv_ncv <- model_summary_table(c_bmds_indiv_ncv)

plot_bmds(c_bmds_indiv_ncv)


###############################################################################
####TREND TESTS#####


## Dichotomous data (Code adapted from Todd B)

#Cochran-Armitage
library(CATTexact)
catt_exact(rank(dds$dose), dds$n, dds$incidence)


## Continuous individual data (Code adapted from David F)

#Jonkheree
library(clinfun)  # jonckheere

jonckheere.test(cds_indiv$responses, rank(cds_indiv$doses))

library(multcomp) # williams / marcus (modified Williams)
#Create anova object
cds_indiv_cat <- transform(cds_indiv, catdoses = as.factor(doses))
cds_indiv_anv <- aov(responses ~ catdoses, data = cds_indiv_cat)

#Anova
print(summary(cds_indiv_anv))

#Dunnett
m.dunnett <- glht(cds_indiv_anv, linfct=mcp(catdoses = "Dunnett"))
print(summary(m.dunnett))

#Williams
m.willi <- glht(cds_indiv_anv, linfct=mcp(catdoses = "Williams"))
print(summary(m.willi))

#Marcus
m.marcus <- glht(cds_indiv_anv, linfct=mcp(catdoses = "Marcus"))
print(summary(m.marcus))



