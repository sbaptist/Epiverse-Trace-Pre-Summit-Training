#Load packages
library(epidemics)
library(socialmixr)
library(tidyverse)

#Heret
gam_pop <- 2.5e6
gam_pop_grp <- gam_pop*c(.4, .35, .25)

gam_survey <- socialmixr::get_survey("https://doi.org/10.5281/zenodo.13101862")
gam_contact_data <- socialmixr::contact_matrix(
  survey = gam_survey,
  countries = "Gambia",
  age.limits = c(0,20,40)
)

#Contact matrix
gam_contact_data$participants
gam_contact_data$matrix

gam_contact_matrix <- t(gam_contact_data$matrix)#transport
names(gam_pop_grp) <- rownames(gam_contact_matrix)

#Initial conditions
init_cond1 <- c(S=1- 1/gam_pop,
                     E= 0,
                     I=0,
                     R=0,
                     v=0
)
init_cond2 <- c(S=1,
                     E=1,
                     I=1/1e6,
                     R=0,
                     v=.01
)

init_cond3 <- c(S=1,
                     E=0,
                     I=0,
                     R=0,
                     v=0
)
init_conditions_matrix <- rbind(
  init_cond1,#First group
  init_cond2,#Second group
  init_cond3 #Third group
)



gam_pop_grp_obj <- epidemics::population(
  name = "The Gambia",
  demography_vector = gam_pop_grp,
  initial_conditions = init_conditions_matrix,
  contact_matrix = gam_contact_matrix
)

# Create age-structured model
baseline_model <- epidemics::model_default(
  population = gam_pop_grp_obj,
  infectiousness_rate = 1/8,
  recovery_rate = 1/5,
  transmission_rate = 9/5,
  time_end = 120,
  increment = 1
)

#Build a model
baseline_model |> 
  ggplot(aes(
    x = time,
    y = value,
    colour = compartment,
    linetype = demography_group
  ))+
  geom_line(linewidth = 1.2)+
  theme_bw()+
  labs(
    x = "Time (in days)",
    y = "Cases",
    title = "Baseline model"
  )

#Create vaccine intervention
#matrix(c(rep(5)), nrow = 5, ncol = )
vaccine_rollout <- epidemics::vaccination(
  name  = "vaccine rollout",
  time_begin = matrix(c(25,25,25,25,25), nrow = 5, ncol = 1),
  time_end = matrix(25 + 50, nrow(gam_contact_matrix)),
  nu = matrix(c(.5, .1, .1, 0, 0))
)

#Vaccination model
vaccine_model <- epidemics::model_default(
  population = bf_pop_grp_obj,
  infectiousness_rate = 1/8,
  recovery_rate = 1/5,
  transmission_rate = 9/5,
  time_end = 120,
  increment = 1,
  vaccination = vaccine_rollout
)

vaccine_model |> 
  ggplot(aes(
    x = time,
    y = value,
    colour = compartment,
    linetype = demography_group
  ))+
  geom_line(linewidth = 1.2)+
  theme_bw()+
  labs(
    x = "Time (in days)",
    y = "Number of cases",
    title = "Vaccination Model"
  )
#Calculate new infections from baseline
baseline_data <-epidemics::new_infections(
  baseline_model, by_group = F)
baseline_data$scenario <- "Baseline"
view(baseline_data)


vaccine_data <-epidemics::new_infections(
  vaccine_model, by_group = F,
  compartments_from_susceptible = "vaccinated"
)
vaccine_data$scenario <- "Vaccine"

baseline_vaccine_combined <- rbind(baseline_data,
                                   vaccine_data)

baseline_vaccine_combined |> 
  ggplot(aes(x= time,
             y = new_infections,
             col = scenario))+
  geom_line()
