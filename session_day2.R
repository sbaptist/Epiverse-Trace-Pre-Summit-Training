#Load packages
library(epidemics)
library(socialmixr)
library(tidyverse)

#Definition SEIR of initial values:
#1. Population = 2.5e6
#2. susceptible(s) = 0, 
#3. Exposed(E)=0, 
#4. Recover(R)= 0, 
#5. Vaccination(V)=0
#6. Infection=1/bf_pop

bf_pop <- 23.5e6
bf_pop_obj <- epidemics::population(
  name = "Burkina Faso",
  demography_vector = bf_pop,
  contact_matrix = matrix(1.0),
  initial_conditions = matrix(c(1-(1/bf_pop), 0, 1/bf_pop, 0, 0),
  nrow = 1, ncol = 5)
)

#Define the model
simple_model <- epidemics::model_default(
  population = bf_pop_obj, #pop of interest
  recovery_rate = 1/5, #alpha
  transmission_rate = 9/5,#beta
  infectiousness_rate = 1/8, #alpha
  time_end = 120,#length of running the model
  increment = 1 #sequence 
)


#Graph the model
library(plotly)
p <- simple_model |> 
  ggplot(aes(x = time,
             y = value, 
             colour = compartment))+
  geom_line()

ggplotly(p)


#Heret
bf_pop_grp <- bf_pop*c(.44, 0.195, 0.29,.05,.025)

bf_survey <- socialmixr::get_survey("https://doi.org/10.5281/zenodo.13101862")
bf_contact_data <- socialmixr::contact_matrix(
  survey = bf_survey,
  countries = "Gambia",
  age.limits = c(0,15, 25, 55, 65)
  )

#Contact matrix
bf_contact_data$participants
bf_contact_data$matrix

bf_contact_matrix <- t(bf_contact_data$matrix)#transport
names(bf_pop_grp) <- rownames(bf_contact_matrix)

#Initial conditions
init_conditions <- c(S=1- 1/1e6,
                     E=0,
                     I=1/1e6,
                     R=0,
                     v=0
  )
init_conditions_matrix <- rbind(
  init_conditions,#First group
  init_conditions,#Second group
  init_conditions,#Third group
  init_conditions,#Fourth group
  init_conditions #Fifth group
)



bf_pop_grp_obj <- epidemics::population(
  name = "B Faso",
  demography_vector = bf_pop_grp,
  initial_conditions = init_conditions_matrix,
  contact_matrix = bf_contact_matrix
)

# Create age-structured model
baseline_model <- epidemics::model_default(
  population = bf_pop_grp_obj,
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
  time_end = matrix(25 + 50, nrow(bf_contact_matrix)),
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

