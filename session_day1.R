#Load the required package
library(tidyverse)

#Data importation
cases <- read_csv(here::here("data", "linelist.csv"))

#A view of the dataset
cases

#Count the number of outcomes
cases |>                           #Ctrl + Shift + M
  dplyr::count(outcome) |>         #Count from dplyr pkg
  tidyr::pivot_wider(names_from = outcome, values_from = n) |> 
  cleanepi::standardize_column_names() |>   #colnames to lower
  dplyr::mutate(cfr_naive = death/(death + recover)) #Create "cfr_naive" column

cases %>% 
  dplyr::select(
    case_id,
    date_of_hospitalisation,
    date_of_onset) |> 
  dplyr::mutate(reporting_delay = date_of_hospitalisation - date_of_onset ) |>
  ggplot(aes(x = reporting_delay)) +
  geom_histogram(binwidth = 1
                 )
