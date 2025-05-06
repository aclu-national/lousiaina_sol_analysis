# --------------------------------- Loading Libraries and Data -------------------------------

# Importing Libraries
library(gsheet)
library(janitor)
library(lubridate)
library(tidyverse)
library(tidycensus)

# --------------------------------- Data Cleaning -------------------------------

# Set the Census API key with an option to overwrite the existing key and install necessary packages.
census_api_key("KEY", overwrite = TRUE, install = TRUE)

# Load variable definitions for the year 2020 and geography "pl" (presumably "place").
vars_2020 <- load_variables(2020, "pl")

# Filter the variables related to the concept of "RACE" from the loaded variables.
race_vars <- vars_2020 %>%
  filter(concept == "RACE")

# Extract the names of race-related variables.
v <- race_vars$name

# Fetch decennial census data for the year 2020, focusing on the state geography and the selected race-related variables.
census_data <- get_decennial(geography = "state", variables = v, year = "2020", sumfile = "pl") %>%
  clean_names() %>%
  rename(state = name,
         name = variable) %>%
  left_join(race_vars, by = "name") %>%
  select(state, value, label) %>%
  pivot_wider(names_from = "label", values_from = "value", values_fn = sum) %>%
  clean_names() %>%
  
  # Calculate a new column "any_part_black" by summing across columns containing "black" in their names.
  mutate(any_part_black = rowSums(across(contains("black")))) %>%
  select(state, total, white_alone = total_population_of_one_race_white_alone, any_part_black)

# Assuming that "sheet_url" is defined elsewhere, read data from the specified Google Sheets URL.
court_data <- read_csv(here::here("sol_data.csv")) %>%
  clean_names() %>%
  
  # Filter out specific states from the data.
  filter(!(state %in% c("Northern Mariana Islands", 
                        "Guam", 
                        "Puerto Rico",
                        "Virgin Islands"))) %>%
  group_by(state) %>%
  
  # Left join the census_data with the current data based on the "state" column.
  left_join(census_data, by = "state")

state_data <- data %>%
  
  # Grouping by staate
  group_by(state) %>%
  
  # Converting totals we do not want summed to characters
  mutate(total = as.character(total),
         white_alone = as.character(white_alone),
         any_part_black = as.character(any_part_black)) %>%
  
  # Summing across all numerical columns
  summarise(across(where(is.numeric), sum),
            
            # Converting characters back to integers
            total = as.integer(unique(total)),
            white_alone = as.integer(unique(white_alone)),
            any_part_black = as.integer(unique(any_part_black)),
            statute_of_limitations = as.integer(unique(statute_of_limitations))) %>%
  mutate(n_courts = n())

# --------------------------------- Analysis Data Creation -------------------------------

# Function to create necessary datasets for analysis
create_year_dataset <- function(data, years) {
  result <- data %>%
    filter(statute_of_limitations %in% years) %>%
    mutate(statute_of_limitations = ifelse(statute_of_limitations == 1,
                                           paste0(statute_of_limitations, " Year"),
                                           paste0(statute_of_limitations, " Years"))
    )
  return(result)
}

# 1 vs. 2
court_data_1_2 <- create_year_dataset(court_data, c(1, 2))
state_data_1_2 <- create_year_dataset(state_data, c(1, 2))

# 1 vs. 3
court_data_1_3 <- create_year_dataset(court_data, c(1, 3))
state_data_1_3 <- create_year_dataset(state_data, c(1, 3))

# 1 vs. 4
court_data_1_4 <- create_year_dataset(court_data, c(1, 4))
state_data_1_4 <- create_year_dataset(state_data, c(1, 4))

# 1 vs. 5
court_data_1_5 <- create_year_dataset(court_data, c(1, 5))
state_data_1_5 <- create_year_dataset(state_data, c(1, 5))

# 1 vs. 6
court_data_1_6 <- create_year_dataset(court_data, c(1, 6))
state_data_1_6 <- create_year_dataset(state_data, c(1, 6))

# 1 vs. 2+
court_data_1_2_plus <- create_year_dataset(court_data, 1:6)
state_data_1_2_plus <- create_year_dataset(state_data, 1:6)


# --------------------------------- Statistics Data Creation -------------------------------

# Filtering data into 1 year SOL
court_1_year <- court_data %>% filter(statute_of_limitations == 1)
state_1_year <- state_data %>% filter(statute_of_limitations == 1)

# Filtering data into 2 year SOL
court_2_year <- court_data %>% filter(statute_of_limitations == 2)
state_2_year <- state_data %>% filter(statute_of_limitations == 2)

# Filtering data into 3 year SOL
court_3_year <- court_data %>% filter(statute_of_limitations == 3)
state_3_year <- state_data %>% filter(statute_of_limitations == 3)

# Filtering data into 4 year SOL
court_4_year <- court_data %>% filter(statute_of_limitations == 4)
state_4_year <- state_data %>% filter(statute_of_limitations == 4)

# Filtering data into 5 year SOL
court_5_year <- court_data %>% filter(statute_of_limitations == 5)
state_5_year <- state_data %>% filter(statute_of_limitations == 5)

# Filtering data into 6 year SOL
court_6_year <- court_data %>% filter(statute_of_limitations == 6)
state_6_year <- state_data %>% filter(statute_of_limitations == 6)

# Filtering data into 2+ year SOL
court_2_year_plus <- court_data %>% filter(statute_of_limitations >= 2)
state_2_year_plus <- state_data %>% filter(statute_of_limitations >= 2)

