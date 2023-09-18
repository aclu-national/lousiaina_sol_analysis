# --------------------------------- Loading Libraries and Data -------------------------------

# Importing Libraries
library(gsheet)
library(janitor)
library(lubridate)
library(tidyverse)
library(googlesheets4)
library(tidycensus)

# Defining the data's URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1T0w-t-ia70W9FADavcGceQPpG4nMujpUWHD-4vBbzak/edit#gid=0"

# --------------------------------- Data Creation and Cleaning -------------------------------

# Set the Census API key with an option to overwrite the existing key and install necessary packages.
census_api_key("143ad25712071130fe552ba928d16e4df70e1f39", overwrite = TRUE, install = TRUE)

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
data <- read_sheet(sheet_url) %>%
  clean_names() %>%
  # Filter out specific states from the data.
  filter(!(state %in% c("Northern Mariana Islands", 
                        "Guam", 
                        "Puerto Rico",
                        "Virgin Islands"))) %>%
  # Create new columns "sol_length," "one_year," "general_win_extended," and "police_action_win_extended" based on conditions.
  mutate(sol_length = case_when(
    statute_of_limitations == 1 ~ "One SOL",
    statute_of_limitations == 2 ~ "Two SOL",
    statute_of_limitations >= 3 ~ "Three SOL"
  ),
  one_year = ifelse(statute_of_limitations == 1, "1 Year SOL", "2 and More Year SOL"),
  general_win_extended = general_wins + general_settlements,
  police_action_win_extended = police_action_wins + police_action_settlements) %>%
  group_by(state) %>%
  # Count the number of courts in each group.
  mutate(n_courts = n()) %>%
  # Left join the census_data with the current data based on the "state" column.
  left_join(census_data, by = "state")

state_data <- data %>%
  group_by(state) %>%
  mutate(n_courts = as.character(n_courts),
         total = as.character(total),
         white_alone = as.character(white_alone),
         any_part_black = as.character(any_part_black)) %>%
  summarise(across(where(is.numeric), sum),
            n_courts = as.integer(unique(n_courts)),
            total = as.integer(unique(total)),
            white_alone = as.integer(unique(white_alone)),
            any_part_black = as.integer(unique(any_part_black)),
            statute_of_limitations = as.integer(unique(statute_of_limitations))) %>%
  mutate(n_courts = as.integer(n_courts),
         classification = ifelse(statute_of_limitations == "1", "One Year SOL", "More Than One Year SOL")) %>%
  group_by(classification)

state_data_one_year <- state_data %>%
  filter(classification == "One Year SOL")

state_data_more_one_year <- state_data %>%
  filter(classification == "More Than One Year SOL")

# --------------------------------- Defining functions -------------------------------

# Defining a function to extract pertinent data 
data_extraction <- function(df){
  df %>%
    summarize(
    pa_cases_per_court_per_capita = mean(police_action_cases / total*n_courts),
    percent_pa_wins = police_action_wins/police_action_cases,
    percent_pa_settlements = police_action_settlements/ police_action_cases,
    percent_pa_wins_extended = police_action_win_extended / police_action_cases,
    percent_dismissal = police_action_dismissals / police_action_cases)
}

two_group_metric <- function(df){
  df %>% 
    pivot_longer(cols = -classification, names_to = "metric") %>%
    pivot_wider(names_from = classification, values_from = value) %>%
    mutate(percent_difference = ((`More Than One Year SOL` - `One Year SOL`)/ `More Than One Year SOL`) * 100)
}

# ---------------------- States with longer SOL have Less Cases -------------------------------

# --------- Average # of cases ------------

# The average number of cases in 1 year SOL states is more than the number of cases in 1+ year SOL states
state_data %>%
  summarize(
    mean(general_cases)
  ) %>%
  two_group_metric()

# Not Statistically Significant  
t.test(
  state_data_one_year$general_cases,
  state_data_more_one_year$general_cases,
  conf.level = 0.95
)

# --------- Average # of police action cases per population ------------

# The average number of police action cases per state population is higher
# in states with 1 year SOL than states with 1+ year SOL
state_data %>%
  summarize(
    mean(police_action_cases / total)
  ) %>%
  two_group_metric()

# Not Statistically Significant
t.test(
  state_data_one_year$total,
  state_data_more_one_year$total,
  conf.level = 0.95
)

# --------- Average # of police action cases per population per court ------------

# The average number of police action cases per state population per court is higher
# in states with 1 year SOL than states with 1+ year SOL
state_data %>%
  summarize(
    mean(police_action_cases / (total*n_courts))
  ) %>%
  two_group_metric()

# Statistically Significant
t.test(
  state_data_one_year$police_action_cases / (state_data_one_year$total * state_data_one_year$n_courts),
  state_data_more_one_year$police_action_cases / (state_data_more_one_year$total * state_data_more_one_year$n_courts),
  conf.level = 0.95
)

# ---------------------- States with longer SOL have more wins -------------------------------

# --------- Average # of police action wins ------------

# The average number of police action wins in states with 1+ year SOLs is higher than 
# states with 1 year SOLs
state_data %>%
  summarize(
    mean(police_action_wins)
  ) %>%
  two_group_metric()

# Not Statistically Significant 
t.test(
  state_data_one_year$police_action_wins,
  state_data_more_one_year$police_action_wins,
  conf.level = 0.95
)

# --------- Average # of police action wins per court ------------

# The average number of police action wins per court is much higher in states with
# 1+ year SOLs than 1 year SOLs
state_data %>%
  summarize(
    mean(police_action_wins / n_courts)
  ) %>%
  two_group_metric()

# Statistically Significant
t.test(
  state_data_one_year$police_action_wins / state_data_one_year$n_courts,
  state_data_more_one_year$police_action_wins / state_data_more_one_year$n_courts,
  conf.level = 0.95
)


# --------- Average # of case wins per court ------------

# The average number of total case wins per court is much higher in states with
# 1+ year SOLs than 1 year SOLs
state_data %>%
  summarize(
    mean(general_wins / n_courts)
  ) %>%
  two_group_metric()

# Not Statistically significant
t.test(
  state_data_one_year$general_wins / state_data_one_year$n_courts,
  state_data_more_one_year$general_wins / state_data_more_one_year$n_courts,
  conf.level = 0.95
)


# --------- Average % of police action wins ------------

# The average proportion of police action wins is higher in states with
# 1+ year SOLs than 1 year SOLs
state_data %>%
  summarize(
    mean(police_action_wins / police_action_cases)
  ) %>%
  two_group_metric()

# Not Statistically Significant
t.test(
  state_data_one_year$police_action_wins / state_data_one_year$police_action_cases,
  state_data_more_one_year$police_action_wins / state_data_more_one_year$police_action_cases,
  conf.level = 0.95
)

# ---------  Proportion of police action wins ------------

# The proportion of police actions wins per cases is much higher in states with 
# 1+ year SOLs than 1 year SOLs
state_data %>%
  summarize(
    sum(police_action_wins)/ sum(police_action_cases)
  ) %>%
  two_group_metric()

# Not Statistically Significant
prop.test(
  x = c(sum(state_data_one_year$police_action_wins), sum(state_data_more_one_year$police_action_wins)),
  n = c(sum(state_data_one_year$police_action_cases), sum(state_data_more_one_year$police_action_cases)),
  conf.level = 0.95
)

# ---------  Proportion of general wins ------------



















# States with Longer SOLs have far more wins per cases on average
state_data %>%
  summarize(
    mean(general_wins / general_cases)
  ) %>%
  two_group_metric()

t.test(
  state_data_one_year$general_wins / state_data_one_year$general_cases,
  state_data_more_one_year$general_wins / state_data_more_one_year$general_cases,
  conf.level = 0.95
)

state_data %>%
  summarize(
    sum(general_wins)/ sum(general_cases)
  ) %>%
  two_group_metric()

prop.test(
  x = c(sum(state_data_one_year$general_wins), sum(state_data_more_one_year$general_wins)),
  n = c(sum(state_data_one_year$general_cases), sum(state_data_more_one_year$general_cases)),
  conf.level = 0.95
)


# States with Longer SOLs have more police action settlements on average
state_data %>%
  summarize(
    mean(police_action_settlements)
  ) %>%
  two_group_metric()

t.test(
  state_data_one_year$police_action_settlements,
  state_data_more_one_year$police_action_settlements,
  conf.level = 0.95
)

# States with Longer SOLs have less losses on average
state_data %>%
  summarize(
    mean(general_losses)
  ) %>%
  two_group_metric()

state_data %>%
  summarize(
    sum(police_action_losses) / sum(police_action_cases)
  ) %>%
  two_group_metric()

prop.test(
  x = c(sum(state_data_one_year$police_action_losses), sum(state_data_more_one_year$police_action_losses)),
  n = c(sum(state_data_one_year$police_action_cases), sum(state_data_more_one_year$police_action_cases)),
  conf.level = 0.95
)


# States with Longer SOLs have more settlements per court on average
state_data %>%
  summarize(
    mean(police_action_settlements / n_courts)
  ) %>%
  two_group_metric()

t.test(
  state_data_one_year$police_action_settlements / state_data_one_year$n_courts,
  state_data_more_one_year$police_action_settlements / state_data_more_one_year$n_courts,
  conf.level = 0.95
)

state_data %>%
  summarize(
    sum(police_action_settlements) / sum(n_courts)
  ) %>%
  two_group_metric()




# States with Longer SOLs have more settlements per police action cases on average
state_data %>%
  summarize(
    mean(police_action_settlements / police_action_cases)
  ) %>%
  two_group_metric()

t.test(
  state_data_one_year$police_action_settlements / state_data_one_year$police_action_cases,
  state_data_more_one_year$police_action_settlements / state_data_more_one_year$police_action_cases,
  conf.level = 0.95
)

state_data %>%
  summarize(
    sum(police_action_settlements) / sum(police_action_cases)
  ) %>%
  two_group_metric()

prop.test(
  x = c(sum(state_data_one_year$police_action_settlements), sum(state_data_more_one_year$police_action_settlements)),
  n = c(sum(state_data_one_year$police_action_cases), sum(state_data_more_one_year$police_action_cases)),
  conf.level = 0.95
)



# The rate of losses police action wins for states with Longer SOLs is much lower
state_data %>%
  summarize(
    sum(police_action_losses)/sum(police_action_cases)
  ) %>%
  two_group_metric()

prop.test(
  x = c(sum(state_data_one_year$police_action_losses), sum(state_data_more_one_year$police_action_losses)),
  n = c(sum(state_data_one_year$police_action_cases), sum(state_data_more_one_year$police_action_cases)),
  conf.level = 0.95
)

# The rate of police action wins for states with Longer SOLS is much higher
state_data %>%
  summarize(
    sum(police_action_win_extended)/sum(police_action_cases)
  ) %>%
  two_group_metric()

prop.test(
  x = c(sum(state_data_one_year$police_action_win_extended), sum(state_data_more_one_year$police_action_win_extended)),
  n = c(sum(state_data_one_year$police_action_cases), sum(state_data_more_one_year$police_action_cases)),
  conf.level = 0.95
)

# The rate of wins for states with Longer SOLS is much higher
state_data %>%
  summarize(
    sum(general_wins)/sum(general_cases)
  ) %>%
  two_group_metric()

prop.test(
  x = c(sum(state_data_one_year$general_wins), sum(state_data_more_one_year$general_wins)),
  n = c(sum(state_data_one_year$general_cases), sum(state_data_more_one_year$general_cases)),
  conf.level = 0.95
)

# The rate of general settlements for states with Longer SOLS is much higher
state_data %>%
  summarize(
    sum(general_settlements)/sum(general_cases)
  ) %>%
  two_group_metric()

prop.test(
  x = c(sum(state_data_one_year$general_settlements), sum(state_data_more_one_year$general_settlements)),
  n = c(sum(state_data_one_year$general_cases), sum(state_data_more_one_year$general_cases)),
  conf.level = 0.95
)

# States with 1 year SOL have far more black people
state_data %>%
  summarize(
    mean(any_part_black)
  )

t.test(
  state_data_one_year$any_part_black,
  state_data_more_one_year$any_part_black,
  conf.level = 0.95
)

# States with 1 year SOL have far more black people
state_data %>%
  group_by(statute_of_limitations) %>%
  summarize(
    mean(any_part_black)
  )

# States with 1 year SOL have far higher percentage of black people
state_data %>%
  summarize(
    sum(any_part_black)/sum(total)
  )

prop.test(
  x = c(sum(state_data_one_year$any_part_black), sum(state_data_more_one_year$any_part_black)),
  n = c(sum(state_data_one_year$total), sum(state_data_more_one_year$total)),
  conf.level = 0.95
)

# States with 1 year SOL have far higher percentage of black people
state_data %>%
  group_by(statute_of_limitations) %>%
  summarize(
    mean(any_part_black/total)
  )

# States with 1 year SOL have far more people of color
state_data %>%
  summarize(
    mean((total - white_alone))
  )

# States with 1 year SOL hold 6.77% of the US's black population and only 5.56% of the US's White population
state_data %>%
  summarize(percent_black = sum(any_part_black)/46936733,
            percent_white = sum(white_alone)/204277273)

# Average black population in states with less SOL is higher and white population is lower
state_data %>%
  mutate(category = ifelse(statute_of_limitations < 4, "Small", "Large")) %>%
  group_by(category) %>%
  summarize(percent_black = mean(any_part_black/total),
            percent_white = mean(white_alone/total))

