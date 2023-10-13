# ---------------------------------- Defining Necessary Functions  -------------------------------

# Create a function to calculate the summary statistics with p-values when comparing with 1 year SOL
calculate_summary_stats_with_p_values <- function(data, col_name, group_col) {
  data %>%
    group_by({{ group_col }}) %>%
    summarize(summary = mean({{ col_name }})) %>%
    mutate(diff = 100 * (summary / summary[{{ group_col }} == 1]) - 100) %>%
    mutate(
      p_value = map_dbl(unique({{ group_col }}), function(.x) {
        subset1 <- filter(data, {{ group_col }} == 1) %>% pull({{ col_name }})
        subset2 <- filter(data, {{ group_col }} == .x) %>% pull({{ col_name }})
        
        if (length(subset1) < 2 || length(subset2) < 2) {
          return(NA)  # Return NA if there are not enough observations for the t-test
        } else {
          return(t.test(subset1, subset2, conf.level = 0.95)$p.value)
        }
      }),
      significant = ifelse(p_value <= 0.05, "Yes", "No")
    )
}

# Calculate the average for different columns by statute_of_limitations with p-values when comparing with 1 year SOL
average_stats_by_statute <- function(data, col_name) {
  data %>%
    group_by(statute_of_limitations) %>%
    calculate_summary_stats_with_p_values({{ col_name }}, statute_of_limitations)
}

# Calculate the proportion for different columns by statute_of_limitations with p-values when comparing with 1 year SOL
proportion_stats_by_statute <- function(data, col_name, total_col) {
  data %>%
    group_by(statute_of_limitations) %>%
    summarize(
      proportion = sum({{ col_name }}) / sum({{ total_col }})
    ) %>%
    mutate(
      p_value = map_dbl(statute_of_limitations, function(.x) {
        subset1 <- filter(data, statute_of_limitations == 1)
        subset2 <- filter(data, statute_of_limitations == .x)
        prop.test(
          x = c(
            sum(subset1 %>% pull({{ col_name }})), 
            sum(subset2 %>% pull({{ col_name }}))
          ),
          n = c(
            sum(subset1 %>% pull({{ total_col }})), 
            sum(subset2 %>% pull({{ total_col }}))
          )
        )$p.value
      }),
      significant = ifelse(p_value <= 0.05, "Yes", "No")
    ) 
}

# Calculate the proportion for different columns by statute_of_limitations with p-values (and chi-squired test) when comparing with 1 year SOL
chi_stats_by_statute <- function(data, col_name, total_col) {
  data %>%
    group_by(statute_of_limitations) %>%
    summarize(
      ratio = sum({{ col_name }}) / sum({{ total_col }})
    ) %>%
    mutate(
      p_value = map_dbl(statute_of_limitations, function(.x) {
        subset1 <- filter(data, statute_of_limitations == 1)
        subset2 <- filter(data, statute_of_limitations == .x)
        chisq.test(
          data.frame(
          x = c(
            sum(subset1 %>% pull({{ col_name }})), 
            sum(subset2 %>% pull({{ col_name }}))
          ),
          n = c(
            sum(subset1 %>% pull({{ total_col }})), 
            sum(subset2 %>% pull({{ total_col }}))
          )
        )
        )$p.value
      }),
      significant = ifelse(p_value <= 0.05, "Yes", "No")
    ) 
}

# --------------- Longer Statutes of Limitations Result in Less Police Action Cases  -------------------------------

## How many cases are police action?
court_data %>%
  pull(police_action_cases) %>%
  sum(.)

## How many cases per court?
court_data %>%
  pull(police_action_cases) %>%
  mean(.)

## How many cases per court per year?
date1 <- as.Date("2009-01-01")
date2 <- as.Date("2023-08-25")
interval(date1, date2) %/% days() / 365

## What proportion of all cases do police action cases make up?
sum(court_data$police_action_cases) / sum(court_data$general_cases)

# ------------------------------  Number of Cases  -------------------------------

## Average number of police action cases per court
average_stats_by_statute(court_data, police_action_cases)

## Average number of police action cases per state
average_stats_by_statute(state_data, police_action_cases)

## Proportion of police action cases per court
proportion_stats_by_statute(court_data, police_action_cases, general_cases)

## Proportion of police action cases per residents per state
proportion_stats_by_statute(state_data, police_action_cases, total)

## Average number of terminated police action cases per court
average_stats_by_statute(court_data, terminated_police_action_cases)

## Average number of terminated police action cases per state
average_stats_by_statute(state_data, terminated_police_action_cases)

## Proportion of terminated police action cases per court
proportion_stats_by_statute(state_data, terminated_police_action_cases, general_cases)

## Proportion of terminated police action cases per residents per state
proportion_stats_by_statute(state_data, terminated_police_action_cases, total)

# -------------------------  Number of Wins and Settlements  -------------------------------

## Average number of police action wins per court
average_stats_by_statute(court_data, police_action_wins)

## Average number of police action wins per state
average_stats_by_statute(state_data, police_action_wins)

## Proportion of police action wins per court
proportion_stats_by_statute(court_data, police_action_wins, police_action_cases)

## Proportion of terminated police action wins per court
proportion_stats_by_statute(court_data, police_action_wins, terminated_police_action_cases)

## Average number of police action settlements per court
average_stats_by_statute(court_data, police_action_settlements)

## Average number of police action settlements per state
average_stats_by_statute(state_data, police_action_settlements)

## Proportion of police action settlements per court
proportion_stats_by_statute(court_data, police_action_settlements, police_action_cases)

## Proportion of terminated police action settlements per court
proportion_stats_by_statute(court_data, police_action_settlements, terminated_police_action_cases)

## Average number of police action wins + settlements per court
average_stats_by_statute(court_data, police_action_wins_extended)

## Average number of police action wins + settlements per state
average_stats_by_statute(state_data, police_action_wins_extended)

## Proportion of police action police action wins + settlements per court
proportion_stats_by_statute(court_data, police_action_wins_extended, police_action_cases)

## Proportion of terminated police action police action wins + settlements per court
proportion_stats_by_statute(court_data, police_action_wins_extended, terminated_police_action_cases)

# -------------------------  Number of Losses and Dismissals  -------------------------------

## Average number of police action losses per court
average_stats_by_statute(court_data, police_action_losses)

## Average number of police action losses per state
average_stats_by_statute(state_data, police_action_losses)

## Proportion of police action losses per court
proportion_stats_by_statute(court_data, police_action_losses, police_action_cases)

## Proportion of terminated police action losses per court
proportion_stats_by_statute(court_data, police_action_losses, terminated_police_action_cases)

## Average number of police action dismissals per court
average_stats_by_statute(court_data, police_action_dismissals)

## Average number of police action dismissals per state
average_stats_by_statute(state_data, police_action_dismissals)

## Proportion of police action dismissals per court
proportion_stats_by_statute(court_data, police_action_dismissals, police_action_cases)

## Proportion of terminated police action dismissals per court
proportion_stats_by_statute(court_data, police_action_dismissals, terminated_police_action_cases)

## Average number of police action losses + dismissals per court
average_stats_by_statute(court_data, police_action_losses_extended)

## Average number of police action losses + dismissals per state
average_stats_by_statute(state_data, police_action_losses_extended)

## Proportion of police action losses + dismissals per court
proportion_stats_by_statute(court_data, police_action_losses_extended, police_action_cases)

## Proportion of terminated police action losses + dismissals per court
proportion_stats_by_statute(court_data, police_action_losses_extended, terminated_police_action_cases)

# --------------------  Ratios of Wins, Settlements, Losses, and Dismissals  -------------------------------

## Ratio of Wins to Losses per court
chi_stats_by_statute(court_data, police_action_wins, police_action_losses)

## Ratio of Wins to Dismissals per court
chi_stats_by_statute(court_data, police_action_wins, police_action_dismissals)


## Ratio of Wins to Losses + Dismissals per court
chi_stats_by_statute(court_data, police_action_wins, police_action_losses_extended)

## Ratio of Settlements to Losses per court
chi_stats_by_statute(court_data, police_action_settlements, police_action_losses)

## Ratio of Settlements to Dismissals per court
chi_stats_by_statute(court_data, police_action_settlements, police_action_dismissals)

## Ratio of Settlements to Losses + Dismissals per court
chi_stats_by_statute(court_data, police_action_settlements, police_action_losses_extended)

## Ratio of Wins + Settlements to Losses per court
chi_stats_by_statute(court_data, police_action_wins_extended, police_action_losses)

## Ratio of Wins + Settlements to Dismissals per court
chi_stats_by_statute(court_data, police_action_wins_extended, police_action_dismissals)

## Ratio of Wins + Settlements to Losses + Dismissals per court
chi_stats_by_statute(court_data, police_action_wins_extended, police_action_losses_extended)

# ---------------   Impact on Black Residents -------------------------------

## Proportion of Black residents
proportion_stats_by_statute(state_data, any_part_black, total)


## What is the change in proportion of black vs. total population
total_black_pop = sum(state_data$any_part_black)
total_pop = sum(state_data$total)

state_data %>%
  group_by(statute_of_limitations) %>%
  summarize(prop_black = sum(any_part_black) / total_black_pop,
            prop_total = sum(total) / total_pop) %>%
  mutate(diff = 100* (prop_black / prop_total) - 100)
