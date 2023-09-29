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

## What is the change in rate of police action cases per population?
state_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(prop_pa = sum(police_action_cases) / sum(total)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = prop_pa) %>%
  mutate(`Percent Difference` = 100*`1 Year`/`2 Years` - 100)

# --------------- Longer Statutes of Limitations Result in More Police Action Wins and Settlements  -------------------------------

## What is the change in average number of police action wins?
court_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(mean_wins = mean(police_action_wins)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = mean_wins) %>%
  mutate(`Percent Difference` = 100*`2 Years`/`1 Year` - 100)

## What is the change in average number of police action settlements?
court_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(mean_settlements = mean(police_action_settlements)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = mean_settlements) %>%
  mutate(`Percent Difference` = 100*`2 Years`/`1 Year` - 100)

## What is the change in proportion of police action settlements?
court_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(prop_settlements = sum(police_action_settlements) / sum(police_action_cases)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = prop_settlements) %>%
  mutate(`Percent Difference` = 100*`2 Years`/`1 Year` - 100)


# ---------------  Longer Statutes of Limitations Result in Less Police Action Losses  -------------------------------

## What is the change in proportion of police action losses?
court_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(prop_losses = sum(police_action_losses) / sum(police_action_cases)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = prop_losses) %>%
  mutate(`Percent Difference` = 100*`1 Year`/`2 Years` - 100)

## What is the change in ratio of wins to losses? 
court_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(ratio_w_l = sum(police_action_wins) / sum(police_action_losses)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = ratio_w_l) %>%
  mutate(`Percent Difference` = 100*`2 Years`/`1 Year` - 100)

## What is the change in ratio of settlements to losses?
court_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(ratio_black = sum(any_part_black) / sum(total)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = ratio_black) %>%
  mutate(`Percent Difference` = 100*`1 Year`/`2 Years` - 100)

# ---------------   The Impacts of Short Statutes of Limitations are Disproportionate -------------------------------

## What is the change in proportion of Black residents?
state_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(ratio_s_l = sum(police_action_settlements) / sum(police_action_losses)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = ratio_s_l) %>%
  mutate(`Percent Difference` = 100*`2 Years`/`1 Year` - 100)

## What is the change in proportion of black vs. total population
total_black_pop = sum(state_data$any_part_black)
total_pop = sum(state_data$total)

state_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(prop_black = sum(any_part_black) / total_black_pop,
            prop_total = sum(total) / total_pop)
