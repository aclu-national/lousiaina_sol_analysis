# What is the change in rate of police action cases per population?

  ## I NEED A TEST THAT CAN COMPARE THIS??
state_data_binary %>%
  group_by(statute_of_limitations) %>%
  summarize(prop_pa = sum(police_action_cases) / sum(total)) %>%
  pivot_wider(names_from = statute_of_limitations, values_from = prop_pa) %>%
  mutate(`Percent Difference` = 100*`1 Year`/`2 Years` - 100)

# What is the change in average number of police action wins?
t.test(
  data_1_year$police_action_wins,
  data_2_year$police_action_wins,
  conf.level = 0.95
)

# What is the change in average number of police action settlements?
t.test(
  data_1_year$police_action_settlements,
  data_2_year$police_action_settlements,
  conf.level = 0.95,
)

# What is the change in proportion of police action settlements?
prop.test(
  x = c(sum(data_1_year$police_action_settlements), sum(data_2_year$police_action_settlements)),
  n = c(sum(data_1_year$police_action_cases),sum(data_2_year$police_action_cases)),
  conf.level = 0.95
)

# What is the change in proportion of police action losses?
prop.test(
  x = c(sum(data_1_year$police_action_losses), sum(data_2_year$police_action_losses)),
  n = c(sum(data_1_year$police_action_cases),sum(data_2_year$police_action_cases)),
  conf.level = 0.95
)

# What is the change in ratio of wins to losses? 
chisq.test(data.frame(
  Court = c("1-year SOL", "2-year SOL"),
  Wins = c(sum(court_1_year$police_action_wins), sum(court_2_year$police_action_wins)),
  Losses = c(sum(court_1_year$police_action_losses), sum(court_2_year$police_action_losses))
  )[c("Wins", "Losses")]
)

# What is the change in ratio of settlements to losses? 
chisq.test(data.frame(
  Court = c("1-year SOL", "2-year SOL"),
  Settlements = c(sum(court_1_year$police_action_settlements), sum(court_2_year$police_action_settlements)),
  Losses = c(sum(court_1_year$police_action_losses), sum(court_2_year$police_action_losses))
  )[c("Settlements", "Losses")]
)

