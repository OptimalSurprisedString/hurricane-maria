# Plot death vs. day, separating by year
tab_tidy %>%
  filter(year != 2018) %>%
  ggplot(aes(x = DAY, y = deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20)