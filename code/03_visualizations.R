# Info --------------------------------------------------------------------

# This script creates visualizations in an html-format based on the created
# forecast- and draws-objects

# Setup -------------------------------------------------------------------

# Load forecast
forecast <- readRDS("forecast/forecast.RDS")
draws <- readRDS("forecast/draws.RDS")

# Libraries
library(tidyverse)
library(ggthemes)
library(plotly)
library(tidybayes)
library(maps)

# Final EV distribution ---------------------------------------------------

# Getting probabilities
p_dem <- forecast$prob_dem[forecast$state == "--" & forecast$t == max(forecast$t)]
p_rep <- forecast$prob_rep[forecast$state == "--" & forecast$t == max(forecast$t)]
p_draw <- forecast$prob_draw[forecast$state == "--" & forecast$t == max(forecast$t)]

# Static plot
ev_distribution <- draws |> 
  filter(t == max(t)) %>%
  group_by(draw) %>%
  summarise(dem_ev = sum(ev * (p_harris > 0.5))) |> 
  mutate(winner = case_when(
    dem_ev >= 270 ~ "Democratic",
    dem_ev < 269 ~ "Republican",
    dem_ev == 269 ~ "No winner")) |> 
  ggplot(aes(x = dem_ev, fill = winner)) +
  geom_vline(xintercept = 269.5) +
  geom_histogram(binwidth = 1) +
  theme_fivethirtyeight() +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(),
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 8)) +
  scale_fill_manual(
    name = 'Winner in the EC',
    values = c('Democratic' = '#3A4EB1',
               'Republican' = '#E40A04',
               "No winner" = "black")) +
  labs(x = 'Electoral votes for Kamala Harris',
       subtitle = paste0(
         "Probability of Harris win: ", round(p_dem * 100, 1), "%\n",
         "Probability of Trump win: ", round(p_rep * 100, 1), "%\n",
         "Probability of no EC winner: ", round(p_draw * 100, 1), "%"),
       caption = paste(
         "Last updated:",
         format(Sys.time(), "%Y-%m-%d %H:%M")))
ev_distribution
ggsave(filename = "forecast/ev_distribution.png",
       height = 4.5, width = 7)

# Dynamic plot
dynamic_ev_distribution <- ggplotly(ev_distribution) |> 
  layout(
    xaxis = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE),
    dragmode = "pan",
    annotations = list(
      list(
        x = 0.5,  # Center alignment
        y = 1.05,  # Position below the plot area
        text = paste0(
          "Probability of Harris win: ", round(p_dem * 100, 1), "%  |  ",
          "Probability of Trump win: ", round(p_rep * 100, 1), "%  |  ",
          "Probability of no EC winner: ", round(p_draw * 100, 1), "%"
        ),
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        xanchor = "center", yanchor = "top",
        font = list(size = 16, color = "black")
      ),
      list(
        text = paste("Last updated", format(Sys.time(), "%Y-%m-%d %H:%M")),
        y = -0.11, x = 1.2,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(family = "italic", size = 16)
      )
    )
  )
dynamic_ev_distribution
saveRDS(dynamic_ev_distribution, paste0(web_dir, "/web_files/ev_distribution.RDS"))


# Candidate distributions -------------------------------------------------

medians <- data.frame(
  candidate = c("Kamala Harris", "Donald Trump"),
  value = c(
    forecast$median_dem_ev[forecast$state == "--" & forecast$t == max(forecast$t)],
    forecast$median_rep_ev[forecast$state == "--" & forecast$t == max(forecast$t)]))

candidate_distributions <- draws |> 
  filter(t == max(draws$t)) |> 
  group_by(draw) %>%
  summarise(dem_ev = sum(ev * (p_harris > 0.5)),
            rep_ev = sum(ev * (p_harris < 0.5))) |> 
  pivot_longer(cols = c(dem_ev, rep_ev),
               names_to = "candidate") |> 
  mutate(candidate = if_else(
    candidate == "dem_ev",
    "Kamala Harris",
    "Donald Trump")) |> 
  ggplot(aes(y = candidate, x = value, fill = candidate)) +
  stat_halfeye(
    .width = c(0.8, 0.95)
  ) +
  geom_vline(xintercept = 269, linetype = "dashed") +
  geom_text(
    data = medians,
    aes(label = value),
    nudge_y = -.08,
    size = 4
  ) +
  scale_fill_manual(
    name = '',
    values = c('Kamala Harris' = '#3A4EB1',
               'Donald Trump' = '#E40A04')) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 8)) +
  labs(
    title = "Distribution of simulation outcomes",
    subtitle = "80 and 95 pct. credible intervals and median outcome",
    x = "Electoral votes",
    caption = paste(
      "Last updated:",
      format(Sys.time(), "%Y-%m-%d %H:%M")
    )) +
  guides(fill = "none")

candidate_distributions
ggsave(filename = "forecast/candidate_distribution.png",
       height = 4.5, width = 7)
saveRDS(candidate_distributions, paste0(web_dir, "/web_files/candidate_distributions.RDS"))


# State vote-predictions --------------------------------------------------

# Static plot
state_voting <- forecast |> 
  filter(t == max(t) & state != "--") |> 
  mutate(
    label = factor(
      state),
    winner = ifelse(
      mean_dem >= .5,
      'Democratic',
      'Republican')) |> 
  ggplot(aes(
    x = label,
    y = mean_dem,
    ymax = high_dem,
    ymin = low_dem,
    color = winner,
    text = paste("State:", state, "<br>",
                 "Predicted winner:", winner, "<br>",
                 "Predicted democratic vote share:", round(mean_dem, 2), "<br>",
                 "Lower-bound:", round(low_dem, 2), "<br>",
                 "Upper-bound:", round(high_dem, 2)))) +
  geom_point(size = 2)+
  geom_linerange(size = 2.5,
                 alpha = .4) +
  geom_hline(yintercept = .5,
             linetype = "dashed") +
  scale_y_continuous("Predicted Harris two-party vote-share") +
  scale_x_discrete(
    "",
    limits = forecast$state[
      forecast$t == max(forecast$t) & forecast$state != "--"][order(
        forecast$mean_dem[forecast$t == max(forecast$t) & forecast$state != "--"],
        decreasing = TRUE)]) +
  scale_color_manual(
    name = 'State winner',
    values = c('Democratic' = '#3A4EB1',
               'Republican' = '#E40A04')) +
  geom_text(
    aes(label = round(mean_dem, digits = 2)),
    nudge_y = .07,
    size = 3) +
  coord_flip() +
  theme_fivethirtyeight() +
  labs(title = "Predicted democratic two-party voteshare")
state_voting
ggsave(filename = "forecast/state_voting.png",
       height = 8, width = 7)

# Interactive plot
state_voting_interactive <- ggplotly(state_voting, tooltip = "text")
saveRDS(state_voting_interactive, paste0(web_dir, "/web_files/state_voting.RDS"))




# Electoral map -----------------------------------------------------------

us_states <- map_data("state")

forecast <- forecast %>% 
  mutate(region = tolower(state.name[match(state, state.abb)]))

map_data <- us_states %>%
  left_join(
    forecast |> 
      filter(t == max(forecast$t) & state != "--"),
    by = "region")

# Static map
electoral_map <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = prob_dem,
                                      text = paste("State:", tools::toTitleCase(region), "<br>",
                                                   "Democratic Win Probability:", round(prob_dem * 100, 2), "%", "<br>",
                                                   "Republican Win Probability:", round(prob_rep * 100, 2), "%"))) +
  geom_polygon(color = "black", size = 0.2) + 
  scale_fill_gradient(low = "#E40A04", high = "#3A4EB1", 
                      name = "Democratic Win Probability") +
  labs(title = "Win probabilities by state") +
  theme_minimal() +
  coord_fixed(1.3) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  guides(fill = "none")

# Interactive map
electoral_map_interactive <- ggplotly(electoral_map, tooltip = "text")
saveRDS(electoral_map_interactive, paste0(web_dir, "/web_files/electoral_map.RDS"))



# Development: Win probability --------------------------------------------

forecast |> 
  filter(state == "--" & t <= Sys.Date()) |> 
  pivot_longer(cols = c(prob_dem, prob_rep),
               names_to = "candidate") |> 
  mutate(candidate = if_else(
    candidate == "prob_dem",
    "Kamala Harris",
    "Donald Trump")) |> 
  ggplot(aes(
    x = t,
    y = value,
    color = candidate,
    group = candidate,
    text = paste(candidate, "<br>",
                 "Date:", t, "<br>",
                 "Win probability:", round(value, 2)))) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_color_manual(
    name = '',
    values = c('Kamala Harris' = '#3A4EB1',
               'Donald Trump' = '#E40A04')) +
  scale_x_date(limits = c(min(forecast$t), max(forecast$t))) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_fivethirtyeight()
  
win_prob_time <- ggplotly(tooltip = "text")
saveRDS(win_prob_time, paste0(web_dir, "/web_files/time_win_prob.RDS"))


# Development: Electoral votes --------------------------------------------

forecast |> 
  filter(state == "--" & t <= Sys.Date()) |> 
  pivot_longer(cols = c(median_dem_ev, median_rep_ev),
               names_to = "candidate") |> 
  mutate(candidate = if_else(
    candidate == "median_dem_ev",
    "Kamala Harris",
    "Donald Trump")) |> 
  ggplot(aes(
    x = t,
    y = value,
    color = candidate,
    group = candidate,
    text = paste(candidate, "<br>",
                 "Date:", t, "<br>",
                 "Electoral votes:", round(value, 2)))) +
  geom_line(size = 1) +
  theme_fivethirtyeight() +
  geom_ribbon(aes(ymin = low_rep_ev,
                  ymax = high_rep_ev,
                  x = t,
                  xmin = min(forecast$t),
                  xmax = Sys.Date()),
              alpha = 0.2,
              inherit.aes = FALSE,
              fill = '#E40A04') +
  geom_ribbon(aes(ymin = low_dem_ev,
                  ymax = high_dem_ev,
                  x = t,
                  xmin = min(forecast$t),
                  xmax = Sys.Date()),
              alpha = 0.2,
              inherit.aes = FALSE,
              fill = '#3A4EB1') +
  geom_hline(yintercept = 269, linetype = "dashed") +
  scale_color_manual(
    name = '',
    values = c('Kamala Harris' = '#3A4EB1',
               'Donald Trump' = '#E40A04')) +
  scale_x_date(limits = c(min(forecast$t), max(forecast$t))) +
  scale_y_continuous(limits = c(0, 538))

ec_votes_time <- ggplotly(tooltip = "text")
saveRDS(ec_votes_time, paste0(web_dir, "/web_files/time_ec_votes.RDS"))


# Development: Popular vote -----------------------------------------------

forecast |> 
  filter(state == "--" & t <= Sys.Date()) |> 
  pivot_longer(cols = c(mean_dem, mean_rep),
               names_to = "candidate") |> 
  mutate(candidate = if_else(
    candidate == "mean_dem",
    "Kamala Harris",
    "Donald Trump")) |> 
  ggplot(aes(
    x = t,
    y = value,
    color = candidate,
    group = candidate,
    text = paste(candidate, "<br>",
                 "Date:", t, "<br>",
                 "Twoparty vote-share:", round(value, 2)))) +
  geom_line(size = 1) +
  theme_fivethirtyeight() +
  geom_ribbon(aes(ymin = low_rep,
                  ymax = high_rep,
                  x = t,
                  xmin = min(forecast$t),
                  xmax = Sys.Date()),
              alpha = 0.2,
              inherit.aes = FALSE,
              fill = '#E40A04') +
  geom_ribbon(aes(ymin = low_dem,
                  ymax = high_dem,
                  x = t,
                  xmin = min(forecast$t),
                  xmax = Sys.Date()),
              alpha = 0.2,
              inherit.aes = FALSE,
              fill = '#3A4EB1') +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_color_manual(
    name = '',
    values = c('Kamala Harris' = '#3A4EB1',
               'Donald Trump' = '#E40A04')) +
  scale_x_date(limits = c(min(forecast$t), max(forecast$t))) +
  scale_y_continuous(limits = c(0.25, 0.75))

pop_vote_time <- ggplotly(tooltip = "text")
saveRDS(pop_vote_time, paste0(web_dir, "/web_files/time_pop_vote.RDS"))



# Harris road to victory --------------------------------------------------



dem_wins_mi <- draws |> 
  filter(t == max(draws$t) & state == "PA" & p_harris > .5)

draws |> 
  filter(t == max(draws$t)) |> 
  group_by(draw) %>%
  summarise(dem_ev = sum(ev * (p_harris > 0.5)),
            rep_ev = sum(ev * (p_harris < 0.5))) |> 
  mutate(outcome = if_else(
    draw %in% dem_wins_mi$draw,
    "Harris wins state",
    "Trump wins state")) |>
  pivot_longer(cols = c(dem_ev, rep_ev),
               names_to = "candidate") |> 
  mutate(candidate = if_else(
    candidate == "dem_ev",
    "Kamala Harris",
    "Donald Trump")) |> 
  ggplot(aes(y = candidate, x = value, fill = candidate)) +
  stat_halfeye(
    .width = c(0.8, 0.95)
  ) +
  geom_vline(xintercept = 269, linetype = "dashed") +
  facet_wrap(~outcome, ncol = 1) +
  scale_fill_manual(
    name = '',
    values = c('Kamala Harris' = '#3A4EB1',
               'Donald Trump' = '#E40A04')) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 8)) +
  labs(
    title = "Distribution of simulation outcomes",
    subtitle = "80 and 95 pct. credible intervals and median outcome",
    x = "Electoral votes",
    caption = paste(
      "Last updated:",
      format(Sys.time(), "%Y-%m-%d %H:%M")
    )) +
  guides(fill = "none")



  

  

