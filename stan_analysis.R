#Loading pre-installed libraries
library(tidyverse)
library(ggplot2)
library(lubridate)

library(rstan)


#turning off warnings
options(warn=-1)

df_plays <- readRDS("df_plays_coverage.rds")

stan_path <- "stan_bdb.stan"

simple_stan_path <- "stan_bdb_simple.stan"

simple_unscaled_stan_path <- "stan_bdb_simple_unscaled.stan"

simple_off_stan_path <- "stan_bdb_simple_off.stan"

off_stan_path <- "stan_bdb_off.stan"

##Clean data

df_plays <- df_plays %>% 
  dplyr::mutate(game_clock_small = str_sub(game_clock, 1, 5)) %>%
  dplyr::mutate(game_clock_small = paste0("00:", game_clock_small)) %>%
  dplyr::mutate(game_sec = period_to_seconds(hms(game_clock_small))) %>%
  dplyr::mutate(extra_sec = case_when(quarter == 1 ~ 121,
                                      quarter == 3 ~ 121,
                                      TRUE ~ 0)) %>%
  dplyr::mutate(game_sec_quarter = game_sec + extra_sec) %>%
  dplyr::filter(game_sec_quarter >= 120) %>%
  dplyr::filter(!is.na(absolute_yardline_number)) %>%
  dplyr::filter(absolute_yardline_number <= 90) %>%
  dplyr::filter(!is.na(coverage)) %>%
  dplyr::filter(wp >= 0.2) %>%
  dplyr::filter(wp <= 0.8)

##clean the data up for Stan modeling

df_stan_use <- df_plays %>% 
  dplyr::select(game_id, play_id, possession_team, defense_team, wp, coverage, epa)

##Create numerical values for defense teams

team_ids <- df_stan_use %>%
  dplyr::pull(defense_team) %>% 
  unique() %>%
  data.frame()

names(team_ids) <- "defense_team"

team_ids <- team_ids %>%
  dplyr::arrange(defense_team)

team_ids$team_numeric <- 1:32

df_stan_use <- df_stan_use %>%
  dplyr::left_join(team_ids, by = "defense_team")

##Use same df to make offensive team ids

team_ids_off <- team_ids
team_ids_off <- team_ids_off %>%
  dplyr::rename(possession_team = defense_team,
                off_team_numeric = team_numeric)

df_stan_use <- df_stan_use %>%
  dplyr::left_join(team_ids_off, by = "possession_team")

##Create numerical values for coverages

coverage_ids <- df_stan_use %>%
  dplyr::pull(coverage) %>% 
  unique() %>%
  data.frame()

names(coverage_ids) <- "coverage"

coverage_ids <- coverage_ids %>%
  dplyr::arrange(coverage)

coverage_ids$coverage_numeric <- 1:7

df_stan_use <- df_stan_use %>%
  dplyr::left_join(coverage_ids, by = "coverage")

##Get data organized

set.seed(20)

y <- df_stan_use$epa
N <- length(y)
team <- df_stan_use$team_numeric
off_team <- df_stan_use$off_team_numeric
coverage <- df_stan_use$coverage_numeric
n_teams <- max(team)
n_coverages <- max(coverage)
team_mean <- rep(0, n_teams)
coverage_mean <- rep(0, n_coverages)

set.seed(296)
stan_data <- list(N = N,
                  y = y,
                  n_teams = n_teams,
                  team = team,
                  off_team = off_team,
                  n_coverages = n_coverages,
                  coverage = coverage)

print(Sys.time())
print("Started compiling model.")

fit_coverage <- stan(file = off_stan_path,
                     data = stan_data,
                     warmup = 15000, iter = 30000,
                     chains = 1,
                     #cores = min(2, parallel::detectCores()),
                     thin = 1,
                     #control = list(max_treedepth = 20),
                     seed = 11)

print(Sys.time())
print("Model finished.")

posterior <- extract(fit_coverage)
names(posterior)

traceplot(fit_coverage, "mu")
plot(density(posterior$mu))
abline(v = mean(posterior$mu))

##Cover 0 Man
traceplot(fit_coverage, "beta_coverage[1]")
plot(density(posterior$beta_coverage[,1]))
abline(v = mean(posterior$beta_coverage[,1])) #.001

##Cover 1 Man
traceplot(fit_coverage, "beta_coverage[2]")
plot(density(posterior$beta_coverage[,2]))
abline(v = mean(posterior$beta_coverage[,2])) #.119

##Cover 2 Man
traceplot(fit_coverage, "beta_coverage[3]")
plot(density(posterior$beta_coverage[,3]))
abline(v = mean(posterior$beta_coverage[,3])) #-.085

##Cover 2 Zone
traceplot(fit_coverage, "beta_coverage[4]")
plot(density(posterior$beta_coverage[,4]))
abline(v = mean(posterior$beta_coverage[,4])) #-.072

##Cover 3 Zone
traceplot(fit_coverage, "beta_coverage[5]")
plot(density(posterior$beta_coverage[,5]))
abline(v = mean(posterior$beta_coverage[,5])) #-.004

##Cover 4 Zone
traceplot(fit_coverage, "beta_coverage[6]")
plot(density(posterior$beta_coverage[,6]))
abline(v = mean(posterior$beta_coverage[,6])) #.031

##Cover 6 Zone
traceplot(fit_coverage, "beta_coverage[7]")
plot(density(posterior$beta_coverage[,7]))
abline(v = mean(posterior$beta_coverage[,7])) #-.066

##Make a plot of these

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

RE_means <- apply(posterior$beta_coverage, 2, mean) %>% data.frame()
names(RE_means) <- "means"
RE_means$col <- 1:7
RE_means %>% dplyr::arrange(desc(means))

#Cover 0 Man, Cover 1 Man, Cover 2 Man, Cover 2 Zone, Cover 3 Zone, Cover 4 Zone, Cover 6 Zone

myorder <- c("Cover 1 Man", "Cover 4 Zone", "Cover 0 Man", "Cover 3 Zone", 
             "Cover 6 Zone", "Cover 2 Man", "Cover 2 Zone")


plot_data_RE <- data.frame(c(posterior$beta_coverage[,2],
                             posterior$beta_coverage[,6],
                             posterior$beta_coverage[,1],
                             posterior$beta_coverage[,5],
                             posterior$beta_coverage[,7],
                             posterior$beta_coverage[,3],
                             posterior$beta_coverage[,4]))

names(plot_data_RE) <- "EPA"

plot_data_RE$Coverage <- c(rep("Cover 1 Man", 15000),
                           rep("Cover 4 Zone", 15000),
                           rep("Cover 0 Man", 15000),
                           rep("Cover 3 Zone", 15000),
                           rep("Cover 6 Zone", 15000),
                           rep("Cover 2 Man", 15000),
                           rep("Cover 2 Zone", 15000))

plot_data_RE <- plot_data_RE %>%
    dplyr::mutate(Coverage = factor(Coverage, levels = rev(myorder)))

ggplot(plot_data_RE, aes(x = `EPA`, y = `Coverage`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Coverage", option = "D") +
  labs(title = 'Coverage Random Effects') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlim(-0.4,0.4)

##Perform similar analysis for team effects

RE_means <- apply(posterior$gamma_defense, 2, mean) %>% data.frame()
names(RE_means) <- "means"
RE_means$col <- 1:32
myorder <- RE_means %>% dplyr::arrange(desc(means)) %>%
  dplyr::left_join(team_ids, by = c("col" = "team_numeric")) %>%
  dplyr::pull(defense_team)

plot_data_team <- data.frame(c(posterior$gamma_defense[,1],
                               posterior$gamma_defense[,2],
                               posterior$gamma_defense[,3],
                               posterior$gamma_defense[,4],
                               posterior$gamma_defense[,5],
                               posterior$gamma_defense[,6],
                               posterior$gamma_defense[,7],
                               posterior$gamma_defense[,8],
                               posterior$gamma_defense[,9],
                               posterior$gamma_defense[,10],
                               posterior$gamma_defense[,11],
                               posterior$gamma_defense[,12],
                               posterior$gamma_defense[,13],
                               posterior$gamma_defense[,14],
                               posterior$gamma_defense[,15],
                               posterior$gamma_defense[,16],
                               posterior$gamma_defense[,17],
                               posterior$gamma_defense[,18],
                               posterior$gamma_defense[,19],
                               posterior$gamma_defense[,20],
                               posterior$gamma_defense[,21],
                               posterior$gamma_defense[,22],
                               posterior$gamma_defense[,23],
                               posterior$gamma_defense[,24],
                               posterior$gamma_defense[,25],
                               posterior$gamma_defense[,26],
                               posterior$gamma_defense[,27],
                               posterior$gamma_defense[,28],
                               posterior$gamma_defense[,29],
                               posterior$gamma_defense[,30],
                               posterior$gamma_defense[,31],
                               posterior$gamma_defense[,32]))

names(plot_data_team) <- "EPA"

plot_data_team$Team <- c(rep("ARI", 15000),
                         rep("ATL", 15000),
                         rep("BAL", 15000),
                         rep("BUF", 15000),
                         rep("CAR", 15000),
                         rep("CHI", 15000),
                         rep("CIN", 15000),
                         rep("CLE", 15000),
                         rep("DAL", 15000),
                         rep("DEN", 15000),
                         rep("DET", 15000),
                         rep("GB", 15000),
                         rep("HOU", 15000),
                         rep("IND", 15000),
                         rep("JAX", 15000),
                         rep("KC", 15000),
                         rep("LA", 15000),
                         rep("LAC", 15000),
                         rep("MIA", 15000),
                         rep("MIN", 15000),
                         rep("NE", 15000),
                         rep("NO", 15000),
                         rep("NYG", 15000),
                         rep("NYJ", 15000),
                         rep("OAK", 15000),
                         rep("PHI", 15000),
                         rep("PIT", 15000),
                         rep("SEA", 15000),
                         rep("SF", 15000),
                         rep("TB", 15000),
                         rep("TEN", 15000),
                         rep("WAS", 15000))

plot_data_team <- plot_data_team %>%
  dplyr::mutate(Team = factor(Team, levels = rev(myorder)))

ggplot(plot_data_team, aes(x = `EPA`, y = `Team`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Team", option = "D") +
  labs(title = 'Team Defense Random Effects') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) + 
  xlim(-0.3, 0.3)

##Perform similar analysis for offense effects

RE_means <- apply(posterior$xi_offense, 2, mean) %>% data.frame()
names(RE_means) <- "means"
RE_means$col <- 1:32
myorder <- RE_means %>% dplyr::arrange(desc(means)) %>%
  dplyr::left_join(team_ids, by = c("col" = "team_numeric")) %>%
  dplyr::pull(defense_team)

plot_data_team <- data.frame(c(posterior$xi_offense[,1],
                               posterior$xi_offense[,2],
                               posterior$xi_offense[,3],
                               posterior$xi_offense[,4],
                               posterior$xi_offense[,5],
                               posterior$xi_offense[,6],
                               posterior$xi_offense[,7],
                               posterior$xi_offense[,8],
                               posterior$xi_offense[,9],
                               posterior$xi_offense[,10],
                               posterior$xi_offense[,11],
                               posterior$xi_offense[,12],
                               posterior$xi_offense[,13],
                               posterior$xi_offense[,14],
                               posterior$xi_offense[,15],
                               posterior$xi_offense[,16],
                               posterior$xi_offense[,17],
                               posterior$xi_offense[,18],
                               posterior$xi_offense[,19],
                               posterior$xi_offense[,20],
                               posterior$xi_offense[,21],
                               posterior$xi_offense[,22],
                               posterior$xi_offense[,23],
                               posterior$xi_offense[,24],
                               posterior$xi_offense[,25],
                               posterior$xi_offense[,26],
                               posterior$xi_offense[,27],
                               posterior$xi_offense[,28],
                               posterior$xi_offense[,29],
                               posterior$xi_offense[,30],
                               posterior$xi_offense[,31],
                               posterior$xi_offense[,32]))

names(plot_data_team) <- "EPA"

plot_data_team$Team <- c(rep("ARI", 15000),
                         rep("ATL", 15000),
                         rep("BAL", 15000),
                         rep("BUF", 15000),
                         rep("CAR", 15000),
                         rep("CHI", 15000),
                         rep("CIN", 15000),
                         rep("CLE", 15000),
                         rep("DAL", 15000),
                         rep("DEN", 15000),
                         rep("DET", 15000),
                         rep("GB", 15000),
                         rep("HOU", 15000),
                         rep("IND", 15000),
                         rep("JAX", 15000),
                         rep("KC", 15000),
                         rep("LA", 15000),
                         rep("LAC", 15000),
                         rep("MIA", 15000),
                         rep("MIN", 15000),
                         rep("NE", 15000),
                         rep("NO", 15000),
                         rep("NYG", 15000),
                         rep("NYJ", 15000),
                         rep("OAK", 15000),
                         rep("PHI", 15000),
                         rep("PIT", 15000),
                         rep("SEA", 15000),
                         rep("SF", 15000),
                         rep("TB", 15000),
                         rep("TEN", 15000),
                         rep("WAS", 15000))

plot_data_team <- plot_data_team %>%
  dplyr::mutate(Team = factor(Team, levels = rev(myorder)))

ggplot(plot_data_team, aes(x = `EPA`, y = `Team`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Team", option = "D") +
  labs(title = 'Stan Coverage Team Offense Effects') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) + 
  xlim(-0.6, 0.6)

##Could do the same for offensive effects, but that isn't the point of this project

##Defense-coverage effects

##Look at best and worst effects for each team in two divisions

##Look at NFC South and NFC North
##NFC South: ATL 2, CAR 5, NO 22, TB 30
##NFC North: CHI 6, DET 11, GB 12, MIN 20

ATL_total <- posterior$delta_defense_coverage[,2,]
dim(ATL_total) <- c(15000, 7)
ATL_means <- apply(ATL_total, 2, mean) %>% data.frame()
names(ATL_means) <- "EPA"
ATL_means$col <- 1:7
ATL_means <- ATL_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
ATL_means_final <- ATL_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(ATL_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_South_plot_val <- c(posterior$delta_defense_coverage[,2,5], 
                        posterior$delta_defense_coverage[,2,2])

NFC_South_labels <- c(rep("ATL Cover 3 Zone", 15000),
                      rep("ATL Cover 1 Man", 15000))

CAR_total <- posterior$delta_defense_coverage[,5,]
dim(CAR_total) <- c(15000, 7)
CAR_means <- apply(CAR_total, 2, mean) %>% data.frame()
names(CAR_means) <- "EPA"
CAR_means$col <- 1:7
CAR_means <- CAR_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
CAR_means_final <- CAR_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(CAR_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_South_plot_val <- c(NFC_South_plot_val,
                        posterior$delta_defense_coverage[,5,5], 
                        posterior$delta_defense_coverage[,5,6])

NFC_South_labels <- c(NFC_South_labels,
                      rep("CAR Cover 3 Zone", 15000),
                      rep("CAR Cover 4 Zone", 15000))

NO_total <- posterior$delta_defense_coverage[,22,]
dim(NO_total) <- c(15000, 7)
NO_means <- apply(NO_total, 2, mean) %>% data.frame()
names(NO_means) <- "EPA"
NO_means$col <- 1:7
NO_means <- NO_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
NO_means_final <- NO_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(NO_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_South_plot_val <- c(NFC_South_plot_val,
                        posterior$delta_defense_coverage[,22,2], 
                        posterior$delta_defense_coverage[,22,3])

NFC_South_labels <- c(NFC_South_labels,
                      rep("NO Cover 1 Man", 15000),
                      rep("NO Cover 2 Man", 15000))

TB_total <- posterior$delta_defense_coverage[,30,]
dim(TB_total) <- c(15000, 7)
TB_means <- apply(TB_total, 2, mean) %>% data.frame()
names(TB_means) <- "EPA"
TB_means$col <- 1:7
TB_means <- TB_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
TB_means_final <- TB_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(TB_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_South_plot_val <- c(NFC_South_plot_val,
                        posterior$delta_defense_coverage[,30,2], 
                        posterior$delta_defense_coverage[,30,6])

NFC_South_labels <- c(NFC_South_labels,
                      rep("TB Cover 1 Man", 15000),
                      rep("TB Cover 4 Zone", 15000))

##Order of plots: Worst to best, as before

NFC_South_levels <- ATL_means_final %>% dplyr::mutate(team = "ATL") %>%
  bind_rows(CAR_means_final %>% dplyr::mutate(team = "CAR")) %>%
  bind_rows(NO_means_final %>% dplyr::mutate(team = "NO")) %>%
  bind_rows(TB_means_final %>% dplyr::mutate(team = "TB")) %>%
  arrange(desc(EPA)) %>%
  dplyr::mutate(coverage = as.character(coverage)) %>%
  dplyr::mutate(team_coverage = paste0(team, " ", coverage)) %>%
  pull(team_coverage)

NFC_South_plot_data <- data.frame(NFC_South_plot_val)

names(NFC_South_plot_data) <- "EPA"

NFC_South_plot_data$Team_Coverage <- NFC_South_labels

NFC_South_plot_data <- NFC_South_plot_data %>%
  dplyr::mutate(Team_Coverage = factor(Team_Coverage, levels = rev(NFC_South_levels)))

ggplot(NFC_South_plot_data, aes(x = `EPA`, y = `Team_Coverage`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Team_Coverage", option = "D") +
  labs(title = 'Stan Coverage Team Random Effects') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

##Look at effects using team_coverage_effects

ATL_total <- posterior$team_coverage_effects[,2,]
dim(ATL_total) <- c(15000, 7)
ATL_means <- apply(ATL_total, 2, mean) %>% data.frame()
names(ATL_means) <- "EPA"
ATL_means$col <- 1:7
ATL_means <- ATL_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
ATL_means_final <- ATL_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(ATL_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_South_plot_val <- c(posterior$team_coverage_effects[,2,2], 
                        posterior$team_coverage_effects[,2,4])

NFC_South_labels <- c(rep("ATL Cover 1 Man", 15000),
                      rep("ATL Cover 2 Zone", 15000))

CAR_total <- posterior$team_coverage_effects[,5,]
dim(CAR_total) <- c(15000, 7)
CAR_means <- apply(CAR_total, 2, mean) %>% data.frame()
names(CAR_means) <- "EPA"
CAR_means$col <- 1:7
CAR_means <- CAR_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
CAR_means_final <- CAR_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(CAR_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_South_plot_val <- c(NFC_South_plot_val,
                        posterior$team_coverage_effects[,5,2], 
                        posterior$team_coverage_effects[,5,7])

NFC_South_labels <- c(NFC_South_labels,
                      rep("CAR Cover 1 Man", 15000),
                      rep("CAR Cover 6 Zone", 15000))

NO_total <- posterior$team_coverage_effects[,22,]
dim(NO_total) <- c(15000, 7)
NO_means <- apply(NO_total, 2, mean) %>% data.frame()
names(NO_means) <- "EPA"
NO_means$col <- 1:7
NO_means <- NO_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
NO_means_final <- NO_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(NO_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_South_plot_val <- c(NFC_South_plot_val,
                        posterior$team_coverage_effects[,22,2], 
                        posterior$team_coverage_effects[,22,4])

NFC_South_labels <- c(NFC_South_labels,
                      rep("NO Cover 1 Man", 15000),
                      rep("NO Cover 2 Zone", 15000))

TB_total <- posterior$team_coverage_effects[,30,]
dim(TB_total) <- c(15000, 7)
TB_means <- apply(TB_total, 2, mean) %>% data.frame()
names(TB_means) <- "EPA"
TB_means$col <- 1:7
TB_means <- TB_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
TB_means_final <- TB_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(TB_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_South_plot_val <- c(NFC_South_plot_val,
                        posterior$team_coverage_effects[,30,2], 
                        posterior$team_coverage_effects[,30,4])

NFC_South_labels <- c(NFC_South_labels,
                      rep("TB Cover 1 Man", 15000),
                      rep("TB Cover 2 Zone", 15000))

##Order of plots: Worst to best, as before

NFC_South_levels <- ATL_means_final %>% dplyr::mutate(team = "ATL") %>%
  bind_rows(CAR_means_final %>% dplyr::mutate(team = "CAR")) %>%
  bind_rows(NO_means_final %>% dplyr::mutate(team = "NO")) %>%
  bind_rows(TB_means_final %>% dplyr::mutate(team = "TB")) %>%
  arrange(desc(EPA)) %>%
  dplyr::mutate(coverage = as.character(coverage)) %>%
  dplyr::mutate(team_coverage = paste0(team, " ", coverage)) %>%
  pull(team_coverage)

NFC_South_plot_data <- data.frame(NFC_South_plot_val)

names(NFC_South_plot_data) <- "EPA"

NFC_South_plot_data$Team_Coverage <- NFC_South_labels

NFC_South_plot_data <- NFC_South_plot_data %>%
  dplyr::mutate(Team_Coverage = factor(Team_Coverage, levels = rev(NFC_South_levels)))

ggplot(NFC_South_plot_data, aes(x = `EPA`, y = `Team_Coverage`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Team_Coverage", option = "D") +
  labs(title = 'NFC South Random Effects') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) + 
  xlim(-0.75, 0.75)

##Look at best and worst effects for each team in two divisions

##Look at NFC South and NFC North
##NFC South: ATL 2, CAR 5, NO 22, TB 30
##NFC North: CHI 6, DET 11, GB 12, MIN 20

CHI_total <- posterior$delta_defense_coverage[,6,]
dim(CHI_total) <- c(15000, 7)
CHI_means <- apply(CHI_total, 2, mean) %>% data.frame()
names(CHI_means) <- "EPA"
CHI_means$col <- 1:7
CHI_means <- CHI_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
CHI_means_final <- CHI_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(CHI_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_North_plot_val <- c(posterior$delta_defense_coverage[,6,3], 
                        posterior$delta_defense_coverage[,6,4])

NFC_North_labels <- c(rep("CHI Cover 2 Man", 15000),
                      rep("CHI Cover 2 Zone", 15000))

DET_total <- posterior$delta_defense_coverage[,11,]
dim(DET_total) <- c(15000, 7)
DET_means <- apply(DET_total, 2, mean) %>% data.frame()
names(DET_means) <- "EPA"
DET_means$col <- 1:7
DET_means <- DET_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
DET_means_final <- DET_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(DET_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_North_plot_val <- c(NFC_North_plot_val,
                        posterior$delta_defense_coverage[,11,2], 
                        posterior$delta_defense_coverage[,11,6])

NFC_North_labels <- c(NFC_North_labels,
                      rep("DET Cover 1 Man", 15000),
                      rep("DET Cover 4 Zone", 15000))

GB_total <- posterior$delta_defense_coverage[,12,]
dim(GB_total) <- c(15000, 7)
GB_means <- apply(GB_total, 2, mean) %>% data.frame()
names(GB_means) <- "EPA"
GB_means$col <- 1:7
GB_means <- GB_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
GB_means_final <- GB_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(GB_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_North_plot_val <- c(NFC_North_plot_val,
                        posterior$delta_defense_coverage[,12,6], 
                        posterior$delta_defense_coverage[,12,7])

NFC_North_labels <- c(NFC_North_labels,
                      rep("GB Cover 4 Zone", 15000),
                      rep("GB Cover 6 Zone", 15000))

MIN_total <- posterior$delta_defense_coverage[,20,]
dim(MIN_total) <- c(15000, 7)
MIN_means <- apply(MIN_total, 2, mean) %>% data.frame()
names(MIN_means) <- "EPA"
MIN_means$col <- 1:7
MIN_means <- MIN_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
MIN_means_final <- MIN_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(MIN_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_North_plot_val <- c(NFC_North_plot_val,
                        posterior$delta_defense_coverage[,20,5], 
                        posterior$delta_defense_coverage[,20,6])

NFC_North_labels <- c(NFC_North_labels,
                      rep("MIN Cover 3 Zone", 15000),
                      rep("MIN Cover 4 Zone", 15000))

##Order of plots: Worst to best, as before

NFC_North_levels <- CHI_means_final %>% dplyr::mutate(team = "CHI") %>%
  bind_rows(DET_means_final %>% dplyr::mutate(team = "DET")) %>%
  bind_rows(GB_means_final %>% dplyr::mutate(team = "GB")) %>%
  bind_rows(MIN_means_final %>% dplyr::mutate(team = "MIN")) %>%
  arrange(desc(EPA)) %>%
  dplyr::mutate(coverage = as.character(coverage)) %>%
  dplyr::mutate(team_coverage = paste0(team, " ", coverage)) %>%
  pull(team_coverage)

NFC_North_plot_data <- data.frame(NFC_North_plot_val)

names(NFC_North_plot_data) <- "EPA"

NFC_North_plot_data$Team_Coverage <- NFC_North_labels

NFC_North_plot_data <- NFC_North_plot_data %>%
  dplyr::mutate(Team_Coverage = factor(Team_Coverage, levels = rev(NFC_North_levels)))

ggplot(NFC_North_plot_data, aes(x = `EPA`, y = `Team_Coverage`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Team_Coverage", option = "D") +
  labs(title = 'Stan Coverage Team Random Effects') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

##Look at effects using team_coverage_effects

CHI_total <- posterior$team_coverage_effects[,6,]
dim(CHI_total) <- c(15000, 7)
CHI_means <- apply(CHI_total, 2, mean) %>% data.frame()
names(CHI_means) <- "EPA"
CHI_means$col <- 1:7
CHI_means <- CHI_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
CHI_means_final <- CHI_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(CHI_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_North_plot_val <- c(posterior$team_coverage_effects[,6,2], 
                        posterior$team_coverage_effects[,6,4])

NFC_North_labels <- c(rep("CHI Cover 1 Man", 15000),
                      rep("CHI Cover 2 Zone", 15000))

DET_total <- posterior$team_coverage_effects[,11,]
dim(DET_total) <- c(15000, 7)
DET_means <- apply(DET_total, 2, mean) %>% data.frame()
names(DET_means) <- "EPA"
DET_means$col <- 1:7
DET_means <- DET_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
DET_means_final <- DET_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(DET_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_North_plot_val <- c(NFC_North_plot_val,
                        posterior$team_coverage_effects[,11,2], 
                        posterior$team_coverage_effects[,11,7])

NFC_North_labels <- c(NFC_North_labels,
                      rep("DET Cover 1 Man", 15000),
                      rep("DET Cover 6 Zone", 15000))

GB_total <- posterior$team_coverage_effects[,12,]
dim(GB_total) <- c(15000, 7)
GB_means <- apply(GB_total, 2, mean) %>% data.frame()
names(GB_means) <- "EPA"
GB_means$col <- 1:7
GB_means <- GB_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
GB_means_final <- GB_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(GB_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_North_plot_val <- c(NFC_North_plot_val,
                        posterior$team_coverage_effects[,12,2], 
                        posterior$team_coverage_effects[,12,7])

NFC_North_labels <- c(NFC_North_labels,
                      rep("GB Cover 1 Man", 15000),
                      rep("GB Cover 6 Zone", 15000))

MIN_total <- posterior$team_coverage_effects[,20,]
dim(MIN_total) <- c(15000, 7)
MIN_means <- apply(MIN_total, 2, mean) %>% data.frame()
names(MIN_means) <- "EPA"
MIN_means$col <- 1:7
MIN_means <- MIN_means %>% left_join(coverage_ids, by = c("col" = "coverage_numeric"))
MIN_means_final <- MIN_means %>%
  dplyr::top_n(1, wt = EPA) %>%
  bind_rows(MIN_means %>%
              dplyr::arrange(EPA) %>%
              dplyr::top_n(-1, wt = EPA))

NFC_North_plot_val <- c(NFC_North_plot_val,
                        posterior$team_coverage_effects[,20,2], 
                        posterior$team_coverage_effects[,20,4])

NFC_North_labels <- c(NFC_North_labels,
                      rep("MIN Cover 1 Man", 15000),
                      rep("MIN Cover 2 Zone", 15000))

##Order of plots: Worst to best, as before

NFC_North_levels <- CHI_means_final %>% dplyr::mutate(team = "CHI") %>%
  bind_rows(DET_means_final %>% dplyr::mutate(team = "DET")) %>%
  bind_rows(GB_means_final %>% dplyr::mutate(team = "GB")) %>%
  bind_rows(MIN_means_final %>% dplyr::mutate(team = "MIN")) %>%
  arrange(desc(EPA)) %>%
  dplyr::mutate(coverage = as.character(coverage)) %>%
  dplyr::mutate(team_coverage = paste0(team, " ", coverage)) %>%
  pull(team_coverage)

NFC_North_plot_data <- data.frame(NFC_North_plot_val)

names(NFC_North_plot_data) <- "EPA"

NFC_North_plot_data$Team_Coverage <- NFC_North_labels

NFC_North_plot_data <- NFC_North_plot_data %>%
  dplyr::mutate(Team_Coverage = factor(Team_Coverage, levels = rev(NFC_North_levels)))

ggplot(NFC_North_plot_data, aes(x = `EPA`, y = `Team_Coverage`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Team_Coverage", option = "D") +
  labs(title = 'NFC North Random Effects') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlim(-0.6, 0.6)
