#Loading pre-installed libraries
library(tidyverse)
library(factoextra)
library(ggplot2)
library(gganimate)
library(cowplot)
library(repr)


#turning off warnings
options(warn=-1)

#setting plot width and height
options(repr.plot.width=10, repr.plot.height = 10)

##reading in non-tracking data

#includes schedule info for games
df_games <- read_csv("../input/nfl-big-data-bowl-2021/games.csv",
                    col_types = cols()) %>%
            janitor::clean_names()

#includes play-by-play info on specific plays
df_plays <- read_csv("../input/nfl-big-data-bowl-2021/plays.csv",
                    col_types = cols()) %>%
            janitor::clean_names()

#includes background info for players
df_players <- read_csv("../input/nfl-big-data-bowl-2021/players.csv",
                      col_types = cols()) %>%
            janitor::clean_names()

##read in coverage data
df_coverages <- read_csv("../input/nfl-big-data-bowl-2021-bonus/coverages_week1.csv",
                         col_types = cols()) %>%
                janitor::clean_names()

##read in secondary pca coordinates
df_tracking_pca <- readRDS("../input/df-tracking-pca-init/df_tracking_pca.rds")

##Perform PCA on simplified route data

##Get simplified route data

start <- Sys.time()

##Get variable means and sds for later
pca_data_small <- df_tracking_pca %>% dplyr::select(-game_id, -play_id, -nfl_id)

pca_means <- apply(pca_data_small, 2, mean)
pca_sds <- apply(pca_data_small, 2, sd)

pca_data_small <- scale(pca_data_small)

##Perform PCA first, then select first $m$ features to cluster on

tracking_pca <- prcomp(pca_data_small, scale = TRUE)

#fviz_eig(tracking_pca, ncp = 14)

#eig_val <- get_eigenvalue(tracking_pca)

#eig_val

##Use first two loadings: explain 93.4% of variation.
#Could use 3 and get up to 98.3 percent of variation, but likely not worth it

tracking_pca_coords <- get_pca_ind(tracking_pca)$coord

##Get PCA values for each game_id, play_id, nfl_id

df_tracking_pca <- df_tracking_pca %>%
    dplyr::select(game_id, play_id, nfl_id)

df_tracking_pca$route_pca_1 <- tracking_pca_coords[,1]
df_tracking_pca$route_pca_2 <- tracking_pca_coords[,2]

end <- Sys.time()

print(end - start)

##Save resulting model for use in prediction later

saveRDS(df_tracking_pca, "df_tracking_pca.rds")
