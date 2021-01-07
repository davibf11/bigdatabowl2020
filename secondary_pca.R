library(gganimate)
library(cowplot)
library(repr)
library(mclust)
library(factoextra)
library(tidyverse)

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")

##Input data from secondary_data.R script; bind all rows together from that run

secondary_df <- suppressMessages(readRDS("../input/secondary-df-new/secondary_df_new.rds")) %>%
    janitor::clean_names() %>%
    dplyr::filter_all(all_vars(!is.na(.))) %>%
    dplyr::filter_all(all_vars(!is.nan(.))) %>%
    dplyr::filter_all(all_vars(!is.infinite(.)))

df_players <- read_csv("../input/nfl-big-data-bowl-2021/players.csv",
                      col_types = cols()) %>%
            janitor::clean_names()

##Create PCA to use in xgboost

secondary_use <- secondary_df %>% dplyr::select(-game_id, -play_id, -nfl_id)
secondary_extra <- secondary_df %>% dplyr::select(game_id, play_id, nfl_id)

secondary_use <- scale(secondary_use)
secondary_pca <- prcomp(secondary_use, scale = TRUE)

fviz_eig(secondary_pca, ncp = 20)

secondary_eig_val <- get_eigenvalue(secondary_pca)
secondary_eig_val %>% dplyr::filter(variance.percent > 4) %>% tail(1) ##5 variables to cluster on

secondary_pca_coords <- get_pca_ind(secondary_pca)$coord[,1:5] %>% data.frame()
names(secondary_pca_coords) <- c("sec_pca_1", "sec_pca_2", "sec_pca_3", "sec_pca_4", "sec_pca_5")

secondary_pca_coords <- secondary_extra %>% bind_cols(secondary_pca_coords)

saveRDS(secondary_pca_coords, "secondary_pca_coords.rds")
