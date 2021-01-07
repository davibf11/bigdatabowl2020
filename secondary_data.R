library(tidyverse) # metapackage of all tidyverse packages

library(gganimate)
library(cowplot)
library(repr)

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")

##some user-defined functions

angle_diff_abs <- function(theta_1, theta_2) {
    temp <- (theta_1 - theta_2) %% 360
    out <- ifelse(temp <= 180, temp, 360 - temp)
    return(out)
}

df_games <- suppressMessages(read_csv("../input/nfl-big-data-bowl-2021/games.csv", col_types = cols())) %>%
    janitor::clean_names()
df_players <- suppressMessages(read_csv("../input/nfl-big-data-bowl-2021/players.csv", col_types = cols())) %>%
    janitor::clean_names()
df_plays <- suppressMessages(read_csv("../input/nfl-big-data-bowl-2021/plays.csv", col_types = cols())) %>%
    janitor::clean_names()

# secondary_df <- readRDS("../input/secondary-df-1/secondary_df.rds")

##CHANGE THE WEEK to get data for all other weeks

df_week <- suppressMessages(read_csv("../input/nfl-big-data-bowl-2021/week3.csv", col_types = cols())) %>%
    janitor::clean_names()

##Adjust x and y coordinates

df_week <- df_week %>%
                mutate(x = ifelse(play_direction == "left", 120-x, x),
                       y = ifelse(play_direction == "left", 160/3 - y, y),
                       dir = ifelse(play_direction == "left", dir - 180, dir),
                       o = ifelse(play_direction == "left", o - 180, o)) %>%
                mutate(o = o %% 360,
                       dir = dir %% 360) %>%
                inner_join(df_games %>% inner_join(df_plays, by = "game_id"),
                              by = c("game_id", "play_id")) %>%
                dplyr::mutate(side_of_ball = ifelse(
                    #if tracked player is home and home has ball
                    ((team == "home") &
                     (possession_team == home_team_abbr)) |
                    #if tracked player is away and away has ball
                    ((team == "away") &
                     (possession_team == visitor_team_abbr)), "offense", "defense"),
                    #defining defensive team
                    defensive_team = ifelse(possession_team == home_team_abbr, visitor_team_abbr, home_team_abbr))
                    
##ball_snap: point at which the ball is snapped
##pass_forward: point at which the ball is thrown

pass_thrown_events <- c('pass_forward',
                        'pass_shovel')

pass_arrival_events <- c('pass_outcome_caught',
                        'pass_arrived',
                        'pass_outcome_incomplete',
                        'pass_outcome_interception',
                        'pass_outcome_touchdown')

secondary <- c('CB', 'DB', 'FS', 'S', 'SS')

offense_positions <- c('FB', 'HB', 'QB', 'RB', 'TE', 'WR')

defense_positions <- c('ILB', 'OLB', 'SS', 'CB', 'FS', 'LB', 'DB', 'S', 'MLB')

##Filter out plays with kickers, punters, long snappers

df_week_badids <- df_week %>%
    dplyr::filter(position %in% c('K', 'P', 'LS')) %>%
    dplyr::select(game_id, play_id) %>% distinct()

df_week <- df_week %>%
    dplyr::anti_join(df_week %>%
                        dplyr::filter(game_id %in% df_week_badids$game_id,
                                      play_id %in% df_week_badids$play_id))

##Filter out spike plays

df_week_spikes <- df_week %>%
    dplyr::filter(event == 'qb_spike') %>%
    dplyr::select(game_id, play_id) %>% distinct()

df_week <- df_week %>%
    dplyr::anti_join(df_week %>%
                        dplyr::filter(game_id %in% df_week_spikes$game_id,
                                      play_id %in% df_week_spikes$play_id))

##Nest data; turn rows into game_id, play_id, and relevant data

df_week_nest <- df_week %>%
    dplyr::select(game_id,
                  play_id,
                  x, 
                  y, 
                  s,
                  dis,
                  o,
                  dir,
                  event,
                  nfl_id, 
                  position, 
                  frame_id,
                  team, 
                  play_direction,
                  route, 
                  play_description, 
                  quarter, 
                  down, 
                  yards_to_go, 
                  possession_team, 
                  play_type, 
                  yardline_side, 
                  yardline_number,
                  offense_formation, 
                  personnel_o, 
                  defenders_in_the_box, 
                  number_of_pass_rushers,
                  personnel_d, 
                  type_dropback, 
                  absolute_yardline_number,
                  penalty_codes, 
                  pass_result, 
                  offense_play_result, 
                  play_result, 
                  epa, 
                  is_defensive_pi, 
                  side_of_ball, 
                  defensive_team) %>%
    tidyr::nest(play_data = c(x, 
                              y, 
                              s, 
                              dis, 
                              o, 
                              dir, 
                              event, 
                              nfl_id, 
                              position, 
                              frame_id, 
                              team, 
                              play_direction, 
                              route, 
                              play_description, 
                              quarter, 
                              down, 
                              yards_to_go, 
                              possession_team, 
                              play_type, 
                              yardline_side, 
                              yardline_number, 
                              offense_formation, 
                              personnel_o, 
                              defenders_in_the_box, 
                              number_of_pass_rushers, 
                              personnel_d, 
                              type_dropback, 
                              absolute_yardline_number, 
                              penalty_codes, 
                              pass_result, 
                              offense_play_result, 
                              play_result, 
                              epa, 
                              is_defensive_pi, 
                              side_of_ball, 
                              defensive_team))

##Function which will do the following for each play:
##Take in game_id, play_id
##Original df has the following variables:
##game_id, play_id, nfl_id of each secondary defender
##play_group: 1 if pre-snap, 2 if post-snap, pre-throw, 3 if post-throw
##for each secondary defender: x_db, y_db, s_db, o_db, dir_db, db_facing_los (1 or 0) for each frame
##  mean_off_dist (mean distance from each offensive player), mean_def_dist (mean distance from each defensive player),
##  dist_nearest_offense (distance to nearest offensive player), dist_nearest_defense (distance to nearest defensive player),
##  dir_angle_diff_offense (difference in degrees of the direction of motion between the player and nearest offensive player),
##  o_angle_diff_offense (same, but orientation angles),
##  offense_min_distance (closest offensive player's shortest distance to another defensive player),
##  off_dist_ratio (dist_nearest_offense / offense_min_distance)
##Returned df has the following variables, grouped and aggregated:

get_secondary_data <- function(tibble_in) {
        
    ##Obtain some of the frames for grouping later
    
    snap_frame <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        pull(frame_id) %>% unique() %>% min()
    
    throw_frame <- tibble_in %>%
        dplyr::filter(event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'tackle', 'qb_spike')) %>%
        pull(frame_id) %>% unique() %>% min()
    
#     arrive_frame <- tibble_in %>%
#         dplyr::filter(event %in% c('pass_outcome_caught', 'pass_arrived', 'pass_outcome_incomplete', 'pass_outcome_interception', 'pass_outcome_touchdown')) %>%
#         pull(frame_id) %>% unique() %>% min()
    
    tibble_in <- tibble_in %>%
        dplyr::mutate(play_group = case_when(
            frame_id <= snap_frame ~ 1,
            frame_id <= throw_frame ~ 2,
#             frame_id <= arrive_frame ~ 3,
            TRUE ~ 3))
    
    ##Identify nfl_ids for players which will be relevant later
    
    secondary_ids <- tibble_in %>%
        dplyr::filter(position %in% secondary) %>%
        pull(nfl_id) %>% unique()
    
    defense_ids <- tibble_in %>%
        dplyr::filter(position %in% defense_positions) %>%
        pull(nfl_id) %>% unique()
    
    offense_ids <- tibble_in %>%
        dplyr::filter(position %in% offense_positions) %>%
        pull(nfl_id) %>% unique()
    
    if(length(offense_ids) > 1 && length(defense_ids) > 1) {
    
    ##make df of offense_ids for later use
    offense_ids_df <- data.frame(1:length(offense_ids))
    names(offense_ids_df) <- "min_index"
    offense_ids_df$nfl_id <- offense_ids
    
    defender_df_list <- list()
    
    for(i in 1:length(defense_ids)) {
            
        ###################################################
        #####Calculate everything for first secondary defender in case there is only one
        #get the df for defender with relevant x,y coordinates for calculating distance
        defender_df_temp <- tibble_in %>%
            dplyr::filter(nfl_id == defense_ids[i]) %>%
            dplyr::select(x, y, frame_id) %>%
            dplyr::rename(x_db = x,
                          y_db = y)
        
        ########
        ####First, calculate nearest offensive player info
        offense_dfs <- list()
        
        for(m in 1:length(offense_ids)) {
            
            #calculate distance to each offensive player at each frame
                offense_dfs[[m]] <- defender_df_temp %>%
                    dplyr::left_join(tibble_in %>%
                        dplyr::filter(nfl_id == offense_ids[m]) %>%
                        dplyr::select(nfl_id, x, y, frame_id), by = "frame_id") %>%
                    drop_na() %>%
                    dplyr::mutate("dist_to_offense_{m}" := sqrt((x - x_db)^2 + (y - y_db)^2)) %>%
                    dplyr::select(-x_db, -y_db, -x, -y)
            
        }
        
        #join up all distances
        offense_dist_df <- offense_dfs[[1]] %>% dplyr::select(-nfl_id)
        for(m in 2:length(offense_dfs)) {
            offense_dist_df <- offense_dist_df %>%
                dplyr::inner_join(offense_dfs[[m]] %>% dplyr::select(-nfl_id),
                                  by = "frame_id")
        }
        
        #find which distance is smallest by using function which.min(). also, get mean distance to all offensive players
        offense_dist_matrix <- as.matrix(offense_dist_df)
        offense_dist_df$min_index = apply(offense_dist_matrix[,-1], 1, which.min)
        offense_dist_df$mean_off_dist = apply(offense_dist_matrix[,-1], 1, mean)
        
        #save only the frame number and the nfl_id of player who is closest, as well as average dist
        offense_dist_df <- offense_dist_df %>%
            dplyr::select(frame_id, min_index, mean_off_dist) %>%
            dplyr::left_join(offense_ids_df, by = "min_index") %>%
            dplyr::select(-min_index) %>%
            dplyr::left_join(tibble_in %>%
                                dplyr::select(frame_id, nfl_id, x, y, o, dir), 
                            by = c("nfl_id" = "nfl_id", 
                                  "frame_id" = "frame_id")) %>%
            dplyr::rename(nfl_id_off = nfl_id,
                         x_off = x,
                         y_off = y,
                         o_off = o,
                         dir_off = dir)
        
        ########
        ####Next, calculate nearest defensive player info
        defense_dfs <- list()
        
        defense_other_ids <- setdiff(defense_ids, defense_ids[i])
        
        defense_other_ids_df <- data.frame(1:length(defense_other_ids))
        names(defense_other_ids_df) <- "min_index"
        defense_other_ids_df$nfl_id <- defense_other_ids
        
        for(m in 1:length(defense_other_ids)) {
            
            #calculate distance to each offensive player at each frame
            defense_dfs[[m]] <- defender_df_temp %>%
                dplyr::left_join(tibble_in %>%
                    dplyr::filter(nfl_id == defense_other_ids[m]) %>%
                    dplyr::select(nfl_id, x, y, frame_id), by = "frame_id") %>%
                drop_na() %>%
                dplyr::mutate("dist_to_defender_{m}" := sqrt((x - x_db)^2 + (y - y_db)^2)) %>%
                dplyr::select(-x_db, -y_db, -x, -y)
            
        }
        
        #join up all distances
        defense_dist_df <- defense_dfs[[1]] %>% dplyr::select(-nfl_id)
        for(m in 2:length(defense_dfs)) {
            defense_dist_df <- defense_dist_df %>%
                dplyr::inner_join(defense_dfs[[m]] %>% dplyr::select(-nfl_id),
                                  by = "frame_id")
        }
        #find which distance is smallest by using function which.min(). also, get mean distance to all defensive players
        defense_dist_matrix <- as.matrix(defense_dist_df)
        defense_dist_df$min_index = apply(defense_dist_matrix[,-1], 1, which.min)
        defense_dist_df$mean_def_dist = apply(defense_dist_matrix[,-1], 1, mean)
        
        #save only the frame number and the nfl_id of player who is closest, as well as average dist
        defense_dist_df <- defense_dist_df %>%
            dplyr::select(frame_id, min_index, mean_def_dist) %>%
            dplyr::left_join(defense_other_ids_df, by = "min_index") %>%
            dplyr::select(-min_index) %>%
            dplyr::left_join(tibble_in %>%
                                dplyr::select(frame_id, nfl_id, x, y), 
                            by = c("nfl_id" = "nfl_id", 
                                  "frame_id" = "frame_id")) %>%
            dplyr::rename(nfl_id_def = nfl_id,
                         x_def = x,
                         y_def = y)
        
        ########
        ####Last, compute the nearest offensive players' distances to their nearest defender
        
        relevant_offense_ids <- offense_dist_df %>% 
            dplyr::pull(nfl_id_off) %>%
            unique()
        
        ##Create df for first offensive player
        offense_df_temp <- tibble_in %>%
            dplyr::filter(nfl_id == relevant_offense_ids[1]) %>%
            dplyr::select(x, y, frame_id) %>%
            dplyr::rename(x_player = x,
                          y_player = y)
        
        defense_dfs <- list()
        
        for(m in 1:length(defense_ids)) {
            
            #calculate distance to each offensive player at each frame
            defense_dfs[[m]] <- offense_df_temp %>%
                dplyr::left_join(tibble_in %>%
                                dplyr::filter(nfl_id == defense_ids[m]) %>%
                                dplyr::select(nfl_id, x, y, frame_id), by = "frame_id") %>%
                drop_na() %>%
                dplyr::mutate("dist_to_defender_{m}" := sqrt((x - x_player)^2 + (y - y_player)^2)) %>%
                dplyr::select(-x_player, -y_player, -x, -y)
            
        }
        
        #join up all distances
        defense_dist_to_off_df <- defense_dfs[[1]] %>% dplyr::select(-nfl_id)
        for(m in 2:length(defense_dfs)) {
            defense_dist_to_off_df <- defense_dist_to_off_df %>%
                dplyr::inner_join(defense_dfs[[m]] %>% dplyr::select(-nfl_id),
                                  by = "frame_id")
        }
        #find which distance is smallest by using function which.min(). also, get mean distance to all defensive players
        defense_dist_matrix <- as.matrix(defense_dist_to_off_df)
        defense_dist_to_off_df$offense_min_distance = apply(defense_dist_matrix[,-1], 1, min)
        
        #save only the frame number and the nfl_id of offensive player, as well as dist to closest defender
        offense_dist_to_def_df <- defense_dist_to_off_df %>%
            dplyr::select(frame_id, offense_min_distance)
        offense_dist_to_def_df$nfl_id <- relevant_offense_ids[1]
        
        if(length(relevant_offense_ids) > 1) {
            for(j in 2:length(relevant_offense_ids)) {
                
                ##Create df for next offensive player
                offense_df_temp <- tibble_in %>%
                    dplyr::filter(nfl_id == relevant_offense_ids[j]) %>%
                    dplyr::select(x, y, frame_id) %>%
                    dplyr::rename(x_player = x,
                                  y_player = y)
            
                defense_dfs <- list()
                
                for(m in 1:length(defense_ids)) {
                    
                    #calculate distance to each offensive player at each frame
                    defense_dfs[[m]] <- offense_df_temp %>%
                    dplyr::left_join(tibble_in %>%
                                    dplyr::filter(nfl_id == defense_ids[m]) %>%
                                    dplyr::select(nfl_id, x, y, frame_id), by = "frame_id") %>%
                    drop_na() %>%
                    dplyr::mutate("dist_to_defender_{m}" := sqrt((x - x_player)^2 + (y - y_player)^2)) %>%
                    dplyr::select(-x_player, -y_player, -x, -y)
                    
                }
        
                #join up all distances
                defense_dist_to_off_df <- defense_dfs[[1]] %>% dplyr::select(-nfl_id)
                for(m in 2:length(defense_dfs)) {
                    defense_dist_to_off_df <- defense_dist_to_off_df %>%
                        dplyr::inner_join(defense_dfs[[m]] %>% dplyr::select(-nfl_id),
                                          by = "frame_id")
                }
                #find which distance is smallest by using function which.min(). also, get mean distance to all defensive players
                defense_dist_matrix <- as.matrix(defense_dist_to_off_df)
                defense_dist_to_off_df$offense_min_distance = apply(defense_dist_matrix[,-1], 1, min)
                
                #add the frame number and the nfl_id of offensive player, as well as dist to closest defender
                offense_dist_df_temp <- defense_dist_to_off_df %>%
                    dplyr::select(frame_id, offense_min_distance)
                offense_dist_df_temp$nfl_id <- relevant_offense_ids[j]
                
                offense_dist_to_def_df <- offense_dist_to_def_df %>%
                    bind_rows(offense_dist_df_temp)
                
            }
        }
        
        ########
        ####Now, for this play, obtain the relevant player information as well as the information of the closest offensive player
        defender_df_list[[i]] <- tibble_in %>%
            dplyr::filter(nfl_id == defense_ids[i]) %>%
            dplyr::select(frame_id, nfl_id, x, y, s, o, dir, play_group) %>%
            dplyr::rename(x_db = x,
                          y_db = y,
                          s_db = s,
                          o_db = o,
                          dir_db = dir) %>%
            dplyr::mutate(db_facing_los = ifelse((o_db <= 315) & (o_db >= 225), 1, 0)) %>%
            dplyr::left_join(offense_dist_df, by = c("frame_id" = "frame_id")) %>%
            dplyr::left_join(defense_dist_df, by = c("frame_id" = "frame_id")) %>%
            dplyr::left_join(offense_dist_to_def_df, by = c("frame_id" = "frame_id", "nfl_id_off" = "nfl_id")) %>%
            #Compute relevant statistics
            dplyr::mutate(dist_nearest_offense = sqrt((x_db - x_off)^2 + (y_db - y_off)^2),
                          dist_nearest_defense = sqrt((x_db - x_def)^2 + (y_db - y_def)^2),
                          dir_angle_diff_offense = angle_diff_abs(dir_db, dir_off),
                          o_angle_diff_offense = angle_diff_abs(o_db, o_off),
                          off_dist_ratio = dist_nearest_offense/offense_min_distance) %>%
            ##Finally, save the variables of importance with which we will cluster
            dplyr::select(nfl_id,
                          frame_id,
                          play_group,
                          x_db,
                          y_db,
                          s_db,
                          o_db,
                          dir_db,
                          db_facing_los,
                          mean_off_dist,
                          mean_def_dist,
                          dist_nearest_offense,
                          dist_nearest_defense,
                          dir_angle_diff_offense,
                          o_angle_diff_offense,
                          offense_min_distance,
                          off_dist_ratio)
        
    }
    
    defender_df <- defender_df_list[[1]]
    
    if(length(defense_ids) > 1) {
        
        for(i in 2:length(defense_ids)) {
            
            defender_df <- defender_df %>%
                bind_rows(defender_df_list[[i]])
            
        }
        
    }
    
    ##Now: group by play_group and calculate relevant statistics
    
    ##First: need to work with sin and cosine of orientation for variance purposes.
    
    defender_df <- defender_df %>%
        dplyr::mutate(o_sin = sin(o_db * pi / 180),
                      o_cos = cos(o_db * pi / 180))
    
    defender_df_grouped <- defender_df %>%
        dplyr::filter(play_group == 1) %>%
        dplyr::group_by(nfl_id) %>%
        dplyr::summarize(x_var_1 = var(x_db),
                         y_var_1 = var(y_db),
                         s_var_1 = var(s_db),
                         o_sin_var_1 = var(o_sin),
                         o_cos_var_1 = var(o_cos),
                         mean_min_o_dis_1 = mean(dist_nearest_offense),
                         var_min_o_dis_1 = var(dist_nearest_offense),
                         mean_min_d_dis_1 = mean(dist_nearest_defense),
                         var_min_d_dis_1 = var(dist_nearest_defense),
                         mean_avg_o_dis_1 = mean(mean_off_dist),
                         mean_avg_d_dis_1 = mean(mean_def_dist),
                         mean_dir_angle_1 = mean(dir_angle_diff_offense),
                         var_dir_angle_1 = var(dir_angle_diff_offense),
                         mean_o_angle_1 = mean(o_angle_diff_offense),
                         var_o_angle_1 = var(o_angle_diff_offense),
                         pct_los_1 = mean(db_facing_los),
                         mean_dist_ratio_1 = mean(off_dist_ratio),
                         var_dist_ratio_1 = var(off_dist_ratio),
                         .groups = "drop_last") %>%
        ungroup() %>%
        inner_join(defender_df %>%
            dplyr::filter(play_group == 2) %>%
            dplyr::group_by(nfl_id) %>%
            dplyr::summarize(x_var_2 = var(x_db),
                             y_var_2 = var(y_db),
                             s_var_2 = var(s_db),
                             o_sin_var_2 = var(o_sin),
                             o_cos_var_2 = var(o_cos),
                             mean_min_o_dis_2 = mean(dist_nearest_offense),
                             var_min_o_dis_2 = var(dist_nearest_offense),
                             mean_min_d_dis_2 = mean(dist_nearest_defense),
                             var_min_d_dis_2 = var(dist_nearest_defense),
                             mean_avg_o_dis_2 = mean(mean_off_dist),
                             mean_avg_d_dis_2 = mean(mean_def_dist),
                             mean_dir_angle_2 = mean(dir_angle_diff_offense),
                             var_dir_angle_2 = var(dir_angle_diff_offense),
                             mean_o_angle_2 = mean(o_angle_diff_offense),
                             var_o_angle_2 = var(o_angle_diff_offense),
                             pct_los_2 = mean(db_facing_los),
                             mean_dist_ratio_2 = mean(off_dist_ratio),
                             var_dist_ratio_2 = var(off_dist_ratio),
                             .groups = "drop_last") %>%
                 ungroup(), by = c("nfl_id" = "nfl_id")) %>%
        inner_join(defender_df %>%
            dplyr::filter(play_group == 3) %>%
            dplyr::group_by(nfl_id) %>%
            dplyr::summarize(x_var_3 = var(x_db),
                             y_var_3 = var(y_db),
                             s_var_3 = var(s_db),
                             o_sin_var_3 = var(o_sin),
                             o_cos_var_3 = var(o_cos),
                             mean_min_o_dis_3 = mean(dist_nearest_offense),
                             var_min_o_dis_3 = var(dist_nearest_offense),
                             mean_min_d_dis_3 = mean(dist_nearest_defense),
                             var_min_d_dis_3 = var(dist_nearest_defense),
                             mean_avg_o_dis_3 = mean(mean_off_dist),
                             mean_avg_d_dis_3 = mean(mean_def_dist),
                             mean_dir_angle_3 = mean(dir_angle_diff_offense),
                             var_dir_angle_3 = var(dir_angle_diff_offense),
                             mean_o_angle_3 = mean(o_angle_diff_offense),
                             var_o_angle_3 = var(o_angle_diff_offense),
                             pct_los_3 = mean(db_facing_los),
                             mean_dist_ratio_3 = mean(off_dist_ratio),
                             var_dist_ratio_3 = var(off_dist_ratio),
                             .groups = "drop_last") %>%
                 ungroup(), by = c("nfl_id" = "nfl_id")) %>%
        inner_join(defender_df %>%
            dplyr::filter(play_group <= 2) %>%
            dplyr::group_by(nfl_id) %>%
            dplyr::summarize(x_var_4 = var(x_db),
                             y_var_4 = var(y_db),
                             s_var_4 = var(s_db),
                             o_sin_var_4 = var(o_sin),
                             o_cos_var_4 = var(o_cos),
                             mean_min_o_dis_4 = mean(dist_nearest_offense),
                             var_min_o_dis_4 = var(dist_nearest_offense),
                             mean_min_d_dis_4 = mean(dist_nearest_defense),
                             var_min_d_dis_4 = var(dist_nearest_defense),
                             mean_avg_o_dis_4 = mean(mean_off_dist),
                             mean_avg_d_dis_4 = mean(mean_def_dist),
                             mean_dir_angle_4 = mean(dir_angle_diff_offense),
                             var_dir_angle_4 = var(dir_angle_diff_offense),
                             mean_o_angle_4 = mean(o_angle_diff_offense),
                             var_o_angle_4 = var(o_angle_diff_offense),
                             pct_los_4 = mean(db_facing_los),
                             mean_dist_ratio_4 = mean(off_dist_ratio),
                             var_dist_ratio_4 = var(off_dist_ratio),
                             .groups = "drop_last") %>%
                 ungroup(), by = c("nfl_id" = "nfl_id")) %>%
        inner_join(defender_df %>%
            dplyr::filter(play_group >= 2) %>%
            dplyr::group_by(nfl_id) %>%
            dplyr::summarize(x_var_5 = var(x_db),
                             y_var_5 = var(y_db),
                             s_var_5 = var(s_db),
                             o_sin_var_5 = var(o_sin),
                             o_cos_var_5 = var(o_cos),
                             mean_min_o_dis_5 = mean(dist_nearest_offense),
                             var_min_o_dis_5 = var(dist_nearest_offense),
                             mean_min_d_dis_5 = mean(dist_nearest_defense),
                             var_min_d_dis_5 = var(dist_nearest_defense),
                             mean_avg_o_dis_5 = mean(mean_off_dist),
                             mean_avg_d_dis_5 = mean(mean_def_dist),
                             mean_dir_angle_5 = mean(dir_angle_diff_offense),
                             var_dir_angle_5 = var(dir_angle_diff_offense),
                             mean_o_angle_5 = mean(o_angle_diff_offense),
                             var_o_angle_5 = var(o_angle_diff_offense),
                             pct_los_5 = mean(db_facing_los),
                             mean_dist_ratio_5 = mean(off_dist_ratio),
                             var_dist_ratio_5 = var(off_dist_ratio),
                             .groups = "drop_last") %>%
                 ungroup(), by = c("nfl_id" = "nfl_id")) 
        
    } else {defender_df_grouped <- NULL}
    
    return(defender_df_grouped)
    
}

start <- Sys.time()

secondary_df <- df_week_nest %>%
    dplyr::mutate(secondary_data = purrr::map(play_data, get_secondary_data)) %>%
    dplyr::select(-play_data) %>%
    tidyr::unnest(cols = c(secondary_data))

end <- Sys.time()

end - start

saveRDS(secondary_df, "secondary_df_3.rds")
