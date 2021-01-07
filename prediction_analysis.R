#Loading pre-installed libraries
library(tidyverse)
library(factoextra)
library(ggplot2)
library(gganimate)
library(cowplot)
library(repr)
library(lubridate)


#turning off warnings
options(warn=-1)

#setting plot width and height
options(repr.plot.width=10, repr.plot.height = 10)

##gg_field function to make everything more easily visible

gg_field <- function(yardmin=0, yardmax=120, buffer=5, direction="horiz",
                     field_color="forestgreen",line_color="white",
                     sideline_color=field_color, endzone_color="darkgreen"){
  
  ## field dimensions (units=yards)
  xmin <- 0
  xmax <- 120
  
  ymin <- 0
  ymax <- 53.33
  
  
  ## distance from sideline to hash marks in middle (70 feet, 9 inches)
  hash_dist <- (70*12+9)/36
  
  ## yard lines locations (every 5 yards) 
  yd_lines <- seq(15,105,by=5)
  
  ## hash mark locations (left 1 yard line to right 1 yard line)
  yd_hash <- 11:109
  
  ## field number size
  num_size <- 5
  
  ## rotate field numbers with field direction
  ## first element is for right-side up numbers, second for upside-down
  angle_vec <- switch(direction, "horiz" = c(0, 180), "vert" = c(270, 90))
  num_adj <- switch(direction, "horiz" = c(-1, 1), "vert" = c(1, -1))
  
  ## list of annotated geoms
  p <- list(
    
    ## add field background 
    annotate("rect", xmin=xmin, xmax=xmax, ymin=ymin-buffer, ymax=ymax+buffer, 
             fill=field_color),
    
    ## add end zones
    annotate("rect", xmin=xmin, xmax=xmin+10, ymin=ymin, ymax=ymax, fill=endzone_color),
    annotate("rect", xmin=xmax-10, xmax=xmax, ymin=ymin, ymax=ymax, fill=endzone_color),
    
    ## add yardlines every 5 yards
    annotate("segment", x=yd_lines, y=ymin, xend=yd_lines, yend=ymax,
             col=line_color),
    
    ## add thicker lines for endzones, midfield, and sidelines
    annotate("segment",x=c(0,10,60,110,120), y=ymin, xend=c(0,10,60,110,120), yend=ymax,
             lwd=1.3, col=line_color),
    annotate("segment",x=0, y=c(ymin, ymax), xend=120, yend=c(ymin, ymax),
             lwd=1.3, col=line_color) ,
    
    ## add field numbers (every 10 yards)
    ## field numbers are split up into digits and zeros to avoid being covered by yard lines
    ## numbers are added separately to allow for flexible ggplot stuff like facetting
    
    ## 0
    annotate("text",x=seq(20,100,by=10) + num_adj[2], y=ymin+12, label=0, angle=angle_vec[1],
             col=line_color, size=num_size),
    
    ## 1
    annotate("text",label=1,x=c(20,100) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 2
    annotate("text",label=2,x=c(30,90) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 3
    annotate("text",label=3,x=c(40,80) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 4
    annotate("text",label=4,x=c(50,70) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 5
    annotate("text",label=5,x=60 + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    
    
    ## upside-down numbers for top of field
    
    ## 0
    annotate("text",x=seq(20,100,by=10) + num_adj[1], y=ymax-12, angle=angle_vec[2],
             label=0, col=line_color, size=num_size),
    ## 1
    annotate("text",label=1,x=c(20,100) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 2
    annotate("text",label=2,x=c(30,90) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 3
    annotate("text",label=3,x=c(40,80) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 4
    annotate("text",label=4,x=c(50,70) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 5
    annotate("text",label=5,x=60 + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    
    
    ## add hash marks - middle of field
    annotate("segment", x=yd_hash, y=hash_dist - 0.5, xend=yd_hash, yend=hash_dist + 0.5,
             color=line_color),
    annotate("segment", x=yd_hash, y=ymax - hash_dist - 0.5, 
             xend=yd_hash, yend=ymax - hash_dist + 0.5,color=line_color),
    
    ## add hash marks - sidelines
    annotate("segment", x=yd_hash, y=ymax, xend=yd_hash, yend=ymax-1, color=line_color),
    annotate("segment", x=yd_hash, y=ymin, xend=yd_hash, yend=ymin+1, color=line_color),
    
    ## add conversion lines at 2-yard line
    annotate("segment",x=12, y=(ymax-1)/2, xend=12, yend=(ymax+1)/2, color=line_color),
    annotate("segment",x=108, y=(ymax-1)/2, xend=108, yend=(ymax+1)/2, color=line_color),
    
    ## cover up lines outside of field with sideline_color
    annotate("rect", xmin=0, xmax=xmax, ymin=ymax, ymax=ymax+buffer, fill=sideline_color),
    annotate("rect",xmin=0, xmax=xmax, ymin=ymin-buffer, ymax=ymin, fill=sideline_color),
    
    ## remove axis labels and tick marks
    labs(x="", y=""),
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank()),
    
    ## clip axes to view of field
    if(direction=="horiz"){
      coord_cartesian(xlim=c(yardmin, yardmax), ylim = c(ymin-buffer,ymax+buffer), 
                      expand = FALSE)
      
    } else if (direction=="vert"){
      ## flip entire plot to vertical orientation
      coord_flip(xlim=c(yardmin, yardmax), ylim = c(ymin-buffer,ymax+buffer), expand = FALSE)
      
    }
  )
  
  return(p)

}

animate_play_function <- function(data_in, game_id_val, play_id_val) {
    
    example_play <- data_in %>%
        dplyr::filter(game_id == game_id_val,
                      play_id == play_id_val) 
    
    ggplot() + 
      gg_field(yardmax = max(example_play$x + 5),
               yardmin = min(example_play$x -5),
               field_color='white', line_color='black', 
               sideline_color = 'gray', endzone_color='white') + 
      geom_point(data = example_play, 
                 aes(x = x,
                     y = y, 
                     fill = team,  
                     group = nfl_id,
                     colour = team),
                 cex = 5,
                 alpha = 0.7) + 
      geom_text(data = example_play, aes(x = x, y = y, label = jersey_number), colour = "black", 
                vjust = 0.36, size = 3.5) + 
      geom_vline(xintercept = example_play$absolute_yardline_number[1],
                 linetype = "dotted", color = "yellow") + 
      transition_time(frame_id)  +
      ease_aes('linear') + 
      NULL
    
}

##User-defined functions

angle_diff_abs <- function(theta_1, theta_2) {
    temp <- (theta_1 - theta_2) %% 360
    out <- ifelse(temp <= 180, temp, 360 - temp)
    return(out)
}

##First function: determine how many safeties are high, and if we're in 1-high or 2-high.
##  If in one-high: checks to see if safety is more than 4 yards outside of hashed within 2.5 sec of snap
##  If in two-high: checks to see if safety is bailing middle or staying down

safety_info <- function(tibble_in) {
    
    hash_top <- 29.96667
    hash_bottom <- 23.36667
    yardline <- tibble_in$absolute_yardline_number[1]
    
    football_y_val <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(display_name == "Football") %>%
        dplyr::pull(y)
    
    football_x_val <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(display_name == "Football") %>%
        dplyr::pull(x)
    
    mean_x = tibble_in %>% 
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(side_of_ball == "defense") %>%
        dplyr::filter(position %in% c('ILB', 'OLB', 'SS', 'CB', 'FS', 'LB', 'DB', 'S', 'MLB')) %>%
        pull(x) %>% mean()
    
    df_safeties_snap <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(side_of_ball == "defense") %>%
        arrange(desc(y)) %>%
        dplyr::filter(!is.na(nfl_id)) %>%
        head(-1) %>%
        tail(-1) %>%
        dplyr::filter(x >= (yardline + 9)) %>% #doing 9 or more yards back, as 10 seems too stringent
        dplyr::filter(x >= mean_x)
    
    safety_ids <- df_safeties_snap %>% pull(nfl_id)
    
    num_high <- length(safety_ids)
    
    tibble_out <- data.frame(num_high)
    
    #Record which safeties were where
    
    top_safety <- NA
    bottom_safety <- NA
    
    if(num_high > 0) {
        
        top_safety <- df_safeties_snap %>% head(1) %>% pull(nfl_id)
        bottom_safety <- df_safeties_snap %>% tail(1) %>% pull(nfl_id)
        
    }
    
    #check 2-high
    
    df_safeties_out <- df_safeties_snap %>%
        dplyr::filter(y < (hash_bottom - 2) | y > (hash_top + 2))
    
    tibble_out$two_high <- ifelse(tibble_out$num_high == 2 && length(df_safeties_out$x) == 2, 1, 0)
    
    #check 1-high
    
    df_safeties_in <- df_safeties_snap %>%
        dplyr::filter(y > hash_bottom & y < hash_top)
    
    tibble_out$one_high <- ifelse(tibble_out$num_high == 1 && length(df_safeties_in$x) == 1, 1, 0)    
    
    ##for 2-high: check if there's a safety bailing middle and/or a safety staying down
    
    safety_bail_middle <- NA
    safety_stay_down <- NA
    safety_bail_out <- NA
    
    if(tibble_out$two_high == 1) {
        
        safety_back <- tibble_in %>%
            dplyr::filter(nfl_id %in% safety_ids,
                          frame_id == df_safeties_snap$frame_id[1] + 15) %>%
            dplyr::select(nfl_id, x, y, dir) %>% 
            arrange(desc(y)) %>%
            dplyr::rename(x_new = x, y_new = y)
        
        safety_back$comp_angle <- c(140, 40)
        
        safety_back <- safety_back %>%
            dplyr::mutate(angle_diff = angle_diff_abs(comp_angle, dir))

       safety_bail_middle <- safety_back %>%
            dplyr::filter(angle_diff <= 20) %>%
            pull(nfl_id)
        
        safety_stay_down <- df_safeties_snap %>%
            dplyr::select(nfl_id, x, y) %>%
            left_join(safety_back, by = "nfl_id") %>%
            dplyr::mutate(x_traveled = (x - x_new)) %>%
            #dplyr::mutate(dir_down = angle_diff_abs(270, dir)) %>%
            #dplyr::mutate(is_down = ifelse((dir_down <= 25) & (dist_traveled >= 1), 1, 0)) %>% 
            dplyr::mutate(is_down = ifelse(x_traveled >= 1, 1, 0)) %>% 
            dplyr::filter(is_down == 1) %>%
            pull(nfl_id)
        
    }
    
    if(tibble_out$one_high == 1) {
        
        safety_bail_out <- tibble_in %>%
            dplyr::filter(nfl_id %in% safety_ids,
                          frame_id == df_safeties_snap$frame_id[1] + 25) %>%
            dplyr::select(nfl_id, x, y, dir) %>%
            dplyr::mutate(out_top = ifelse(y > (29.96 + 4), 1, 0),
                          out_bottom = ifelse(y < (23.36 - 4), 1, 0)) %>%
            dplyr::mutate(out_hash = out_top + out_bottom) %>%
            dplyr::filter(out_hash == 1) %>%
            pull(nfl_id)
        
    }
    
    ##Identify apex/corner defenders
    
    df_coverage_snap <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(side_of_ball == "defense") %>%
        dplyr::filter(!nfl_id %in% safety_ids) %>%
        dplyr::filter(position %in% c('ILB', 'OLB', 'SS', 'CB', 'FS', 'LB', 'DB', 'S', 'MLB')) %>%
        arrange(desc(y))
    
    top_corner <- NA
    bottom_corner <- NA
    top_apex <- NA
    bottom_apex <- NA
    
    if(length(df_coverage_snap$nfl_id > 0)) {
        
        top_corner <- df_coverage_snap %>% head(1) %>% pull(nfl_id)
        bottom_corner <- df_coverage_snap %>% tail(1) %>% pull(nfl_id)
        
    }
    
    if(length(df_coverage_snap$nfl_id) > 2) {
        
        df_coverage_snap <- df_coverage_snap %>%
            head(-1) %>%
            tail(-1)
        
        top_apex <- df_coverage_snap %>% head(1) %>% pull(nfl_id)
        bottom_apex <- df_coverage_snap %>% tail(1) %>% pull(nfl_id)
        
    }
    
    ##Identify WR numbers for top and bottom of formation
    
    df_offense_snap <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(side_of_ball == "offense")
    
    df_offense_snap_line_top <- df_offense_snap %>%
        dplyr::filter(y > (football_y_val + 3)) %>%
        arrange(desc(y))

    df_offense_snap_line_bottom <- df_offense_snap %>%
        dplyr::filter(y < (football_y_val - 3)) %>%
        arrange(y)
    
    top_WR1 <- NA
    top_WR2 <- NA
    top_WR3 <- NA
    top_WR4 <- NA
    top_WR5 <- NA
    bottom_WR1 <- NA
    bottom_WR2 <- NA
    bottom_WR3 <- NA
    bottom_WR4 <- NA
    bottom_WR5 <- NA
    
    if(length(df_offense_snap_line_top$nfl_id == 1)) {
        top_WR1 <- df_offense_snap_line_top$nfl_id[1]
    }
    
    if(length(df_offense_snap_line_top$nfl_id == 2)) {
        top_WR1 <- df_offense_snap_line_top$nfl_id[1]
        top_WR2 <- df_offense_snap_line_top$nfl_id[2]
    }
    
    if(length(df_offense_snap_line_top$nfl_id == 3)) {
        top_WR1 <- df_offense_snap_line_top$nfl_id[1]
        top_WR2 <- df_offense_snap_line_top$nfl_id[2]
        top_WR3 <- df_offense_snap_line_top$nfl_id[3]
    }
    
    if(length(df_offense_snap_line_top$nfl_id == 4)) {
        top_WR1 <- df_offense_snap_line_top$nfl_id[1]
        top_WR2 <- df_offense_snap_line_top$nfl_id[2]
        top_WR3 <- df_offense_snap_line_top$nfl_id[3]
        top_WR4 <- df_offense_snap_line_top$nfl_id[4]
    }
    
    if(length(df_offense_snap_line_top$nfl_id == 5)) {
        top_WR1 <- df_offense_snap_line_top$nfl_id[1]
        top_WR2 <- df_offense_snap_line_top$nfl_id[2]
        top_WR3 <- df_offense_snap_line_top$nfl_id[3]
        top_WR4 <- df_offense_snap_line_top$nfl_id[4]
        top_WR5 <- df_offense_snap_line_top$nfl_id[5]
    }
    
    if(length(df_offense_snap_line_bottom$nfl_id == 1)) {
        bottom_WR1 <- df_offense_snap_line_bottom$nfl_id[1]
    }
    
    if(length(df_offense_snap_line_bottom$nfl_id == 2)) {
        bottom_WR1 <- df_offense_snap_line_bottom$nfl_id[1]
        bottom_WR2 <- df_offense_snap_line_bottom$nfl_id[2]
    }
    
    if(length(df_offense_snap_line_bottom$nfl_id == 3)) {
        bottom_WR1 <- df_offense_snap_line_bottom$nfl_id[1]
        bottom_WR2 <- df_offense_snap_line_bottom$nfl_id[2]
        bottom_WR3 <- df_offense_snap_line_bottom$nfl_id[3]
    }
    
    if(length(df_offense_snap_line_bottom$nfl_id == 4)) {
        bottom_WR1 <- df_offense_snap_line_bottom$nfl_id[1]
        bottom_WR2 <- df_offense_snap_line_bottom$nfl_id[2]
        bottom_WR3 <- df_offense_snap_line_bottom$nfl_id[3]
        bottom_WR4 <- df_offense_snap_line_bottom$nfl_id[4]
    }
    
    if(length(df_offense_snap_line_bottom$nfl_id == 5)) {
        bottom_WR1 <- df_offense_snap_line_bottom$nfl_id[1]
        bottom_WR2 <- df_offense_snap_line_bottom$nfl_id[2]
        bottom_WR3 <- df_offense_snap_line_bottom$nfl_id[3]
        bottom_WR4 <- df_offense_snap_line_bottom$nfl_id[4]
        bottom_WR5 <- df_offense_snap_line_bottom$nfl_id[5]
    }
    
    tibble_out$top_WR1 <- top_WR1
    tibble_out$top_WR2 <- top_WR2
    tibble_out$top_WR3 <- top_WR3
    tibble_out$top_WR4 <- top_WR4
    tibble_out$top_WR5 <- top_WR5
    tibble_out$bottom_WR1 <- bottom_WR1
    tibble_out$bottom_WR2 <- bottom_WR2
    tibble_out$bottom_WR3 <- bottom_WR3
    tibble_out$bottom_WR4 <- bottom_WR4
    tibble_out$bottom_WR5 <- bottom_WR5
    tibble_out$top_safety <- top_safety
    tibble_out$bottom_safety <- bottom_safety
    tibble_out$top_corner <- top_corner
    tibble_out$bottom_corner <- bottom_corner
    tibble_out$top_apex <- top_apex
    tibble_out$bottom_apex <- bottom_apex
    tibble_out$safety_bail_middle <- ifelse(length(safety_bail_middle > 0), safety_bail_middle, NA)
    tibble_out$safety_bail_out <- ifelse(length(safety_bail_out > 0), safety_bail_out, NA)
    tibble_out$safety_stay_down <- ifelse(length(safety_stay_down > 0), safety_stay_down, NA)
    
    ##Measure total distance traveled for each side of ball, as well as mean distance traveled, before ball is thrown 
    ##  (or 4 seconds after snap, whichever comes first)
    
    df_coverage_ids <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(side_of_ball == "defense") %>%
        dplyr::filter(position %in% c('ILB', 'OLB', 'SS', 'CB', 'FS', 'LB', 'DB', 'S', 'MLB')) %>%
        dplyr::pull(nfl_id)

    snap_frame <- tibble_in %>% 
        dplyr::filter(event == "ball_snap") %>%
        head(1) %>% pull(frame_id)

    pass_frame <- tibble_in %>%
        dplyr::filter(event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'tackle')) %>%
        pull(frame_id) %>% unique() %>% min()

    pass_frame <- min(pass_frame, snap_frame + 40)
    
    football_x_val_throw <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        pull(x)
    
    football_y_val_throw <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        pull(y)
    
    df_receiver_ids <- tibble_in %>% 
        dplyr::filter(side_of_ball == "offense") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        dplyr::mutate(dis_to_ball = sqrt((x - football_x_val_throw)^2 + (y - football_y_val_throw)^2)) %>%
        dplyr::arrange(dis_to_ball) %>%
        tail(-1) %>% pull(nfl_id)
    
    distance_ids <- c(df_coverage_ids, df_receiver_ids)

    df_distance <- tibble_in %>%
        dplyr::filter(frame_id > snap_frame,
                      frame_id <= pass_frame) %>%
        dplyr::filter(nfl_id %in% distance_ids) %>%
        group_by(side_of_ball) %>%
        summarize(dis_traveled = sum(dis), .groups = "drop_last") %>%
        ungroup() %>%
        arrange(desc(side_of_ball))

    num_offense <- length(df_offense_snap_line_top$nfl_id) + length(df_offense_snap_line_bottom$nfl_id)
    num_defense <- length(df_coverage_ids)
    df_distance$num_players <- c(num_offense, num_defense)
    df_distance <- df_distance %>%
        dplyr::mutate(mean_dis_traveled = round(dis_traveled / num_players, 2))
    
    tibble_out$dis_traveled_def <- df_distance %>% 
        dplyr::filter(side_of_ball == "defense") %>%
        dplyr::pull(dis_traveled)
    
    tibble_out$mean_dis_traveled_def <- df_distance %>% 
        dplyr::filter(side_of_ball == "defense") %>%
        dplyr::pull(mean_dis_traveled)
    
    tibble_out$dis_traveled_off <- df_distance %>% 
        dplyr::filter(side_of_ball == "offense") %>%
        dplyr::pull(dis_traveled)
    
    tibble_out$mean_dis_traveled_off <- df_distance %>% 
        dplyr::filter(side_of_ball == "offense") %>%
        dplyr::pull(mean_dis_traveled)
        
    return(tibble_out)
    
}

##Next function: determine if there are detached receivers. If so, identify nickel responsible and if they're lined up inside/outside and by how much
##  Positive alignment numbers correspond to outside, negative correspond to inside

nickel_detached_assignments <- function(tibble_in) {
    
    tibble_temp <- tibble_in
    
    football_y_val <- tibble_temp %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(display_name == "Football") %>%
        dplyr::pull(y)
    
    football_x_val <- tibble_temp %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(display_name == "Football") %>%
        dplyr::pull(x)
    
    tibble_temp$football_y <- football_y_val
    
    ##find which WR/TEs were detached at snap
    
    detached_wrs <- tibble_temp %>% 
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(side_of_ball == "offense")
    
    detached_wrs_top <- detached_wrs %>%
        dplyr::filter(y > football_y_val) %>%
        arrange(desc(y)) %>%
        tail(-1)
    
    detached_wrs_bottom <- detached_wrs %>%
        dplyr::filter(y < football_y_val) %>%
        arrange(y) %>%
        tail(-1)
    
    detached_wrs <- detached_wrs_top %>% 
        bind_rows(detached_wrs_bottom) %>%
        dplyr::filter(abs(y - football_y) >= 8) %>%
        dplyr::select(nfl_id) %>%
        dplyr::mutate(is_detached = 1)
    
    detached_wr_ids <- detached_wrs %>% pull(nfl_id)
    
    tibble_out <- tibble_temp %>%
        dplyr::filter(frame_id == 1) %>%
        dplyr::select(nfl_id) %>%
        dplyr::left_join(detached_wrs, by = "nfl_id") %>%
        dplyr::mutate(is_detached = replace_na(is_detached, 0))
    
    ##find which CB DB FS ILB LB MLB OLB S SS lined up over detached WR
    
    tibble_out$nickel_alignment <- NA
    
    if(length(detached_wr_ids) > 0) {
        
        tibble_out <- tibble_out %>% dplyr::select(-nickel_alignment)
        
        def_pos <- tibble_temp %>%
            dplyr::filter(event == "ball_snap") %>%
            dplyr::filter(side_of_ball == "defense") %>%
            dplyr::filter(position %in% c('CB', 'DB', 'FS', 'ILB', 'LB', 'MLB', 'OLB', 'S', 'SS')) %>%
            dplyr::select(frame_id, nfl_id, x, y)
        
        closest_def <- def_pos %>%
            dplyr::left_join(tibble_temp %>%
                                dplyr::filter(nfl_id == detached_wr_ids[1]) %>%
                                dplyr::select(nfl_id, frame_id, x, y) %>% 
                                dplyr::rename(x_wr = x, y_wr = y, nfl_id_wr = nfl_id), by = "frame_id") %>%
            dplyr::mutate(nickel_alignment = y - y_wr,
                          nickel_alignment_back = x - football_x_val)
        
        closest_def_1 <- closest_def %>%
            dplyr::filter(abs(nickel_alignment) < 2.5,
                          nickel_alignment_back < 2)
        
        closest_def_2 <- closest_def %>%
            dplyr::filter(abs(nickel_alignment) < 4,
                          nickel_alignment_back >= 2,
                          nickel_alignment_back < 10)
        
        closest_def_3 <- closest_def %>%
            dplyr::filter(nickel_alignment_back >= 10)
        
        closest_def <- closest_def_1 %>% 
            bind_rows(closest_def_2) %>%
            bind_rows(closest_def_3) %>%
            dplyr::mutate(dis_to_receiver = sqrt((x - x_wr)^2+(y - y_wr)^2)) %>%
            top_n(-1, wt = dis_to_receiver) %>%
            dplyr::mutate(nickel_alignment = ifelse(y > football_y_val, nickel_alignment, -nickel_alignment)) %>%
            dplyr::select(nfl_id_wr, nickel_alignment)
        
        if(length(detached_wr_ids) > 1) {
            
            for(i in 2:length(detached_wr_ids)) {
            
                        closest_def_temp <- def_pos %>%
                                dplyr::left_join(tibble_temp %>%
                                dplyr::filter(nfl_id == detached_wr_ids[i]) %>%
                                dplyr::select(nfl_id, frame_id, x, y) %>% 
                                dplyr::rename(x_wr = x, y_wr = y, nfl_id_wr = nfl_id), by = "frame_id") %>%
                                dplyr::mutate(nickel_alignment = y - y_wr,
                                              nickel_alignment_back = x - football_x_val)
                        
                        closest_def_1 <- closest_def_temp %>%
                                dplyr::filter(abs(nickel_alignment) < 2.5,
                                              nickel_alignment_back < 2)
                        
                        closest_def_2 <- closest_def_temp %>%
                                dplyr::filter(abs(nickel_alignment) < 4,
                                              nickel_alignment_back >= 2,
                                              nickel_alignment_back < 10)
                        
                        closest_def_3 <- closest_def_temp %>%
                                dplyr::filter(nickel_alignment_back >= 10)
                        
                        closest_def_temp <- closest_def_1 %>% 
                                bind_rows(closest_def_2) %>%
                                bind_rows(closest_def_3) %>%
                                dplyr::mutate(dis_to_receiver = sqrt((x - x_wr)^2+(y - y_wr)^2)) %>%
                                top_n(-1, wt = dis_to_receiver) %>%
                                dplyr::mutate(nickel_alignment = ifelse(y > football_y_val, nickel_alignment, -nickel_alignment)) %>%
                                dplyr::select(nfl_id_wr, nickel_alignment)
                
                        closest_def <- closest_def %>% bind_rows(closest_def_temp)
            
            }
            
        }
        
        tibble_out <- tibble_out %>%
            left_join(closest_def, by = c("nfl_id" = "nfl_id_wr"))
        
    }
    
    return(tibble_out)
    
}

##Next function: measure which player each defender in coverage is responsible for
##  Also, measures distance from defensive back to closest receiver

closest_receiver <- function(tibble_in) {
    
    football_x_val <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(display_name == "Football") %>%
        dplyr::pull(x)
    
    snap_df <- tibble_in %>%
        dplyr::filter(event == "ball_snap")

    snap_frame <- snap_df %>%
        pull(frame_id) %>% head(1)

    one_sec_df <- tibble_in %>% 
        dplyr::filter(frame_id == (snap_frame + 10))

    cover_df <- tibble_in %>% 
        dplyr::filter(frame_id == (snap_frame + 20))

    coverage_ids <- cover_df %>%
        dplyr::filter(side_of_ball == "defense") %>%
        dplyr::filter(display_name != "Football") %>%
        dplyr::filter(x > football_x_val) %>%
        dplyr::pull(nfl_id)

    defender_df <- cover_df %>%
        dplyr::filter(side_of_ball == "offense") %>%
        dplyr::select(nfl_id, x, y, frame_id) %>%
        dplyr::left_join(cover_df %>%
                            dplyr::filter(nfl_id == coverage_ids[1]) %>%
                            dplyr::select(nfl_id, x, y, frame_id) %>%
                            dplyr::rename(nfl_id_def = nfl_id, x_def = x, y_def = y), by = "frame_id") %>%
        dplyr::mutate(dis_to_receiver_2 = sqrt((x - x_def)^2 + (y - y_def)^2)) %>%
        dplyr::arrange(dis_to_receiver_2) %>%
        head(1) %>%
        dplyr::left_join(snap_df %>%
                            dplyr::rename(x_snap = x, y_snap = y) %>%
                            dplyr::select(nfl_id, x_snap, y_snap), by = c("nfl_id_def" = "nfl_id")) %>%
        dplyr::left_join(snap_df %>%
                            dplyr::rename(x_wr_snap = x, y_wr_snap = y) %>%
                            dplyr::select(nfl_id, x_wr_snap, y_wr_snap), by = "nfl_id") %>%
        dplyr::mutate(dis_to_receiver_snap = sqrt((x_snap - x_wr_snap)^2 + (y_snap - y_wr_snap)^2)) %>%
        dplyr::left_join(one_sec_df %>%
                            dplyr::rename(x_one = x, y_one = y) %>%
                            dplyr::select(nfl_id, x_one, y_one), by = c("nfl_id_def" = "nfl_id")) %>%
        dplyr::left_join(one_sec_df %>%
                            dplyr::rename(x_wr_one = x, y_wr_one = y) %>%
                            dplyr::select(nfl_id, x_wr_one, y_wr_one), by = "nfl_id") %>%
        dplyr::mutate(dis_to_receiver_1 = sqrt((x_one - x_wr_one)^2 + (y_one - y_wr_one)^2)) %>%
        dplyr::rename(nfl_id_receiver = nfl_id) %>%
        dplyr::rename(nfl_id = nfl_id_def) %>%
        dplyr::select(nfl_id, nfl_id_receiver, dis_to_receiver_snap, dis_to_receiver_1, dis_to_receiver_2)

    for(i in 2:length(coverage_ids)) {
    
        defender_df_temp <- cover_df %>%
            dplyr::filter(side_of_ball == "offense") %>%
            dplyr::select(nfl_id, x, y, frame_id) %>%
            dplyr::left_join(cover_df %>%
                                dplyr::filter(nfl_id == coverage_ids[i]) %>%
                                dplyr::select(nfl_id, x, y, frame_id) %>%
                                dplyr::rename(nfl_id_def = nfl_id, x_def = x, y_def = y), by = "frame_id") %>%
            dplyr::mutate(dis_to_receiver_2 = sqrt((x - x_def)^2 + (y - y_def)^2)) %>%
            dplyr::arrange(dis_to_receiver_2) %>%
            head(1)%>%
            dplyr::left_join(snap_df %>%
                                dplyr::rename(x_snap = x, y_snap = y) %>%
                                dplyr::select(nfl_id, x_snap, y_snap), by = c("nfl_id_def" = "nfl_id")) %>%
            dplyr::left_join(snap_df %>%
                                dplyr::rename(x_wr_snap = x, y_wr_snap = y) %>%
                                dplyr::select(nfl_id, x_wr_snap, y_wr_snap), by = "nfl_id") %>%
            dplyr::mutate(dis_to_receiver_snap = sqrt((x_snap - x_wr_snap)^2 + (y_snap - y_wr_snap)^2)) %>%
            dplyr::left_join(one_sec_df %>%
                                dplyr::rename(x_one = x, y_one = y) %>%
                                dplyr::select(nfl_id, x_one, y_one), by = c("nfl_id_def" = "nfl_id")) %>%
            dplyr::left_join(one_sec_df %>%
                                dplyr::rename(x_wr_one = x, y_wr_one = y) %>%
                                dplyr::select(nfl_id, x_wr_one, y_wr_one), by = "nfl_id") %>%
            dplyr::mutate(dis_to_receiver_1 = sqrt((x_one - x_wr_one)^2 + (y_one - y_wr_one)^2)) %>%
            dplyr::rename(nfl_id_receiver = nfl_id) %>%
            dplyr::rename(nfl_id = nfl_id_def) %>%
            dplyr::select(nfl_id, nfl_id_receiver, dis_to_receiver_snap, dis_to_receiver_1, dis_to_receiver_2)
        
        defender_df <- defender_df %>% bind_rows(defender_df_temp)
    
    }
    
    return(defender_df)
    
}

route_info <- function(tibble_in) {

    ##Want to determine route types for eligible receivers

    ##Start by identifying elligible receivers

    snap_frame <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        head(1) %>% pull(frame_id)

    pass_frame <- tibble_in %>%
        dplyr::filter(event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'tackle')) %>%
        pull(frame_id) %>% unique() %>% min()

    pass_frame <- min(pass_frame, snap_frame + 40)
    
    football_x_val_snap <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == snap_frame) %>%
        pull(x)
    
    football_y_val_snap <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == snap_frame) %>%
        pull(y)
    
    football_x_val_throw <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        pull(x)
    
    football_y_val_throw <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        pull(y)
    
    df_out <- tibble_in %>% 
        dplyr::filter(side_of_ball == "offense") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        dplyr::mutate(dis_to_ball = sqrt((x - football_x_val_throw)^2 + (y - football_y_val_throw)^2)) %>%
        dplyr::arrange(dis_to_ball) %>%
        tail(-1) %>%
        dplyr::select(nfl_id, x, y, dir) %>%
        dplyr::rename(x_throw = x, 
                      y_throw = y, 
                      dir_throw = dir)
    
    df_out <- df_out %>%
        left_join(tibble_in %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == snap_frame) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_snap = x, y_snap = y), by = "nfl_id") %>%
        left_join(tibble_in %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 10)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_1 = x, y_1 = y), by = "nfl_id")
    
    ##Check for flat receivers
    ##These will be defined as receivers who have run < 5 yards deep, are moving away from the football, and are >8 yards away from ball snap point
    ##  at the point the pass is thrown
    
    df_out <- df_out %>%
        dplyr::mutate(flat_1 = case_when(x_throw > (football_x_val_snap + 5) ~ 0,
                                         abs(y_throw - football_y_val_snap) < 8 ~ 0,
                                         TRUE ~ 1),
                      flat_2 = case_when(y_throw < (football_y_val_snap + 8) ~ 0,
                                         angle_diff_abs(dir_throw, 0) > 25 ~ 0,
                                         TRUE ~ 1),
                      flat_3 = case_when(y_throw > (football_y_val_snap - 8) ~ 0,
                                         angle_diff_abs(dir_throw, 180) > 25 ~ 0,
                                         TRUE ~ 1)) %>%
        dplyr::mutate(flat_4 = flat_2 + flat_3) %>%
        dplyr::mutate(flat = flat_1 * flat_4) %>%
        dplyr::select(-flat_1, -flat_2, -flat_3, -flat_4)
    
    ##Check for shallow receivers
    ##THese will be defined as receivers who have run < 5 yards deep and > 0 yards deep when ball thrown, are moving across the field (i.e. if they started below the 
    ##  football, they are moving up, and vice-versa), and are < 8 yards away from the ball snap point at the point when the pass is thrown
    
    df_out <- df_out %>%
        dplyr::mutate(shallow_1 = case_when(x_throw > (football_x_val_snap + 5) ~ 0,
                                            x_throw < football_x_val_snap ~ 0,
                                            abs(y_throw - football_y_val_snap) > 9 ~ 0,
                                            TRUE ~ 1),
                      shallow_2 = case_when(y_snap < football_y_val_snap ~ 0,
                                            angle_diff_abs(dir_throw, 180) > 25 ~ 0,
                                            TRUE ~ 1),
                      shallow_3 = case_when(y_snap > football_y_val_snap ~ 0,
                                            angle_diff_abs(dir_throw, 0) > 25 ~ 0,
                                            TRUE ~ 1)) %>%
        dplyr::mutate(shallow_4 = shallow_2 + shallow_3) %>%
        dplyr::mutate(shallow = shallow_1 * shallow_4) %>%
        dplyr::select(-shallow_1, -shallow_2, -shallow_3, -shallow_4)
    
    ##Check for mid-length route receivers
    ##THese will be defined as receivers who have run between 6 and 10 yards deep when ball thrown
    
    df_out <- df_out %>%
        dplyr::mutate(mid_route = case_when(x_throw < (x_snap + 5) ~ 0,
                                            x_throw > (x_snap + 10) ~ 0,
                                            TRUE ~ 1)) 
    
    ##Check for vertical receivers
    ##THese will be defined as receivers who have run > 10 yards deep when ball thrown
    
    df_out <- df_out %>%
        dplyr::mutate(vertical = ifelse(x_throw > (x_snap + 10), 1, 0)) 
    
    ##Additional potential: check for if receiver broke
    ##Identify, for each receiver, if they broke. Check their position 1 second after the snap (to account for leftover motion).
    ##  Then check their position again when the ball is thrown.
    ##  If they are 3 yards or more above or below their y  position, count that as a break
    ##  Determine if break is in or out as well
    
    df_out <- df_out %>%
        dplyr::select(nfl_id, flat, shallow, mid_route, vertical)
    
    return(df_out)
}

path_pca_data <- function(tibble_in) {
    
    ##Let's make a function to downsample x and y coordinates of relevant receivers
##Need to find a way to deal with plays that have less than 3 seconds. Shouldn't be many.

    ##Start by identifying elligible receivers

    snap_frame <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        head(1) %>% pull(frame_id)

    pass_frame <- tibble_in %>%
        dplyr::filter(event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'tackle')) %>%
        pull(frame_id) %>% unique() %>% min()

    pass_frame <- min(pass_frame, snap_frame + 40)
    
    football_x_val_snap <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == snap_frame) %>%
        pull(x)
    
    football_y_val_snap <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == snap_frame) %>%
        pull(y)
    
    ##Center x and y values
    
    tibble_temp <- tibble_in %>%
        dplyr::mutate(x = x - football_x_val_snap,
                      y = y - football_y_val_snap)
    
    football_x_val_throw <- tibble_temp %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        pull(x)
    
    football_y_val_throw <- tibble_temp %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        pull(y)
    
    ##Create receiver and defender df
    
    df_out <- tibble_temp %>% 
        dplyr::filter(side_of_ball == "offense") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        dplyr::mutate(dis_to_ball = sqrt((x - football_x_val_throw)^2 + (y - football_y_val_throw)^2)) %>%
        dplyr::arrange(dis_to_ball) %>%
        tail(-1) %>%
        dplyr::select(nfl_id) %>%
        bind_rows(tibble_temp %>% 
                  dplyr::filter(side_of_ball == "defense") %>%
                  dplyr::filter(frame_id == snap_frame) %>%
                  dplyr::filter(position %in% c("ILB", "OLB", "SS", "CB", "FS", "DB", "LB", "S", "MLB")) %>%
                  dplyr::select(nfl_id))
    
    df_out <- df_out %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == snap_frame) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_0 = x, y_0 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 1)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_1 = x, y_1 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 2)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_2 = x, y_2 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 3)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_3 = x, y_3 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 4)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_4 = x, y_4 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 5)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_5 = x, y_5 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 6)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_6 = x, y_6 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 7)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_7 = x, y_7 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 8)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_8 = x, y_8 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 9)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_9 = x, y_9 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 10)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_10 = x, y_10 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 11)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_11 = x, y_11 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 12)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_12 = x, y_12 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 13)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_13 = x, y_13 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 14)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_14 = x, y_14 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 15)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_15 = x, y_15 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 16)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_16 = x, y_16 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 17)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_17 = x, y_17 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 18)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_18 = x, y_18 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 19)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_19 = x, y_19 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 20)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_20 = x, y_20 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 21)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_21 = x, y_21 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 22)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_22 = x, y_22 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 23)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_23 = x, y_23 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 24)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_24 = x, y_24 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 25)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_25 = x, y_25 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 26)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_26 = x, y_26 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 27)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_27 = x, y_27 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 28)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_28 = x, y_28 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 29)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_29 = x, y_29 = y), by = "nfl_id") %>%
        left_join(tibble_temp %>% 
                     dplyr::filter(nfl_id %in% df_out$nfl_id) %>%
                     dplyr::filter(frame_id == (snap_frame + 30)) %>%
                     dplyr::select(nfl_id, x, y) %>%
                     dplyr::rename(x_30 = x, y_30 = y), by = "nfl_id")

    return(df_out)
    
}

xgboost_data_prep <- function(tibble_in) {
    
    ##Want to make function to assemble data to use in xgboost modeling
    
    hash_top <- 29.96667
    hash_bottom <- 23.36667
    yardline <- tibble_in$absolute_yardline_number[1]
    
    football_y_val <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(display_name == "Football") %>%
        dplyr::pull(y)
    
    football_x_val <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(display_name == "Football") %>%
        dplyr::pull(x)
    
    pass_frame <- tibble_in %>%
        dplyr::filter(event %in% c('pass_forward', 'pass_shovel', 'qb_sack', 'qb_strip_sack', 'tackle')) %>%
        pull(frame_id) %>% unique() %>% min() %>% as.numeric()
    
    snap_frame <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        pull(frame_id) %>% min() %>% as.numeric()

    pass_frame <- min(pass_frame, snap_frame + 40)
    
    football_x_val_throw <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        pull(x)
    
    football_y_val_throw <- tibble_in %>% 
        dplyr::filter(display_name == "Football") %>%
        dplyr::filter(frame_id == pass_frame) %>%
        pull(y)
    
    num_rushers <- tibble_in %>% head(1) %>% pull(number_of_pass_rushers)
    num_rushers <- replace_na(num_rushers, 3)
    
    num_defenders <- (11 - num_rushers) %>% as.integer()
    
    defender_ids <- tibble_in %>%
        dplyr::filter(frame_id == pass_frame) %>%
        dplyr::filter(side_of_ball == "defense") %>%
        dplyr::mutate(dist_to_ball = sqrt((x - football_x_val_throw)^2 + (y - football_y_val_throw)^2)) %>%
        arrange(desc(dist_to_ball)) %>%
        head(num_defenders) %>% pull(nfl_id)
    
    mean_x = tibble_in %>% 
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(side_of_ball == "defense") %>%
        dplyr::filter(position %in% c('ILB', 'OLB', 'SS', 'CB', 'FS', 'LB', 'DB', 'S', 'MLB')) %>%
        pull(x) %>% mean()
    
    ##First: get ids of safeties, in order
    df_safeties_snap <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(nfl_id %in% defender_ids) %>%
        arrange(desc(y)) %>%
        dplyr::filter(!is.na(nfl_id)) %>%
        head(-1) %>%
        tail(-1) %>%
        dplyr::filter(x >= (yardline + 9)) %>%
        dplyr::filter(x >= mean_x) %>%
        dplyr::select(nfl_id)
    
    safety_1 <- NA
    safety_2 <- NA
    safety_3 <- NA
    safety_4 <- NA
    safety_ids <- c()

    if(length(df_safeties_snap$nfl_id) == 1) {
        
        safety_1 <- df_safeties_snap$nfl_id[1]
        safety_ids <- df_safeties_snap$nfl_id
        
    } 

    if(length(df_safeties_snap$nfl_id) == 2) {
        
        safety_1 <- df_safeties_snap$nfl_id[1]
        safety_2 <- df_safeties_snap$nfl_id[2]
        safety_ids <- df_safeties_snap$nfl_id
        
    } 

    if(length(df_safeties_snap$nfl_id) == 3) {
        
        safety_1 <- df_safeties_snap$nfl_id[1]
        safety_2 <- df_safeties_snap$nfl_id[2]
        safety_3 <- df_safeties_snap$nfl_id[3]
        safety_ids <- df_safeties_snap$nfl_id
        
    } 
    
    if(length(df_safeties_snap$nfl_id) >= 4) {
        
        safety_1 <- df_safeties_snap %>% head(1) %>% pull(nfl_id)
        safety_2 <- df_safeties_snap %>% head(2) %>% tail(1) %>% pull(nfl_id)
        safety_3 <- df_safeties_snap %>% tail(2) %>% head(1) %>% pull(nfl_id)
        safety_4 <- df_safeties_snap %>% tail(1) %>% pull(nfl_id)
        safety_ids <- c(safety_1, safety_2, safety_3, safety_4)
    }
    
    ##Next, get other defenders, in order
    
    num_defenders_1 <- num_defenders - length(safety_ids)
    num_defenders_2 <- length(defender_ids) - length(safety_ids) 
    num_defenders <- min(num_defenders_1, num_defenders_2) %>% as.integer()
    
    defender_ids_first <- tibble_in %>%
        dplyr::filter(frame_id == 1) %>%
        dplyr::filter(nfl_id %in% defender_ids) %>%
        dplyr::filter(!nfl_id %in% safety_ids) %>%
        head(num_defenders) %>% arrange(desc(y)) %>%
        pull(nfl_id)
    
    top_corner <- NA
    top_apex <- NA
    top_hook <- NA
    bottom_hook <- NA
    bottom_apex <- NA
    bottom_corner <- NA

    if(length(defender_ids_first) == 1) {
        
        top_apex <- defender_ids_first[1]
        
    }

    if(length(defender_ids_first) == 2) {
        
        top_apex <- defender_ids_first[1]
        bottom_apex <- defender_ids_first[2]
        
    }

    if(length(defender_ids_first) == 3) {
        
        top_corner <- defender_ids_first[1]
        top_apex <- defender_ids_first[2]
        bottom_corner <- defender_ids_first[3]
        
    }

    if(length(defender_ids_first) == 4) {
        
        top_corner <- defender_ids_first[1]
        top_apex <- defender_ids_first[2]
        bottom_apex <- defender_ids_first[3]
        bottom_corner <- defender_ids_first[4]
        
    }

    if(length(defender_ids_first) == 5) {
        
        top_corner <- defender_ids_first[1]
        top_apex <- defender_ids_first[2]
        top_hook <- defender_ids_first[3]
        bottom_apex <- defender_ids_first[4]
        bottom_corner <- defender_ids_first[5]
        
    }

    if(length(defender_ids_first) >= 6) {
        
        top_corner <- defender_ids_first[1]
        top_apex <- defender_ids_first[2]
        top_hook <- defender_ids_first[3]
        bottom_hook <- defender_ids_first %>% tail(3) %>% head(1)
        bottom_apex <- defender_ids_first %>% tail(2) %>% head(1)
        bottom_corner <- defender_ids_first %>% tail(1)
        
    }
    
    ##Same process for wide receivers
    
    offense_ids <- tibble_in %>%
        dplyr::filter(frame_id == pass_frame) %>%
        dplyr::filter(position %in% c("FB", "HB", "QB", "RB", "TE", "WR")) %>%
        dplyr::filter(side_of_ball == "offense") %>%
        dplyr::mutate(dis_to_ball = sqrt((x - football_x_val_throw)^2 + (y - football_y_val_throw)^2)) %>%
        dplyr::arrange(dis_to_ball) %>%
        head(-1) %>% pull(nfl_id)
    
    receiver_ids <- tibble_in %>%
        dplyr::filter(event == "ball_snap") %>%
        dplyr::filter(side_of_ball == "offense") %>%
        dplyr::filter(nfl_id %in% offense_ids) %>%
        dplyr::arrange(desc(y)) %>%
        pull(nfl_id)
    
    WR1 <- NA
    WR2 <- NA
    WR3 <- NA
    WR4 <- NA
    WR5 <- NA
    
    if(length(receiver_ids) == 1) {
        
        WR1 <- receiver_ids[1]
        
    }
    
    if(length(receiver_ids) == 2) {
        
        WR1 <- receiver_ids[1]
        WR5 <- receiver_ids[2]
        
    }
    
    if(length(receiver_ids) == 3) {
        
        WR1 <- receiver_ids[1]
        WR2 <- receiver_ids[2]
        WR5 <- receiver_ids[3]
        
    }
    
    if(length(receiver_ids) == 4) {
        
        WR1 <- receiver_ids[1]
        WR2 <- receiver_ids[2]
        WR4 <- receiver_ids[3]
        WR5 <- receiver_ids[4]
        
    }
    
    if(length(receiver_ids) == 5) {
        
        WR1 <- receiver_ids[1]
        WR2 <- receiver_ids[2]
        WR3 <- receiver_ids[3]
        WR4 <- receiver_ids[4]
        WR5 <- receiver_ids[5]
        
    }
    
    tibble_out <- data.frame(top_corner)
    names(tibble_out) <- "top_corner"
    tibble_out$top_apex <- top_apex
    tibble_out$top_hook <- top_hook
    tibble_out$bottom_hook <- bottom_hook
    tibble_out$bottom_apex <- bottom_apex
    tibble_out$bottom_corner <- bottom_corner
    tibble_out$safety_1 <- safety_1
    tibble_out$safety_2 <- safety_2
    tibble_out$safety_3 <- safety_3
    tibble_out$safety_4 <- safety_4
    tibble_out$WR1 <- WR1
    tibble_out$WR2 <- WR2
    tibble_out$WR3 <- WR3
    tibble_out$WR4 <- WR4
    tibble_out$WR5 <- WR5
    
    return(tibble_out)
    
}

##Events which make a difference in calculating things

ball_snap_events <- 'ball_snap'

pass_thrown_events <- c('pass_forward',
                        'pass_shovel')

pass_arrival_events <- c('pass_outcome_caught',
                        'pass_arrived',
                        'pass_outcome_incomplete',
                        'pass_outcome_interception',
                        'pass_outcome_touchdown')

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

##read in secondary data
df_secondary_assignments <- readRDS("../input/secondary-assignments/secondary_assignments.rds")

##read in nflfastr data
df_nflfastr <- readRDS("../input/nflfastr-2018/play_by_play_2018.rds")

##read in coverage predictions
coverage_assignments <- readRDS("../input/coverage-assignments/df_final.rds")

##Reading relevant tracking data (needs to be done iteratively)

#weeks of NFL season
weeks <- seq(2, 17)

#blank dataframe to store tracking data
#CHANGE TO CORRECT WEEK
df_tracking <- read_csv("../input/nfl-big-data-bowl-2021/week1.csv",
                                col_types = cols())

#iterating through all weeks
for(w in weeks){
    
    #temperory dataframe used for reading week for given iteration
    df_tracking_temp <- read_csv(paste0("../input/nfl-big-data-bowl-2021/week",w,".csv"),
                                col_types = cols())
    
    #storing temporary dataframe in full season dataframe
    df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
    
}

#df_tracking <- read_csv("../input/nfl-big-data-bowl-2021/week1.csv", col_types = cols())

#f_tracking <- df_tracking %>%
#    janitor::clean_names() %>%
#    dplyr::select(game_id, play_id, play_direction) %>%
#    unique()

#df_plays <- df_plays  %>%
#    dplyr::left_join(df_tracking %>% dplyr::select(game_id, play_id, play_direction) %>% unique(), by = c("game_id" = "game_id", "play_id" = "play_id")) %>%
#    mutate(absolute_yardline_number = ifelse(play_direction == "left", 120 - absolute_yardline_number, absolute_yardline_number)) %>%
#    dplyr::inner_join(df_games %>% dplyr::select(game_id, home_team_abbr, visitor_team_abbr), by = "game_id") %>%
#    dplyr::mutate(defense_team = ifelse(possession_team == home_team_abbr, visitor_team_abbr, home_team_abbr))

coverage_assignments %>% dplyr::filter(game_id == 2018092302, play_id == 2582)
df_plays %>% dplyr::filter(game_id == 2018092302, play_id == 2582)
animate_play_function(df_tracking, 2018092302, 2582)

##Need to add coverage and win probability into everything

df_plays <- df_plays %>%
    dplyr::left_join(df_nflfastr %>% dplyr::select(play_id, game_id, wp), 
                     by = c("game_id" = "game_id", "play_id" = "play_id"))

df_plays_week1 <- df_plays %>%
    dplyr::left_join(df_coverages, by = c("game_id" = "game_id", "play_id" = "play_id")) %>%
    dplyr::filter(!is.na(coverage))

df_plays_remainder <- df_plays %>%
    dplyr::left_join(coverage_assignments, by = c("game_id" = "game_id", "play_id" = "play_id")) %>%
    dplyr::filter(!is.na(coverage))

df_plays <- df_plays_week1 %>%
    bind_rows(df_plays_remainder)

df_plays <- df_plays %>%
    dplyr::mutate(coverage = as.factor(coverage))
    
saveRDS(df_plays, "df_plays_coverage.rds")

##League-wide

df_plays %>% 
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
    dplyr::filter(wp <= 0.8) %>%
    dplyr::group_by(coverage) %>%
    summarize(count = n(),
              avg_epa = round(mean(epa), 3))
