# Example Plots for data visualization# 


#Set example birds 
#Example birds for Fig 
Familiar_ex <- "bl70bl30"
Novel_ex <- "bl186gr54"

#create example dfs 
Circ_example_birds <- U_Birds_90 %>% filter(BirdID == Familiar_ex | BirdID == Novel_ex)
Circ_example_birds_bin1 <- Circ_example_birds %>% filter(time_bin_3min ==1)


# CV example Fig4C --------------------------------------------------------

# Compute density of head angles within each time_bin and condition
angle_density <- Circ_example_birds %>%
  group_by(Condition, time_bin_3min) %>%
  mutate(head_angle_bin = cut(head_angle_deg, breaks = seq(0, 360, by = 10), include.lowest = TRUE)) %>%
  count(head_angle_bin) %>%
  mutate(
    head_angle_deg = as.numeric(sub("[[(]([0-9.]+),.*", "\\1", head_angle_bin)) + 2.5, # robust regex
    density = n / sum(n)
  ) %>%
  ungroup()

# Ensure time_bin is ordered correctly
angle_density <- angle_density %>%
  mutate(time_bin_3min = factor(time_bin_3min, levels = sort(unique(time_bin_3min))))

angle_density$Condition <- factor(
  angle_density$Condition,
  levels = c("N", "F")
)

angle_density_wrap <- angle_density %>%
  filter(time_bin_3min == 1) %>%
  bind_rows(
    angle_density %>%
      filter(time_bin_3min == 1, head_angle_deg == 352.5) %>%
      mutate(head_angle_deg = 360)
  )
range(angle_density$head_angle_deg)

## one time only circular  
ggplot(
  angle_density_wrap %>% filter(time_bin_3min == 1),
  aes(
    x = head_angle_deg,
    y = time_bin_3min,
    fill = density
  )
) +
  geom_tile(width = 10, height = 1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Density"
  ) +
  scale_x_continuous(
    breaks = seq(0, 360, by = 90),
    limits = c(-5, 365),
    expand = c(0, 0)
  ) +
  #scale_y_continuous(
  #limits = c(0.5, 1.5),
  #expand = c(0, 0)
  #) +
  facet_wrap(
    ~ Condition,
    ncol = 1,
    strip.position = "left",
    labeller = labeller(
      Condition = c(
        "F" = "CV: 0.38",
        "N" = "CV: 0.008"
      )
    )
  ) +
  coord_polar(theta = "x", start = pi/2) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0.05, "lines"),
    strip.placement = "outside",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.4),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = ""
  )

pi <- pi 
####Hole in center 
library(ggforce)
# Parameters you can adjust
inner_radius <- 0.5         # creates the blank hole in the center
ring_thickness <- 1     # controls distance between rings
bin_width <- 10           # angular bin width (should match your data’s cut width)
rotation <- pi/2         # start angle (pi/2 = top)

marker_deg <- c(90, 180, 270, 360)

marker_df <- data.frame(
  angle = marker_deg * (pi / 180) 
)

marker_df$r_start <- inner_radius + 1.5
marker_df$r_end   <- inner_radius + 1.6
marker_df$label <- paste0(" ")

# push labels a bit farther out than the ticks
marker_df$r_label <- inner_radius + 1.9

# Create the plot
ggplot(angle_density %>% filter(time_bin_3min == 1)) +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = as.numeric(time_bin_3min) - ring_thickness/2 + inner_radius,
      r  = as.numeric(time_bin_3min) + ring_thickness/2 + inner_radius,
      start = (head_angle_deg - bin_width/2) * pi/180 + rotation,
      end   = (head_angle_deg + bin_width/2) * pi/180 + rotation,
      fill = density
    ),
    color = NA
  ) +
  geom_segment(
    data = marker_df,
    aes(
      x    = r_start * cos(angle),
      y    = r_start * sin(angle),
      xend = r_end   * cos(angle),
      yend = r_end   * sin(angle)
    ),
    inherit.aes = FALSE,
    linewidth = 0.6,
    color = "black"
  )+
  geom_text(
    data = marker_df,
    aes(
      x = r_label * cos(-angle+rotation),
      y = r_label * sin(-angle+rotation),
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5
  )+
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Density") +
  coord_fixed() +
  facet_wrap(
    ~ Condition,
    ncol = 1,
    strip.position = "left",
    labeller = labeller(
      Condition = c(
        "F" = "CV: 0.38",
        "N" = "CV: 0.008"
      )
    )
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0.05, "lines"),
    strip.placement = "outside",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.2)
  ) +
  labs(
    title = "",
    fill = "Density"
  )

# By condition ------------------------------------------------------------
angle_density_1 <- angle_density %>% filter(time_bin_3min ==1)

F_Circ_var_P<- ggplot(angle_density_1 %>% filter(Condition == "F")) +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = as.numeric(time_bin_3min) - ring_thickness/2 + inner_radius,
      r  = as.numeric(time_bin_3min) + ring_thickness/2 + inner_radius,
      start = (head_angle_deg - bin_width/2) * pi/180 + rotation,
      end   = (head_angle_deg + bin_width/2) * pi/180 + rotation,
      fill = density
    ),
    color = NA
  ) +
  geom_segment(
    data = marker_df,
    aes(
      x    = r_start * cos(angle),
      y    = r_start * sin(angle),
      xend = r_end   * cos(angle),
      yend = r_end   * sin(angle)
    ),
    inherit.aes = FALSE,
    linewidth = 0.6,
    color = "black"
  )+
  geom_text(
    data = marker_df,
    aes(
      x = r_label * cos(-angle+rotation),
      y = r_label * sin(-angle+rotation),
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5
  )+
  #scale_fill_viridis_c(option = "plasma", direction = -1, name = "Density") +
  scale_fill_distiller(palette = "Greys", direction = 1, name = "Density")+
  coord_fixed() +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0.05, "lines"),
    strip.placement = "outside",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "none",
    #panel.border = element_rect(color = "black", fill = NA, size = 0.2)
    panel.border = element_blank()
  ) +
  labs(
    title = "",
    fill = "Density"
  )

N_circ_var_O<-ggplot(angle_density_1 %>% filter(Condition == "N")) +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = as.numeric(time_bin_3min) - ring_thickness/2 + inner_radius,
      r  = as.numeric(time_bin_3min) + ring_thickness/2 + inner_radius,
      start = (head_angle_deg - bin_width/2) * pi/180 + rotation,
      end   = (head_angle_deg + bin_width/2) * pi/180 + rotation,
      fill = density
    ),
    color = NA
  ) +
  geom_segment(
    data = marker_df,
    aes(
      x    = r_start * cos(angle),
      y    = r_start * sin(angle),
      xend = r_end   * cos(angle),
      yend = r_end   * sin(angle)
    ),
    inherit.aes = FALSE,
    linewidth = 0.6,
    color = "black"
  )+
  geom_text(
    data = marker_df,
    aes(
      x = r_label * cos(-angle+rotation),
      y = r_label * sin(-angle+rotation),
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5
  )+
  #scale_fill_viridis_c(option = "plasma", direction = -1, name = "Density") +
  scale_fill_distiller(palette = "Greys", direction = 1, name = "Density")+
  coord_fixed() +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0.05, "lines"),
    strip.placement = "outside",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "none",
    #panel.border = element_rect(color = "black", fill = NA, size = 0.2)
    panel.border = element_blank()
  ) +
  labs(
    title = "",
    fill = "Density"
  )


grid.arrange(N_circ_var_O, F_Circ_var_P,ncol=1 )




# speed example figs -------------------------------------------------------
#Fig 4b 
## Full exp ex 
ggplot(Circ_example_birds, aes(x=adj_frame, y=speed_head_psec, group=Condition, color =Condition))+
  geom_line(linewidth = 0.6, alpha=0.8)+
  #geom_smooth(aes(y = speed_head_psec), method = "loess", span = 0.1, se = FALSE, linewidth = 1)+
  labs(x="Frame",y="Speed (cm/sec)")+
  scale_color_manual(values = c("F"='purple',"N"="orange"))+
  theme_minimal()+
  facet_wrap(~Condition,ncol=1)+
  geom_vline(xintercept=5095, color="black", linetype= "dashed", size=1)+
  geom_vline(xintercept=0, color="black", linetype= "dashed", size=1)+
  xlim(-1,25471)+
  theme(
    legend.position='none',
    panel.grid = element_blank(),
    text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    #strip.text = element_text(size = 12, face = "bold"),
    strip.text = element_blank(),
    strip.text.y = element_text(angle = 0, size = 12, face = "bold")
  )


#Fig 4b 
ggplot(Circ_example_birds_bin1, aes(x=adj_frame, y=speed_head_psec, color = Condition))+
  geom_line()+
  facet_wrap(~Condition, ncol=1)+
  theme_minimal()+
  labs(x="Frame", y="Head speed (cm/sec)")+
  xlim(-1,5095)+
  theme(
    legend.position='none',
    panel.grid = element_blank(),
    text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    #strip.text = element_text(size = 12, face = "bold"),
    strip.text = element_blank(),
    strip.text.y = element_text(angle = 0, size = 12, face = "bold"))+
  scale_color_manual(values = c("F"="purple", "N" = "orange"))



# Entropy and area (fig4D)  -----------------------------------------------

Circ_example_birds_bin1$Condition <- factor(Circ_example_birds_bin1$Condition, 
                                            levels = c("N", "F"))
concave_hulls_df <- Circ_example_birds_bin1 %>%
  group_by(BirdID, Condition) %>%
  summarise(
    hull = list(
      concaveman(
        as.matrix(cur_data()[, c("x_head", "y_head_neg")]),
        concavity = 2,
        length_threshold = 1
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    hull = map(hull, ~ as.data.frame(.x))
  ) %>%
  unnest(hull) %>%
  rename(x = V1, y = V2)

concave_hulls_df$Condition <- factor(concave_hulls_df$Condition, 
                                     levels = c("N", "F"))
Circ_example_birds_bin1$Condition<-factor(Circ_example_birds_bin1$Condition, 
                                levels = c("N", "F"))

full_grid <- expand.grid(
  x_bin = seq(10 + bin_size/2, 30, by = bin_size),
  y_bin = seq(-20 + bin_size/2, 0, by = bin_size),
  Condition = unique(entropy_df$Condition)
)

full_grid$Condition <- factor(full_grid$Condition,levels = c("N", "F"))

ggplot() +
  geom_tile(
    data = full_grid,
    aes(x = x_bin, y = y_bin),
    width = bin_size, height = bin_size,
    fill = NA,
    color = "grey80",
    linewidth = 0.2
  ) +
  geom_point(
    data = Circ_example_birds_bin1,
    aes(x = x_head, y = y_head_neg, color = Condition, fill = Condition),
    size = 0.8,
    alpha = 1
  ) +
  geom_polygon(
    data = concave_hulls_df %>% filter(Condition == "N"),
    aes(x = x, y = y, fill = Condition),
    color = "black",
    linewidth = 1.2,
    alpha = 0.25
  ) +
  geom_polygon(
    data = concave_hulls_df %>% filter(Condition == "F"),
    aes(x = x, y = y, fill = Condition),
    color = "black",
    linewidth = 1.2,
    alpha = 0.25
  ) +
  facet_wrap(~Condition, ncol = 2) +
  scale_x_continuous(
    limits = c(10, 30),
    breaks = seq(10, 30, by = 5),
    labels = seq(0, 20, by = 5)       # relabel 10→0, 15→5, 20→10, 25→15, 30→20
  ) +
  scale_y_continuous(
    limits = c(-20, 0),
    breaks = seq(-20, 0, by = 5),
    labels = seq(20, 0, by = -5)      # relabel -20→20, -15→15, ..., 0→0
  ) +
  scale_color_manual(values = c("F" = "purple", "N" = "orange")) +
  scale_fill_manual(values = c("F" = "purple", "N" = "orange")) +
  coord_equal() +
  theme_classic2() +
  labs(x = 'x', y = "y") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid = element_blank(),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

