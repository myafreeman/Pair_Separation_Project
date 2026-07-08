# Behavioral analysis of pre-cleaned tracking data

##Load packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(lmerTest)
library(car)
library(performance)
library(emmeans)
library(SpatEntropy)
library(spatstat.geom)
library(concaveman)
library(sf)
library(readxl)
library(writexl)

#Load in data
#Large pre-processed df with:
# metadata (BirdID, Phase, Day, Condition, SessionID)
# cm conversion
# interpolated vectors of values for missing data
PairSep_data <- read_csv("../Tracking_data_prep/PairSep_combined_cm.csv")

##Calculate speed on interpolated values
  #In processing script was done on raw - change to interpolated for continious data
fps <- 60

# NOTE: grouped by BirdID, Phase, Day, SessionID 
PairSep_data <- PairSep_data %>%
  group_by(BirdID, Phase, Day, SessionID) %>%
  mutate(
    delta_x_head = x_head - lag(x_head, default = first(x_head)),
    delta_y_head = y_head - lag(y_head, default = first(y_head)),
    speed_head_pframe = sqrt(delta_x_head^2 + delta_y_head^2),
    speed_head_psec   = speed_head_pframe * fps
  ) %>%
  ungroup()


# Check Raw data ##########################
# Get metrics for amount of missing frames
# Metrics for amount of partial detection
##Percent instances missing in all cases

PairSep_data_metrics <- PairSep_data %>%
  mutate(
    head_only = !is.na(x_head_raw) &  is.na(x_beak_raw),
    beak_only =  is.na(x_head_raw) & !is.na(x_beak_raw),
    both_seen = !is.na(x_head_raw) & !is.na(x_beak_raw),
    neither   =  is.na(x_head_raw) &  is.na(x_beak_raw)
  )

PairSep_long <- PairSep_data_metrics %>%
  select(Condition, BirdID, Phase, Day, head_only, beak_only, both_seen, neither) %>%
  pivot_longer(
    cols = c(head_only, beak_only, both_seen, neither),
    names_to = "detection_type",
    values_to = "present"
  )

summary_detection <- PairSep_long %>%
  group_by(Condition, BirdID, Phase, Day, detection_type) %>%
  summarise(
    n_frames = sum(present),
    total_frames = n(),
    percent = 100 * n_frames / total_frames,
    .groups = "drop"
  )

summary_detection$detection_type <- factor(summary_detection$detection_type,
                                           levels = c("beak_only","head_only","neither","both_seen"))
ggplot(
  summary_detection,
  aes(
    x = detection_type,
    y = percent,
    fill = detection_type
  )
) +
  geom_violin(
    position = position_dodge(width = 0.8),
    outlier.shape = NA,
    alpha = 0.5
  ) +
  geom_point(
    aes(color = detection_type),
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.8
    ),
    size = 1,
    alpha = 0.7
  ) +
  labs(
    x = "Detection type",
    y = "Percentage of frames",
    fill = "Detection type",
    color = "Detection type",
    title = ""
  ) +
  theme_minimal(base_size = 13)+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+
  scale_color_manual(values = c("beak_only"="steelblue", "head_only"="chartreuse4", "both_seen"="indianred", "neither"="deeppink"))+
  scale_fill_manual(values = c("beak_only"="steelblue", "head_only"="chartreuse4", "both_seen"="indianred", "neither"="deeppink"))


all_birds_combined <- summary_detection %>%
  mutate(ID = paste(BirdID, Phase, Day, sep = "_")) %>%
  select(ID, detection_type, percent) %>%
  pivot_wider(names_from = detection_type, values_from = percent) %>%
  mutate(
    beak_combined = beak_only + both_seen,
    head_combined = head_only + both_seen
  )

all_beak_head_combined <- all_birds_combined %>%
  select(ID, beak_combined, head_combined) %>%
  pivot_longer(
    cols = c(beak_combined, head_combined),
               names_to = "detection_type",
               values_to = "percent"
  )

#Supp Fig 2b
ggplot(all_beak_head_combined, aes(x=detection_type, y=percent, fill=detection_type))+
  geom_violin(
    position = position_dodge(width = 0.8),
    outlier.shape = NA,
    alpha = 0.5
  ) +
  geom_point(
    aes(color = detection_type),
    position = position_jitter(width = 0.15),
    size = 1,
    alpha = 0.7
  ) +
  labs(
    x = "Detection type",
    y = "Percentage of frames",
    fill = "Detection type",
    color = "Detection type",
    title = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(size = 11),
    axis.ticks.y = element_line(colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_y_continuous(limits = c(0, 100))+
  scale_fill_manual(values = c(
    "beak_combined" = "steelblue",
    "head_combined" = "chartreuse4"
  )) +
  scale_color_manual(values = c(
    "beak_combined" = "steelblue",
    "head_combined" = "chartreuse4"
  ))


##Viz scale check
ggplot(PairSep_data, aes(x=x_head, y=y_head, color=BirdID))+
  geom_point()+
  theme_minimal()


# Check and filter Outliers ###########
#Remove outliers / errors --- Run filtering then run analysis vs no filtering
# using speed as outlier detector because it relies on both
# x and y cords instead of doing multiple filtering for each dimension
hist(PairSep_data$speed_head_psec, breaks=500)

upper_treshold <- quantile(PairSep_data$speed_head_psec, probs = 0.99, na.rm=TRUE)
print(upper_treshold)

PairSep_data <- PairSep_data %>% filter(speed_head_psec < upper_treshold)
hist(PairSep_data$speed_head_psec, breaks=500)

##Check of sample size after filtering
summary_PairSep <- PairSep_data %>%
  group_by(Condition, BirdID, Phase, Day) %>%
  summarise(
    total_frames   = n(),
    .groups = "drop"
  )




#Analysis Metrics ---------------------------------------------------------
#With data already filtered to some upper_threshold based on quantile measure

##Flip data to orient into correct quadrant (used by the area metric below)
PairSep_data$y_head_neg <- PairSep_data$y_head * -1


# AREA --------------------------------------------------------------------

#function to calculate concave hull area
concave_hull_area <- function(data) {

  coords <- as.matrix(data[, c("x_head", "y_head_neg")])

  # Need at least 3 unique points
  if (nrow(unique(coords)) < 3) {
    return(NA_real_)
  }

  hull_coords <- concaveman(
    coords,
    concavity = 2,
    length_threshold = 1
  )

  # Close the polygon if needed
  if (!all(hull_coords[1, ] == hull_coords[nrow(hull_coords), ])) {
    hull_coords <- rbind(hull_coords, hull_coords[1, ])
  }

  polygon <- st_polygon(list(hull_coords))

  as.numeric(st_area(polygon))
}


# Compute concave hull area per session (BirdID x Phase x Day)
Concave_hull_areas <- PairSep_data %>%
  group_by(BirdID, Condition, Phase, Day) %>%
  summarise(area = concave_hull_area(across(c(x_head, y_head_neg))), .groups = "drop")  # Correct way to pass data


# concave area stats ---------------------------------------------------------
hist(Concave_hull_areas$area)
hist(Concave_hull_areas$area^(1/3))
hist(log(Concave_hull_areas$area))

# ---- Experimental-birds-only subset for the inferential model ----
Concave_hull_areas_expt <- Concave_hull_areas %>% filter(Condition == "Experimental")

concave_hull_model_log <- glmmTMB(log(area) ~ Phase * Day + (1|BirdID),
                           family = gaussian(link = "identity"), data = Concave_hull_areas_expt,
                           control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#lower AIC (USED)
concave_hull_model <- glmmTMB((area)^(1/3) ~ Phase * Day + (1|BirdID),
                                   family = gaussian(link = "identity"), data = Concave_hull_areas_expt,
                                   control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

anova(concave_hull_model_log, concave_hull_model)

Anova(concave_hull_model)
summary(concave_hull_model)
simulationOutput <- simulateResiduals(fittedModel = concave_hull_model, plot = TRUE)

pairs(emmeans(concave_hull_model, ~ Phase | Day, at = list(Day = c(1, 3, 5))), adjust = "holm")
emtrends(concave_hull_model, ~ Phase, var = "Day")

#plot model predictions over data
emm_poly <- emmeans(
  concave_hull_model,
  ~ Phase * Day,
  at = list(Day = 1:5)
)

emm_df_poly <- as.data.frame(emm_poly) %>%
  mutate(
    area_hat = emmean,
    lower.CL = asymp.LCL,
    upper.CL = asymp.UCL
  )

ggplot(Concave_hull_areas,
       aes(x = Day, y = (area)^(1/3), color = Phase)) +

  # Raw data (all birds, including control)
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +

  geom_line(
    aes(group = interaction(BirdID, Phase), linetype = Condition),
    alpha = 0.15,
    linewidth = 0.4
  ) +

  # Model-predicted means (experimental birds only, correct scale)
  geom_line(
    data = emm_df_poly,
    aes(y = area_hat, group = Phase),
    linewidth = 1.2
  ) +

  # 95% CI ribbon
  geom_ribbon(
    data = emm_df_poly,
    aes(
      y = area_hat,
      ymin = lower.CL,
      ymax = upper.CL,
      fill = Phase,
      group = Phase
    ),
    alpha = 0.25,
    color = NA
  ) +

  scale_color_manual(values = c("Baseline"="purple", "PairSep"="orange")) +
  scale_fill_manual(values = c("Baseline"="purple", "PairSep"="orange")) +
  scale_linetype_manual(values = c("Experimental" = "solid", "Control" = "dashed")) +
  labs(x = "Day", y = "(Area)^(1/3)", linetype = "Condition (bird)") +
  theme_classic()+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )




# ENTROPY -----------------------------------------------------------------

#Compute Shannon entropy per session (BirdID x Phase x Day)
compute_spatial_entropy_time <- function(data, grid_width, grid_height, grid_size = 100, jitter_amount = 0.0001) {
  results <- data %>%
    group_by(BirdID, Condition, Phase, Day) %>%
    summarise(
      entropy = list({
        # Subset data for the current group
        data_group <- cur_data()

        # Jitter positions to avoid duplicate points
        data_group$x_jittered <- jitter(data_group$x_head, amount = jitter_amount)
        data_group$y_jittered <- jitter(data_group$y_head, amount = jitter_amount)

        #Ensure jitter is not above grid size
        data_group$x_jittered <- pmin(
          pmax(data_group$x_jittered, 0),
          grid_width
        )

        data_group$y_jittered <- pmin(
          pmax(data_group$y_jittered, 0),
          grid_height
        )


        # Define fixed grid (based on total data)
        x_breaks <- seq(0, grid_width, length.out = grid_size + 1)
        y_breaks <- seq(0, grid_height, length.out = grid_size + 1)

        # Assign each (x, y) to a grid cell
        x_bins <- cut(data_group$x_jittered, breaks = x_breaks,
                      labels = FALSE, include.lowest = TRUE)
        y_bins <- cut(data_group$y_jittered, breaks = y_breaks,
                      labels = FALSE, include.lowest = TRUE)
        grid_labels <- factor(paste(x_bins, y_bins, sep = "_"))  # Grid cell ID

        # Create full grid with all possible grid labels
        full_grid <- expand.grid(x = 1:grid_size, y = 1:grid_size) %>%
          mutate(grid_id = factor(paste(x, y, sep = "_")))

        # Count visits to each grid cell
        grid_counts <- as.data.frame(table(grid_labels))
        colnames(grid_counts) <- c("grid_id", "count")

        # Merge with full grid to include unvisited cells
        grid_counts <- full_grid %>%
          left_join(grid_counts, by = "grid_id") %>%
          mutate(count = ifelse(is.na(count), 0, count))  # Set unvisited cells to zero

        # Compute probabilities
        grid_counts$prob <- grid_counts$count / sum(grid_counts$count)

        # Define spatial window
        win <- owin(xrange = c(0, grid_width), yrange = c(0, grid_height))

        # Create spatial point pattern with jittered points
        ppp_obj <- ppp(data_group$x_jittered, data_group$y_jittered, window = win, marks = grid_labels)

        # Compute Shannon entropy
        entropy_values <- shannon(ppp_obj)

        # Return entropy values
        tibble(
          shannon_entropy = entropy_values$shann,
          rel_shannon_entropy = entropy_values$rel.shann,
          occupied_grid_cells = length(unique(grid_labels))  # This is I
        )
      }),
      .groups = "drop"
    ) %>%
    unnest(entropy)  # Unpack entropy columns

  return(results)
}

# Grid extent set from this dataset's observed coordinate range rather than
# assuming a 25.4x25.4cm square window: y_head spans ~0-25.4cm as expected
# from the 10in/25.4cm arena height, but x_head spans ~6-41cm -- notably
# wider than the cage footprint. Worth checking the camera FOV / arena
# calibration upstream. grid_width/grid_height below just cover the observed
# range so points aren't clipped; grid_size (bin count per axis) matches the
# original.
grid_size <- 42

PairSep_int_ent <- compute_spatial_entropy_time(PairSep_data, grid_width = 42, grid_height = 26, grid_size = 42)


# Entropy stats ---------------------------------------------------
hist(PairSep_int_ent$shannon_entropy)
hist(sqrt(PairSep_int_ent$shannon_entropy))
hist(PairSep_int_ent$shannon_entropy^(1/3))
hist(log(PairSep_int_ent$shannon_entropy))

# ---- Experimental-birds-only subset for the inferential model ----
PairSep_int_ent_expt <- PairSep_int_ent %>% filter(Condition == "Experimental")

ent_model <- glmmTMB(shannon_entropy ~ Phase * Day + (1|BirdID),
                           family = gaussian(link = "identity"), data = PairSep_int_ent_expt)


Anova(ent_model)
summary(ent_model)

#residuals
simulationOutput <- simulateResiduals(fittedModel = ent_model, plot = TRUE)
plot(residuals(ent_model))
qqnorm(resid(ent_model))
qqline(resid(ent_model))

pairs(emmeans(ent_model, ~ Phase | Day, at = list(Day = c(1, 3, 5))), adjust = "holm")
emtrends(ent_model, ~ Phase, var = "Day")

#Predicted and raw points plot
emm_ent <- emmeans(
  ent_model,
  ~ Phase * Day,
  at = list(Day = 1:5)
)

emm_df_ent <- as.data.frame(emm_ent) %>%
  mutate(lower.CL = asymp.LCL, upper.CL = asymp.UCL)

ggplot(PairSep_int_ent,
       aes(x = Day, y = shannon_entropy, color = Phase)) +

  # Raw data (all birds, including control)
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +

  geom_line(
    aes(group = interaction(BirdID, Phase), linetype = Condition),
    alpha = 0.15,
    linewidth = 0.4
  ) +

  # Model-predicted means (experimental birds only)
  geom_line(
    data = emm_df_ent,
    aes(y = emmean, group = Phase),
    linewidth = 1.2
  ) +

  # 95% CI ribbon
  geom_ribbon(
    data = emm_df_ent,
    aes(
      y = emmean,
      ymin = lower.CL,
      ymax = upper.CL,
      fill = Phase,
      group = Phase
    ),
    alpha = 0.25,
    color = NA
  ) +
  scale_color_manual(values = c("Baseline"="purple", "PairSep"="orange"))+
  scale_fill_manual(values = c("Baseline"="purple", "PairSep"="orange"))+
  scale_linetype_manual(values = c("Experimental" = "solid", "Control" = "dashed")) +
  labs(x="Day", y="Shannon Entropy", linetype = "Condition (bird)")+
  theme_classic()+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )




# SPEED  ------------------------------------------------------------

hist(PairSep_data$speed_head_psec) #check outliers gone

#Average speed per session (BirdID x Phase x Day)
PairSep_binned_speed <- PairSep_data %>%
  group_by(BirdID, Condition, Phase, Day) %>%
  summarise(
    mean_speed = mean(speed_head_psec),
    median_head_speed = median(speed_head_psec),
    .groups = "drop"
  )

# speed stats ---------------------------------------------------------
hist(PairSep_binned_speed$mean_speed)
hist(sqrt(PairSep_binned_speed$mean_speed))
hist(PairSep_binned_speed$mean_speed^(1/3))
hist(log(PairSep_binned_speed$mean_speed)) #most normal

# ---- Experimental-birds-only subset for the inferential model ----
PairSep_binned_speed_expt <- PairSep_binned_speed %>% filter(Condition == "Experimental")

speed_model <- glmmTMB(log(mean_speed) ~ Phase * Day + (1|BirdID),
                          family = gaussian(link = "identity"), data = PairSep_binned_speed_expt)

Anova(speed_model)
summary(speed_model)
simulationOutput <- simulateResiduals(fittedModel = speed_model, plot = TRUE)
plot(residuals(speed_model))
qqnorm(resid(speed_model))
qqline(resid(speed_model))

pairs(emmeans(speed_model, ~ Phase | Day, at = list(Day = c(1, 3, 5))), adjust = "fdr")
emtrends(speed_model, ~ Phase, var = "Day")


### Raw points and model predictions plot #########
emm_speed <- emmeans(
  speed_model,
  ~ Phase * Day,
  at = list(Day = 1:5)
)

#to plot log values in back
emm_df_speed <- as.data.frame(emm_speed) %>%
  mutate(
    area_hat = emmean,
    lower.CL = asymp.LCL,
    upper.CL = asymp.UCL
  )

ggplot(PairSep_binned_speed,
       aes(x = Day, y = log(mean_speed), color = Phase)) +

  # Raw data (all birds, including control)
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +

  geom_line(
    aes(group = interaction(BirdID, Phase), linetype = Condition),
    alpha = 0.15,
    linewidth = 0.4
  ) +

  # Model-predicted means (experimental birds only, correct scale)
  geom_line(
    data = emm_df_speed,
    aes(y = area_hat, group = Phase),
    linewidth = 1.2
  ) +

  # 95% CI ribbon
  geom_ribbon(
    data = emm_df_speed,
    aes(
      y = area_hat,
      ymin = lower.CL,
      ymax = upper.CL,
      fill = Phase,
      group = Phase
    ),
    alpha = 0.25,
    color = NA
  ) +

  scale_color_manual(values = c("Baseline"="purple", "PairSep"="orange")) +
  scale_fill_manual(values = c("Baseline"="purple", "PairSep"="orange")) +
  scale_linetype_manual(values = c("Experimental" = "solid", "Control" = "dashed")) +
  labs(x = "Day", y = "log(Mean Speed (cm/sec))", linetype = "Condition (bird)") +
  theme_classic()+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )




# Combine all metrics into 1 data frame ------------------------------------------

#Format metric data
PairSep_int_ent$ID <- paste(PairSep_int_ent$BirdID, PairSep_int_ent$Phase, PairSep_int_ent$Day, sep = "_")

PairSep_binned_speed$ID <- paste(PairSep_binned_speed$BirdID, PairSep_binned_speed$Phase, PairSep_binned_speed$Day, sep = "_")

Concave_hull_areas$ID <- paste(Concave_hull_areas$BirdID, Concave_hull_areas$Phase, Concave_hull_areas$Day, sep = "_")
Concave_hull_areas <- rename(Concave_hull_areas, concave_area = area)

#Combine metrics into one data frame
all_metrics <- merge(PairSep_binned_speed, PairSep_int_ent[, c("ID", "shannon_entropy")], by = "ID", all.x = TRUE)
all_metrics <- merge(all_metrics, Concave_hull_areas[, c("ID", "concave_area")], by = "ID", all.x = TRUE)
