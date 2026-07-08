# Analysis script for pre-cleaned tracking data #
  
##Load packages 
library(readr)
library(emmeans)
library(writexl)
library(readxl)
library(sjPlot)
library(dplyr)
library(ggplot2)
library(circular)
library(knitr)
library(pracma)
library(gridExtra)
library(ggquiver)
library(glmmTMB)
library(DHARMa)  
library(lmerTest)  
library(car)       
library(performance)
library(SpatEntropy)
library(spatstat.geom)  
library(tidyr)
library(geometry)
library(ggpubr)
library(concaveman)
library(sf) 

#Load in data 
#Large pre-processed dfs with: 
# metadata
# cm conversion  
# interpolated vectors of values for missing data

#Unmanipulated birds
U_Birds_90 <- read.csv("/Users/michaelabierman/Downloads/LAB/Daria/Results/data/speed profiles/Final_model/UnmanipulatedBirds/UnmanipulatedBirds_combined_cm.csv")

#Injected Birds 
Inj_Birds_all <- read.csv("/Users/michaelabierman/Downloads/LAB/Daria/Results/data/speed profiles/Final_model/FN90_InjectedBirds/FN90_InjectedBirds_combined_cm.csv")


#Create 3 min time bins 
U_Birds_90 <- U_Birds_90 %>%
  group_by(BirdID) %>%
  mutate(
    time_bin_3min = cut(adj_frame, breaks = 5, labels = FALSE)  # Divide into 5 equal bins (3min)
  ) %>%
  ungroup()

#Time bin 3 min 
Inj_Birds_all <- Inj_Birds_all %>%
  group_by(BirdID) %>%
  mutate(
    time_bin_3min = cut(adj_frame, breaks = 5, labels = FALSE)  # Divide into 5 equal bins (3min)
  ) %>%
  ungroup()


#Nest injection and lesion 
Inj_Birds_all$Inj_Lesion <- paste(Inj_Birds_all$Injection, Inj_Birds_all$Lesion, sep = "_")  


#Remove excluded birds 
excluded_birds <- c("bl85gy195","bl21or11", "bl48pu128", "bl191or41", "bl10or50", "bl25or15")

Inj_Birds_all <- Inj_Birds_all %>% filter(!(BirdID %in% excluded_birds))


#Example birds for Fig 
Familiar_ex <- "bl143or73"
Novel_ex <- "bl186gr54"


final_birds <- Inj_Birds_all %>% 
  select(BirdID, Condition, Inj_Lesion) %>% 
  distinct()
write.csv(final_birds, "/Users/michaelabierman/Downloads/LAB/Daria/Results/Figures/Neural Figs/final_birds.csv")

##Calculate speed on interpolated values
  #In processing script was done on raw - change to interpolated for continious data  
fps <- 30

U_Birds_90 <- U_Birds_90 %>%
  group_by(BirdID, Condition) %>%
  mutate(
    delta_x_head = x_head - lag(x_head, default = first(x_head)),
    delta_y_head = y_head - lag(y_head, default = first(y_head)),
    speed_head_pframe = sqrt(delta_x_head^2 + delta_y_head^2),
    speed_head_psec   = speed_head_pframe * fps
  )

Inj_Birds_all <- Inj_Birds_all %>%
  group_by(BirdID, Condition) %>%
  mutate(
    delta_x_head = x_head - lag(x_head, default = first(x_head)),
    delta_y_head = y_head - lag(y_head, default = first(y_head)),
    speed_head_pframe = sqrt(delta_x_head^2 + delta_y_head^2),
    speed_head_psec   = speed_head_pframe * fps
  )


# Check Raw data ##########################
# Get metrics for amount of missing frames
# Metrics for amount of partial detection
##Percent instances missing in all cases 

##   UM    ##
U_Birds_90_metrics <- U_Birds_90 %>%
  mutate(
    head_only = !is.na(x_head_raw) &  is.na(x_beak_raw),
    beak_only =  is.na(x_head_raw) & !is.na(x_beak_raw),
    both_seen = !is.na(x_head_raw) & !is.na(x_beak_raw),
    neither   =  is.na(x_head_raw) &  is.na(x_beak_raw)
  )

UM90_long <- U_Birds_90_metrics %>%
  select(Condition, BirdID, head_only, beak_only, both_seen, neither) %>%
  pivot_longer(
    cols = c(head_only, beak_only, both_seen, neither),
    names_to = "detection_type",
    values_to = "present"
  )

summary_UM90 <- UM90_long %>%
  group_by(Condition, BirdID, detection_type) %>%
  summarise(
    n_frames = sum(present),
    total_frames = n(),
    percent = 100 * n_frames / total_frames,
    .groups = "drop"
  )

##.   Inj   ##
Inj_Birds_all_metrics <- Inj_Birds_all %>%
  mutate(
    head_only = !is.na(x_head_raw) &  is.na(x_beak_raw),
    beak_only =  is.na(x_head_raw) & !is.na(x_beak_raw),
    both_seen = !is.na(x_head_raw) & !is.na(x_beak_raw),
    neither   =  is.na(x_head_raw) &  is.na(x_beak_raw)
  )


Inj_long <- Inj_Birds_all_metrics %>%
  select(Injection, Condition, BirdID, head_only, beak_only, both_seen, neither) %>%
  pivot_longer(
    cols = c(head_only, beak_only, both_seen, neither),
    names_to = "detection_type",
    values_to = "present"
  )

summary_Inj <- Inj_long %>%
  group_by(Injection, Condition, BirdID, detection_type) %>%
  summarise(
    n_frames = sum(present),
    total_frames = n(),
    percent = 100 * n_frames / total_frames,
    .groups = "drop"
  )


all_birds_nframes <- bind_rows(summary_Inj, summary_UM90)

all_birds_nframes$Injection[is.na(all_birds_nframes$Injection)] <- "UM"
all_birds_nframes$ID <- paste(all_birds_nframes$Injection, all_birds_nframes$BirdID, sep = "_")  

all_birds_nframes$detection_type <- factor(all_birds_nframes$detection_type, 
                                           levels = c("beak_only","head_only","neither","both_seen"))
ggplot(
  all_birds_nframes, 
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


all_birds_combined <- all_birds_nframes %>%
  select(ID, detection_type, percent) %>%  # keep only the columns you need
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
ggplot(U_Birds_90, aes(x=x_head, y=y_head, color=BirdID))+
  geom_point()+
  theme_minimal()

ggplot(Inj_Birds_all, aes(x=x_head, y=y_head, color=BirdID))+
  geom_point()+
  theme_minimal()


# Check and filter Outliers ###########
#Remove outliers / errors --- Run filtering then run analysis vs no filtering 

## Unmanipulated Birds ##
# using speed as outlier detector because it relies on both
# x and y cords instead of doing multiple filtering for each dimension
hist(U_Birds_90$speed_head_psec, breaks=500)

upper_treshold <- quantile(U_Birds_90$speed_head_psec, probs = 0.99, na.rm=TRUE)
print(upper_treshold)

U_Birds_90 <- U_Birds_90 %>% filter (speed_head_psec < upper_treshold)
hist(U_Birds_90$speed_head_psec, breaks=500)


##Check of sample size after filtering 
summary_UM_90 <- U_Birds_90 %>%
  group_by(Condition, BirdID, time_bin_3min) %>%
  summarise(
    total_frames   = n(),
    .groups = "drop"
  )



# Inj Birds # 
hist(Inj_Birds_all$speed_head_psec, breaks = 500)

upper_treshold <- quantile(Inj_Birds_all$speed_head_psec, probs = 0.99, na.rm=TRUE)
print(upper_treshold)

Inj_Birds_all <- Inj_Birds_all %>% filter (speed_head_psec < upper_treshold)
hist(Inj_Birds_all$speed_head_psec, breaks=500)

##Check of sample size after filtering 
summary_inj <- Inj_Birds_all %>%
  group_by(Condition, BirdID, time_bin_3min) %>%
  summarise(
    total_frames   = n(),
    .groups = "drop"
  )






#Analysis Metrics ---------------------------------------------------------
#With data already filtered to some upper_threshold based on quantile measure 

# Circular Variance of Head angles ----------------------------------------
##Flip data to orient into correct quadrant 
U_Birds_90$y_head_neg <- U_Birds_90$y_head * -1 
U_Birds_90$y_beak_neg <- U_Birds_90$y_beak * -1 

Inj_Birds_all$y_head_neg <- Inj_Birds_all$y_head * -1 
Inj_Birds_all$y_beak_neg <- Inj_Birds_all$y_beak * -1 

# Function to compute head angle in radians
compute_head_angle <- function(x_head, y_head_neg, x_beak, y_beak_neg) {
  atan2(y_beak_neg - y_head_neg, x_beak - x_head)  # Angle from head to beak
}

# Add head angle column to data frame unmanipualted 
U_Birds_90 <- U_Birds_90 %>%
  mutate(head_angle = compute_head_angle(x_head, y_head_neg, x_beak, y_beak_neg))

U_Birds_90 <- U_Birds_90 %>%
  mutate(head_angle_deg = (head_angle * 180 / pi) %% 360)


# Add head angle column to data frame Injected 
Inj_Birds_all <- Inj_Birds_all %>%
  mutate(head_angle = compute_head_angle(x_head, y_head_neg, x_beak, y_beak_neg))

Inj_Birds_all <- Inj_Birds_all %>%
  mutate(head_angle_deg = (head_angle * 180 / pi) %% 360)


# Unmanipulated Birds Circular Variance ------- 
#  UM CIRC VAR  ##
circular_variance <- U_Birds_90 %>%
  group_by(BirdID, Condition, time_bin_3min) %>%
  summarise(head_var = var.circular(circular(head_angle), na.rm = TRUE), .groups = "drop")


# Unmanipulated Circ Var Stats  #
circular_variance$time_bin_3min <- factor(circular_variance$time_bin_3min, ordered = TRUE) 

hist(circular_variance$head_var)

var_model_UM <- glmmTMB(head_var ~Condition * time_bin_3min + (1|BirdID),
                        family = beta_family(link = "logit"), data = circular_variance,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

Anova(var_model_UM)
summary(var_model_UM)

#residuals
simulationOutput <- simulateResiduals(fittedModel = var_model_UM, plot = TRUE)
plot(residuals(var_model_UM))
qqnorm(resid(var_model_UM))
qqline(resid(var_model_UM))
pairs(emmeans(var_model_UM, ~ Condition | time_bin_3min), adjust = "holm")

### UM Raw points and model predictions plot ######### 
emm_var <- emmeans(
  var_model_UM,
  ~ Condition * time_bin_3min,
  type = "response"   # IMPORTANT: back-transforms from logit to original scale
)

emm_df <- as.data.frame(emm_var)

#Fig 3a
ggplot(circular_variance,
       aes(x = time_bin_3min, y = head_var, color = Condition)) +
  
  # Raw data
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +
  
  geom_line(
    aes(group = BirdID),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  
  # Model-predicted means
  geom_line(
    data = emm_df,
    aes(y = response, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon (asymptotic)
  geom_ribbon(
    data = emm_df,
    aes(
      y = response,
      ymin = asymp.LCL,
      ymax = asymp.UCL,
      fill = Condition,
      group = Condition
    ),
    alpha = 0.25,
    color = NA
  ) +
  scale_color_manual(values = c("F"="purple", "N"="orange"))+
  scale_fill_manual(values = c("F"="purple", "N"="orange"))+
  labs(x="Time bin (3min)", y="CV of Head Angles")+
  theme_classic()+
  theme(
    legend.position = "none",
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



###          INJ CIRC VAR          ###        #####
circular_variance_inj <- Inj_Birds_all %>%
  group_by(BirdID, Condition, Inj_Lesion, time_bin_3min) %>%
  summarise(head_var = var.circular(circular(head_angle), na.rm=TRUE), .groups = "drop")


circular_variance_inj$time_bin_3min <- factor(circular_variance_inj$time_bin_3min, ordered = TRUE)  

##Run before model or plot will be mixed up 
circular_variance_inj$Inj_Lesion <- factor(
  circular_variance_inj$Inj_Lesion,
  levels = c("IgG-SAP_No", "anti-DBH-SAP_No", "anti-DBH-SAP_Yes")
)

##Full Model with nested Inj_Lesion 
Inj_var_model <- glmmTMB(head_var ~Condition * time_bin_3min * Inj_Lesion + (1|BirdID),
                         family = beta_family(link = "logit"), data = circular_variance_inj,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

Anova(Inj_var_model)
summary(Inj_var_model)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_var_model, plot = TRUE)
plot(residuals(Inj_var_model))
qqnorm(resid(Inj_var_model))
qqline(resid(Inj_var_model))


### INJ Raw points and model predictions plot ######### 
emm_var_inj <- emmeans(
  Inj_var_model,
  ~ Condition * time_bin_3min | Inj_Lesion,
  type = "response"
)

emm_var_inj_df <- as.data.frame(emm_var_inj)

ggplot(circular_variance_inj,
       aes(x = time_bin_3min, y = head_var, color = Condition)) +
  
  # Raw data
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +
  
  geom_line(
    aes(group = BirdID),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  
  # Model-predicted means
  geom_line(
    data = emm_var_inj_df,
    aes(y = response, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon (asymptotic)
  geom_ribbon(
    data = emm_var_inj_df,
    aes(
      y = response,
      ymin = asymp.LCL,
      ymax = asymp.UCL,
      fill = Condition,
      group = Condition
    ),
    alpha = 0.25,
    color = NA
  ) +
  
  # Separate panels by injection/lesion group
  facet_wrap(~ Inj_Lesion, nrow = 1) +
  
  scale_color_manual(values = c("F"="purple", "N"="orange"))+
  scale_fill_manual(values = c("F"="purple", "N"="orange"))+
  labs(x="Time bin (3min)", y="CV of Head Angles")+
  theme_classic()+
  theme(
    legend.position = "none",
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


###  POST HOC models ##############
DBH_no <- circular_variance_inj %>% filter(Inj_Lesion == "anti-DBH-SAP_No")
DBH_yes <- circular_variance_inj %>% filter(Inj_Lesion == "anti-DBH-SAP_Yes")
IgG <- circular_variance_inj %>% filter(Inj_Lesion == "IgG-SAP_No")


##Cir var stats 
#######DBH no 
DBH_no$time_bin_3min <- as.factor(DBH_no$time_bin_3min) 

dbh_no_var_model <- glmmTMB(head_var ~Condition * time_bin_3min + (1|BirdID),
                            family = beta_family(link = "logit"), data = DBH_no,
                            control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

Anova(dbh_no_var_model)

summary(dbh_no_var_model)

#residuals
plot(residuals(dbh_no_var_model))
qqnorm(resid(dbh_no_var_model))
qqline(resid(dbh_no_var_model))

pairs(emmeans(dbh_no_var_model, ~ Condition|time_bin), adjust = "holm")



#####DBH Yes
DBH_yes$time_bin_3min <- as.factor(DBH_yes$time_bin_3min)

dbh_yes_var_model <- glmmTMB(head_var ~Condition * time_bin_3min + (1|BirdID),
                             family = beta_family(link = "logit"), data = DBH_yes,
                             control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

Anova(dbh_yes_var_model)

summary(dbh_yes_var_model)

#residuals
plot(residuals(dbh_yes_var_model))
qqnorm(resid(dbh_yes_var_model))
qqline(resid(dbh_yes_var_model))
pairs(emmeans(dbh_yes_var_model, ~ Condition|time_bin_3min), adjust = "fdr")


#####IgG
IgG$time_bin_3min <- as.factor(IgG$time_bin_3min)

igg_var_model <- glmmTMB(head_var ~Condition * time_bin_3min + (1|BirdID),
                         family = beta_family(link = "logit"), data =IgG,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

Anova(igg_var_model)

summary(igg_var_model)

#residuals
plot(residuals(igg_var_model))
qqnorm(resid(igg_var_model))
qqline(resid(igg_var_model))

pairs(emmeans(igg_var_model, ~ Condition | time_bin_3min), adjust = "holm")













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


# UM Compute concave hull area over time (3min)
Concave_hull_areas_3min <- U_Birds_90 %>%
  group_by(BirdID, Condition, time_bin_3min) %>%
  summarise(area = concave_hull_area(across(c(x_head, y_head_neg))))  # Correct way to pass data


# UM concave area stats ---------------------------------------------------------
Concave_hull_areas_3min$time_bin_3min <- factor(Concave_hull_areas_3min$time_bin_3min, ordered = TRUE)

hist(Concave_hull_areas_3min$area)
hist(Concave_hull_areas_3min$area^(1/3))
hist(log(Concave_hull_areas_3min$area))

concave_hull_model_3min_log <- glmmTMB(log(area) ~ Condition * time_bin_3min + (1|BirdID),
                           family = gaussian(link = "identity"), data = Concave_hull_areas_3min,
                           control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
#lower AIC (USED)
concave_hull_model_3min <- glmmTMB((area)^(1/3) ~ Condition * time_bin_3min + (1|BirdID),
                                   family = gaussian(link = "identity"), data = Concave_hull_areas_3min,
                                   control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

anova(concave_hull_model_3min_log,concave_hull_model_3min)

Anova(concave_hull_model_3min)
summary(concave_hull_model_3min)
simulationOutput <- simulateResiduals(fittedModel = concave_hull_model_3min, plot = TRUE)

#plot model predictions over data 
emm_poly <- emmeans(
  concave_hull_model_3min,
  ~ Condition * time_bin_3min
)

emm_df_poly <- as.data.frame(emm_poly) %>%
  mutate(
    area_hat = emmean,
    lower.CL = lower.CL,
    upper.CL = upper.CL
  )

ggplot(Concave_hull_areas_3min,
       aes(x = time_bin_3min, y = (area)^(1/3), color = Condition)) +
  
  # Raw data
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +
  
  geom_line(
    aes(group = BirdID),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  
  # Model-predicted means (correct scale)
  geom_line(
    data = emm_df_poly,
    aes(y = area_hat, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon
  geom_ribbon(
    data = emm_df_poly,
    aes(
      y = area_hat,
      ymin = lower.CL,
      ymax = upper.CL,
      fill = Condition,
      group = Condition
    ),
    alpha = 0.25,
    color = NA
  ) +
  
  scale_color_manual(values = c("F"="purple", "N"="orange")) +
  scale_fill_manual(values = c("F"="purple", "N"="orange")) +
  labs(x = "Time bin (3 min)", y = "(Area)^(1/3)") +
  theme_classic()+
  theme(
    legend.position = "none",
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

### Inj concave area 
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


# Compute concave hull area for each condition (3min)
Concave_hull_areas_3min_inj <- Inj_Birds_all %>%
  group_by(BirdID, Condition, time_bin_3min, Inj_Lesion) %>%
  summarise(area = concave_hull_area(across(c(x_head, y_head_neg))))  # Correct way to pass data


# Inj concave area stats --------------------------------------------------
Concave_hull_areas_3min_inj$time_bin_3min <- factor(Concave_hull_areas_3min_inj$time_bin_3min, ordered = TRUE)


concave_hull_model_inj <- glmmTMB((concave_area^(1/3)) ~ Condition * time_bin_3min * Inj_Lesion + (1|BirdID),
                          family = gaussian(link = "identity"), data = Concave_hull_areas_3min_inj)

simulationOutput <- simulateResiduals(fittedModel = concave_hull_model_inj, plot = TRUE)
Anova(concave_hull_model_inj)
summary(hull_model_inj)

#residuals
plot(residuals(hull_model_inj))
qqnorm(resid(hull_model_inj))
qqline(resid(hull_model_inj))

##DBH no lesion 
hull_area_DBH_no <- Concave_hull_areas_3min_inj %>% filter(Inj_Lesion =="anti-DBH-SAP_No")
hull_area_DBH_no$time_bin_3min <- factor(hull_area_DBH_no$time_bin_3min, ordered = TRUE) 

##DBH lesion 
hull_area_DBH_yes <- Concave_hull_areas_3min_inj %>% filter(Inj_Lesion =="anti-DBH-SAP_Yes")
hull_area_DBH_yes$time_bin_3min <- factor(hull_area_DBH_yes$time_bin_3min, ordered = TRUE) 

##IgG 
hull_area_IgG <- Concave_hull_areas_3min_inj %>% filter(Inj_Lesion =="IgG-SAP_No")
hull_area_IgG$time_bin_3min <- factor(hull_area_IgG$time_bin_3min, ordered = TRUE) 


##DBHno _____________
hist(hull_area_DBH_no$concave_area)
hist(sqrt(hull_area_DBH_no$concave_area))
hist(hull_area_DBH_no$concave_area^(1/3))
hist(log(hull_area_DBH_no$concave_area))

hull_model_DBH_no <- glmmTMB((concave_area^(1/3)) ~ Condition * time_bin_3min + (1|BirdID),
                             family = gaussian(link = "identity"), data = hull_area_DBH_no)


simulationOutput <- simulateResiduals(fittedModel = hull_model_DBH_no, plot = TRUE)
Anova(hull_model_DBH_no)
summary(hull_model_DBH_no)
pairs(emmeans(hull_model_DBH_no, ~ Condition | time_bin), adjust = "holm")


#residuals
plot(residuals(hull_model_DBH_no))
qqnorm(resid(hull_model_DBH_no))
qqline(resid(hull_model_DBH_no))

##DBHyes ______________
hull_model_DBH_yes <- glmmTMB((concave_area^(1/3)) ~ Condition * time_bin_3min + (1|BirdID),
                              family = gaussian(link = "identity"), data = hull_area_DBH_yes)

Anova(hull_model_DBH_yes)
summary(hull_model_DBH_yes)
pairs(emmeans(hull_model_DBH_yes, ~ Condition | time_bin), adjust = "holm")


#residuals
plot(residuals(hull_model_DBH_yes))
qqnorm(resid(hull_model_DBH_yes))
qqline(resid(hull_model_DBH_yes))

##IgG 
hull_model_IgG <- glmmTMB((concave_area^(1/3)) ~ Condition * time_bin_3min + (1|BirdID),
                          family = gaussian(link = "identity"), data = hull_area_IgG)

Anova(hull_model_IgG)
summary(hull_model_IgG)
pairs(emmeans(hull_model_IgG, ~ Condition | time_bin_3min), adjust = "fdr")


#residuals
plot(residuals(hull_model_IgG))
qqnorm(resid(hull_model_IgG))
qqline(resid(hull_model_IgG))




# ENTROPY -----------------------------------------------------------------

# UM entropy ---------------------------------------------------
#Compute Shannon entropy over time for UM birds 
compute_spatial_entropy_time <- function(data, grid_width, grid_height, grid_size = 100, jitter_amount = 0.0001) {
  results <- data %>%
    group_by(BirdID, Condition, time_bin_3min) %>%
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

grid_size <- 42 #~0.5cm grid cells 

U_int_ent <- compute_spatial_entropy_time(U_Birds_90, grid_width = 40, grid_height = 21, grid_size = 42)


# UM bird Entropy stats ---------------------------------------------------
hist(U_int_ent$shannon_entropy)
hist(sqrt(U_int_ent$shannon_entropy))
hist(U_int_ent$shannon_entropy^(1/3))
hist(log(U_int_ent$shannon_entropy))

U_int_ent$time_bin_3min <- factor(U_int_ent$time_bin_3min, ordered = TRUE)

U_int_ent_model <- glmmTMB(shannon_entropy ~ Condition * time_bin_3min + (1|BirdID), 
                           family=gaussian(link="identity"), data=U_int_ent)


Anova(U_int_ent_model)
summary(U_int_ent_model)

#residuals
simulationOutput <- simulateResiduals(fittedModel = U_int_ent_model, plot = TRUE)
plot(residuals(U_int_ent_model))
qqnorm(resid(U_int_ent_model))
qqline(resid(U_int_ent_model)) 


#Predicted and raw points plot 
emm_ent <- emmeans(
  U_int_ent_model,
  ~ Condition * time_bin_3min
)

emm_df_ent <- as.data.frame(emm_ent)

ggplot(U_int_ent,
       aes(x = time_bin_3min, y = shannon_entropy, color = Condition)) +
  
  # Raw data
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +
  
  geom_line(
    aes(group = BirdID),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  
  # Model-predicted means
  geom_line(
    data = emm_df_ent,
    aes(y = emmean, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon 
  geom_ribbon(
    data = emm_df_ent,
    aes(
      y = emmean,
      ymin = lower.CL,
      ymax = upper.CL,
      fill = Condition,
      group = Condition
    ),
    alpha = 0.25,
    color = NA
  ) +
  scale_color_manual(values = c("F"="purple", "N"="orange"))+
  scale_fill_manual(values = c("F"="purple", "N"="orange"))+
  labs(x="Time bin (3min)", y="Shannon Entropy")+
  theme_classic()+
  theme(
    legend.position = "none",
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

# Inj Interpolated entropy ---------------------------------------------------------
#function to compute entropy for inj birds 
compute_spatial_entropy_time_inj <- function(data, grid_width, grid_height, grid_size = 100, jitter_amount = 0.0001) {
  results <- data %>%
    group_by(BirdID, Condition, time_bin_3min, Inj_Lesion) %>%
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

Inj_int_ent <- compute_spatial_entropy_time_inj(Inj_Birds_all, grid_width = 40, grid_height = 21, grid_size = 42)

#Order groups
Inj_int_ent$Inj_Lesion <- factor(
  Inj_int_ent$Inj_Lesion,
  levels = c("IgG-SAP_No", "anti-DBH-SAP_No", "anti-DBH-SAP_Yes")
)


# Inj Birds Entropy stats -------------------------------------------------


hist(Inj_int_ent$shannon_entropy)
hist(log(Inj_int_ent$shannon_entropy))

Inj_int_ent$time_bin_3min <- factor(Inj_int_ent$time_bin_3min, ordered = TRUE)

Inj_raw_ent_model <- glmmTMB(shannon_entropy ~ Condition * time_bin_3min * Inj_Lesion + (1|BirdID), 
                             family = gaussian(link=identity), data =Inj_int_ent)

Anova(Inj_raw_ent_model)
summary(Inj_raw_ent_model)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_raw_ent_model, plot = TRUE)

plot(residuals(Inj_raw_ent_model))
qqnorm(resid(Inj_raw_ent_model))
qqline(resid(Inj_raw_ent_model)) 

#Predicted and raw points plot ########
emm_ent_inj <- emmeans(
  Inj_raw_ent_model,
  ~ Condition * time_bin_3min | Inj_Lesion
)

emm_df_ent_inj <- as.data.frame(emm_ent_inj)

ggplot(Inj_int_ent,
       aes(x = time_bin_3min, y = shannon_entropy, color = Condition)) +
  
  # Raw data
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +
  
  geom_line(
    aes(group = BirdID),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  
  # Model-predicted means
  geom_line(
    data = emm_df_ent_inj,
    aes(y = emmean, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon 
  geom_ribbon(
    data = emm_df_ent_inj,
    aes(
      y = emmean,
      ymin = lower.CL,
      ymax = upper.CL,
      fill = Condition,
      group = Condition
    ),
    alpha = 0.25,
    color = NA
  ) +
  facet_wrap(~Inj_Lesion)+
  labs(x="Time bin (3min)", y="Shannon Entropy")+
  scale_color_manual(values = c("F"="purple", "N"="orange"))+
  scale_fill_manual(values = c("F"="purple", "N"="orange"))+
  theme_classic()+
  theme(
    legend.position = "none",
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


# Post HOC models ---------------------------------------------------------
ent_IgG <- Inj_int_ent %>% filter(Inj_Lesion == "IgG-SAP_No")
ent_IgG$time_bin_3min <- factor(ent_IgG$time_bin_3min, ordered = TRUE)

ent_No <- Inj_int_ent %>% filter(Inj_Lesion == "anti-DBH-SAP_No")
ent_No$time_bin_3min <- factor(ent_No$time_bin_3min, ordered = TRUE)

ent_Yes <- Inj_int_ent %>% filter(Inj_Lesion == "anti-DBH-SAP_Yes")
ent_Yes$time_bin_3min <- factor(ent_Yes$time_bin_3min, ordered = TRUE)

#IgG
ent_igg_model <- glmmTMB(shannon_entropy ~ Condition * time_bin_3min + (1|BirdID), 
                             family = gaussian(link=identity), data =ent_IgG)

Anova(ent_igg_model)
pairs(emmeans(ent_igg_model, ~Condition | time_bin_3min, adjust = "fdr"))


#DBH no 
ent_no_model <- glmmTMB(shannon_entropy ~ Condition * time_bin_3min + (1|BirdID), 
                         family = gaussian(link=identity), data =ent_No)

Anova(ent_no_model)
pairs(emmeans(ent_igg_model, ~Condition | time_bin_3min, adjust = "fdr"))

#DBH yes
ent_yes_model <- glmmTMB(shannon_entropy ~ Condition * time_bin_3min + (1|BirdID), 
                        family = gaussian(link=identity), data =ent_Yes)

Anova(ent_yes_model)
pairs(emmeans(ent_igg_model, ~Condition | time_bin_3min, adjust = "fdr"))



# SPEED  ------------------------------------------------------------

# UM Birds speed w/0s -----------------------------------------------------
hist(U_Birds_90$speed_head_psec) #check outliers gone 

#Average speed over time bins 
UM_binned_speed <- U_Birds_90 %>%
  group_by(BirdID, Condition, time_bin_3min) %>%
  summarise(
    mean_speed = mean(speed_head_psec),
    median_head_speed = median(speed_head_psec),
    .groups = "drop"
  )


#Stats 
hist(UM_binned_speed$mean_speed)
hist(sqrt(UM_binned_speed$mean_speed))
hist(UM_binned_speed$mean_speed^(1/3))
hist(log(UM_binned_speed$mean_speed)) #most normal  


UM_binned_speed$time_bin_3min <- factor(UM_binned_speed$time_bin_3min, ordered = TRUE)


UM_speed_model <- glmmTMB(log(mean_speed) ~Condition * time_bin_3min + (1|BirdID),
                          family = gaussian(link = "identity"), data = UM_binned_speed)

Anova(UM_speed_model)
summary(UM_speed_model)
simulationOutput <- simulateResiduals(fittedModel = UM_speed_model, plot = TRUE)
plot(residuals(UM_speed_model))
qqnorm(resid(UM_speed_model))
qqline(resid(UM_speed_model))
pairs(emmeans(UM_speed_model, ~ Condition | time_bin_3min), adjust = "fdr")


### UM Raw points and model predictions plot ######### 
emm_speed <- emmeans(
  UM_speed_model,
  ~ Condition * time_bin_3min
)


#to plot log values in back 
emm_df_speed <- as.data.frame(emm_speed) %>%
  mutate(
    area_hat = emmean,
    lower.CL = lower.CL,
    upper.CL = upper.CL
  )

ggplot(UM_binned_speed,
       aes(x = time_bin_3min, y = log(mean_speed), color = Condition)) +
  
  # Raw data
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +
  
  geom_line(
    aes(group = BirdID),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  
  # Model-predicted means (correct scale)
  geom_line(
    data = emm_df_speed,
    aes(y = area_hat, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon
  geom_ribbon(
    data = emm_df_speed,
    aes(
      y = area_hat,
      ymin = lower.CL,
      ymax = upper.CL,
      fill = Condition,
      group = Condition
    ),
    alpha = 0.25,
    color = NA
  ) +
  
  scale_color_manual(values = c("F"="purple", "N"="orange")) +
  scale_fill_manual(values = c("F"="purple", "N"="orange")) +
  labs(x = "Time bin (3 min)", y = "log(Mean Speed (cm/sec))") +
  theme_classic()+
  theme(
    legend.position = "none",
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













# Inj Birds speed w/0s -----------------------------------------------------
hist(Inj_Birds_all$speed_head_psec) #check outliers gone 

Inj_binned_speed <- Inj_Birds_all %>%
  group_by(BirdID, Condition, time_bin_3min, Inj_Lesion) %>%
  summarise(
    median_head_speed = median(speed_head_psec),
    mean_speed = mean(speed_head_psec),
    .groups = "drop"
  )

# Inj birds speed stats ---------------------------------------------------


hist(Inj_binned_speed$mean_speed)
hist(sqrt(Inj_binned_speed$mean_speed))
hist((Inj_binned_speed$mean_speed)^(1/3))
hist(log(Inj_binned_speed$mean_speed))


library(bestNormalize)

bn <- boxcox(Inj_binned_speed$median_head_speed)

bn$lambda
Inj_binned_speed$speed_bc <- predict(bn)

hist(Inj_binned_speed$speed_bc)


Inj_binned_speed$time_bin_3min <- factor(Inj_binned_speed$time_bin_3min, ordered = TRUE)

#Order groups
Inj_binned_speed$Inj_Lesion <- factor(
  Inj_binned_speed$Inj_Lesion,
  levels = c("IgG-SAP_No", "anti-DBH-SAP_No", "anti-DBH-SAP_Yes")
)

Inj_speed_model <- glmmTMB(mean_speed ~Condition * time_bin_3min * Inj_Lesion + (1|BirdID),
                           family = tweedie(link = "log"), data = Inj_binned_speed, 
                           control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))


Anova(Inj_speed_model)

summary(Inj_speed_model)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_speed_model, plot = TRUE)

plot(residuals(Inj_speed_model))
qqnorm(resid(Inj_speed_model))
qqline(resid(Inj_speed_model))


# Raw points and model predictions plot ######### 
emm_speed_inj <- emmeans(
  Inj_speed_model,
  ~ Condition * time_bin_3min | Inj_Lesion
)

#TO plot raw values in back
emm_df_speed_inj <- as.data.frame(emm_speed_inj) %>%
  mutate(
    area_hat = exp(emmean),
    lower.CL = exp(lower.CL),
    upper.CL = exp(upper.CL)
  )

#to plot log values in back 
emm_df_speed_inj <- as.data.frame(emm_speed_inj) %>%
  mutate(
    area_hat = emmean,
    lower.CL = lower.CL,
    upper.CL = upper.CL
  )

ggplot(Inj_binned_speed,
       aes(x = time_bin_3min, y = log(mean_speed), color = Condition)) +
  
  # Raw data
  geom_jitter(
    aes(group = BirdID),
    width = 0.15,
    alpha = 0.3,
    size = 1
  ) +
  
  geom_line(
    aes(group = BirdID),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  
  # Model-predicted means (correct scale)
  geom_line(
    data = emm_df_speed_inj,
    aes(y = area_hat, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon
  geom_ribbon(
    data = emm_df_speed_inj,
    aes(
      y = area_hat,
      ymin = lower.CL,
      ymax = upper.CL,
      fill = Condition,
      group = Condition
    ),
    alpha = 0.25,
    color = NA
  ) +
  
  facet_wrap(~Inj_Lesion, nrow = 1)+
  scale_color_manual(values = c("F"="purple", "N"="orange")) +
  scale_fill_manual(values = c("F"="purple", "N"="orange")) +
  labs(x = "Time bin (3 min)", y = "log(mean speed)") +
  theme_classic()
















# Combine all metrics into 1 data frame (for INJ birds PCA analysis) ------------------------------------------

#Format metric data 
Inj_int_ent$ID <- paste(Inj_int_ent$BirdID, Inj_int_ent$Condition, Inj_int_ent$time_bin_3min, sep = "_")

Inj_binned_speed$ID <- paste(Inj_binned_speed$BirdID, Inj_binned_speed$Condition, Inj_binned_speed$time_bin_3min, sep = "_")

circular_variance_inj$ID <- paste(circular_variance_inj$BirdID, circular_variance_inj$Condition, circular_variance_inj$time_bin_3min, sep = "_")

Concave_hull_areas_3min_inj$ID <- paste(Concave_hull_areas_3min_inj$BirdID, Concave_hull_areas_3min_inj$Condition, Concave_hull_areas_3min_inj$time_bin_3min, sep = "_")
Concave_hull_areas_3min_inj <- rename(Concave_hull_areas_3min_inj, concave_area = area)

#Combine metrics into one data frame 
all_metrics_inj <- merge(Inj_binned_speed, circular_variance_inj[, c("ID", "head_var")], by = "ID", all.x = TRUE)
all_metrics_inj <- merge(all_metrics_inj, Inj_int_ent[, c("ID", "shannon_entropy")], by = "ID", all.x = TRUE)
all_metrics_inj <- merge(all_metrics_inj, Concave_hull_areas_3min_inj[, c("ID", "concave_area")], by = "ID", all.x = TRUE)

