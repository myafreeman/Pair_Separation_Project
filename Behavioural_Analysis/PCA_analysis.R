# Principal Component Analysis of Pose-estimation metrics # 
library(ggfortify)

# PCA -----------------------------------------------------------
#load in all metrics df, removing lesion birds 
all_metrics_cnt_no <- all_metrics_inj %>% filter(Inj_Lesion != "anti-DBH-SAP_Yes")

###PCA on sleap metrics - whole exp 
# Going with simple version - 4 metrics only 
# Removing Lesion birds 
metrics_inj_simp <- all_metrics_cnt_no %>%
  select(
    mean_speed,
    head_var,
    concave_area,
    shannon_entropy,
  ) %>%
  drop_na()

pca_res_inj_simp <- prcomp(
  metrics_inj_simp,
  center = TRUE,
  scale. = TRUE
)

summary(pca_res_inj_simp)

pca_scores_inj_simp <- as.data.frame(pca_res_inj_simp$x) %>%
  bind_cols(
    all_metrics_cnt_no %>%
      select(BirdID, Condition, time_bin_3min, Inj_Lesion) %>%
      slice(rownames(metrics_inj_simp) |> as.integer())
  )

loadings_inj_simp <- as.data.frame(pca_res_inj_simp$rotation)
loadings_inj_simp

variance <- pca_res_inj_simp$sdev^2
pve <- variance / sum(variance)

scree_df <- data.frame(
  PC = 1:length(pve),
  variance = pve
)

#Scree plot 
ggplot(scree_df, aes(x = PC, y = variance)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_bar(
    stat = "summary",
    fun = "mean",
    alpha = 0.8,
    fill = "steelblue"
  ) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 1:nrow(scree_df)) +
  labs(
    x = "Principal Component",
    y = "Proportion of Variance Explained",
    title = ""
  ) +
  theme_minimal()+
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

#Vector bi-plot 
autoplot(pca_res_inj_simp, data = all_metrics_cnt_no,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 4) +
  theme_minimal() +
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
  )+
  geom_point(size = 1, alpha = 0.3)


# Stats PCA ---------------------------------------------------------------
pca_scores_inj_simp$time_bin_3min <- factor(pca_scores_inj_simp$time_bin_3min, ordered = TRUE)

Inj_PC1_model_simp <- glmmTMB(PC1 ~ Condition * time_bin_3min * Inj_Lesion + (1|BirdID),
                              family = gaussian(link = "identity"), data = pca_scores_inj_simp)
Anova(Inj_PC1_model_simp)

summary(Inj_PC1_model_simp)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_PC1_model_simp, plot = TRUE)

##Just IgG
pca_igg_simp <- pca_scores_inj_simp %>% filter(Inj_Lesion == "IgG-SAP_No")

pca_igg_simp$time_bin_3min <- factor(pca_igg_simp$time_bin_3min, ordered = TRUE)

IgG_PC1_model_simp <- glmmTMB(PC1 ~ Condition * time_bin_3min + (1 | BirdID),
                              family = gaussian(link = "identity"), data = pca_igg_simp)

Anova(IgG_PC1_model_simp)

summary(IgG_PC1_model_simp)

pairs(emmeans(IgG_PC1_model_simp, ~ Condition | time_bin_3min), adjust = "fdr")

#residuals
simulationOutput <- simulateResiduals(fittedModel = IgG_PC1_model, plot = TRUE)

##just anti-dbh_no 
pca_anti_no_simp <- pca_scores_inj_simp %>% filter(Inj_Lesion == "anti-DBH-SAP_No")
pca_anti_no_simp$time_bin_3min <- factor(pca_anti_no_simp$time_bin_3min, ordered = TRUE)

No_PC1_model_simp <- glmmTMB(PC1 ~ Condition * time_bin_3min + (1|BirdID),
                             family = gaussian(link = "identity"), data = pca_anti_no_simp)

Anova(No_PC1_model_simp)

summary(No_PC1_model_simp)

pairs(emmeans(No_PC1_model_simp, ~ Condition | time_bin_3min), adjust = "fdr")

#residuals
simulationOutput <- simulateResiduals(fittedModel = No_PC1_model_simp, plot = TRUE)

### Raw points and model predictions plot ######### 
emm_PCA_inj_simp <- emmeans(
  Inj_PC1_model_simp,
  ~ Condition * time_bin_3min | Inj_Lesion
)


#to plot raw values in back (no log on data)
emm_df_PCA_inj_simp <- as.data.frame(emm_PCA_inj_simp) %>%
  mutate(
    area_hat = emmean,
    lower.CL = lower.CL,
    upper.CL = upper.CL
  )

ggplot(pca_scores_inj_simp,
       aes(x = time_bin_3min, y = PC1, color = Condition)) +
  
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
    data = emm_df_PCA_inj_simp,
    aes(y = area_hat, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon
  geom_ribbon(
    data = emm_df_PCA_inj_simp,
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
  
  facet_wrap(~Inj_Lesion, nrow = 1, labeller = labeller(Inj_Lesion = c("IgG-SAP_No" = "IgG-SAP", 
                                                                       "anti-DBH-SAP_No" = "DBH-SAP")))+
  scale_color_manual(values = c("F"="purple", "N"="orange")) +
  scale_fill_manual(values = c("F"="purple", "N"="orange")) +
  labs(x = "Time bin (3 min)", y = "Movement Intensity (PC1)") +
  theme_minimal()+
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





# First 3 mins only PCA  --------------------------------------------------
  #Re-bin data and calculate metrics over 30-sec bins 
#Make 30 sec time bins 
Inj_Birds_all_30sec <- Inj_Birds_all %>%
  group_by(BirdID) %>%
  mutate(
    time_bin_30sec = cut(adj_frame, breaks = 30, labels = FALSE)  # Divide into 30 equal bins (30 sec)
  ) %>%
  ungroup()

#CV in 30 sec bins 
### Inj circ var 
circular_variance_inj_30sec <- Inj_Birds_all_30sec %>%
  group_by(BirdID, Condition, Inj_Lesion, time_bin_30sec) %>%
  summarise(head_var = var.circular(circular(head_angle), na.rm=TRUE), .groups = "drop")



# CV stats 3 mins only  ---------------------------------------------------


circular_variance_inj_30sec_3min <- circular_variance_inj_30sec %>% 
                                        filter(time_bin_30sec<7 
                                            & Inj_Lesion != "anti-DBH-SAP_Yes")

circular_variance_inj_30sec_3min$time_bin_30sec <- factor(circular_variance_inj_30sec_3min$time_bin_30sec, ordered=TRUE)

##Full Model with nested Inj_Lesion 
Inj_var_model_3min <- glmmTMB(head_var ~Condition * time_bin_30sec * Inj_Lesion + (1|BirdID),
                         family = beta_family(link = "logit"), data = circular_variance_inj_30sec_3min,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

Anova(Inj_var_model_3min)
summary(Inj_var_model_3min)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_var_model_3min, plot = TRUE)
plot(residuals(Inj_var_model_3min))
qqnorm(resid(Inj_var_model_3min))
qqline(resid(Inj_var_model_3min))


### INJ Raw points and model predictions plot ######### 
emm_var_inj <- emmeans(
  Inj_var_model_3min,
  ~ Condition * time_bin_30sec | Inj_Lesion,
  type = "response"
)

emm_var_inj_df <- as.data.frame(emm_var_inj)

ggplot(circular_variance_inj_30sec_3min,
       aes(x = time_bin_30sec, y = head_var, color = Condition)) +
  
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
DBH_no <- circular_variance_inj_30sec_3min %>% filter(Inj_Lesion == "anti-DBH-SAP_No")
IgG <- circular_variance_inj_30sec_3min %>% filter(Inj_Lesion == "IgG-SAP_No")


##Cir var stats 
#######DBH no 
DBH_no$time_bin_30sec <- as.factor(DBH_no$time_bin_30sec) 

dbh_no_var_model <- glmmTMB(head_var ~Condition * time_bin_30sec + (1|BirdID),
                            family = beta_family(link = "logit"), data = DBH_no,
                            control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

Anova(dbh_no_var_model)

summary(dbh_no_var_model)

#residuals
plot(residuals(dbh_no_var_model))
qqnorm(resid(dbh_no_var_model))
qqline(resid(dbh_no_var_model))

pairs(emmeans(dbh_no_var_model, ~ Condition|time_bin_30sec), adjust = "holm")


#####IgG
IgG$time_bin_30sec <- as.factor(IgG$time_bin_30sec)

igg_var_model <- glmmTMB(head_var ~Condition * time_bin_30sec + (1|BirdID),
                         family = beta_family(link = "logit"), data =IgG,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

Anova(igg_var_model)

summary(igg_var_model)

#residuals
plot(residuals(igg_var_model))
qqnorm(resid(igg_var_model))
qqline(resid(igg_var_model))

pairs(emmeans(igg_var_model, ~ Condition | time_bin_3min), adjust = "holm")







# area 30 sec bins --------------------------------------------------------
#Area 30 sec bins 
Concave_hull_areas_3min_inj_30 <- Inj_Birds_all_30sec %>%
  group_by(BirdID, Condition, time_bin_30sec, Inj_Lesion) %>%
  summarise(area = concave_hull_area(across(c(x_head, y_head_neg))))  # Correct way to pass data

Concave_hull_areas_3min_inj_30_3min <- Concave_hull_areas_3min_inj_30 %>% 
  filter(time_bin_30sec<7 
         & Inj_Lesion != "anti-DBH-SAP_Yes")
Concave_hull_areas_3min_inj_30_3min$time_bin_30sec <- factor(Concave_hull_areas_3min_inj_30_3min$time_bin_30sec, ordered=TRUE)


concave_hull_model_inj_3min <- glmmTMB((area^(1/3)) ~ Condition * time_bin_30sec * Inj_Lesion + (1|BirdID),
                                  family = gaussian(link = "identity"), data = Concave_hull_areas_3min_inj_30_3min)

simulationOutput <- simulateResiduals(fittedModel = concave_hull_model_inj_3min, plot = TRUE)
Anova(concave_hull_model_inj_3min)
summary(concave_hull_model_inj_3min)

#residuals
plot(residuals(concave_hull_model_inj_3min))
qqnorm(resid(concave_hull_model_inj_3min))
qqline(resid(concave_hull_model_inj_3min))



emm_area_inj <- emmeans(
  concave_hull_model_inj_3min,
  ~ Condition * time_bin_30sec | Inj_Lesion,
  type = "response"
)

emm_area_inj_df <- as.data.frame(emm_area_inj)

ggplot(Concave_hull_areas_3min_inj_30_3min,
       aes(x = time_bin_30sec, y = area^(1/3), color = Condition)) +
  
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
    data = emm_area_inj_df,
    aes(y = response, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon (asymptotic)
  geom_ribbon(
    data = emm_area_inj_df,
    aes(
      y = response,
      ymin = lower.CL,
      ymax = upper.CL,
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
  labs(x="Time bin (min)", y="Area")+
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



##DBH no lesion 
hull_area_DBH_no <- Concave_hull_areas_3min_inj_30_3min %>% filter(Inj_Lesion =="anti-DBH-SAP_No")
hull_area_DBH_no$time_bin_30sec <- factor(hull_area_DBH_no$time_bin_30sec, ordered = TRUE) 

##IgG 
hull_area_IgG <- Concave_hull_areas_3min_inj_30_3min %>% filter(Inj_Lesion =="IgG-SAP_No")
hull_area_IgG$time_bin_30sec <- factor(hull_area_IgG$time_bin_30sec, ordered = TRUE) 


##DBHno _____________
hist(hull_area_DBH_no$area)
hist(sqrt(hull_area_DBH_no$area))
hist(hull_area_DBH_no$area^(1/3))
hist(log(hull_area_DBH_no$area))

hull_model_DBH_no <- glmmTMB(log(area) ~ Condition * time_bin_30sec + (1|BirdID),
                             family = gaussian(link = "identity"), data = hull_area_DBH_no)


simulationOutput <- simulateResiduals(fittedModel = hull_model_DBH_no, plot = TRUE)
Anova(hull_model_DBH_no)
summary(hull_model_DBH_no)
pairs(emmeans(hull_model_DBH_no, ~ Condition | time_bin_30sec), adjust = "fdr")


#residuals
plot(residuals(hull_model_DBH_no))
qqnorm(resid(hull_model_DBH_no))
qqline(resid(hull_model_DBH_no))

##IgG 
hull_model_IgG <- glmmTMB((area^(1/3)) ~ Condition * time_bin_30sec + (1|BirdID),
                          family = gaussian(link = "identity"), data = hull_area_IgG)

Anova(hull_model_IgG)
summary(hull_model_IgG)
pairs(emmeans(hull_model_IgG, ~ Condition | time_bin_3min), adjust = "fdr")


#residuals
plot(residuals(hull_model_IgG))
qqnorm(resid(hull_model_IgG))
qqline(resid(hull_model_IgG))






# Entropy 30 sec ----------------------------------------------------------


#Entropy 30 sec bins 
##Inj 
compute_spatial_entropy_time_inj <- function(data, grid_width, grid_height, grid_size = 100, jitter_amount = 0.0001) {
  results <- data %>%
    group_by(BirdID, Condition, time_bin_30sec, Inj_Lesion) %>%
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

Inj_int_ent_30sec <- compute_spatial_entropy_time_inj(Inj_Birds_all_30sec, grid_width = 40, grid_height = 21, grid_size = 42)

# Inj Birds Entropy stats -------------------------------------------------

hist(Inj_int_ent_30sec$shannon_entropy)

Inj_int_ent_30sec$time_bin_30sec <- factor(Inj_int_ent_30sec$time_bin_30sec, ordered = TRUE)

Inj_int_ent_30sec_3min <- Inj_int_ent_30sec %>% filter(
                            time_bin_30sec<7 & Inj_Lesion !="anti-DBH-SAP_Yes"
)

Inj_raw_ent_model_3min <- glmmTMB(shannon_entropy ~ Condition * time_bin_30sec * Inj_Lesion + (1|BirdID), 
                             family = gaussian(link=identity), data =Inj_int_ent_30sec_3min)

Anova(Inj_raw_ent_model_3min)
summary(Inj_raw_ent_model_3min)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_raw_ent_model_3min, plot = TRUE)

plot(residuals(Inj_raw_ent_model_3min))
qqnorm(resid(Inj_raw_ent_model_3min))
qqline(resid(Inj_raw_ent_model_3min)) 

#Predicted and raw points plot ########
emm_ent_inj <- emmeans(
  Inj_raw_ent_model_3min,
  ~ Condition * time_bin_30sec | Inj_Lesion
)

emm_df_ent_inj <- as.data.frame(emm_ent_inj)

ggplot(Inj_int_ent_30sec_3min,
       aes(x = time_bin_30sec, y = shannon_entropy, color = Condition)) +
  
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
ent_IgG <- Inj_int_ent_30sec_3min %>% filter(Inj_Lesion == "IgG-SAP_No")
ent_IgG$time_bin_30sec <- factor(ent_IgG$time_bin_30sec, ordered = TRUE)

ent_No <- Inj_int_ent_30sec_3min %>% filter(Inj_Lesion == "anti-DBH-SAP_No")
ent_No$time_bin_30sec <- factor(ent_No$time_bin_30sec, ordered = TRUE)

#IgG
ent_igg_model <- glmmTMB(shannon_entropy ~ Condition * time_bin_30sec + (1|BirdID), 
                         family = gaussian(link=identity), data =ent_IgG)

Anova(ent_igg_model)
pairs(emmeans(ent_igg_model, ~Condition | time_bin_3min, adjust = "fdr"))


#DBH no 
ent_no_model <- glmmTMB(shannon_entropy ~ Condition * time_bin_30sec + (1|BirdID), 
                        family = gaussian(link=identity), data =ent_No)

Anova(ent_no_model)
pairs(emmeans(ent_no_model, ~Condition | time_bin_30sec, adjust = "fdr"))

#DBH yes
ent_yes_model <- glmmTMB(shannon_entropy ~ Condition * time_bin_3min + (1|BirdID), 
                         family = gaussian(link=identity), data =ent_Yes)

Anova(ent_yes_model)
pairs(emmeans(ent_igg_model, ~Condition | time_bin_3min, adjust = "fdr"))




# Speed 30 sec bins  ------------------------------------------------------

#Average speed 30 sec bins 
Inj_binned_speed_30sec <- Inj_Birds_all_30sec %>%
  group_by(BirdID, Condition, time_bin_30sec, Inj_Lesion) %>%
  summarise(
    median_head_speed = median(speed_head_psec),
    mean_speed = mean(speed_head_psec),
    .groups = "drop"
  )

Inj_binned_speed_30sec_3min <- Inj_binned_speed_30sec %>% filter(time_bin_30sec<7 & Inj_Lesion !="anti-DBH-SAP_Yes")
Inj_binned_speed_30sec_3min$time_bin_30sec <- factor(Inj_binned_speed_30sec_3min$time_bin_30sec, ordered=TRUE)

hist(Inj_binned_speed_30sec_3min$mean_speed)
hist(sqrt(Inj_binned_speed_30sec_3min$mean_speed))
hist((Inj_binned_speed_30sec_3min$mean_speed)^(1/3))
hist(log(Inj_binned_speed_30sec_3min$mean_speed))


Inj_speed_model <- glmmTMB(log(mean_speed) ~Condition * time_bin_30sec * Inj_Lesion + (1|BirdID),
                           family = gaussian(link = "identity"), data = Inj_binned_speed_30sec_3min, 
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
  ~ Condition * time_bin_30sec | Inj_Lesion
)

#TO plot model (log) values in back
emm_df_speed_inj <- as.data.frame(emm_speed_inj) %>%
  mutate(
    area_hat = emmean,
    lower.CL = lower.CL,
    upper.CL = upper.CL
  )

ggplot(Inj_binned_speed_30sec_3min,
       aes(x = time_bin_30sec, y = log(mean_speed), color = Condition)) +
  
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






# PCA 30 sec  -------------------------------------------------------------

#Combine metric dataframes 
Inj_int_ent_30sec$ID <- paste(Inj_int_ent_30sec$BirdID, Inj_int_ent_30sec$Condition, Inj_int_ent_30sec$time_bin_30sec, sep = "_")
Concave_hull_areas_3min_inj_30$ID <- paste(Concave_hull_areas_3min_inj_30$BirdID, Concave_hull_areas_3min_inj_30$Condition, Concave_hull_areas_3min_inj_30$time_bin_30sec, sep = "_")
Concave_hull_areas_3min_inj_30 <- rename(Concave_hull_areas_3min_inj_30, concave_area = area)
circular_variance_inj_30sec$ID <- paste(circular_variance_inj_30sec$BirdID, circular_variance_inj_30sec$Condition, circular_variance_inj_30sec$time_bin_30sec, sep = "_")
Inj_binned_speed_30sec$ID <- paste(Inj_binned_speed_30sec$BirdID, Inj_binned_speed_30sec$Condition, Inj_binned_speed_30sec$time_bin_30sec, sep = "_")

all_metrics_inj_30sec <- merge(Inj_binned_speed_30sec, circular_variance_inj_30sec[, c("ID", "head_var")], by = "ID", all.x = TRUE)
all_metrics_inj_30sec <- merge(all_metrics_inj_30sec, Inj_int_ent_30sec[, c("ID", "shannon_entropy")], by = "ID", all.x = TRUE)
all_metrics_inj_30sec <- merge(all_metrics_inj_30sec, Concave_hull_areas_3min_inj_30[, c("ID", "concave_area")], by = "ID", all.x = TRUE)

#Subset to first 3 mins 
all_metrics_inj_bin1_30 <- all_metrics_inj_30sec %>% filter(time_bin_30sec < 7)
#Remove lesion birds 
all_metrics_30sec_simp <- all_metrics_inj_bin1_30 %>% filter(Inj_Lesion != "anti-DBH-SAP_Yes")

#PCA 
###PCA on all sleap metrics 
metrics_30sec_simp <- all_metrics_30sec_simp %>%
  select(
    mean_speed,
    head_var,
    concave_area,
    shannon_entropy
  ) %>%
  drop_na()

pca_res_30sec_simp <- prcomp(
  metrics_30sec_simp,
  center = TRUE,
  scale. = TRUE
)

summary(pca_res_30sec_simp)

pca_scores_30sec_simp <- as.data.frame(pca_res_30sec_simp$x) %>%
  bind_cols(
    all_metrics_30sec_simp %>%
      select(BirdID, Condition, time_bin_30sec, Inj_Lesion) %>%
      slice(rownames(metrics_30sec_simp) |> as.integer())
  )

loadings_30sec_simp <- as.data.frame(pca_res_30sec_simp$rotation)
loadings_30sec_simp

screeplot(pca_res_30sec_simp, type = "lines", main = "Scree Plot (Base R)")

variance <- pca_res_30sec_simp$sdev^2
pve <- variance / sum(variance)

scree_df <- data.frame(
  PC = 1:length(pve),
  variance = pve
)
#Scree plot ()
ggplot(scree_df, aes(x = PC, y = variance)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_bar(
    stat = "summary",
    fun = "mean",
    alpha = 0.8,
    fill = "steelblue"
  ) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 1:nrow(scree_df)) +
  labs(
    x = "Principal Component",
    y = "Proportion of Variance Explained",
    title = ""
  ) +
  theme_minimal()+
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

#Vector bi-plot ()
autoplot(pca_res_30sec_simp, data = all_metrics_30sec_simp,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 4) +
  theme_minimal() +
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
  )+
  geom_point(size = 1, alpha = 0.3)


# First 3 min PCA stats ---------------------------------------------------
hist(pca_scores_30sec_simp$PC1)

pca_scores_30sec_simp$time_bin_30sec <- factor(pca_scores_30sec_simp$time_bin_30sec, ordered = TRUE)
Inj_PC1_model_bin1_30_simp <- glmmTMB(PC1 ~ Condition * Inj_Lesion * time_bin_30sec + (1|BirdID),      
                                      family = gaussian(link = "identity"), data = pca_scores_30sec_simp)


Anova(Inj_PC1_model_bin1_30_simp)

summary(Inj_PC1_model_bin1_30_simp)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_PC1_model_bin1_30_simp, plot = TRUE)


### Raw points and model predictions plot ######### 
emm_PCA_inj_30_simp <- emmeans(
  Inj_PC1_model_bin1_30_simp,
  ~ Condition * time_bin_30sec | Inj_Lesion
)


#to plot raw values in back (no log on data)
emm_df_PCA_inj_30_simp <- as.data.frame(emm_PCA_inj_30_simp) %>%
  mutate(
    area_hat = emmean,
    lower.CL = lower.CL,
    upper.CL = upper.CL
  )

pca_scores_30sec_simp <- pca_scores_30sec_simp %>%
  mutate(Inj_Lesion = factor(Inj_Lesion, levels = c("IgG-SAP_No", "anti-DBH-SAP_No")))
emm_df_PCA_inj_30_simp <- emm_df_PCA_inj_30_simp %>%
  mutate(Inj_Lesion = factor(Inj_Lesion, levels = c("IgG-SAP_No", "anti-DBH-SAP_No")))

#Figure 4E
ggplot(pca_scores_30sec_simp,
       aes(x = time_bin_30sec, y = PC1, color = Condition)) +
  
  # Raw data
  # geom_jitter(
  #   aes(group = BirdID),
  #   width = 0.15,
  #   alpha = 0.3,
  #   size = 1.2
  # ) +
  geom_point(aes(group = BirdID),
             width = 0.15,
             alpha = 0.3,
             size = 1.2
  ) +
  geom_line(
    aes(group = BirdID),
    alpha = 0.15,
    linewidth = 0.4
  ) +
  
  # Model-predicted means (correct scale)
  geom_line(
    data = emm_df_PCA_inj_30_simp,
    aes(y = area_hat, group = Condition),
    linewidth = 1.2
  ) +
  
  # 95% CI ribbon
  geom_ribbon(
    data = emm_df_PCA_inj_30_simp,
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
  
  facet_wrap(~Inj_Lesion, nrow = 1, labeller = labeller(Inj_Lesion = c("IgG-SAP_No" = "CON", 
                                                                       "anti-DBH-SAP_No" = "DBH-SAP"))) +
  scale_color_manual(values = c("F"="purple", "N"="orange")) +
  scale_fill_manual(values = c("F"="purple", "N"="orange")) +
  labs(x = "Time bin (min)", y = "Movement Intensity (PC1)") +
  theme_minimal() +
  scale_x_discrete(
    labels = function(x) {
      x <- as.numeric(x)
      paste0(((x - 1) * 0.5), "-", x * 0.5)  # Convert to minutes (30s = 0.5 min)
    }
  ) +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, face ="bold"),  # Rotate x-axis labels
    axis.text.y = element_text(face ="bold"),  # Rotate x-axis labels
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black")
  )

##Just IgG
pca_igg_bin1_30_simp <- pca_scores_30sec_simp %>% filter(Inj_Lesion == "IgG-SAP_No")

pca_igg_bin1_30_simp$time_bin_30sec <- factor(pca_igg_bin1_30_simp$time_bin_30sec, ordered = TRUE)

IgG_PC1_model_bin1_30_simp <- glmmTMB(PC1 ~ Condition * time_bin_30sec + (1 | BirdID),
                                      family = gaussian(link = "identity"), data = pca_igg_bin1_30_simp)

Anova(IgG_PC1_model_bin1_30_simp)

summary(IgG_PC1_model_bin1_30_simp)

pairs(emmeans(IgG_PC1_model_bin1_30_simp, ~ Condition | time_bin_30sec), adjust = "fdr")

#residuals
simulationOutput <- simulateResiduals(fittedModel = IgG_PC1_model_bin1_30, plot = TRUE)



##just anti-dbh_no 
pca_dbh_no_bin1_30_simp <- pca_scores_30sec_simp %>% filter(Inj_Lesion == "anti-DBH-SAP_No")

pca_dbh_no_bin1_30_simp$time_bin_30sec <- factor(pca_dbh_no_bin1_30_simp$time_bin_30sec, ordered = TRUE)

No_PC1_model_bin1_30_simp <- glmmTMB(PC1 ~ Condition * time_bin_30sec + (1 | BirdID),
                                     family = gaussian(link = "identity"), data = pca_dbh_no_bin1_30_simp)

Anova(No_PC1_model_bin1_30_simp)

summary(No_PC1_model_bin1_30_simp)

pairs(emmeans(No_PC1_model_bin1_30_simp, ~ Condition | time_bin_30sec), adjust = "fdr")

#residuals
simulationOutput <- simulateResiduals(fittedModel = No_PC1_model_bin1_30_simp, plot = TRUE)




