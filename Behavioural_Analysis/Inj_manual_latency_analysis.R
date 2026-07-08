# --- Injected birds manual latencies analysis --  


# Load df 
all_coh <- read_xlsx("/Users/michaelabierman/Downloads/LAB/Daria/Results/data/manual/Saporin_batches/familiar_vs_unfamiliar_saporin_allbatches.xlsx")

all_coh_6 <-all_coh %>% filter(Behaviour != "Eating")
all_coh_6 <-all_coh_6 %>% filter(Behaviour != "Grooming")

excluded_birds <- c("bl85gy195","bl21or11", "bl48pu128", "bl191or41", "bl10or50", "bl25or15")

all_coh_6 <- all_coh_6 %>% 
  filter(!(BirdID %in% excluded_birds))

#Nest injection and lesion 
all_coh_6$Inj_Lesion <- paste(all_coh_6$Injection, all_coh_6$Lesion, sep = "_")  

#Subset to movements and vocalizations 
Inj_lat_move <- all_coh_6 %>% filter(Behaviour == "Long Hop" | Behaviour == "Short Hop"| Behaviour == "Beak Swipe")
Inj_lat_vocal <- all_coh_6 %>% filter(Behaviour == "Long Call" | Behaviour == "Short Call"| Behaviour == "Singing")

#count stimuli 
print(sum(Inj_lat_move$Condition =="F" & Inj_lat_move$Inj_Lesion =="IgG-SAP_No" & Inj_lat_move$Behaviour =="Short Hop"))
print(sum(Inj_lat_move$Condition =="N" & Inj_lat_move$Inj_Lesion =="IgG-SAP_No" & Inj_lat_move$Behaviour =="Short Hop"))
print(sum(Inj_lat_move$Condition =="F" & Inj_lat_move$Inj_Lesion =="anti-DBH-SAP_No" & Inj_lat_move$Behaviour =="Short Hop"))
print(sum(Inj_lat_move$Condition =="N" & Inj_lat_move$Inj_Lesion =="anti-DBH-SAP_No" & Inj_lat_move$Behaviour =="Short Hop"))


#MOVEMENT STATS -----------------------
hist(Inj_lat_move$Latencies)
hist(log(Inj_lat_move$Latencies))

#Including Lesion birds 
Inj_lat_move_model <- glmmTMB(
  Latencies ~ Condition * Inj_Lesion * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_move
)

#Excluding Lesion birds
Inj_lat_move_model <- glmmTMB(
  Latencies ~ Condition * Inj_Lesion * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_move %>% filter(Inj_Lesion != "anti-DBH-SAP_Yes")
)

Anova(Inj_lat_move_model)
summary(Inj_lat_move_model)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_lat_move_model, plot = TRUE)
plot(residuals(Inj_lat_move_model))
qqnorm(resid(Inj_lat_move_model))
qqline(resid(Inj_lat_move_model))

#MOVEMENTS POST-HOC 
# excluding lesions model 
# emm comparisons: two-way condition:behaviour 
emm_beh <- emmeans(
  Inj_lat_move_model,
  ~ Condition | Behaviour,
  type = "response"   # back-transforms from log scale
)
pairs(emm_beh)


# Post hoc models (for model including lesion birds)
# IgG #
Inj_lat_move_igg <- Inj_lat_move %>% filter(Inj_Lesion == "IgG-SAP_No")

Inj_lat_move_model_igg <- glmmTMB(
  Latencies ~ Condition * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_move_igg
)

Anova(Inj_lat_move_model_igg)
summary(Inj_lat_move_model_igg)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_lat_move_model_igg, plot = TRUE)
plot(residuals(Inj_lat_move_model_igg))
qqnorm(resid(Inj_lat_move_model_igg))
qqline(resid(Inj_lat_move_model_igg))
pairs(emmeans(Inj_lat_move_model_igg, ~ Condition | Behaviour, adjust = "fdr"))

# DBH NO #
Inj_lat_move_no <- Inj_lat_move %>% filter(Inj_Lesion == "anti-DBH-SAP_No")

Inj_lat_move_model_no <- glmmTMB(
  Latencies ~ Condition * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_move_no
)

Anova(Inj_lat_move_model_no)
summary(Inj_lat_move_model_no)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_lat_move_model_no, plot = TRUE)
plot(residuals(Inj_lat_move_model_no))
qqnorm(resid(Inj_lat_move_model_no))
qqline(resid(Inj_lat_move_model_no))
pairs(emmeans(Inj_lat_move_model_no, ~ Condition | Behaviour, adjust = "fdr"))

# DBH YES #
Inj_lat_move_yes <- Inj_lat_move %>% filter(Inj_Lesion == "anti-DBH-SAP_Yes")

Inj_lat_move_model_yes <- glmmTMB(
  Latencies ~ Condition * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_move_yes
)

Anova(Inj_lat_move_model_yes)
summary(Inj_lat_move_model_yes)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_lat_move_model_yes, plot = TRUE)
plot(residuals(Inj_lat_move_model_yes))
qqnorm(resid(Inj_lat_move_model_yes))
qqline(resid(Inj_lat_move_model_yes))
pairs(emmeans(Inj_lat_move_model_yes, ~ Condition | Behaviour, adjust = "fdr"))






# Vocalization STATS --------------------------
hist(Inj_lat_vocal$Latencies)
hist(log(Inj_lat_vocal$Latencies))

#Including Lesions 
Inj_lat_vocal_model <- glmmTMB(
  Latencies ~ Condition * Inj_Lesion * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_vocal
)

#Excluding Lesions 
Inj_lat_vocal_model <- glmmTMB(
  Latencies ~ Condition * Inj_Lesion * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_vocal%>%filter(Inj_Lesion != "anti-DBH-SAP_Yes")
)


Anova(Inj_lat_vocal_model)
summary(Inj_lat_vocal_model)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_lat_vocal_model, plot = TRUE)
plot(residuals(Inj_lat_vocal_model))
qqnorm(resid(Inj_lat_vocal_model))
qqline(resid(Inj_lat_vocal_model))

#POST-HOC 
# Excluding lesions (two-way trend condition:Inj_Lesion) 
# and including lesions (three way)

# IgG #
Inj_lat_vocal_igg <- Inj_lat_vocal %>% filter(Inj_Lesion == "IgG-SAP_No")

Inj_lat_move_vocal_igg <- glmmTMB(
  Latencies ~ Condition * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_vocal_igg
)

Anova(Inj_lat_move_vocal_igg)

summary(Inj_lat_move_vocal_igg)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_lat_move_vocal_igg, plot = TRUE)
plot(residuals(Inj_lat_move_vocal_igg))
qqnorm(resid(Inj_lat_move_vocal_igg))
qqline(resid(Inj_lat_move_vocal_igg))
pairs(emmeans(Inj_lat_move_vocal_igg, ~ Condition | Behaviour, adjust = "fdr"))

# DBH NO #
Inj_lat_vocal_no <- Inj_lat_vocal %>% filter(Inj_Lesion == "anti-DBH-SAP_No")

Inj_lat_vocal_model_no <- glmmTMB(
  Latencies ~ Condition * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_vocal_no
)

Anova(Inj_lat_vocal_model_no)
summary(Inj_lat_vocal_model_no)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_lat_vocal_model_no, plot = TRUE)
plot(residuals(Inj_lat_vocal_model_no))
qqnorm(resid(Inj_lat_vocal_model_no))
qqline(resid(Inj_lat_vocal_model_no))
pairs(emmeans(Inj_lat_vocal_model_no, ~ Condition | Behaviour, adjust = "fdr"))

# DBH YES #
Inj_lat_vocal_yes <- Inj_lat_vocal %>% filter(Inj_Lesion == "anti-DBH-SAP_Yes")

Inj_lat_vocal_model_yes <- glmmTMB(
  Latencies ~ Condition * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = Inj_lat_vocal_yes
)

Anova(Inj_lat_vocal_model_yes)
summary(Inj_lat_vocal_model_yes)

#residuals
simulationOutput <- simulateResiduals(fittedModel = Inj_lat_vocal_model_yes, plot = TRUE)
plot(residuals(Inj_lat_vocal_model_yes))
qqnorm(resid(Inj_lat_vocal_model_yes))
qqline(resid(Inj_lat_vocal_model_yes))
pairs(emmeans(Inj_lat_vocal_model_yes, ~ Condition | Behaviour, adjust = "fdr"))





# Plots # -----------
# MOVEMENTS 
Inj_lat_move$Inj_Lesion <- factor(
  Inj_lat_move$Inj_Lesion,
  levels = c("IgG-SAP_No", "anti-DBH-SAP_No", "anti-DBH-SAP_Yes")
)


Inj_lat_move$Behaviour <- factor(
  Inj_lat_move$Behaviour,
  levels = c("Short Hop", "Long Hop", "Beak Swipe")
)

dodge <- position_dodge(width = 0.6)

#Fig 5a
ggplot(Inj_lat_move%>%filter(Inj_Lesion != "anti-DBH-SAP_Yes"), aes(x = Inj_Lesion, y = Latencies, fill = Condition, color = Condition)) +
  
  # Mean bars
  stat_summary(
    fun = mean,
    geom = "bar",
    position = dodge,
    alpha = 0.7,
    width = 0.5
  ) +
  
  # Error bars
  stat_summary(
    aes(group = Condition),
    fun.data = mean_se,
    geom = "errorbar",
    position = dodge,
    width = 0.2
  ) +
  
  # Raw data
  geom_jitter(
    size = 2,
    position = position_jitterdodge(
      jitter.width = 0.3,
      dodge.width = 0.6
    ),
    show.legend = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 95),
    breaks = seq(0, 90, by = 15)
  )+
  theme_minimal() +
  theme(
    text = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = "none",
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank(),
  ) +
  
  labs(
    x = NULL,
    y = "Latencies (Trial #)",
    fill = "Condition"
  ) +
  facet_wrap(~Behaviour) +
  scale_x_discrete(
    labels = c(
      "IgG-SAP_No" = "IgG",
      "anti-DBH-SAP_No" = "DBH",
      "anti-DBH-SAP_Yes" = "DBH-Yes"
    )
  )+
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
  scale_fill_manual(values = c("N" = "orange", "F" = "purple")) +
  scale_color_manual(values = c("N" = "orange3", "F" = "purple"))




# VOCALIZATIONS 
Inj_lat_vocal$Inj_Lesion <- factor(
  Inj_lat_vocal$Inj_Lesion,
  levels = c("IgG-SAP_No", "anti-DBH-SAP_No", "anti-DBH-SAP_Yes")
)


Inj_lat_vocal$Behaviour <- factor(
  Inj_lat_vocal$Behaviour,
  levels = c("Short Call", "Long Call", "Singing")
)


dodge <- position_dodge(width = 0.6)

#Fig 5b 
ggplot(Inj_lat_vocal%>%filter(Inj_Lesion!="anti-DBH-SAP_Yes"), aes(x = Inj_Lesion, y = Latencies, fill = Condition, color = Condition)) +
  
  # Mean bars
  stat_summary(
    fun = mean,
    geom = "bar",
    position = dodge,
    alpha = 0.7,
    width = 0.5
  ) +
  
  # Error bars
  stat_summary(
    aes(group = Condition),
    fun.data = mean_se,
    geom = "errorbar",
    position = dodge,
    width = 0.2
  ) +
  
  # Raw data
  geom_jitter(
    size = 2,
    position = position_jitterdodge(
      jitter.width = 0.3,
      dodge.width = 0.6
    ),
    show.legend = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 95),
    breaks = seq(0, 90, by = 15)
  )+
  theme_minimal() +
  theme(
    text = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = "none",
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank(),
  ) +
  
  labs(
    x = NULL,
    y = "Latencies (Trial #)",
    fill = "Condition"
  ) +
  facet_wrap(~Behaviour) +
  scale_x_discrete(
    labels = c(
      "IgG-SAP_No" = "IgG",
      "anti-DBH-SAP_No" = "DBH",
      "anti-DBH-SAP_Yes" = "DBH-Yes"
    )
  )+
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
  scale_fill_manual(values = c("N" = "orange", "F" = "purple")) +
  scale_color_manual(values = c("N" = "orange3", "F" = "purple"))



# Supplemental lesion latencies sup fig 3 -------------------------------------

ggplot(Inj_lat_move%>%filter(Inj_Lesion=="anti-DBH-SAP_Yes"), aes(x = Condition, y = Latencies, fill = Condition, color = Condition)) +
  
  # Mean bars
  stat_summary(
    fun = mean,
    geom = "bar",
    position = dodge,
    alpha = 0.7,
    width = 0.5
  ) +
  
  # Error bars
  stat_summary(
    aes(group = Condition),
    fun.data = mean_se,
    geom = "errorbar",
    position = dodge,
    width = 0.2
  ) +
  
  # Raw data
  geom_jitter(
    size = 2,
    position = position_jitterdodge(
      jitter.width = 0.3,
      dodge.width = 0.6
    ),
    show.legend = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 95),
    breaks = seq(0, 90, by = 15)
  )+
  theme_minimal() +
  theme(
    text = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = "none",
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank(),
  ) +
  
  labs(
    x = NULL,
    y = "Latencies (Trial #)",
    fill = "Condition"
  ) +
  facet_wrap(~Behaviour) +
  scale_x_discrete(
    labels = c(
      "anti-DBH-SAP_Yes" = "DBH-Yes"
    )
  )+
  theme(
    legend.position = "none",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+
  scale_fill_manual(values = c("N" = "orange", "F" = "purple")) +
  scale_color_manual(values = c("N" = "orange3", "F" = "purple"))

ggplot(Inj_lat_vocal%>%filter(Inj_Lesion=="anti-DBH-SAP_Yes"), aes(x = Condition, y = Latencies, fill = Condition, color = Condition)) +
  
  # Mean bars
  stat_summary(
    fun = mean,
    geom = "bar",
    position = dodge,
    alpha = 0.7,
    width = 0.5
  ) +
  
  # Error bars
  stat_summary(
    aes(group = Condition),
    fun.data = mean_se,
    geom = "errorbar",
    position = dodge,
    width = 0.2
  ) +
  
  # Raw data
  geom_jitter(
    size = 2,
    position = position_jitterdodge(
      jitter.width = 0.3,
      dodge.width = 0.6
    ),
    show.legend = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 95),
    breaks = seq(0, 90, by = 15)
  )+
  theme_minimal() +
  theme(
    text = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = "none",
    panel.spacing = unit(1.5, "lines"),
    panel.grid = element_blank(),
  ) +
  
  labs(
    x = NULL,
    y = "Latencies (Trial #)",
    fill = "Condition"
  ) +
  facet_wrap(~Behaviour) +
  scale_x_discrete(
    labels = c(
      "anti-DBH-SAP_Yes" = "DBH-Yes"
    )
  )+
  theme(
    legend.position = "none",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+
  scale_fill_manual(values = c("N" = "orange", "F" = "purple")) +
  scale_color_manual(values = c("N" = "orange3", "F" = "purple"))









