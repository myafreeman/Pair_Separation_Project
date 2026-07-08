# --- UNAMIPUALTED BRIDS - Manually Scored Latency Analysis ---

#load packages
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/latest"))
library(readxl)
library(sjPlot)
library(lme4)
library(nlme)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)  # For residual diagnostics
library(lmerTest)  
library(car)       
library(performance)
library(emmeans)
library(tidyverse)
library(viridis)

#Load in csv of latency data
UM_manual <- read.csv("/Users/michaelabierman/Downloads/LAB/Daria/Results/data/manual/All_manual_lat_beh.csv")
UM_90 <- UM_manual %>% filter (Playbacks == 90) # ensure only those with 90 trials 
UM_90 <- UM_90 %>% select(-X.1, -X)

#Median latencies 
ggplot(UM_90, aes(x=Condition, y=Latencies, fill=Condition, color=Condition))+
  #geom_violin()+
  geom_boxplot()+
  geom_point(alpha=0.8)+
  theme_minimal()+
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
  )+
  labs(x="Stimuli")+
  scale_fill_manual(values = c("N" = "orange", "F" = "purple")) +
  scale_color_manual(values = c("N" = "orange3", "F" = "purple3"))

# Subset to movements and vocalizations 
UM_manual_move <- UM_90 %>% filter(Behaviour == "Long Hop" | Behaviour =="Short Hop" | Behaviour =="Beak Swipe")

UM_manual_vocal <- UM_90 %>% filter(Behaviour == "Singing" | Behaviour =="Short Call" | Behaviour =="Long Call")

##Stats for movements 
hist(UM_manual_move$Latencies)

UM_90_Uncapped_model_move <- glmmTMB(
  Latencies ~ Condition * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = UM_manual_move
)

Anova(UM_90_Uncapped_model_move)

pairs(emmeans(UM_90_Uncapped_model_move, ~ Condition|Behaviour), adjust = "fdr")

#residuals
simulationOutput <- simulateResiduals(fittedModel = UM_90_Uncapped_model_move, plot = TRUE)
plot(residuals(UM_90_Uncapped_model_move))
qqnorm(resid(UM_90_Uncapped_model_move))
qqline(resid(UM_90_Uncapped_model_move))


##Stats for vocalizations 
hist(UM_manual_vocal$Latencies)
UM_90_Uncapped_model_vocal <- glmmTMB(
  Latencies ~ Condition * Behaviour + (1|BirdID),
  family = truncated_nbinom2(link = "log"),  
  data = UM_manual_vocal
)

Anova(UM_90_Uncapped_model_vocal)
pairs(emmeans(UM_90_Uncapped_model_vocal, ~ Condition|Behaviour), adjust = "holm")


#residuals
simulationOutput <- simulateResiduals(fittedModel = UM_90_Uncapped_model_vocal, plot = TRUE)
plot(residuals(UM_90_Uncapped_model_vocal))
qqnorm(resid(UM_90_Uncapped_model_vocal))
qqline(resid(UM_90_Uncapped_model_vocal))



# Manual latency plots 
# Movements 
dodge <- position_dodge(width = 0.6)

UM_manual_move$Behaviour <- factor(
  UM_manual_move$Behaviour,
  levels = c("Short Hop", "Long Hop", "Beak Swipe")
)

#Fig 1b
ggplot(UM_manual_move, aes(x = Behaviour, y = Latencies, fill = Condition, color = Condition)) +
  
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
  
  theme_classic() +
  scale_y_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 15)
  )+
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


# Vocalizations
UM_manual_vocal$Behaviour <- factor(
  UM_manual_vocal$Behaviour,
  levels = c("Short Call", "Long Call", "Singing")
)

#Fig 1c
ggplot(UM_manual_vocal, aes(x = Behaviour, y = Latencies, fill = Condition, color = Condition)) +
  
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
  
  theme_classic() +
  scale_y_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 15)
  )+
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
