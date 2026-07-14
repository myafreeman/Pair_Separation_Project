# =============================================================
# Daily song-file counts across isolation phases 
# Two panels: Baseline | Pair separation.
#
# TWO PLOT MODES -- set PLOT_MODE below:
#   "individual" -> one line per bird 
#   "summary"    -> mean +/- error across EXPERIMENTAL birds, with
#                   individual birds as thin lines + control dashed
#
# For "summary" mode, choose the error band with ERROR_TYPE: SEM/SD
# =============================================================

library(readxl)
library(dplyr)
library(ggplot2)

# ---- SETTINGS -----------------------------------------------------------
PLOT_MODE    <- "individual"   # "individual"  or  "summary"
ERROR_TYPE   <- "SEM"          # "SEM" or "SD"  (only used in "summary" mode)
control_bird <- "bl38gy28"
LOG_PATH     <- "Song_Screening_Log.xlsx"

# fixed colors per bird 
bird_colors <- c(
  "bl35or15" = "#1f77b4",  # exp
  "bl95ye30" = "#d62728",  # exp
  "bl38gy28" = "#7f7f7f"   # control
)
exp_color <- "#1f77b4"      # color for the experimental mean/ribbon in summary mode

# ---- 1. Load and prep ---------------------------------------------------
log <- read_excel(LOG_PATH)

df <- log %>%
  rename(
    BirdID    = `Bird ID`,
    Date      = `Date`,
    Phase     = `Phase`,
    RealSongs = `# real songs (keep)`
  ) %>%
  filter(!is.na(RealSongs)) %>%
  mutate(RealSongs = as.numeric(RealSongs), Date = as.Date(Date))

# within-phase day index (works for 5 or 6 days)
df <- df %>%
  group_by(BirdID, Phase) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(DayInPhase = row_number()) %>%
  ungroup() %>%
  mutate(
    Role = ifelse(BirdID == control_bird, "control", "exp"),
    PhasePanel = factor(
      ifelse(Phase == "Baseline", "Baseline isolation", "Pair separation"),
      levels = c("Baseline isolation", "Pair separation")
    )
  )

# ======================================================================
# MODE 1: INDIVIDUAL BIRDS
# ======================================================================
if (PLOT_MODE == "individual") {

  df <- df %>%
    mutate(BirdLabel = paste0(BirdID, " (", Role, ")"))

  lab_colors <- setNames(
    bird_colors[df$BirdID[match(unique(df$BirdLabel), df$BirdLabel)]],
    unique(df$BirdLabel)
  )
  lab_linetypes <- setNames(
    ifelse(grepl("control", unique(df$BirdLabel)), "dashed", "solid"),
    unique(df$BirdLabel)
  )

  p <- ggplot(df, aes(x = DayInPhase, y = RealSongs,
                      colour = BirdLabel, linetype = BirdLabel)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~ PhasePanel) +
    scale_colour_manual(values = lab_colors, name = NULL) +
    scale_linetype_manual(values = lab_linetypes, name = NULL) +
    scale_x_continuous(breaks = 1:6) +
    labs(x = "Day of isolation", y = "# of real song files / day",
         title = "Daily song-file counts across isolation phases") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.position = c(0.12, 0.85),
          legend.background = element_rect(fill = "white", colour = NA),
          axis.line = element_line(colour = "black"))

  out_file <- "song_files_individual.png"
}

# ======================================================================
# MODE 2: MEAN +/- ERROR ACROSS EXPERIMENTAL BIRDS
# ======================================================================
if (PLOT_MODE == "summary") {

  err_fun <- if (ERROR_TYPE == "SD") {
    function(x) sd(x)
  } else {
    function(x) sd(x) / sqrt(length(x))   # SEM
  }

  exp_summary <- df %>%
    filter(Role == "exp") %>%
    group_by(PhasePanel, DayInPhase) %>%
    summarise(mean_songs = mean(RealSongs),
              err = err_fun(RealSongs),
              n = n(), .groups = "drop")

  df_exp     <- df %>% filter(Role == "exp")
  df_control <- df %>% filter(Role == "control")

  p <- ggplot() +
    geom_ribbon(data = exp_summary,
                aes(x = DayInPhase, ymin = mean_songs - err,
                    ymax = mean_songs + err),
                fill = exp_color, alpha = 0.18) +
    geom_line(data = df_exp,
              aes(x = DayInPhase, y = RealSongs, group = BirdID),
              colour = exp_color, linewidth = 0.5, alpha = 0.5) +
    geom_point(data = df_exp,
               aes(x = DayInPhase, y = RealSongs),
               colour = exp_color, size = 1.3, alpha = 0.5) +
    geom_line(data = exp_summary,
              aes(x = DayInPhase, y = mean_songs),
              colour = exp_color, linewidth = 1.6) +
    geom_point(data = exp_summary,
               aes(x = DayInPhase, y = mean_songs),
               colour = exp_color, size = 2.6) +
    geom_line(data = df_control,
              aes(x = DayInPhase, y = RealSongs),
              colour = bird_colors[[control_bird]],
              linewidth = 1.1, linetype = "dashed") +
    geom_point(data = df_control,
               aes(x = DayInPhase, y = RealSongs),
               colour = bird_colors[[control_bird]], size = 2) +
    facet_wrap(~ PhasePanel) +
    scale_x_continuous(breaks = 1:6) +
    labs(x = "Day of isolation", y = "# of real song files / day",
         title = "Daily song-file counts across isolation phases",
         subtitle = paste0("Blue = experimental (thin = individual, heavy = mean \u00B1 ",
                           ERROR_TYPE, "); grey dashed = control")) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.subtitle = element_text(size = 9, colour = "grey30"),
          axis.line = element_line(colour = "black"))

  out_file <- "song_files_summary.png"
}

# ---- Output -------------------------------------------------------------
print(p)
ggsave(out_file, p, width = 11, height = 4.8, dpi = 150)
