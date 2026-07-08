# =============================================================
# Node Detection Coverage Check (extended to include tail base)
#
# Purpose: For each video (BirdID x Phase x Day), report what
# percentage of frames had:
#   - head present
#   - tail base present
#   - both present
#   - only head (tail missing)
#   - only tail base (head missing)   <- the key "fallback" case
#   - neither present
#
# This tells you HOW OFTEN the tail base actually rescues frames
# where the head is missing -- which decides whether you use head
# alone, tail base alone, or a fallback strategy for speed/freezing.
#
# Assumes a combined cleaned df `birds_all` with columns:
#   BirdID, Phase, Day, Condition, x_head_raw, x_tailbase_raw
# (uses the _raw columns because those hold the ORIGINAL detections
#  with NAs -- NOT the interpolated columns, which have no NAs)
# Adjust "x_tailbase_raw" if your tail base column is named differently.
# =============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# -------------------------------------------------------------
# STEP 1: FLAG DETECTION TYPE PER FRAME
# -------------------------------------------------------------
coverage <- birds_all %>%
  mutate(
    head_present = !is.na(x_head_raw),
    tail_present = !is.na(x_tailbase_raw),

    both_seen  =  head_present &  tail_present,
    head_only  =  head_present & !tail_present,
    tail_only  = !head_present &  tail_present,   # <- head rescued by tail base
    neither    = !head_present & !tail_present
  )

# -------------------------------------------------------------
# STEP 2: SUMMARISE PER VIDEO (BirdID x Phase x Day)
# -------------------------------------------------------------
coverage_long <- coverage %>%
  select(BirdID, Phase, Day, Condition,
         both_seen, head_only, tail_only, neither) %>%
  pivot_longer(
    cols = c(both_seen, head_only, tail_only, neither),
    names_to = "detection_type",
    values_to = "present"
  )

coverage_summary <- coverage_long %>%
  group_by(BirdID, Phase, Day, Condition, detection_type) %>%
  summarise(
    n_frames     = sum(present),
    total_frames = n(),
    percent      = 100 * n_frames / total_frames,
    .groups = "drop"
  )

# -------------------------------------------------------------
# STEP 3: THE KEY NUMBERS FOR YOUR DECISION
# -------------------------------------------------------------
# "head_coverage"  = % of frames where head is usable (head_only + both)
# "tail_rescue"    = % of frames where head is MISSING but tail is present
#                    (this is what the tail base actually buys you)
decision_table <- coverage %>%
  group_by(BirdID, Phase, Day) %>%
  summarise(
    total_frames       = n(),
    pct_head_usable    = 100 * mean(head_present),
    pct_tail_usable    = 100 * mean(tail_present),
    pct_head_missing   = 100 * mean(!head_present),
    pct_tail_rescue    = 100 * mean(!head_present & tail_present),
    pct_neither        = 100 * mean(!head_present & !tail_present),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_head_missing))

print(decision_table, n = Inf)

# Overall summary across all videos (quick gut check)
overall <- coverage %>%
  summarise(
    pct_head_usable  = 100 * mean(head_present),
    pct_tail_usable  = 100 * mean(tail_present),
    pct_head_missing = 100 * mean(!head_present),
    pct_tail_rescue  = 100 * mean(!head_present & tail_present),
    pct_neither      = 100 * mean(!head_present & !tail_present)
  )

cat("\n=== OVERALL ACROSS ALL VIDEOS ===\n")
print(overall)

cat("\nInterpretation guide:\n")
cat("- If pct_head_missing is LOW (<~5%): interpolation handles gaps,\n")
cat("  you can just use head alone. Tail base fallback not really needed.\n")
cat("- If pct_head_missing is HIGH (>~20%) AND pct_tail_rescue is high:\n")
cat("  consider using TAIL BASE alone (consistently) for speed/freezing,\n")
cat("  since a consistent node beats head-with-many-gaps.\n")
cat("- If pct_neither is high: those frames are lost regardless -- a\n")
cat("  data quality issue worth investigating (occlusion? poor tracking?).\n")

# -------------------------------------------------------------
# STEP 4: PLOT -- coverage by detection type, faceted by Phase
# -------------------------------------------------------------
coverage_summary$detection_type <- factor(
  coverage_summary$detection_type,
  levels = c("tail_only", "head_only", "neither", "both_seen")
)

ggplot(coverage_summary,
       aes(x = detection_type, y = percent, fill = detection_type)) +
  geom_violin(alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(color = detection_type),
             position = position_jitter(width = 0.15),
             size = 1, alpha = 0.7) +
  facet_wrap(~ Phase) +
  labs(x = "Detection type", y = "Percentage of frames",
       fill = "Detection type", color = "Detection type",
       title = "Node detection coverage (head vs tail base)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_manual(values = c("tail_only"="steelblue", "head_only"="chartreuse4",
                                "both_seen"="indianred", "neither"="deeppink")) +
  scale_fill_manual(values = c("tail_only"="steelblue", "head_only"="chartreuse4",
                               "both_seen"="indianred", "neither"="deeppink"))
