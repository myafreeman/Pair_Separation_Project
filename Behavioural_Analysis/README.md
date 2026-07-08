# Pair_Separation_Project
## Behavioural_Analysis

SLEAP_Analysis.R
- Loads the combined, cleaned tracking data (`Tracking_data_prep/PairSep_combined_cm.csv`): metadata (BirdID, Phase, Day, Condition, SessionID), cm conversion, interpolated tracking
- Recomputes head speed on interpolated coordinates, grouped by BirdID x Phase x Day x SessionID so speed doesn't spike across session boundaries
- QC: per-session node detection coverage (head/beak) and a 99th-percentile speed outlier filter
- Computes 3 metrics per session (BirdID x Phase x Day): concave hull area of head position, Shannon entropy of head position, and mean head speed
- Fits `metric ~ Phase * Day + (1|BirdID)` mixed models on the 2 experimental birds only (the single control bird is excluded from inferential stats but kept, distinctly marked, in the descriptive plots)
- Combines all 3 metrics into one summary data frame (`all_metrics`), one row per session

node_coverage_check.R
- Diagnostic for how often the tail base "rescues" frames where the head is undetected, per BirdID x Phase x Day
- Informs whether head alone, tail base alone, or a fallback strategy should be used for speed/freezing metrics

PCA_analysis.R
- Principal component analysis of movement metrics
- Not yet adapted to this project's design (still uses the original injection/lesion-nested structure)

Example_plots.R
- Example publication-style visualizations of movement metrics
- Not yet adapted to this project's design (still uses the original condition scheme and circular variance metric)
