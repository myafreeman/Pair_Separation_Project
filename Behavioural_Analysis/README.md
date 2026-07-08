# Pair_Separation_Project
## Behavioural_Analysis

SLEAP_Analysis.R
- Uses the combined, cleaned SLEAP tracking data (`Tracking_data_prep`)
- Compute metrics of area and entropy of head position, and speed over levels of time
- Runs statistics for individual metrics


node_coverage_check.R
- Diagnostic for how often the tail base "rescues" frames where the head is undetected, per BirdID x Phase x Day
- Informs whether head alone, tail base alone, or a fallback strategy should be used for speed metrics

PCA_analysis.R
- Principal component analysis of movement metrics
- Not yet adapted to this project's design (still uses the original injection/lesion-nested structure)

Example_plots.R
- Example publication-style visualizations of movement metrics
- Not yet adapted to this project's design (still uses the original condition scheme and circular variance metric)
