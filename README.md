# Replication: "Current Research Overstates American Support for Political Violence"

Replication materials for Westwood, Grimmer, Tyler & Nall (2022), "Current research overstates American support for political violence," *Proceedings of the National Academy of Sciences* (PNAS), 119(12), e2116870119.

Original paper: https://doi.org/10.1073/pnas.2116870119

## Overview

This paper argues that widely-cited survey estimates of American support for political violence are inflated due to survey inattention. The authors demonstrate that "disengaged" respondents (those who fail attention checks) report much higher support for political violence than "engaged" respondents (those who pass attention checks). The key finding: among engaged respondents, support for partisan violence drops from ~18% (as reported in prior media coverage) to roughly 2-3%.

## Study Design

The replication includes five studies:

- **Study 1** (Qualtrics): Car-ramming vignette with partisan vs. apolitical perpetrator
- **Study 2** (Qualtrics): Mass shooting vignette with partisan vs. apolitical perpetrator
- **Study 3** (YouGov): Nationally representative sample with weighted estimates
- **Study 4**: Pardon/sentencing experiment
- **Study 5**: Additional robustness checks

Key outcome variables:
- `justified`: Whether the perpetrator's actions were justified (binary)
- `supportactions`: Support for the perpetrator's actions (1-5 scale)
- `charged`: Whether the perpetrator should be criminally charged (binary)

Treatment conditions vary the perpetrator's partisan alignment relative to the respondent:
- In-Party (perpetrator shares respondent's party)
- Out-Party (perpetrator opposes respondent's party)
- Apolitical (no partisan cues)

## Directory Structure

```
.
├── code/           # R analysis scripts
│   ├── main.R              # Full replication (runs all analyses)
│   ├── run_core.R          # Core figures only (faster)
│   ├── functions.R         # Helper functions
│   ├── preprocess[1-5].R   # Data preprocessing for each study
│   ├── figure[1-7].R       # Figure generation
│   ├── table1.R            # Table 1 generation
│   ├── appendix*.R         # Supplementary analyses
│   ├── partial_id_bounds.R # Partial identification bounds
│   └── study_results.R     # Summary statistics
├── data/           # Survey data (CSV and TAB formats)
│   ├── study14.csv         # Studies 1 & 4 combined
│   ├── study25.csv         # Studies 2 & 5 combined
│   ├── study3.csv          # Study 3 (YouGov)
│   ├── priorestimates.csv  # Prior media estimates for comparison
│   └── newsCoverage*.csv   # News coverage analysis data
├── results/        # Generated figures (PDF)
├── output/         # Replication logs
└── supplement/     # Original PNAS supplementary materials
```

## Running the Analysis

From the `code/` directory:

```r
# Full replication (all figures, tables, appendix analyses)
source("main.R")

# Core figures only (faster, fewer dependencies)
source("run_core.R")
```

### Dependencies

Required R packages:
- tidyverse (dplyr, readr, ggplot2, tibble, forcats, purrr, tidyr, stringr)
- gtools, cowplot, stargazer
- estimatr, modelsummary, texreg, effects
- Hmisc (for weighted statistics)

## Key Results

The central finding is that respondent engagement dramatically affects measured support for political violence:

| Measure | Disengaged | Engaged | Ratio |
|---------|------------|---------|-------|
| Justified (political treatments) | ~17% | ~2-3% | ~6-8x |
| Support > 3 (political treatments) | ~13% | ~2-3% | ~5-6x |

Prior studies (Kalmoe-Mason) reported ~18% support, which aligns with results among disengaged respondents but not engaged respondents.

## Citation

```bibtex
@article{westwood2022current,
  title={Current research overstates American support for political violence},
  author={Westwood, Sean J and Grimmer, Justin and Tyler, Matthew and Nall, Clayton},
  journal={Proceedings of the National Academy of Sciences},
  volume={119},
  number={12},
  pages={e2116870119},
  year={2022},
  publisher={National Acad Sciences}
}
```

## License

Replication code provided for educational purposes.
