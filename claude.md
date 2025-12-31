# HMD Explorer

## Project Overview

HMD Explorer is a Shiny web application for interactive exploration and visualization of Human Mortality Database (HMD) data. It enables researchers and demographers to analyze mortality and population patterns across 52+ countries spanning 1750-2020.

## Technology Stack

- **Framework**: R Shiny
- **Language**: R
- **Key Packages**: tidyverse, ggplot2, plotly, RColorBrewer, viridis, magrittr
- **Data Format**: CSV (~64MB main dataset), RDS for lookups

## Directory Structure

```
hmd_explorer/
├── hmd_explorer/                 # Original app (ui.R/server.R pattern)
│   ├── ui.R                      # UI definition (tabbed interface)
│   ├── server.R                  # Server logic (~44KB monolith)
│   ├── global.R                  # Global config and module definitions
│   └── data/                     # HMD data files
│       ├── hmd_data.csv          # Main dataset (1.5M rows)
│       ├── hmd_e0.csv            # Life expectancy data
│       └── codes_named.rds       # Country code lookups
│
├── hmd_explorer_modularized/     # Refactored modular version (WIP)
│   ├── app.R                     # Single-file entry point
│   ├── global.R                  # Dependencies
│   ├── module_select_data.R      # Data selection module
│   ├── module_make_graphics.R    # Visualization module
│   ├── module_make_corrmaps.R    # Correlation mapping module
│   └── module_colscheme_selector.R
│
├── scripts/                      # Data processing & analysis scripts
│   ├── grab_data_from_hmd_api.R  # Data fetching from HMD
│   ├── calc_indexed_trends.R     # Trend calculations
│   └── lifetable_decomposition.R # Decomposition analysis
│
├── cancer_applications/          # Cancer mortality analysis
├── figures/                      # Generated visualizations
├── tables/                       # Tabular outputs
└── *.Rmd                         # Research analysis documents
```

## Key Features

- **Lexis Surface Plots**: 2D heatmaps showing age × period mortality/population
- **3D Surface Plots**: Interactive Plotly visualizations
- **Sex Ratio Comparisons**: Male vs female mortality/population ratios
- **Group Comparisons**: Cross-population mortality differences
- **Correlation Maps**: Age-specific mortality trend correlations
- **Life Table Display**: Standard demographic life table outputs

## Data Structure

Main dataset (`hmd_data.csv`) columns:
- `code`: Country/population identifier (e.g., GBR_SCO, FRATNP)
- `Year`: Calendar year (1750-2020)
- `Age`: Age in years (0-110)
- `gender`: Male, Female, or Total
- `num_deaths`: Death counts
- `num_population`: Population counts
- `exposure`: Person-years of exposure

## Running the Application

```r
# Original version
shiny::runApp("hmd_explorer")

# Modularized version
shiny::runApp("hmd_explorer_modularized")
```

## Development Notes

- Two parallel implementations exist: original and modularized (in progress)
- Modularized version uses Shiny module pattern for better maintainability
- Data sourced from Human Mortality Database (publicly available)
- Some experimental features are work-in-progress

## Common Tasks

### Adding a New Country
1. Fetch data using `scripts/grab_data_from_hmd_api.R`
2. Append to `hmd_data.csv`
3. Update `codes_named.rds` with new country code mapping

### Adding a New Visualization
1. In modularized version: create new module in `module_*.R`
2. In original version: add to `server.R` and update `ui.R` tabs

### Updating Data
Run `scripts/grab_data_from_hmd_api.R` to fetch latest HMD data

## Testing

The project includes a comprehensive test suite in `tests/testthat/`.

### Running Tests

```r
# Run all tests
source("tests/run_tests.R")

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Test Structure

```
tests/
├── testthat.R                    # Test runner config
├── run_tests.R                   # Standalone test runner script
└── testthat/
    ├── setup.R                   # Test helpers and function definitions
    ├── test-make_z_list.R        # Matrix transformation tests
    ├── test-calc_qx.R            # Life table calculation tests
    ├── test-get_selected_data.R  # Data filtering/aggregation tests
    ├── test-surface_functions.R  # Plot generation integration tests
    └── test-shiny-app.R          # Shiny app integration tests
```

### Test Categories

1. **Unit Tests**: Pure function tests for:
   - `make_z_list()` - mortality data matrix transformation
   - `make_z_list_pop()` - population data matrix transformation
   - `calc_qx()` - probability of death calculations
   - `calculate_survivors_properly()` - cohort survival calculations
   - `get_selected_data()` - data filtering and aggregation

2. **Integration Tests**: Tests for:
   - Surface plot data preparation
   - Sex ratio calculations
   - Group comparison logic
   - Subplot data extraction

3. **Shiny App Tests**: Validation of:
   - Application structure
   - Input control specifications
   - Output component definitions

### Key Test Scenarios

- Age filtering (0-110 years)
- Period filtering (1750-2020)
- Gender-specific calculations (Male, Female, Total)
- Infant mortality special cases (age 0)
- Small number corrections
- Edge cases (division by zero, log(0), extreme ratios)

## Known Issues / Technical Debt

- Large monolithic `server.R` in original version (~44KB)
- No formal dependency management (uses pacman, not renv)
- Modularized refactor incomplete
- No input validation or error handling
- Data loads fully on each session (performance impact)

## Core Functions Reference

These functions from `server.R` must be preserved/replicated in the modularized version:

### Data Transformation Functions

| Function | Purpose | Signature |
|----------|---------|-----------|
| `make_z_list_pop()` | Convert population data to matrix format | `make_z_list_pop(X, what = "num_population")` |
| `make_z_list()` | Convert mortality data to matrix with log transform | `make_z_list(X, what = "lmr_k", adjust = 0, k = 10)` |
| `get_selected_data()` | Filter and aggregate for group comparisons | `get_selected_data(full_data, input)` |

### Life Table Functions (from `scripts/calc_qx.R`)

| Function | Purpose | Signature |
|----------|---------|-----------|
| `calc_qx()` | Calculate probability of death | `calc_qx(age, sex, deaths, exposure)` |
| `calculate_survivors_properly()` | Calculate cohort survivors | `calculate_survivors_properly(x, init_size = 50000)` |

### Surface Plot Functions

| Function | Purpose | Dependencies |
|----------|---------|--------------|
| `return_mort_surface()` | Generate mortality 3D surface | plotly, make_z_list |
| `return_mort_subplot()` | Generate linked mortality subplots | plotly_click event |
| `return_pop_surface()` | Generate population 3D surface | plotly, make_z_list_pop |
| `return_pop_subplot()` | Generate linked population subplots | plotly_click event |
| `return_mortratio_surface()` | Generate sex ratio mortality surface | plotly |
| `return_popratio_surface()` | Generate sex ratio population surface | plotly |
| `return_popratio_subplot()` | Generate linked ratio subplots | plotly_click event |
| `return_mortgroup_surface()` | Generate group comparison surface | eventReactive, plotly |
| `return_mortgroup_subplot()` | Generate linked comparison subplots | plotly_click event |

### Key Formulas

```r
# Mortality rate
mr = (deaths + adjust) / (exposure + adjust)

# Log mortality rate (base 10)
lmr = log(mr, 10)

# Sex ratio
ratio = Male / Female

# Excess mortality/population
excess = (Male - Female) / Female

# Birth cohort
birth_cohort = year - age

# Probability of death (qx)
# For age 0: a_x depends on mortality level and sex
# For age 1+: a_x = 0.5
q_x = m_x / (1 + (1 - a_x) * m_x)

# Survival probability
p_x = 1 - q_x
```

## Future Improvements

See conversation history for detailed refactoring recommendations covering:
- Completing modularization
- Performance optimization (parquet, caching)
- Modern Shiny practices (bslib, golem)
- Testing and documentation
- Containerization and deployment
