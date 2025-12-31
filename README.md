# HMD Explorer

An interactive Shiny web application for exploring Human Mortality Database (HMD) data using 3D surface visualizations.

## Overview

HMD Explorer enables researchers and demographers to analyze mortality and population patterns across 52+ countries spanning 1750-2020. It provides interactive Lexis surface plots, sex ratio comparisons, and group comparison tools.

## Features

- **3D Mortality Surfaces**: Interactive Plotly visualizations of age × period mortality
- **Population Pyramids**: Stacked male/female population by age and year
- **Sex Ratio Analysis**: Male:Female mortality and population ratios
- **Group Comparisons**: Compare mortality patterns between countries/regions
- **Life Tables**: Standard demographic life table displays

## Quick Start

```r
# Run the application
shiny::runApp("hmd_explorer")
```

## Project Structure

```
hmd_explorer/
├── hmd_explorer/              # Main Shiny application
│   ├── ui.R                   # User interface
│   ├── server.R               # Server logic
│   ├── global.R               # Shared configuration
│   └── data/                  # HMD data files
│
├── hmd_explorer_modularized/  # Refactored version (in progress)
├── scripts/                   # Utility scripts
├── tests/                     # Test suite
└── claude.md                  # Detailed project documentation
```

## Data

The application uses data from the [Human Mortality Database](https://www.mortality.org/), which provides detailed mortality and population data for research purposes.

## Documentation

See [claude.md](claude.md) for detailed documentation including:
- Complete function reference
- Testing guide
- Development notes

## Archive

Research documents and analysis outputs from earlier development are preserved in the `archive/research-documents` branch.

## License

See [LICENSE](LICENSE) for details.
