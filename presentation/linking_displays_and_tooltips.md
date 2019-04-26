Linking displays, creating 3D surfaces, and adding custom tooltips in Plotly via Shiny
========================================================
author: Jon Minton
date: 24/4/2019
autosize: true

Population Data as Spatial Data
========================================================

Spatial Data

- Latitude
- Longitude
- Elevation

*** 
Population Data

- Age
- Year (Period)
- Rates, Hazards, Counts, Ratios, etc etc

Identifiability Problem
========================================================

- Age Effects
- Period Effects
- Cohort Effects

- Cohort = Period - Age
    - Equations not uniquely specified
    - A line of solutions, not a single solution
    
***

*But does this matter as much as the modellers think it does?*

- Visual images are [sometimes not uniquely identifiable](https://en.wikipedia.org/wiki/Multistable_perception).
- But that doesn't mean vision is useless.


How not to resolve ambiguous information about three dimensonal surfaces
========================================================

- Rely on a single image, from a single perspective. 
- Rely on one or two simple summary measures
    - The river that's four feet deep **on average**
    - A latitude-elevation correlation coefficient
    
How to resolve ambigious information about three dimensional surfaces    
========================================================

- Walk around! See it from many perspective
- Stop, think, ask new questions
- Take sections and samples - one slice at a time
- Combine qualitative (visual) information with quantitative (numeric) information
- Identify features within a surface, and similarities/differences from other surfaces

Challenges addressed    
========================================================

- *Walk around! See it from many perspective*
- Stop, think, ask new questions
- *Take sections and samples - one slice at a time*
- *Combine qualitative (visual) information with quantitative (numeric) information*
- Identify features within a surface, and similarities/differences from other surfaces

Approach
=========================================================

- Main population surface display at top
- Age, period and cohort slices through surface at loci specifed by user
- Interactive tooltips for both

***

Software environment

- R
- Shiny
- Plotly

Example 
==============================================================================

See [here](https://datascapes.shinyapps.io/hmd_explorer/)

- Population mortality comparison

Github repo: https://github.com/JonMinton/hmd_explorer.git 


The two challenges
===============================================================================

1. Creating tooltips
2. Creating subplots that react to surface clicks, not other interactions with the surface

Challenge 1: Custom Tooltips
==============================================================================

- For the subplots: Easy! 


```r
      plot_ly(x = ~ year, y = ~ mr) %>%
      add_lines(
        hoverinfo = 'text',
        text = ~ paste0(
          "Year: ",
          year,
          '\nLog mortality: ',
          round(log(mr, 10), 3),
          '\nDeaths per 10,000: ',
          round(10000 * mr, 0)
        )
      )
```
[Example location](https://github.com/JonMinton/hmd_explorer/blob/master/hmd_explorer/server.R#L224-L235)

Challenge 1: Custom Tooltips
========================================================

- For the surface plot: more challenging

The basic pattern

- Create a matrix of 'elevation' (Z) values of dimension `{P, A}`
- Create vectors of Period and Age values of same length as `P * A`
- Combine above into a matrix of label values, of same dimension as the Z values 



Challenge 1: Custom Tooltips
========================================================

- For the surface plot: more challenging



```r
  z_list <- dta_ss %>%
    mutate(lmr_list = map(data, make_z_list, adjust = 0.5))
  
  
  xx <- z_list[["lmr_list"]][[1]][["age"]]
  yy <- z_list[["lmr_list"]][[1]][["year"]]
  zz <- 10 ^ z_list[["lmr_list"]][[1]][["vals"]]
  
  n_ages <- length(xx)
  n_years <- length(yy)
  
  zc <- z_list[["lmr_list"]][[1]][["vals"]]
```

========================
  

```r
  custom_text <- paste0(
    "In ",
    rep(yy, times = n_ages),
    ", at age ",
    rep(xx, each = n_years),
    "\n log mortality: ",
    round(zc, 3),
    "\n Deaths per 10,000: ",
    round(10000 * zz, 0)
  ) %>%
    matrix(length(yy), length(xx))
```

===========================


```r
  p <-   plot_ly(
    x = ~ xx,
    y = ~ yy,
    z = ~ zz,
    surfacecolor = ~ zc,
    source = "mort_surface"
  ) %>% add_surface(
    colorbar = list(title = "Log Mortality rate"),
    hoverinfo = "text",
    text = custom_text
  ) 
```

[Example location](https://github.com/JonMinton/hmd_explorer/blob/master/hmd_explorer/server.R#L149-L186)

Challenge 2: Redrawing subplots when surface is clicked on
===============================

- Within *surface* chunk:


```r
 p <-   plot_ly(
    x = ~ xx,
    y = ~ yy,
    z = ~ zz,
    surfacecolor = ~ zc,
    source = "mort_surface" # <- THIS BIT, HERE!
  ) 
```

[Example source](https://github.com/JonMinton/hmd_explorer/blob/master/hmd_explorer/server.R#L176-L182)

***

- Within *subplot* chunk:


```r
  s <- event_data("plotly_click", source = "mort_surface")
  if (length(s) == 0) {
    return(NULL)
  } else {
    this_age <- s$x
    this_year <- s$y
    # et cetera et cetera...
```

[Example location](https://github.com/JonMinton/hmd_explorer/blob/master/hmd_explorer/server.R#L210-L215)

Conclusion/Reflections
================================

- Work driven by idiosyncratic drive to get people thinking about population data as spatial surfaces
    - Making sense of many numbers: A type of complexity people are bad at;
    - Spatial/typological complexity: A type of complexity people are good at 
- But challenges encountered and solutions developed have other use-cases
    - Actual spatial data (as rasters)
    - Compositional population data 
    - Virtual reality
    - CAM: Statistical Sculpture/Data Cakes!
    
Thanks for listening! 

Contact me at: Jon.minton@nhs.net
