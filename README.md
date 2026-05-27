# NYC 311 Service Request Analysis

This repo contains the R Shiny web application for an interactive spatial and temporal analysis of NYC 311 service complaints. The app allows users to dynamically explore complaint patterns across New York City neighborhoods, with a focus on geographic disparities in service response times.

- [Shiny App Source](./app.R): Full application code including all tabs, visualizations, and reactive logic

## Overview

NYC 311 receives millions of service requests each year, but response times and complaint patterns vary significantly across neighborhoods. This project builds an interactive dashboard to surface those disparities and make the data accessible to non-technical users.

- Built and deployed an **interactive Shiny web application** analyzing spatial and temporal patterns in NYC 311 complaints
- Designed the **Response Times tab** — analyzing geographic disparities in service delays across neighborhoods using `sf`, `ggplot2`, and `dplyr`
- Translated static R scripts from group members into **modular Shiny code**, consolidating all visualizations into a unified dashboard
- Implemented dynamic filtering using `leaflet`, `bslib`, and `shinydashboard` for interactive map and chart exploration
- Resolved rendering errors, reactivity bugs, and UI glitches with no prior Shiny experience

## Features

- **Interactive map** of NYC 311 complaints by neighborhood using leaflet
- **Response Times tab** surfacing geographic disparities in service delays
- **Temporal analysis** of complaint volume trends over time
- **Dynamic filters** by complaint type, borough, and date range
- Clean, unified UI built with bslib and shinydashboard

## Data

The NYC 311 dataset is not included in this repository due to its large file size. It is publicly available from [NYC Open Data](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9). The app includes built-in filters to load and process a manageable subset of the data.

## Requirements

- R 4.x
- RStudio (recommended)

## Dependencies

```r
install.packages(c("shiny", "shinydashboard", "bslib", "leaflet", 
                   "ggplot2", "dplyr", "sf", "tidyr", "lubridate"))
```

## Running the App

```r
shiny::runApp("app.R")
```

## Authors

Sophia Huang · Fleishman · Green · Cindy Zheng  
Binghamton University · Spring 2025
