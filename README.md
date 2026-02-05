# DUC2 Viewer - Acoustic Telemetry RShiny Application

An interactive RShiny application for visualizing acoustic telemetry data and environmental layers for the DTO-Bioflow Digital Use Case 2 (DUC2): Impact of Offshore Energy Installations on Marine Life.

![DTO-Bioflow logo](www/Logo_BIO-Flow2023_Final_Positive.png){width="800"}

## 📋 Table of Contents

-   [About](#about)
-   [Features](#features)
-   [Installation](#installation)
-   [Project Structure](#project-structure)
-   [Running the Application](#running-the-application)
-   [Adding New Modules](#adding-new-modules)
-   [Contributing](#contributing)
-   [License](#license)

## 🎯 About {#about}

This RShiny app is part of the Horizon Europe-funded project **DTO-Bioflow** ([dto-bioflow.eu](https://dto-bioflow.eu), Grant ID 101112823, <https://doi.org/10.3030/101112823>). It provides interactive visualizations of:

-   **Acoustic telemetry data** from the [European Tracking Network](https://www.lifewatch.be/etn/)
-   **Species migration predictions** (European seabass, harbour porpoise)
-   **Environmental and Human activities layers** via WMS services, accessed via the [EDITO STAC catalogue](https://viewer.dive.edito.eu/map?c=0,0,2.87&catalog=https:%2F%2Fapi.dive.edito.eu%2Fdata%2Fcatalogs)
-   *To be added later on: An overlap of different species (spatially and temporally) with human activities*

🔗 Learn more: [DUC2 - Impact of Offshore Infrastructures](https://dto-bioflow.eu/use-cases/duc-2-impact-offshore-infrastructures)

## ✨ Features {#features}

### European Seabass (*Dicentrarchus labrax*)

-   Migration predictions (inside/outside offshore wind farms)
-   Acoustic telemetry detection data with interactive temporal charts
-   Environmental layer visualizations

### Harbour Porpoise (*Phocoena phocoena*)

-   Species distribution data
-   Environmental correlates
-   Dashboard enabling exploration of Passive Acoustic Monitoring (PAM) data

## 🚀 Installation {#installation}

### Prerequisites

-   R (≥ 4.0)
-   RStudio (recommended)

### Required R Packages

``` r
install.packages(c(
  "shiny", "DT", "leaflet", "glue", "httr", "terra",
  "htmltools", "leaflet.minicharts", "leaflet.extras", "leafem",
  "tidyr", "RColorBrewer", "rstac", "purrr", "arrow",
  "dplyr", "lubridate"
))
```

### Clone the Repository

``` bash
git clone https://github.com/your-org/DUC2_viewer.git
cd DUC2_viewer
```

## 📁 Project Structure {#project-structure}

```         
DUC2_viewer/
├── global.R                      # Loads libraries, data, and sources all scripts
├── app.R                         # Main application file (UI + Server)
│
├── R/                            # Shiny modules and UI components
│   ├── config.R                  # Configuration (colors, URLs)
│   ├── mod_home.R                # Home page module
│   ├── mod_porpoise.R            # Harbour porpoise module
│   ├── map_base.R                # Base map rendering function
│   ├── map_environmental.R       # Environmental WMS map function
│   │
│   └── seabass/                  # European seabass submodules
│       ├── mod_seabass.R         # Parent seabass module
│       ├── submod_migration.R    # Migration predictions submodule
│       ├── submod_telemetry.R    # Acoustic telemetry submodule
│       └── submod_environmental.R # Environmental layers submodule
│
├── helpers/                      # Data processing & utility functions
│   ├── source_all_files.R        # Recursively source all R files
│   ├── load_acoustic_telemetry_GAM_s3.R  # Load GAM predictions from S3
│   ├── load_STAC_metadata.R      # Load STAC catalog metadata
│   └── wrangle_acoustic_telemetry_data.R # Data wrangling functions
│
├── data/                         # Data files
│   ├── deployments.rds           # Acoustic receiver deployments
│   └── etn_sum_seabass_monthyear_individual.rds  # Detection summaries
│
└── www/                          # Static web assets
    ├── app.css                   # Custom CSS styles
    └── *.png                     # Images (logos, icons)
```

### Key Files

| File | Purpose |
|-----------------------------|-------------------------------------------|
| `global.R` | Runs once on app startup; loads libraries, data, and sources all R scripts |
| `app.R` | Defines UI and server logic; calls module UI/server functions |
| `R/config.R` | Stores configuration variables (colors, URLs, etc.) |
| `R/mod_*.R` | Shiny modules (self-contained UI/server pairs) |
| `helpers/*.R` | Pure R functions for data loading and processing |

## 🏃 Running the Application {#running-the-application}

### From RStudio

1.  Open `app.R`
2.  Click "Run App" button

### From R Console

``` r
shiny::runApp()
```

### From Command Line

``` bash
R -e "shiny::runApp()"
```

## 🔧 Adding New Modules {#adding-new-modules}

This application uses a **modular architecture**. Each tab/feature is a self-contained module.

### Step 1: Create Module File

Create a new file in `R/` (e.g., `R/mod_mynewspecies.R`):

``` r
# R/mod_mynewspecies.R

# UI Function
mod_mynewspecies_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("My New Species"),
    leafletOutput(ns("map"), height = 700)
  )
}

# Server Function
mod_mynewspecies_server <- function(id, 
                                     data,           # Data parameter
                                     base_map_fun) { # Function parameter
  moduleServer(id, function(input, output, session) {
    
    output$map <- renderLeaflet({
      base_map_fun() %>%
        addMarkers(data = data, ~lon, ~lat)
    })
    
  })
}
```

### Step 2: Add Module to UI

In `app.R`, add a new tab panel:

``` r
# In app.R ui section
tabsetPanel(
  # ... existing tabs ...
  
  tabPanel(
    title = "My New Species",
    mod_mynewspecies_ui("mynewspecies")  # Call module UI
  )
)
```

### Step 3: Call Module Server

In `app.R` server function:

``` r
# In app.R server section
server <- function(input, output, session) {
  # ... existing modules ...
  
  mod_mynewspecies_server(
    "mynewspecies",
    data = my_species_data,           # Pass data
    base_map_fun = make_base_map      # Pass functions
  )
}
```

### Step 4: Load Required Data

In `global.R`, add data loading:

``` r
# In global.R
my_species_data <- readRDS("data/my_species_data.rds")
```

### 🔑 Key Principles

1.  **Function Parameters**: Always pass functions and data as parameters to modules
2.  **Use Parameter Names**: Inside modules, use parameter names (e.g., `base_map_fun`), not global function names (e.g., `make_base_map`)
3.  **Namespace**: Use `ns <- NS(id)` in UI and wrap input/output IDs with `ns()`
4.  **Self-Contained**: Each module should be independent and reusable

### Example Module Call Chain

``` r
# In global.R (define once)
make_base_map <- function() { ... }

# In app.R (pass to module)
mod_seabass_server(
  "seabass",
  base_map_fun = make_base_map  # ← Global function name
)

# In R/mod_seabass.R (receive as parameter)
mod_seabass_server <- function(id, base_map_fun) {
  moduleServer(id, function(input, output, session) {
    
    # Pass to submodule
    mod_seabass_migration_server(
      "migration",
      base_map_fun = base_map_fun  # ← Use parameter name
    )
  })
}

# In R/seabass/submod_migration.R (use parameter)
mod_seabass_migration_server <- function(id, base_map_fun) {
  moduleServer(id, function(input, output, session) {
    
    output$map <- renderLeaflet({
      base_map_fun()  # ← Call the function
    })
  })
}
```

## 📊 Data Sources

-   **Acoustic Telemetry**: [European Tracking Network (ETN)](https://www.lifewatch.be/etn/)
-   **Environmental Data**: STAC catalog via `load_STAC_metadata()`
-   **Species Predictions**: GAM models from S3 storage

## 🤝 Contributing {#contributing}

### Development Workflow

1.  Create a feature branch: `git checkout -b feature/my-new-feature`
2.  Make changes following the modular structure
3.  Test the app locally
4.  Commit with clear messages: `git commit -m "Add new species module"`
5.  Push and create a pull request

### Code Style

-   Use `snake_case` for function and variable names
-   Comment complex logic
-   Keep modules self-contained
-   Pass data and functions as parameters (avoid global dependencies)

## 👥 Contributors

-   **Flanders Marine Institute (VLIZ)** [Marine Observation Centre, VLIZ](https://vliz.be/en/what-we-do/research/marine-observation-centre) - Lotte Pohl - [lotte.pohl\@vliz.be](mailto:lotte.pohl@vliz.be){.email}, Jo-Hannes Nowé - [johannes.nowe\@vliz.be](mailto:johannes.nowe@vliz.be){.email}

-   Aarhus University: Emily T. Griffiths, Mia Kronborg, Ellen Jacobs, (..)

-   Technical University of Denmark: Asbjørn Christensen

## 📄 License {#license}

MIT

## 🔗 Links

-   **DTO-Bioflow Project**: <https://dto-bioflow.eu>
-   **DUC2 Use Case**: <https://dto-bioflow.eu/use-cases/duc-2-impact-offshore-infrastructures>
-   **European Tracking Network**: <https://www.lifewatch.be/etn/>
-   **VLIZ**: <https://vliz.be/en>

------------------------------------------------------------------------

**Grant Information**: This project has received funding from the European Union's Horizon Europe research and innovation programme under grant agreement No. 101112823 (DTO-Bioflow).
