# DUC2_viewer

RShiny Viewer for DUC2

this app aims to be modular, so the contents of the different tabs are saved in ./R/module\_\*.R scripts.

## Folder structure:

```         
/
  app.R
  R/
    config.R
    helpers_ui.R
    map_environmental.R
    map_acoustic.R
    module_home.R
    module_seabass.R
    module_porpoise.R
  www/
    app.css
    Logo_BIO-Flow2023_Final_Positive.png
    north_arrow.png
    seabass.png
    porpoise.png
    ...
```

Every subpage that viewers of the Shiny App can see is its own .R file at the moment, to be able to switch pages around easily.