#set up/load packages
packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }
# create a string of package names
packages <- c('tidyverse',
              'rmarkdown',
              'ggthemes',
              'RColorBrewer',
              'viridis',
              'ggdark',
              'plotly',
              'dataRetrieval',
              'snotelr',
              'patchwork',
              'lubridate',
              'rstatix',
              'ggpubr',
              'corrplot',
              'purrr',
              'sf',
              'terra',
              'tmap',
              'tigris',
              'elevatr',
              'rgbif',
              'soilDB',
              'plotly',
              'mapview',
              'leaflet',
              'jsonlite',
              'httr')

packageLoad(packages)

