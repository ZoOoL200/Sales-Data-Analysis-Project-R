# ======================================================
# Environment Setup Script
# Installs required packages if not already installed
# ======================================================
required_packages <- c(
  "tidyverse",
  "lubridate",
  "janitor",
  "ggplot2",
  "scales",
  "ggtext",
  "patchwork"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()
                                    [,"Package"])]

if(length(new_packages)){
  install.packages(new_packages)
}

lapply(required_packages, library, character.only = TRUE)
