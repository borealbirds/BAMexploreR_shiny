options(repos = c(CRAN = "https://cran.rstudio.com"))

required_packages <- c(
  "leaflet", "shiny", "rmarkdown", "markdown", "shinydashboard", "shinyjs", "shinyBS", 
  "shinycssloaders",  "dplyr", "bslib", "leafem", "glue", "purrr",
  "terra", "stringr",  "DT",  "httr", "RColorBrewer", "sf", "tools", "viridis", "knitr", "ggplot2"
)

# Install any missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load the packages
invisible(lapply(required_packages, library, character.only = TRUE))

# data component related
bcrv4.map <- vect('www/data/4326/BAM_BCRNMv4_4326.shp')
bcrv5.map <- vect('www/data/4326/BAM_BCRNMv5_4326.shp')
BCRNMv4 <- vect('www/data/5072/BAM_BCRNMv4_5072.shp')
BCRNMv5 <- vect('www/data/5072/BAM_BCRNMv5_5072.shp')
load("www/data/sysdata.rda")

spp.grp <- c("COSEWIC","Cavity_Birds", "Waterfowl", "Marine_Birds","Shorebirds", "Wetland_Birds", "Birds_of_Prey",
             "Forest_Birds", "Grassland_Birds", "Aerial_Insectivores", "Arctic_Birds", "Long_Distance_Migrants")

#model.year <- c("1985","1990", "1995", "2000","2005", "2010", "2015", "2020")
model.year <- c("2000","2005", "2010", "2015", "2020")

spp_list <- read.csv('www/data/spp_List.csv')

MB <- 1024^2

UPLOAD_SIZE_MB <- 1000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)


SAVE_SESSION_SIZE_MB_WARNING <- 100

source("./R/bamexplorer.R")
source("./R/utils.R")

# Load all base modules (old format)
# TODO this should not exist after moving all modules to the new format
base_module_files <- list.files('modules', pattern = "\\.R$", full.names = TRUE)
for (file in base_module_files) source(file, local = TRUE)

# The components that have modules. These names must match the values of the
# tabs of the components in the UI.
COMPONENTS <- c("data", "popstats")

# Information about modules that various parts of the app need access to
COMPONENT_MODULES <- list()




