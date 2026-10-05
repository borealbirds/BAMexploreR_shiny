options(repos = c(CRAN = "https://cran.rstudio.com"))

# Packages are loaded with explicit library() calls so that rsconnect/renv can
# detect them when writing manifest.json for deployment (a loop over a vector is invisible to them)
suppressPackageStartupMessages({
  library(leaflet)
  library(shiny)
  library(markdown)
  library(shinyjs)
  library(shinycssloaders)
  library(dplyr)
  library(bslib)
  library(leafem)
  library(purrr)
  library(opticut)
  library(readr)
  library(terra)
  library(stringr)
  library(DT)
  library(httr)
  library(RColorBrewer)
  library(sf)
  library(ggplot2)
  library(zip)
})

# data component related
bcrv4.map <- vect('www/data/4326/BAM_BCRNMv4_4326.shp')
bcrv5.map <- vect('www/data/4326/BAM_BCRNMv5_4326.shp')
BCRNMv4 <- vect('www/data/3978/BAM_BCRNMv4_3978.shp')
BCRNMv5 <- vect('www/data/3978/BAM_BCRNMv5_3978.shp')
load("www/data/sysdata.rda")

can.bcr <- c("can3","can5","can9","can10","can11","can12","can13","can14","can4-0","can4-4","can4-3","can71","can72","can73",  
             "can74","can75","can76","can77-0", "can77-1")
  
alaska.bcr <-c("usa2","usa4-0","usa4-1","usa4-2","usa5")
  
lower48.bcr <- c("usa5", "usa9","usa10","usa11","usa12","usa13","usa14","usa23","usa28","usa30")


spp.grp <- c("COSEWIC","Cavity_Birds", "Waterfowl", "Marine_Birds","Shorebirds", "Wetland_Birds", "Birds_of_Prey",
             "Forest_Birds", "Grassland_Birds", "Aerial_Insectivores", "Arctic_Birds", "Long_Distance_Migrants")

#model.year <- c("1985","1990", "1995", "2000","2005", "2010", "2015", "2020")
model.year <- c("2020")

#spp_list <- read.csv('www/data/spp_List.csv')
#bird_matrix <- readRDS('www/data/birdlist.rds')

MB <- 1024^2

UPLOAD_SIZE_MB <- 1000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)


# Source all helper functions and .rda object
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)
for (f in list.files("www/data", pattern = "\\.rda$", full.names = TRUE)) load(f)

# Load all base modules (old format)
# TODO this should not exist after moving all modules to the new format
base_module_files <- list.files('modules', pattern = "\\.R$", full.names = TRUE)
for (file in base_module_files) source(file, local = TRUE)



