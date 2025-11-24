<center>

# Welcome to the BAM Landbird Models Explorer

</center>

<br>

## Overvew

The BAM Landbird Model Explorer is an Shiny application for downloading and analyzing landbird density models produced by the Boreal Avian Modelling Centre (BAM). 

<br>

The BAM landbird density models are species-specific predictions of the density of breeding male birds per hectare at a 1km resolution across the boreal forest. They are produced with a generalized analytical approach to model landbird species density in relation to environmental  predictors, using in-person or ARU point-count surveys and widely available spatial predictors. See the help section for more details. 

<br>

The BAM Landbird Models Explorer has three sections:

**1.** *Model Access:* Select, view, and download the model products of interest.
*Note you must complete this section before using the Population Distribution section*

**2.** *Population Distribution:* Evaluate distribution and population size from the models.   

**3.** *Predictor Importance:* Explore the relative importance of environmental predictors in the models.  

<br>

## Model Versions

Two versions of the BAM landbird density models are available in the BAM Landbird Model Explorer. 

| Feature                                                             | BAM V4                                                | BAM V5                                                                 |
|---------------------------------------------------------------------|--------------------------------------------------------|------------------------------------------------------------------------|
| **Release year**                                                   | 2020                                                 | 2025 |
| **Species included**                                               | 143                                                  | 67 priority species; 77 in progress |
| **Dataset size**                                                   | 0.3 million surveys                                  | 1.4 million surveys, including eBird |
| **Geographic extent**                                              | Canada only                                          | Canada; US boreal & hemiboreal in progress |
| **Temporal resolution**                                            | Predictions for 2017                                 | Predictions at five-year intervals from 2000 to 2020; 1990 to 1995 in progress |
| **Model subregions**                                           | Bird conservation region (BCR)          | Updated BCRs and country |
| **Environmental predictors**                                       | Landcover, biomass, climate                          | Time-matched predictors for vegetation biomass, human disturbance, and annual climate |
| **Model reliability information**                                  | Cross-validated model performance                    | Map of coefficient of variation across bootstraps; cross-validated modelperformance, maps of model extrapolation & detection distribution in progress |

<br>

## Other Access Options

Other options for model access include:
- *1. [BAMexploreR R package](https://github.com/borealbirds/BAMexploreR)* - download and analyze rasters directly in the R environment.
- *2. [Google Earth Engine viewer](https://borealbirds-gee.projects.earthengine.app/view/landbirdmodels)* - view and explore the version 5 Canada-wide models and uncertainty over Google imagery.
- *3. [BAM model website](https://borealbirds.github.io/)* - view predictions and metadata for the version 4 Canada-wide models.
- *4. [BAM Geoportal](http://data.borealbirds.ca/srv/eng/catalog.search#/home)* - download the landbird models and BAM's other model products.