# QLDM - Quebec Landscape Dynamic Model

## Introduction

The **QLDM** package provides a set of functions to simulate landscape-scale processes that influence the structure and composition of forest stands in a region, such as forest management (timber harvesting, salvagge logging), wildfires and insect outbreaks.

The Quebec Landscape Dynamic Model is initialized for the commercial forests of Quebec province (Canada) in 2020. It is a spatially explicit model calibrated to work at 2 km of spatial resolution and 5-year time step.


## Package installation

Users can download and install the latest stable version of the **QLDM** package from GitHub as follows (required package devtools should be installed/updated first):

```R
devtools::install_github("nuaquilue/QLDM")
```
Additionally, users can have help to run package functions directly as package vignettes, by forcing their inclusion in installation:

```R
devtools::install_github("nuaquilue/QLDM", 
                         build_manual = TRUE),
                         build_vignettes = TRUE)
```

## References

Bouchard, M., Aquilué, N., Périé, C., Lambert, M.-C. 2019. Tree species persistence under warming conditions: A key driver of forest response to climate change. Forest Ecology and Management, 442, 96-104. https://doi.org/10.1016/j.foreco.2019.03.040.
