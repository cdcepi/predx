### `predx`: An R package for predictions

`predx` aims to be a flexible, lightweight framework for working with predictions. The package implements a standardized embedded data frame in R which captures a variety of prediction types. Predictions can be imported from or exported to CSV or JSON files.

Currently, the package supports basic manipulation and verification of predictions, including those for Epidemic Prediction Initiative challenges. The FluSight and Aedes Vignettes demonstrate how the package can be used for forecasts for these challenges.

Four `predx` classes (S4) are now available: `Point`, `Binary`, `BinLwr`, and `BinCat`. More will be added in the future.

To get started, load the devtools package and install `predx` from this repository with the vignettes.
```
library(devtools)
devtools::install_github('cdcepi/predx', build_vignettes = TRUE)
```
The documentation is quite sparse except for the [FluSight](https://github.com/cdcepi/predx/blob/master/vignettes/flusight-vignette.Rmd) and [Aedes](https://github.com/cdcepi/predx/blob/master/vignettes/aedes-vignette.Rmd) vignettes.
```
library(predx)
vignette('flusight-vignette')
vignette('aedes-vignette')
```

If you find bugs, please open an [issue in this respository](https://github.com/cdcepi/predx/issues).
