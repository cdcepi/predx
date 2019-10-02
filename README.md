### `predx`: An R package for predictions

`predx` aims to be a flexible, lightweight framework for working with predictions. The package implements a standardized embedded data frame in R which captures a variety of prediction types. Predictions can be imported from or exported to CSV or JSON files, including a `predx`-specific JSON format that drastically reduces file sizes. This format can be particularly helpful for transfering large sets of forecasts.

Get package details here: [http://cdcepi.github.io/predx](http://cdcepi.github.io/predx)

Currently, the package supports basic manipulation and verification of predictions, including those for Epidemic Prediction Initiative challenges. The [FluSight Vignette](http://cdcepi.github.io/predx/articles/flusight-vignette.html) and [Aedes Vignette](http://cdcepi.github.io/predx/articles/aedes-vignette.html) demonstrate how the package can be used to validate forecasts for these challenges. Code is also provided to interface with the [FluSight](https://github.com/jarad/FluSight) R package for scoring FluSight forecasts (see the [FluSight Vignette](http://cdcepi.github.io/predx/articles/flusight-vignette.html)).

Six `predx` classes (S4) are now available: `Point`, `Binary`, `BinLwr`, `BinCat`, `Sample`, and `SampleCat`. More will be added in the future, brief descriptions of current and planned classes are available [here](https://github.com/cdcepi/predx/blob/master/predx_classes.md).

To get started, load the devtools package and install `predx` from this repository with the vignettes. The documentation is currently quite sparse, but the vignettes provide a guide to basic functions.
```
library(devtools)
devtools::install_github('cdcepi/predx', build_vignettes = TRUE)
```

If you find bugs, please open an [issue](https://github.com/cdcepi/predx/issues).
