# iNZightModules: Add-on modules for iNZight

![R-CMD-check](https://github.com/iNZightVIT/iNZightModules/workflows/R-CMD-check/badge.svg)
[![Coverage status](https://codecov.io/gh/iNZightVIT/iNZightModules/branch/dev/graph/badge.svg)](https://codecov.io/github/iNZightVIT/iNZightModules?branch=dev)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

This R package contains add-on modules for iNZight, each of which targets some particular application.
The modules connect to a related `R` package, which will be downloaded as it is needed (to reduce the size of the iNZightVIT download).

---

The modules are as follows:

- **Quick Explore** modules:
  These include `allBivarPlots.R`, `allUniPlots.R`, and `scatterPlotMatrix.R`,
  which provide UI's for viewing all bivariate plots, all univariate plots, and
  generating a scatter plot matrix, respectively.

- **3D Plot**:
  `plot3Dmode.R` - a UI for producing 3D plots of data, and `supportPlot3D.R` - support functions for the 3D plot.

- **Time Series**:
  `iNZightTS.R` - a UI for visualising and doing basic forecasts of time series data.

- **Model Fitting**:
  `modelFitting.R` - a UI for fitting ([survey] generalised) linear models to data, and generating model checking plots.

- **Multiple Response**:
  `iNZightMultiRes.R` - a UI for visualising multiple response type data (e.g., check all that apply).

- **Maps**:
  `iNZightMaps.R` - a UI for visualising geographical data.
