
=======
# SMC calibration
R package providing functionality for calibration of soil moisture sensors.
* Get example data for calibration via data("SensorVSample").
* Query sites to calibrate and retrieve filtered data frame.
* Perform linear calibration with standars OLS or MM-type regressor.
* Visualize result.
* Interactive shiny app includes subseting of data, choice of method, visualization, diagnostics of the regression model.

## How to start

First install the package with:

```R
if (!require("devtools")) install.packages("devtools")
install_github("JBrenn/SMCcalibration")
```

and then import the library with:

```R
library(SMCcalibration)
```

=======

For running the calibration shiny app:

```R
runShinyapp()
```

The online version of the app can be used as an working example:
https://jgbr.shinyapps.io/shiny/

The same version runs with the following code:
```R
shiny::runGitHub('JBrenn/SMCcalibration', subdir="inst/shiny/", launch.browser = TRUE)
```
## Acknowledgement

For this application I got inspired by the RStudio shiny gallery and used these [code snippets](http://shiny.rstudio.com/gallery/plot-interaction-exclude.html).

R developer of the packages [shiny](https://shiny.rstudio.com), [ggplot2](https://ggplot2.tidyverse.org), [robustbase](https://cran.r-project.org/web/packages/robustbase/index.html), [Cairo](https://cran.r-project.org/web/packages/Cairo/index.html).


