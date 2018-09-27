
=======
# SMC calibration
R package providing functionality for calibration of soil moisture sensors.
* get calibration data via data("SensorVSample")
* query sites to calibrate and retrieve filtered data frame (CAL_doreg_data)
* perform linear calibration and visualization (CAL_doreg)
* interactive shiny app for sensor calibration (how to run see below)


## How to start

First install the package with:

```R
install.packages("devtools")
library(devtools)
install_github("JBrenn/SMCcalibration")
```

and then import the library with:

```R
library(SMCcalibration)
```

=======

For running the calibration shiny app:

```R
if (!require("shiny")) install.packages("shiny")

data("SensorVSample")
data <- unique(data[,-8])

shiny::shinyApp(ui, server)
```

The online version of the app can be used as an working example:
https://euracalpenv.shinyapps.io/SWC_Calibration/

The same version runs with the following code:
```R
if (!require("shiny")) install.packages("shiny")

shiny::runGitHub('GiulioGenova/SMCcalibration',subdir="R",launch.browser = TRUE)
```
## Acknowledgement

For this application I got inspired by the RStudio shiny gallery and used these [code snippets](http://shiny.rstudio.com/gallery/plot-interaction-exclude.html).

R developer of the packages [shiny](https://shiny.rstudio.com), [ggplot2](https://ggplot2.tidyverse.org), [robustbase](https://cran.r-project.org/web/packages/robustbase/index.html), [Cairo](https://cran.r-project.org/web/packages/Cairo/index.html).


