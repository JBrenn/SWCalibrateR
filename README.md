
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
library(shiny)

data("SensorVSample")
data <- unique(data[,-8])

shinyApp(ui, server)
```

For this application I got inspired by the RStudio shiny gallery and used these [code snippets](http://shiny.rstudio.com/gallery/plot-interaction-exclude.html).
