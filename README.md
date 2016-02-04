
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

For running the calibration shiny app:

```R
library(shiny)

data("SensorVSample")
data <- unique(data[,-8])

shinyApp(ui, server)
```


=======

### Branch download

Enables downloading of (un)calibrated soil moisture data (specific sensors) after adjusting the calibration function.
This branch is still a beta version. To use it install via

```R
install.packages("devtools")
library(devtools)
install_github("JBrenn/SMCcalibration@download")
```

and then import the library with:

```R
library(SMCcalibration)
```

Push the database file _swc.sqlite_ into the folder _data_ of the _SMCcalibration_ package file system. The database file can be [downloaded here](https://cloud.scientificnet.org/index.php/s/x6CZtdVdcsoTfvy/download). You can find the path to the package file system by executing

```R
path.package("SMCcalibration")
```

Be aware: you have to run the shiny app externally (e.g. web browser) to enable data download.
For running the calibration shiny app:

```R
library(shiny)

data("SensorVSample")
data <- unique(data[,-8])

shinyApp(ui, server, launch.browser	= TRUE)
``` 


=======

For this application I got inspired by the RStudio shiny gallery and used these [code snippets](http://shiny.rstudio.com/gallery/plot-interaction-exclude.html).

