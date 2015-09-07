
=======
# SMC calibration
R package providing functionality for calibration of soil moisture snsors installed in South Tyrol.
* get calibration data via data("SensorVSample")
* choose parameters to calibrate on and get diminished dataframe (CAL_doreg_data)
* performs linear calibration and visualization (CAL_doreg)
* interactive shiny app for calibration (see below)


# How to start

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
data("SensorVSample")
shinyApp(ui, server)
```