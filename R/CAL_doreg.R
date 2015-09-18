# data:
# setwd("/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/MONALISA/05_Arbeitsbereiche/BrJ/05_calibrationSWC")
# data <- read.csv(file = "SUMMARY_TOTAL.csv")
# 
# data_unique <- unique(data[,-8])

# date: year of observation

CAL_doreg <- function (data, date_obs=NA, landuse=NA, depth=NA, station=NA, project=NA, sensorType = NA, sensorName=NA, plot=TRUE, pdf=TRUE)
{
  choices <- c(project=project, station=station, landuse=landuse, depth=depth, date_obs=date_obs, sensorName=sensorName, sensorType=sensorType)
  choices.na <- is.na(choices)
  
  for (i in 1:length(choices.na))
  {
    
    if (!choices.na[i])
    {
      
      if (names(choices)[i]=="date_obs") {
        data <- data[grepl(choices[i],as.character(data$date_obs)),]
      } else {
        data <- data[data[names(choices)[i]]==choices[i],]
      }
      
    }
  
  }
  
  if (dim(data)[1]>=2) 
  {
    reg.lin <- lm(formula = meansample~meanstation, data = data)
    
    summary <- summary(reg.lin)
    adj.r <- round(summary$adj.r.squared,2)
    res <- residuals(reg.lin)
    
    intercept<-round(summary$coefficients[1,1],2)
    estimate  <- round(summary$coefficients[2,1],2)
    npoints<-length(res)
    meanres <- round(mean(abs(res)),2)
    sdres <- round(sd(res),2)
  } else {
    intercept <- estimate <- adj.r <- npoints <- meanres <- sdres <- NA
    reg.lin <- NA
  }
  
  infodfcunique<- c(project,station,landuse,date_obs,depth,intercept,estimate,adj.r,meanres,sdres,npoints)
  
  output <- list(info = infodfcunique, linMod = reg.lin)
  
  if(plot & !is.na(estimate))
  {
    
    if(estimate < 0) {
      linMod_text <- paste("y = ", intercept, " - ", abs(estimate), " *x , adj.R = ", adj.r)
    } else {
      linMod_text <- paste("y = ", intercept, " + ", abs(estimate), " *x , adj.R = ", adj.r)
    }
    
    if(pdf)
      pdf(file = paste(choices[1],choices[2],choices[3],choices[4],choices[5],choices[6],".pdf",sep="_"), width = 10)
    
    plot(reg.lin$model$meanstation, reg.lin$model$meansample, pch=16, cex=1.2, xlim=c(0,0.6), ylim=c(0,0.6), ylab="MEAM SAMPLE (-)", xlab="MEAN STATION (-)")
    abline(h=seq(0,0.6,0.1), col=grey(level = 0.8, alpha = 0.5))
    abline(v=seq(0,0.6,0.1), col=grey(level = 0.8, alpha = 0.5))
    abline(a=0, b=1, col=grey(level = 0.6, alpha = 0.5))
    text(x = 0.05, y = 0.05, labels = "y = x", col=grey(level = 0.6, alpha = 0.5))
    text(x = 0.55, y = 0.55, labels = "y = x", col=grey(level = 0.6, alpha = 0.5))
    abline(reg.lin, col=rgb(1,0,0,.5), lwd=3)
    # bug choices are NULL/ zero length
    legend("topleft", legend = paste(c("CALIBRATION FOR:", choices[!choices.na])))
    text(x = 0.45, y = 0.05, labels = linMod_text, col=rgb(1,0,0,.75))
    
    if(pdf)
      dev.off()
  }
  
  return(output)
}

# example
#doreg(data = data_unique, date_obs = NA, landuse = NA, depth = NA, station = "B2", project = "matsch", sensorName = "Lsp", plot = T, pdf = F)

