runoffRegression <- function(rawdir, dbName, sheet){
  
  library(readxl)  
  
  # Function to estimate the volume of the inflow and outflow of the test filter by using 
  # already collected run-off data.needed for events where discharge capacity of Kippz?hler 
  # was surpassed or there were other problems (e.g., incorrect tipping)
  
  setwd(rawdir)
  
  # load database
  xls  <- read_excel(dbName, 
                     sheet=sheet, 
                     col_types="text", 
                     skip=3, 
                     na=c("na", "nb","NA"))
  
  # format columns
  xls$tBegRain <- as.POSIXct(xls$tBegRain, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
  xls$tEndRain <- as.POSIXct(xls$tEndRain, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
  xls$Regenhöhe_mm <- as.numeric(xls$Regenhöhe_mm)
  xls$Anzahl_Ereignisse <- as.numeric(xls$Anzahl_Ereignisse)
  xls$tBegHydraul_zu <- as.POSIXct(xls$tBegHydraul_zu, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
  xls$tEndHydraul_zu <- as.POSIXct(xls$tEndHydraul_zu, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
  xls$tBegHydraul_ab <- as.POSIXct(xls$tBegHydraul_ab, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
  xls$tEndHydraul_ab <- as.POSIXct(xls$tEndHydraul_ab, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
  xls$Abflussvol_l_zu <- as.numeric(xls$Abflussvol_l_zu)
  xls$Abflussvol_l_ab <- as.numeric(xls$Abflussvol_l_ab)
  xls$Messwertüberschreitung <- as.logical(xls$Messwertüberschreitung)
  xls$Filter_übergelaufen <- as.logical(xls$Filter_übergelaufen)
  
  # subdivide all rain events into events where the rain volume was measured (xls2), or into events where 
  # the volume must be estimated via linear regression (xls_reg).
  xls2    <- dplyr::filter(xls,!Messwertüberschreitung & !Filter_übergelaufen)
  xls_reg <- dplyr::filter(xls, Messwertüberschreitung & !Filter_übergelaufen)
  
  # select measured Vzu and Vab columns
  Vzu <- xls2$Abflussvol_l_zu
  Vab <- xls2$Abflussvol_l_ab
  
  # build linear regression model with lm(y ~ x, data=.....)
  model1 <- lm(Vab ~ Vzu, data=xls2)
  print(summary(model1))
  
  regression_intercept <- lm(Vab ~ Vzu, data=xls2)$coefficients[1]
  regression_slope <- lm(Vab ~ Vzu, data=xls2)$coefficients[2]
  
  # plot
  library(ggplot2)
  ggplot(xls2,aes(Vzu,Vab)) + geom_point()+geom_smooth(method=lm, se=FALSE)
  
  #predict()
  Vab_reg <- regression_intercept + regression_slope * xls_reg$Abflussvol_l_zu
  Vzu_reg <- (xls_reg$Abflussvol_l_ab - regression_intercept)/regression_slope
  
  #create output table
  output_table <- data.frame(tBegRain=xls_reg$tBegRain,
                             tEndRain=xls_reg$tEndRain,
                             Regenh?he=xls_reg$Regenhöhe_mm,
                             Vzu_reg=Vzu_reg,
                             Vab_reg=Vab_reg)
  print(output_table)
}
