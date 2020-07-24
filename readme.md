# Introduction

A series of functions for collecting hydrological and weather data from different sources.

The functions can be loaded with `source()`:
```R
source('.../stormRunoff.R')
source('.../weather.R')
```

Data from tipping bucket loggers can be read using function `readTipbucket`:
```R
# read data from runoff logger
runoff <- readTipbucket(rawdir='..../runoff_logger_data/', dateFormat='%Y-%m-%d %H:%M:%S', timeZone='Etc/GMT-1')
```					 
Parameters:
- `rawdir`: Directory containing raw data files
- `dateFormat`: Date and time format in the raw data
- `timeZone`: Time zone of time stamps in the raw data

If there are different data files covering different overlapping or non-overlapping time periods (e.g., file 1: 1-Jan-2020 to 15-Jun-2020, file 2: 1-Jun-2020 to 10-Oct-2020, file 3: 15-Nov-2020 to 31-Dec-2020, etc.), the function automatically generates a single time series, filling gaps with `NA`

# update rainfall data base
updateRainDB(rawdir='c:/kwb/SpuR/_Daten/Wetter/',
             rainDBname='rainDB.txt',
             tBeg='20200623',
             tEnd='20200623',
             tFalseRainName='tFalseRain.txt',
             login='xxxxx:xxxx',
             D2Wsid='xxxxxxx',
             skip="",
             overwriteOldDB=TRUE,
             summerTime=data.frame(year=c(2020, 2021), 
                                   start=c("29-Mrz-20 03:00:00 +0200", 
                                           "25-Okt-20 02:55:00 +0200"),
                                   end=c("28-Mrz-21 03:00:00 +0200", 
                                         "31-Okt-21 02:55:00 +0200")))

# update temperature data base
updateWeatherDB(rawdir='c:/kwb/SpuR/_Daten/Wetter/',
                dbName='tempDB.txt',
                dwdCols=c('MESS_DATUM', 'TT_10', 'RF_10', 'QN'),
                dwdStationID=430, # 430 = Tegel
                overwriteOldDB=TRUE)

# update wind data base
updateWeatherDB(rawdir='c:/kwb/SpuR/_Daten/Wetter/',
                dbName='windDB.txt',
                dwdStationID=430, # 430 = Tegel
                dwdCols=c('MESS_DATUM', 'FF_10', 'DD_10', 'QN'),
                overwriteOldDB=TRUE)

# update solar radiation data base
updateWeatherDB(rawdir='c:/kwb/SpuR/_Daten/Wetter/',
                dbName='solarDB.txt',
                dwdStationID=430, # 430 = Tegel
                dwdCols=c('MESS_DATUM', 'DS_10', 'GS_10', 'SD_10', 'LS_10', 'QN'),
                overwriteOldDB=TRUE)

# read weather data bases
rainData <- readWeatherDB(rawdir='c:/kwb/SpuR/_Daten/Wetter/',
                          dbName='rainDB.txt',
                          naStrings=c('NA', '[-11059] No Good Data For Calculation'))
tempData <- readWeatherDB(rawdir='c:/kwb/SpuR/_Daten/Wetter/',
                          dbName='tempDB.txt',
                          naStrings=c('NA'))
windData <- readWeatherDB(rawdir='c:/kwb/SpuR/_Daten/Wetter/',
                          dbName='windDB.txt',
                          naStrings=c('NA'))

# plot rainfall in selected time window
checkRain(rainData=rainData,
          gauges=c('KWB', 'BlnX', 'BlnXI'),
          col=c('black', 'blue', 'red'),
          lty=c(1, 1, 1),
          pch=c(NA, NA, NA),
          tBeg='2020-07-01 09:30',
          tEnd='2020-07-01 11:30',
          dt=0.15*3600,
          dy=1)

checkWeather(tBeg='2020-07-09 00:30',
             tEnd='2020-07-09 21:30',
             dt=3600*1,
             rainData=rainData, 
             windData=windData, 
             tempData=tempData,
             rainScale=150,
             rainGauge='KWB')

plotEvent(tBeg='2020-06-04 10:00',
          tEnd='2020-06-08 23:30',
          dt=3600*1,
          inflowQ=Qzu, outflowQ=Qab, rainData=rainData,
          rainGauge='KWB',
          rainScale=100)



