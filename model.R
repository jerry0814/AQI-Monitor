# model.R
source("check_packages.R")
check_packages(c("shiny","XML","leaflet","geosphere", "zipcode","darksky","dplyr","ggplot2","ggrepel","rvest")) 


data(zipcode)
load("counties.rda")
load("realtime_usa.rda")

zip_info <- subset(zipcode,zipcode == "02906")
############functions########################################################
# AQI to Concentration
Invaqi <- function(aqihigh,aqilow,conchigh,conclow,aqi){
  c <- ((aqi - aqilow)/(aqihigh - aqilow))*(conchigh - conclow) + conclow
  return(c)
}
conc_pm2.5 <- function(aqi){
  if (aqi >= 0 && aqi <= 50) conc <- Invaqi(50,0,12,0,aqi)
  else if (aqi >= 51 && aqi <= 100) conc <- Invaqi(100,51,35.4,12.1,aqi)
  else if (aqi >= 101 && aqi <= 150) conc <- Invaqi(150,101,55.4,35.5,aqi)
  else if (aqi >= 151 && aqi <= 200) conc <- Invaqi(200,151,150.4,55.5,aqi)
  else if (aqi >= 201 && aqi <= 300) conc <- Invaqi(300,201,250.4,150.5,aqi)
  else if (aqi >= 400 && aqi <= 301) conc <- Invaqi(400,301,350.4,250.5,aqi)
  else if (aqi >= 500 && aqi <= 401) conc <- Invaqi(500,401,500.4,350.5,aqi)
  return(round(conc))
}


## concentration to AQI
aqi_conc <- function(aqihigh,aqilow,conchigh,conclow,conc){
  a <- ((conc - conclow)/(conchigh - conclow))*(aqihigh - aqilow) + aqilow
  return(a)
}
pm2.5_aqi <- function(c){
  if (c < 0) conc <- local$PM2.5[1]
  if (c >= 0 && c <= 12) conc <- aqi_conc(50,0,12,0,c)
  else if (c >= 12.1 && c <= 35.4) conc <- aqi_conc(100,51,35.4,12.1,c)
  else if (c >= 35.5 && c <= 55.4) conc <- aqi_conc(150,101,55.4,35.5,c)
  else if (c >= 55.5 && c <= 150.4) conc <- aqi_conc(200,151,150.4,55.5,c)
  else if (c >= 150.5 && c <= 250.4) conc <- aqi_conc(300,201,250.4,150.5,c)
  else if (c >= 250.5 && c <= 350.4) conc <- aqi_conc(400,301,350.4,250.5,c)
  else if (c >= 350.5 && c <= 500.4) conc <- aqi_conc(500,401,500.4,350.5,c)
  return(round(conc))
}
###########################################################################
##match the zipcode with monitor sites


sites_lonlat <- as.matrix(realtime_usa[,2:3])
zip_lonlat <- matrix(rep(c(zip_info$longitude,zip_info$latitude),dim(sites_lonlat)[1]),ncol = 2,byrow = TRUE)
dist <- distGeo(sites_lonlat,zip_lonlat)
dist <- cbind(realtime_usa,dist)
dist <- dist[order(dist$dist),]
# put away non-NA
dist1 <- dist[which(!is.na(dist$PM2.5)),]

##call the matched monitor sites dataset "local" 
local <- dist1[1:3,]

#deal with the same distance situation
if (length(unique(local$dist))<3){
  local$dist[1] <- local$dist[1] + rnorm(1, mean = 0, sd = 0.01)
  local$dist[2] <- local$dist[2] + rnorm(1, mean = 0, sd = 0.01)
  local$dist[3] <- local$dist[3] + rnorm(1, mean = 0, sd = 0.01)
  
}


# azimuth calculation
rad_earth <- 6371.3*1000
# Projection on cartesian coordinate
cartesian <- function(lat, long){
  z1 <- rad_earth*sin(lat*pi/180)
  x1 <- rad_earth*cos(lat*pi/180)*cos(long*pi/180)
  y1 <- rad_earth*cos(lat*pi/180)*sin(long*pi/180)
  return(c(x1,y1,z1))
}
#cartesian coordinate for four point
local0 <- cartesian(zip_info$latitude, zip_info$longitude)
local1 <- cartesian(local$latitude[1], local$longtitude[1])
local2 <- cartesian(local$latitude[2], local$longtitude[2])
local3 <- cartesian(local$latitude[3], local$longtitude[3])

# calculate azimuth for three sites compared to zipcode location 
# central angel of an arc
ab <- acos((local0[1]*local1[1]+local0[2]*local1[2]+local0[3]*local1[3])/rad_earth^2)
ac <- (90 - zip_info$latitude)*pi/180
bc <- (90 - local$latitude[1])*pi/180

p <- (ab+ac+bc)/2
azimuth1 <- 2*acos(sqrt((sin(p)*sin(p-bc))/(sin(ac)*sin(ab))))

ab <- acos((local0[1]*local2[1]+local0[2]*local2[2]+local0[3]*local2[3])/rad_earth^2)
ac <- (90 - zip_info$latitude)*pi/180
bc <- (90 - local$latitude[2])*pi/180

p <- (ab+ac+bc)/2
azimuth2 <- 2*acos(sqrt((sin(p)*sin(p-bc))/(sin(ac)*sin(ab))))

ab <- acos((local0[1]*local3[1]+local0[2]*local3[2]+local0[3]*local3[3])/rad_earth^2)
ac <- (90 - zip_info$latitude)*pi/180
bc <- (90 - local$latitude[3])*pi/180

p <- (ab+ac+bc)/2
azimuth3 <- 2*acos(sqrt((sin(p)*sin(p-bc))/(sin(ac)*sin(ab))))
local$azimuth <- round(c(azimuth1, azimuth2, azimuth3), digits = 3)
# AQI prediction based on zipcode inputed 

#weather data
Sys.setenv(DARKSKY_API_KEY = "fadab9d0a98eb611cf27b65452f46985")
#key = 03513ab72cb66a7c2854e3628e395953
weather <- get_current_forecast(zip_info$latitude, zip_info$longitude, units = "us", language = "en",
                                exclude = "minutely,alerts,flags,currently")
weather_hourly <- data.frame(weather[1])[1:12,]
weather_hourly <- weather_hourly[,c("hourly.temperature","hourly.humidity","hourly.windSpeed","hourly.windBearing",
                                    "hourly.cloudCover","hourly.pressure")]

weather_daily <- data.frame(weather[2])
weather_daily <- weather_daily[,c("daily.time","daily.temperatureMin","daily.temperatureMax",
                                  "daily.humidity","daily.windSpeed","daily.windBearing","daily.cloudCover","daily.pressure")]

# target zipcode's AQI
# normalized inverse distance weights for the three closest monitoring site

a <- local$dist[1]
b <- local$dist[2]
c <- local$dist[3]

w1 <- 1/a^2
w2 <- 1/b^2
w3 <- 1/c^2
s <- w1+w2+w3
w1<- w1/s
w2 <- w2/s
w3 <- w3/s
# wind speed based on hourly
I <- mean(weather_hourly$hourly.windSpeed)


local$conc_pm2.5 <- apply(local["PM2.5"],1,conc_pm2.5)

# criteria for azimuth
wind_angle <- mean(weather_hourly$hourly.windBearing)
range_angle1 <- local$azimuth + 90
range_angle2 <- local$azimuth + 270

# define a monitoring site as nagative or positive
if (wind_angle<=range_angle1[1]|wind_angle >= range_angle2[1]){
  a1 <- -0.01*cos(local$azimuth[1]-wind_angle+360)
} else{a1 <- 0.01*cos(local$azimuth[1]-wind_angle+360)}
if (wind_angle<=range_angle1[2]|wind_angle >= range_angle2[2]){
  a2 <- -0.01*cos(local$azimuth[2]-wind_angle+360)
} else{a2 <- 0.01*cos(local$azimuth[2]-wind_angle+360)}
if (wind_angle<=range_angle1[3]|wind_angle >= range_angle2[3]){
  a3 <- -0.01*cos(local$azimuth[3]-wind_angle+360)
} else{a3 <- 0.01*cos(local$azimuth[3]-wind_angle+360)}

# Apply the equation from reference to concentration of PM2.5 
conc_aqi <- ceiling(local$conc_pm2.5[1]*(w1 + a1*I*w1/(w1+w2)) + 
                      local$conc_pm2.5[2]*(w2 + a2*I*w2/(w1+w2)) + 
                      local$conc_pm2.5[3]*(w3 + a3*I))


  # predicted AQI on "click" moment
  aqi <- pm2.5_aqi(conc_aqi)
  
  
  currentlocal <- colMeans(local[,c(4,6,7)],na.rm = TRUE)
  
  for (i in 1:3) {
    if(is.nan(currentlocal[i])) {currentlocal[i] <- NA}
  }
  currentlocal[1] <- aqi
  evaluation <- function(x){
    if (is.na(x)) {return(NA)} 
    else {
      if (x <= 50) {return("Good")}
      else {if (x <= 100) {return("Moderate")}
        else {if (x <= 150) {return ("Unhealthy for Sensitive Group")}
          else {if (x <= 200) {return("Unhealthy")}
            else {return("Very Unhealty")}}}}
    }
  }
  currentlocal <- round(currentlocal)
  eva <- mapply(evaluation,currentlocal)
  caqitable <- data.frame(c("PM2.5","SO2","O3"),currentlocal,eva)
  names(caqitable) <- c("Pollution", "AQI","Evaluation")
  
  
  
  
  # day-forecasting model
  
  ## calculate the elevation of zipcode-input location
  # x-longitude, y-latitude
  elevation <- function(x,y){
    url <- paste0("http://ned.usgs.gov/epqs/pqs.php?x=", x, "&y=", y, "&units=Meters&output=xml")
    html_text(read_html(url))
    m <- regexpr("\\d+\\.\\d+", html_text(read_html(url)))
    elevation <- as.numeric(regmatches(html_text(read_html(url)),m))
    return(elevation)
  }
  
  elevation0 <- elevation(zip_info$longitude, zip_info$latitude)
  
  ##PM2.5 transportation
  # PM2.5 reside 3-5 days in the atmosphere
  # average wind speed 5m/s
  # On the average, PM2.5 particles are transported 1000 or more km 
  # set up residence time in our model
  res_time <- function(x){
    y <- x/1000
    if (y < 1.5) res_time <- round(runif(1,3,5)) # 3-5 days
    else if (y > 1.5 && y <= 10){
      res_time <- round(sqrt(45*y)) # based on relationship between height and residence time
    }
    return(res_time)
  }
  
  # range of transporation = wind speed * residence time 
  spatial <- weather_daily$daily.windSpeed*res_time(elevation0)*24*60*60/1000
  
  
  ##predict the AQI for input zipcode
  # pollution source move based on wind
  # calculation of local elevation
  elevation1 <- elevation(local$longtitude[1], local$latitude[1])
  elevation2 <- elevation(local$longtitude[2], local$latitude[2])
  elevation3 <- elevation(local$longtitude[3], local$latitude[3])
  local$elevation <- c(elevation1, elevation2, elevation3)
  
  #modified wind speed with Pythagorean theorem
  conc_aqi_new <- rep(0,8)
  
  for (i in 1:res_time(elevation0)){
    I <- weather_daily$daily.windSpeed[i]
    w1 <- 1/(sqrt(local$dist[1]^2 + (elevation1-elevation0)^2) + a1*10*spatial[i])^2
    w2 <- 1/(sqrt(local$dist[2]^2 + (elevation2-elevation0)^2) + a2*10*spatial[i])^2
    w3 <- 1/(sqrt(local$dist[3]^2 + (elevation3-elevation0)^2) + a3*10*spatial[i])^2
    s <- w1+w2+w3
    w1<- w1/s
    w2 <- w2/s
    w3 <- w3/s
    conc_aqi_new[i] <- ceiling(local$conc_pm2.5[1]*(w1 + a1*I*w1/(w1+w2)) + 
                            local$conc_pm2.5[2]*(w2 + a2*I*w2/(w1+w2)) + 
                            local$conc_pm2.5[3]*(w3 + a3*I))
  }
  
  aqi_new <- mapply(pm2.5_aqi,conc_aqi_new)
  
  
  aqi_table <- c(aqi, aqi_new[aqi_new!=0])
  eva <- mapply(evaluation, aqi_table)
  aqitable <- data.frame(aqi_table,eva)
  names(aqitable) <- c("AQI","Evaluation")
  x <- paste("The next", 1:length(aqi_new[aqi_new!=0]), "day")
  rownames(aqitable) <- c("Today", x)
  
  
  
  
  # hour-forecasting model
  # simple linear model based on wind speed
  ## relationship between wind speed and concentration
  ## wind-bearing define positive or negative effect
  if (a1 + a2 + a3 > 0) x1 <- 1*cos(mean(local$azimuth, na.rm = TRUE) - wind_angle + 360)
  if (a1 + a2 + a3 < 0) x1 <- -1*cos(mean(local$azimuth, na.rm = TRUE) - wind_angle + 360)
  aqitable2 <- rep(0,12)
  aqitable2[1] <-  -x1*0.01*(weather_hourly$hourly.windSpeed[1]) + conc_pm2.5(aqi)
  for (i in 2:12){

  aqitable2[i] <- -x1*0.01*(weather_hourly$hourly.windSpeed[i]) + aqitable2[i-1]
  }
  
  aqi_hour <- mapply(pm2.5_aqi, aqitable2)
  
 
  