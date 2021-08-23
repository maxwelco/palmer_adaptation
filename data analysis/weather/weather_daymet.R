
library(tidyverse)

# Constants for ET0 (Cobaner et al., 2017)
# Solar constant
Gsc = 0.0820 # (MJ m-2 min-1)
# Radiation adjustment coefficient (Samani, 2004)
kRs = 0.17

weather_daymet <- function(input, dpp = 0){ 
  input %>%
    mutate(Weather = pmap(list(ID = ID,
                               lat = latitude,
                               lon = longitude,
                               sta = Start - dpp,
                               end = End),
                          
                          # Retrieving daymet data
                          function(ID,lat,lon,sta,end){
                            download_daymet(site = ID,
                                            lat = lat, lon = lon,
                                            # Extracting year
                                            start = as.numeric(substr(sta,1,4)),
                                            end = as.numeric(substr(end,1,4)),
                                            internal = T, simplify = T)})) %>% 
    
    mutate(Weather = Weather %>% 
             
             map(~mutate(.,
                         Date = as_date(as.numeric(yday)-1, # Day of the year
                                        origin = paste0(year,'-01-01')),
                         Year = year(Date),
                         Month = month(Date),
                         Day = mday(Date)))  %>% 
             
             map(~dplyr::select(., yday, Year, Month, Day, Date,
                                measurement,value)) %>% 
             map(~spread(., 'measurement','value'))  %>% 
             map(~rename_all(., ~c("DOY", # Date as Day of the year
                                   "Year", # Year
                                   "Month", # Month 
                                   "Day", # Day of the month
                                   "Date", # Date as normal format
                                   "DL", # Day length (sec)
                                   "PP", # Precipitation (mm)
                                   "Rad", # Radiation (W/m2)
                                   "SWE", # Snow water (kg/m2)
                                   "Tmax", # Max. temp. (degC)
                                   "Tmin", # Min. temp. (degC)
                                   "VPD"))))  %>% # Vap Pres Def (Pa)
    
    mutate(Weather = pmap(list(sta=Start-dpp,
                               end = End,data=Weather), # Requested period
                          #~filter(..3, Date>=..1 & Date<= ..2))) %>% unnest() %>%
                          function(sta, end, data){
                            filter(data, Date >= sta & Date <= end) 
                          } )) %>% unnest(cols = c(Weather)) %>% 
    
    # Converting units or adding variables
    mutate(Rad = Rad*0.000001*DL, # Radiation (W/m2 to MJ/m2)
           Tmean = (Tmax+Tmin)/2, # Mean temperature (degC),
           VPD = VPD / 1000, # VPD (Pa to kPa),
           # Data for ET0
           lat_rad = latitude*0.0174533,
           dr = 1 + 0.033*cos((2*pi/365)*DOY),
           Sd = 0.409*sin((2*pi/365)*DOY - 1.39),
           ws = acos(-tan(lat_rad)*tan(Sd)),
           Ra = (24*60)/(pi) * Gsc * dr * (ws*sin(lat_rad)*sin(Sd)+
                                             cos(lat_rad)*sin(ws)),
           ET0_HS = 0.0135 * kRs * (Ra / 2.45) * (sqrt(Tmax-Tmin)) * (Tmean + 17.8),
           DL = (DL/60)/60 # Day length (hours)
    ) %>% dplyr::select(-lat_rad,-dr,-Sd,-ws,-Ra)
}