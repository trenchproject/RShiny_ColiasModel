Tb_butterfly=function(T_a, Tg, Tg_sh, u, H_sdir, H_sdif, z, D, delta, alpha, r_g=0.3, shade=FALSE){
  
  stopifnot(u>=0, H_sdir>=0, H_sdif>=0, z>=-90, z<=90, D>0, delta>=0, alpha>=0, r_g>=0, r_g<=1, shade %in% c(FALSE, TRUE) )  
  
  # T_a = 20
  # Tg = 25 
  # Tg_sh = 15
  # u = 1 
  # H_sdir = 900
  # H_sdif = 400
  # z = seq(0, 90, by = 10)
  # z = 90
  # D =  0.36
  # delta = 1.46
  # alpha = 0.6
  # r_g=0.3
  
  TaK= T_a+273.15 #ambient temperature in K
  TaK_sh=TaK
  Tg= Tg+273.15 #ground surface temperature in K
  Tg_sh= Tg_sh+273 #shaded ground surface temperature in K
  
  u= u *100;  #u- wind speed, convert m/s to cm/s
  H_sdir=H_sdir/10 #divide by ten to convert W/m2 to W/cm2
  H_sdif=H_sdif/10 #divide by ten to convert W/m2 to W/cm2
  
  #Total solar radiation
  H_sttl= H_sdir + H_sdif
  
  #Butterfly Parameters
  delta<- delta/10     #delta- thoracic fur thickness, cm
  
  epsilon_s=0.97; #surface emisivity, ranges from 0.95-1
  sigma= 5.67*10^-9; #Stefan-Boltzman constant, mW cm^-2 K^04 or 5.67*10^-8 W m-2 K-4
  Ep=1; #Ep- butterfly thermal emissivity
  
  k_e= 1.3; #k_e- thermal conductivity of the fur, 1.3mWcm^-1*K^-1
  r_i=0.15; #r_i- body radius #Kingsolver 1983
  k_a=0.25; #approximate thermal conductivity of air, mWcm^-1*K^-1
  
  v=15.68*10^-2  #cm^2/s, kinematic viscocity of air,  at 300K http://www.engineeringtoolbox.com/air-absolute-kinematic-viscosity-d_601.html
  
  #---------------------------------------------
  
  #Areas, cm^2
  #Calculate total surface area as area of cylinder without ends
  A_sttl= pi*D*2 #2 in length  #cm^2
  
  #For butterflies basking with wings perpendicular to radiation 
  ##A_s,dir, A_s,ref, A_s,ttl- direct, reflected, and total solar radiative heat transfer surface areas 
  A_sdir= A_sttl/2
  A_sref=A_sdir
  
  #RADIATIVE HEAT FLUx, mW
  Q_s= alpha*A_sdir*H_sdir/cos(z*pi/180)+alpha*A_sref*H_sdif+alpha*r_g*A_sref*H_sttl  
  
  #---------------------------------------------		 
  #THERMAL RADIATIVE FLUX
  #Tsky=0.0552*(TaK)^1.5; #Kelvin, black body sky temperature from Swinbank (1963), 
  Tsky= (1.22*T_a -20.4)+273.15 #K, Gates 1980 Biophysical ecology based on Swnback 1960, Kingsolver (1983) estimates using Brunt equation
  
  #Q_t= 0.5* A_sttl * Ep * sigma * (Tb^4 - Tsky^4) +0.5* A_sttl * Ep * sigma * (Tb^4 - Tg^4)
  
  #---------------------------------------------   	               
  # CONVECTIVE HEAT FLUX
  
  #Reynolds number- ratio of interval viscous forces
  R_e=u*D/v
  #Nusselt number- dimensionless conductance
  N_u=0.6*R_e^0.5
  #N_u=2.3; #Kingsolver 1983;
  
  h_c=N_u*k_a/D;
  h_T=(1/h_c+(r_i+delta)*log((r_i+delta)/r_i)/k_e)^-1;  # h_T- total convective heat tranfer coefficient
  #A_c=A_sttl; #A_c- convective heat transfer surface area
  #Q_c= h_T* A_c* (Tb-T_a);     
  #---------------------------------------------   	 
  #HEAT BUDGET              
  
  # Kingsolver 1983
  #Q_s- total radiative heat flux; Q_t- thermal radiative heat flux; Q_c- convective heat flux
  #Q_s=Q_t + Q_c;
  
  #ADJUST PARAMETERS IF SHADE
  if(shade==TRUE){
    #Calculate without basking by dividing areas by two
    A_sttl=A_sttl/2
    #RADIATIVE HEAT FLUX IN SHADE, mW
    A_sdir= A_sttl/2
    A_sref=A_sdir; 
    H_sdir_sh= 0; #No direct radiation
    H_sdif_sh= H_sdif
    H_sttl= H_sdif + H_sdif_sh #only diffuse and reflected
    Q_s= alpha*A_sdir*H_sdir_sh/cos(z*pi/180)+alpha*A_sref*H_sdif_sh+alpha*r_g*A_sref*H_sttl; 
    Tg= Tg_sh #use shaded surface temperature if shade
  }
  
  #t solved in wolfram alpha #Solve[a t^4 +b t -d, t]
  a<- A_sttl * Ep *sigma
  b<-h_T * A_sttl
  d<- h_T*A_sttl*TaK +0.5*A_sttl * Ep *sigma*Tsky^4 +0.5*A_sttl * Ep *sigma*(Tg)^4 +Q_s
  
  {Te=1/2*sqrt((2*b)/(a*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)))-(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)+(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3))-1/2*sqrt((sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)/(2^(1/3)*3^(2/3)*a)-(4*(2/3)^(1/3)*d)/(sqrt(3)*sqrt(256*a^3*d^3+27*a^2*b^4)+9*a*b^2)^(1/3)) }
  #IMPROVE SOLUTION?
  Te
  return(Te-273.15)
} 


Tb_butterfly(25, 30, 20, 1, 900, 400, z = seq(0, 90, by = 10), 0.36, 1.46, 0.6, r_g=0.3, shade=FALSE)



ncdc_stations(limit = 100,
              extent = c(min(Colias$lat)+1.5,
                         min(Colias$lon)+1.5,
                         max(Colias$lat)-1.5,
                         max(Colias$lon)-1.5
                         
                         ),
              token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq")

ghcnd_search("GHCND:USC00051959")
ncdc(datasetid = 'GHCND', stationid = "GHCND:USC00051959", token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", startdate = '2020-06-01', enddate = '2020-06-25')
sum <- ncdc_datasets(datasetid = 'GHCND', stationid = "GHCND:USR0000CTAY", token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq")
USR0000CTAY  
sum
var <- ncdc_datasets("GHCND:US1COGN0002", token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq")

tMin <- ghcnd_search(stationid = "USC00051959", token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", var = "TMIN", date_min = 2020-06-01)
tMax <- ghcnd_search(stationid = "USC00051959", token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", var = "TMAX", date_min = 2020-06-01)

tMax <- as.data.frame(tMax) %>% dplyr::select(c(tmax.tmax, tmax.date))
tMin <- as.data.frame(tMin) %>% dplyr::select(c(tmin.tmin, tmin.date))
as.data.frame(tMin)
tMax <- filter(tMax, tmax.date > Sys.Date() - 9, tmax.date < Sys.Date() - 1)
tMin <- filter(tMin, tmin.date > Sys.Date() - 9, tmin.date < Sys.Date() - 1)
tMax
MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq

"2020-06-30" > "2020-06-29"
tMax[16219,2]
tMax1 <- ncdc(datasetid='GHCND',
              stationid= "GHCND:USC00051959",
              datatypeid= "TMAX",
              startdate = 2020-06-01,
              enddate = 2020-06-25,
              limit=500,
              token="MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq")
tMax1$meta


c(min(Colias$lat),
  min(Colias$lon),
  max(Colias$lat),
  max(Colias$lon))

max(Colias$lat)
ncdc_stations(extent = c(47.5204, -122.2047, 47.6139, -122.1065), token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", limit = 100)
Sys.Date()



weather <- fix_weather(KA_weather)
weather$weather
make_hourly_temps(50.4,weather$weather)
df <- data.frame("Year" = 2020, "Month" = 1, "Day" = c(1,2), "Tmin" = c(5,6), "Tmax" = c(12, 14))
df




df_filter <- filter(df_CO, x >= min(Colias$lon) & x <= max(Colias$lon) & y >= min(Colias$lat) & y <= max(Colias$lat))
df_filter
mTemp <- colMeans(df_filter)  %>% t() %>% as.data.frame()

test <- cbind("Year" = 2020, "Month" = 6, "Day" = 22, mTemp)
colnames(test)[c(6,8)] <- c("Tmax", "Tmin")
test <- rbind(test, test)
test
test <- make_hourly_temps(47.6, test)

make_hourly_temps
class(mTemp)
r$tmax
mTemp
test[1,11:34]
test[2,11:34]
plot(0:23, test[1,11:34])
plot(0:23, test[2,11:34])
install.packages("devtools")   
library("devtools")   
devtools::install_github(build_vignettes = TRUE,repo = "trenchproject/TrenchR")


df <- Colias %>% filter(year == 2020 & absorp %in% 0.4 & gen %in% 1) %>% na.omit()
df <- df[,c("lat", "lon", "absorp", "gen", "lambda", "eggV")] %>% na.omit()
df

df_long <- df %>% gather(Param, value, c("lambda", "eggV"))
df_long
tail(df_long)


map <- get_stamenmap(bbox = c(left = -108.8125, right = -104.9375, bottom = 37.1875, top = 40.8125), zoom = , maptype = "terrain")
ggmap(map)
object.size(map)
