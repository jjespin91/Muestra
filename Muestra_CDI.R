
# Librerías ---------------------------------------------------------------


rm(list=ls())
library(dplyr)
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(zoo)
library(stringr)
library(writexl)
library(haven)
#library(Hmisc)
#library(plyr)
library(scales)
library(lubridate)
library(esquisse)


# Base SRI ----------------------------------------------------------------
sri <- list.files("01 Bases/")
sri <-sri[-6]
list_sri <- list()

# files= dir(path = "01 Bases/")

for(i in seq_along(sri)){
  
  data_sri <- read_delim(paste("01 Bases/",sri[i],sep = ""),delim = "\t")
  list_sri[[i]] <- data_sri
}

ruc_sri <- bind_rows(list_sri)

sri_muestra <- ruc_sri %>%
  filter(ESTADO_CONTRIBUYENTE == "ACTIVO" & ESTADO_ESTABLECIMIENTO == "ABI")

saveRDS(sri_muestra,"03 Resultados/sri_muestra.RDS")


# Base de contacts -------------------------------------------------------

head(sri_muestra)

path <- "01 Bases/Contactos SRI/4.Réplica SRI/Contactos/"
contactos <- list.files(path)

list_contactos <- list()
list_contactos <- map(paste(path,contactos,sep = ""),read_xlsx)
contactos_sercop <- bind_rows(list_contactos)

saveRDS(contactos_sercop,"03 Resultados/contactos.RDS")


# Base para muestra -------------------------------------------------------

contactos_sercop <- readRDS("03 Resultados/contactos.RDS")

contactos_sercop <- contactos_sercop %>%
  select(c(`Numero Ruc`,`Categoria Contribuyente`,Telefono,`Correo Electronico`)) %>%
  filter(is.na(Telefono)==F) %>%
  distinct(`Numero Ruc`, .keep_all = T)
  

base_muestra <- sri_muestra %>% 
  left_join(contactos_sercop,by = c("NUMERO_RUC" = "Numero Ruc")) %>%
  distinct(NUMERO_RUC,.keep_all = T)

saveRDS(base_muestra,"03 Resultados/base_muestra.RDS")

#contactos_mm <- read_xlsx("01 Bases/Contactos SRI/Contactos_marcomuestral/Marco_muestral_encuesta_proveedores.xlsx")


#Actividad económica para CDI 

#Una vez revisado el correlacionador la actividad económica relacionada  a los CPC 
#2931000113, 295200114,293300025,293300027,293300028,293300029 es el código CIIU: C1520.01
#Fabricación de calzado, botines, polainas y artículos similares para todo uso,
#de cualquier material y mediante cualquier proceso, incluido el moldeado (aparado de calzado).

#Filtro de actividad económica en base muestra. 
base_muestra <- readRDS("03 Resultados/base_muestra.RDS")

universo_solicitud <- base_muestra %>% filter(CODIGO_CIIU == "C152001") %>%
  filter(is.na(Telefono)==F)


# Cálculo de Muestra CDI: Calzado -----------------------------------------

#Tamaño de muestra 

#Error del 10% 
library(rsample)
library(sampling)
 library(SamplingUtil)
# library(devtools)
# install_github("DFJL/SamplingUtil")


tamaño_muestral <- function(z,p,N,e){
    (z^2*N*p*(1-p))/(e^2*(N-1)+z^2*p*(1-p))
  }

#Z=1.645;p=0.5;q=1-p;N=1341;e=0.05
#Z = 90% = 1.65
#Z = 80% = 1.28
n = round(tamaño_muestral(1.28,0.5,1341,0.2),0)  


sierra <- c("AZUAY",                          
"BOLIVAR",
"CA�AR",
"CARCHI",                        
"CHIMBORAZO",
"COTOPAXI",
"IMBABURA",
"LOJA",                          
"SANTO DOMINGO DE LOS TSACHILAS",
"TUNGURAHUA",
"PICHINCHA")                     

oriente<- c("SUCUMBIOS",
"MORONA SANTIAGO",
"ORELLANA",                      
"PASTAZA")

costa <- c("EL ORO",
"ESMERALDAS",                    
"GALAPAGOS",
"GUAYAS",
"LOS RIOS",
"MANABI",  
"SANTA ELENA") 
universo_solicitud <- universo_solicitud %>%
  mutate(regiones  = case_when(DESCRIPCION_PROVINCIA %in% sierra ~ "sierra",
                               DESCRIPCION_PROVINCIA %in% costa ~  "costa",
                               DESCRIPCION_PROVINCIA %in% oriente ~ "oriente")) %>%
  mutate(regiones = ifelse(is.na(regiones)==T,"sierra",regiones)) %>%
  mutate(regiones = ifelse(regiones=="costa" | regiones == "oriente","costa y oriente",regiones))

estratos <- universo_solicitud %>%
  group_by(regiones) %>%
  summarise(proveedores = n()) %>%
  ungroup() %>%
  mutate(participacion = prop.table(proveedores))

nsizeProp <-nstrata(n,wh=estratos[,3],method="proportional") 
nsizeProp <- as.array(nsizeProp$participacion)


universo_solicitud <- universo_solicitud %>% 
  arrange(regiones) %>%
  mutate(regiones= as.factor(regiones))

estratos.muestra <- strata(universo_solicitud, stratanames = c("regiones"), size = nsizeProp, method = "srswor")
muestra <- getdata(universo_solicitud, estratos.muestra )


write_csv(universo_solicitud,"03 Resultados/universo.csv")
write_csv(muestra,"03 Resultados/muestraA.csv") #muestra al 90% nivel de confianza y 5% de error
write_csv(muestra,"03 Resultados/muestraB.csv") #muestra al 90% nivel de confianza y 10% de error
write_csv(muestra,"03 Resultados/muestraC.csv") #muestra al 80% nivel de confianza y 20% de error
