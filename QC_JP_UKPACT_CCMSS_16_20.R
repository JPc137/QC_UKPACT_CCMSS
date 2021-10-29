#Instalar paquete pacman 
install.packages(pacman,beepr)
#instalar/Cargar paquetes
pacman::p_load(sf, tidyverse, dplyr,beepr)

#Leer tabla de correspondencia con IPCC para despues hacer el join
setwd("C:/UTEMRV_JP/LCC_2016_2020_UKPACT_CCMSS")
tab_corresp <- read.csv(as.character("tabl_SAMOF_IPCC_INEGI_FINAL_99.csv"))

# crear variable para guardar nombre  y leer Shape
poligonos <- "YUCATAN_974_717_EH.shp"
name <- str_remove(poligonos,".shp")
#===================================================================================================================
#  *****  CAMBIAR EL DIRECTORIO DE TRABAJO hacia la ruta del shp/gpkg a trabajar  *****
poly <- st_read(poligonos)
head(poly)
#crear join (merge) (probar TRUE Y FALSE) con la tabla de correspondencia "LC_2016 O LC2016"
tab_merge <- merge(poly, tab_corresp, by.x = "LC_2016", by.y = "clase_samof", all=FALSE) # False = solo se pegan valores existentes TRUE = pega todo
tail(tab_merge)
#===================================================================================================================
#Aplicar reglas de revisión
poly_rev <- mutate(tab_merge, c_igu_mal = case_when(Val_Cambio  == "C" & Val_T1  ==  Val_T2 ~ 1 , TRUE ~ 0), #REVISION
                   c_anio_mal = case_when(Val_Cambio  ==  'C' &  is.na(C_Anio) ~ 1 , TRUE ~ 0),   #REVISION
                   c2_anio_mal = case_when(Val_Cambio  ==  'C' &  C_Anio == 0  ~ 1 , TRUE ~ 0),  #REVISION 
                   p_mal = case_when(Val_Cambio == 'P' & Val_T1 == Val_T2 & C_Anio != 0 ~ 1 , TRUE ~ 0), #REVISIÓN
                   val_c = case_when(Val_Cambio %in% c("P", "C")  ~ 0 , TRUE ~ 1), #DOMINIO
                   dom_t1 = case_when(Val_T1 %in% c(1:6, 12, 22)  ~ 0 , TRUE ~ 1), #DOMINIO 
                   dom_t2 = case_when(Val_T2 %in% c(1:6, 12, 22)  ~ 0 , TRUE ~ 1), #DOMINIO 
                   dom_an = case_when(C_Anio %in% c(17:20, 2017, 2018, 2019,2020) ~ 0 , TRUE ~ 1), #DOMINIO son los poligonos con C_AnioDif de dominio
                   pdif_na = case_when(Val_Cambio == 'P' & Val_T1 != 0 & Val_T1 != val_ipcc & is.na(C_Anio) ~ 1 , TRUE ~ 0), #SUGERENCIA DE CORRECCIÓN  A 2016 DE "P" T1 <> T2
                   pdif_0 = case_when(Val_Cambio == 'P' & Val_T1 != 0 & Val_T1 != val_ipcc & C_Anio == 0 ~ 1 , TRUE ~ 0), #SUGERENCIA DE CORRECCIÓN  A 2016 DE "P"  T1 <> T2
                   p_igu_na = case_when(Val_Cambio == 'P' & Val_T1 == Val_T2 & is.na(C_Anio) ~ 1 , TRUE ~ 0), #SUGERENCIA DE CORRECCIÓN  A 2016 DE "P" T1 = T2
                   p_igu_0 = case_when(Val_Cambio == 'P' & Val_T1 == Val_T2 &  C_Anio == 0 ~ 1 , TRUE ~ 0), #SUGERENCIA DE CORRECCIÓN  A 2016 DE "P" T1 = T2
                   cdif_na = case_when(Val_Cambio == 'C' & Val_T1 != val_ipcc & is.na(C_Anio)  ~ 1 , TRUE ~ 0), #SUGERENCIA DE CORRECCIÓN  A 2016 DE "C" T1 <> ipcc Anio = NA
                   cdif_0 = case_when(Val_Cambio == 'C' & Val_T1 != val_ipcc & C_Anio == 0 ~ 1 , TRUE ~ 0)) #SUGERENCIA DE CORRECCIÓN  A 2016 DE "C" T1 <> ipcc Anio = 0
#f_c_an = case_when(Val_Cambio == 'C' & is.na(C_Anio) ~ 1 , TRUE ~ 0), #REVISION
#========print resultados============
#imprimir valores de columna
table(poly_rev$Val_Cambio)
table(poly_rev$Val_T1)
table(poly_rev$Val_T2)
table(poly_rev$C_Anio)
#Aplicar Revisión automática - todo debe ser 0
table(poly_rev$c_igu_mal)
table(poly_rev$c_anio_mal)
table(poly_rev$c2_anio_mal)
table(poly_rev$p_mal)
table(poly_rev$f_c_an)
#imprimir resultados de Dominio 
table(poly_rev$val_c)
table(poly_rev$dom_t1)
table(poly_rev$dom_t2)
table(poly_rev$dom_an)
#imprimir  sugerencias a correcciones para LC 2016 #identificar cambios != lc_16 
table(poly_rev$pdif_na)
table(poly_rev$pdif_0)
table(poly_rev$p_igu_na)
table(poly_rev$p_igu_0)
table(poly_rev$cdif_na)
table(poly_rev$cdif_0)



#================crear reporte en excel y shape con el merge de tabla ALL===============================
#crear directorio y guardar los siguiente shp completo y csv
#anterior_st_write(poly_rev, paste0(name, "_merge_All.gpkg")) #"_merge_All.shp"
#anterior_write_csv(poly_rev, paste0(name,"_merge_All.csv"))

#=======Filtros para obtener lo que se sustituirá en LC2016 a nivel de IPCC ("P" y "C"<>"val_ipcc")
#=======Crear las variables que contienen los polígonos que se van a sustituir por "P" y "C"=============

pcorr_16 <- filter(poly_rev, pdif_0 == 1 | pdif_na == 1)
ccorr_16 <- filter(poly_rev, cdif_na == 1 | cdif_0 == 1)

if(dim(pcorr_16)[1] > 0){
  dir.create(paste0(name,"_sust_p16"))
  ruta_dirp <- paste0(paste0(name,"_sust_p16"))
  st_write(pcorr_16, paste0(ruta_dirp,"/",name,"_sust_p16.gpkg"), delete_layer = TRUE) #.shp
}

if(dim(ccorr_16)[1] >0){
  dir.create(paste0(name,"_sust_c16"))
  ruta_dirc <- paste0(paste0(name,"_sust_c16"))
  st_write(ccorr_16, paste0(ruta_dirc,"/",name,"_sust_c16.gpkg"), delete_layer = TRUE)
}

#anterior
#pcorr_16 <- filter(poly_rev, pdif_0 == 1 | pdif_na == 1)
#ccorr_16 <- filter(poly_rev, cdif_na == 1 | cdif_0 == 1)
#comprobar dimensiones EXISTENTES
#dim(ccorr_16)
#dim(pcorr_16)[1]

#crear directorio contenedor de polígonos para actualizar el mapa de LC2016 en caso de que existan (Val_T1 <> val_ipcc)
#anterior_ if(dim(ccorr_16)[1] > 0){
  #dir.create(paste0(name,"_sust_c16"))
 #st_write()
#}

#Anterior_ if(dim(pcorr_16)[1] > 0){
 # dir.create(paste0(name,"_sust_p16"))
#}
#===========================================  Generar shp  para sustituir en LC 2016 ======

#almacenar los shp creados para sustituir en su respectiva carpeta
#ruta <- getwd()
#concatenar ruta &  nueva carpeta creada   ////que se ccrean si y solo si  existe
#ruta_dirc <- as.character(paste0(ruta,"/",paste0(name,"_sust_c16")))
#ruta_dirp <- as.character(paste0(ruta,"/",paste0(name,"_sust_p16"))) 
#Cambiar directorio
# setwd(ruta_dirc)
# setwd(ruta_dirp)
# getwd() #comprobar cambio dentro de la nueva carpeta
#Anterior
#ruta_dirc <- paste0(paste0(name,"_sust_c16"))
#ruta_dirp <- paste0(paste0(name,"_sust_p16"))
#escribir shp para sustituir dentro del directorio
#(ccorr_16, paste0(name,"_sust_c16.gpkg")) #.shp
#....st_write(pcorr_16, paste0(ruta_dirp,"/",name,"_sust_c16.gpkg")) #.shp
#....st_write(pcorr_16, paste0(name,"_sust_p16.gpkg"))

#guardar los shp o gpkg completos y csv
st_write(poly_rev, paste0(name, "_polyrev_All.gpkg")) #o si se desea en shp poner -->> "_merge_All.shp" <<--
#(<- st_transform(JV_E2_212, 4326))
#Reporte en CSV
write_csv(poly_rev, paste0(name,"_merge_All2.csv"))   #está fallando el csv, tal vez porque viene de un gpkg
#Fin... 
beep(2)
#27oct
#=======general y transformar CRS =====
#st_crs(JV_E2_212)
#poly transformado
  #29oct 6pm
