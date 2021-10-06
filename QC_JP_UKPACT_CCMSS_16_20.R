#Instalar paquete pacman 
install.packages(pacman)
#instalar/Cargar paquetes
pacman::p_load(sf, tidyverse, dplyr)

# crear variable para guardar nombre  y leer Shape
poligonos <- "Chiapas_Pol682_Cuadr291_AMG.shp"
name <- str_remove(poligonos,".shp")
#Leer tabla de correspondencia con IPCC
setwd("C:/UTEMRV_JP/LCC_2016_2020_UKPACT_CCMSS/UKPACT_CHIAP_CAMP_TAB")
tabl_corresp <- read.csv(as.character("tabl_SAMOF_IPCC_INEGI_FINAL6.csv"))

#cambiar work directory donde está el shp a trabajar
poly <- st_read(poligonos)

#HACER FILTRO PARA RECONOCER "LC2016" =0 Y PONER ALGUNA CATEGORIA

#1 = InCorrecto O POSIBLE CORRECCION DE LC 2016 , todo debe ser 0
poly_rev <- mutate(poly,c_igual_mal = case_when(Val_Cambio  == "C" & Val_T1  ==  Val_T2 ~ 1 , TRUE ~ 0),
                        an_mal = case_when(Val_Cambio  ==  'C' &  is.na(C_Anio) ~ 1 , TRUE ~ 0),
                        an_mal2 = case_when(Val_Cambio  ==  'C' &  C_Anio == 0  ~ 1 , TRUE ~ 0),
                        pr_16 = case_when(Val_Cambio == 'P' & Val_T1 != Val_T2 & is.na(C_Anio) ~ 1 , TRUE ~ 0),
                        pr_16_0 = case_when(Val_Cambio == 'P' & Val_T1 != Val_T2 &  C_Anio == 0 ~ 1 , TRUE ~ 0),
                        pr_16_2 = case_when(Val_Cambio == 'P' & Val_T1 == Val_T2 & is.na(C_Anio) ~ 1 , TRUE ~ 0),
                        pr_16_2_0 = case_when(Val_Cambio == 'P' & Val_T1 != Val_T2 &  C_Anio == 0 ~ 1 , TRUE ~ 0),
                        #cr_16 = case_when(Val_Cambio == 'C' & Val_T1 != lc_ipcc & is.na(C_Anio) ~ 1 , TRUE ~ 0),
                        #cr_16_0 = case_when(Val_Cambio == 'C' & Val_T1 != lc_ipcc & C_Anio == 0 ~ 1 , TRUE ~ 0),
                        p_mal = case_when(Val_Cambio == 'P' & Val_T1 == Val_T2 & C_Anio > 0 ~ 1 , TRUE ~ 0),
                        f_c_an = case_when(Val_Cambio == 'C' & is.na(C_Anio) ~ 1 , TRUE ~ 0),
                        val_c = case_when(Val_Cambio %in% c("P", "C")  ~ 0 , TRUE ~ 1), 
                        r_t1 = case_when(Val_T1 %in% c(1:6, 12, 22)  ~ 0 , TRUE ~ 1), #V 0.2 completar , si son  0 me tiene que decir
                        r_t2 = case_when(Val_T2 %in% c(1:6, 12, 22)  ~ 0 , TRUE ~ 1), #V 0.2 completar , si son  0 me tiene que decir
                        r_an = case_when(C_Anio %in% c(17:20,2017, 2018, 2019,2020) ~ 0 , TRUE ~ 1))
#para identificar cambios != lc_16 
        #cr_16 = case_when(Val_Cambio == 'C' & Val_T1 != lc_ipcc & is.na(C_Anio)  ~ 1 , TRUE ~ 0),
#lc_ipcc = case_when(LC_2016  < 21  ~ 1 , TRUE ~ 0),

#Aplicar Revisión automática 
table(poly_rev$c_igual_mal)
table(poly_rev$an_mal)
table(poly_rev$an_mal2)
table(poly_rev$pr_16)
table(poly_rev$pr_16_0)
table(poly_rev$pr_16_2)
table(poly_rev$pr_16_2_0)
table(poly_rev$p_mal)
table(poly_rev$f_c_an)

#imprimir valores de columna 
table(poly_rev$Val_T1)
table(poly_rev$Val_T2)
table(poly_rev$C_Anio)
table(poly_rev$Val_Cambio)

#imprimir resultados de Dominio 
table(poly_rev$val_c)
table(poly_rev$r_t1)
table(poly_rev$r_t2)
table(poly_rev$r_an)
table(poly_rev$C_Anio)

names(tabl_merge)
head(poly_rev)

#crear join (merge)  con la tabla de correspondencia
tabl_merge <- merge(poly_rev, tabl_corresp, by.x = "LC2016", by.y = "clase_samof", all=FALSE) # False = solo se pegan valores existentes TRUE = pega todo
dim(tabl_merge)

#crear reporte en excel
write_csv (tabl_merge, paste0(name,"_all.csv"))
#CREAR SHAPE
st_write(tabl_merge,paste0(name,"_tabl_merg.shp"))
#=================================================================================================

#estás son mejoras, pura vanidad pues¡
corr_16p <- filter(poly_rev, pr_16_0 == 1 | pr_16_2_0 == 1)
#corr_16c <- filter(poly_rev, Cr_16_0 == 1 | Cr_16_2_0 == 1)

if(dim(corr_16p)[1] > 0){
  dir.create(sust_p16) 
  }
  st_write(corr_16p, paste0(name,"_sust_p16.shp"))
  
  #st_write(corr_16c, paste0(name,"_sust_c16.shp"))
write_csv(correc_16, paste0(name, ".csv"))
st_write(correc_16, paste0(name,"_all_rev_.shp"))

dim(correc_16)
beep(2)
#===========================================REPORTES============================================
#(C <-filter(poly_rev,Val_Cambio =="C" &  Val_T1 != Val_T2))
#(P <-filter(JV_E2_212,Val_Cambio == "P" &  Val_T1 == Val_T2))

#table(poly_rev$status)
#plot(C$correc_16_0)

#transformar CRS
#st_crs(JV_E2_212)
#(JV_E2_lcc <- st_transform(JV_E2_212, 6372))