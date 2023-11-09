#Introducción a los modelos estadísticos
#Maestría en Ciencia de Datos
#08nov2023

#¿Cómo se instalan librerías?
#Sin proyecto: install.packages()
install.packages(c('dplyr', 'ggplot2'))
#Con proyecto: renv::install() ó renv::snapshot()-> Opción 2
renv::install('dplyr')

#Librerías
library(dplyr)
library(ggplot2)

#Procesamiento
psp <- read.csv('./data/PERSONALSP_2023.csv', encoding = 'UTF-8', sep = ',')
psp <- read.csv('data/PERSONALSP_2023.csv', encoding = 'UTF-8', sep = ',')

#Exploración de dataframe
names(psp)
str(psp)
dim(psp)
summary(psp)
table(psp$SECTOR)

#Managing dataframes
#select
subpsp <- select(psp, PERIODO, MES, SECTOR, PLIEGO, COSTO_TOTAL_ANUAL)

#filter
agricultura <- filter(psp, SECTOR == 'AGRICULTURA')

#arrange()
psp_ord_asc <- arrange(psp, COSTO_TOTAL_ANUAL)
head(select(psp_ord_asc, PERIODO, SECTOR, PLIEGO, COSTO_TOTAL_ANUAL), 5)

psp_ord_des <- arrange(psp, desc(COSTO_TOTAL_ANUAL))
head(select(psp_ord_des, PERIODO, SECTOR, PLIEGO, COSTO_TOTAL_ANUAL), 5)

#rename()
psp_renombrado <-  rename(psp, SECTOR_ECONOMICO = SECTOR)
psp_renombrado <-  rename(psp, SECTOR_ECONOMICO = SECTOR, PERSONAL = CANTIDAD)

#mutate()
psp_calculos <- mutate(psp, 
                       porcentaje_ocasional = round(COSTO_OCASIONAL_ANUAL / COSTO_TOTAL_ANUAL*100, 1),
                       media_costo_anual = mean(COSTO_TOTAL_ANUAL),
                       desviacion_costo_anual = COSTO_TOTAL_ANUAL - media_costo_anual, 
                       cv_costo_anual = sd(COSTO_TOTAL_ANUAL) / mean(COSTO_TOTAL_ANUAL))

#group_by()
sectores <- group_by(psp, SECTOR)
media_sectores <- summarize(sectores, mean(COSTO_TOTAL_ANUAL), min(COSTO_TOTAL_ANUAL), max(COSTO_TOTAL_ANUAL), sd(COSTO_TOTAL_ANUAL))

#pipe operator
#Resumen
psp %>% group_by(PERIODO) %>% count()
psp %>% group_by(PLIEGO) %>% count()
psp %>% group_by(DESC_TIPO_REGISTRO) %>% count()
psp %>% group_by(PERIODO, EJERCICIO, MES) %>% count()

#Cálculos
psp %>% group_by(SECTOR) %>% summarise(sum(COSTO_TOTAL_ANUAL))
t1 <- psp %>% group_by(SECTOR) %>% summarise(`Costo anual (mlls)` = round(sum(COSTO_TOTAL_ANUAL)/1000000, 3))


#Aplicaciones
#¿Cuánto costó la planilla del Congreso de la República durante 2023?
psp %>% filter(SECTOR == 'CONGRESO DE LA REPUBLICA') %>% group_by(SECTOR) %>% summarise(sum(COSTO_TOTAL_ANUAL))

#¿Cuánto cuesta en promedio mensual la planilla del sector educación?
psp %>% filter(SECTOR == 'EDUCACION') %>% 
  group_by(SECTOR,MES) %>% 
  summarise(COSTO_TOTAL_ANUAL = sum(COSTO_TOTAL_ANUAL)) %>% 
  group_by(SECTOR) %>% 
  mutate(mean(COSTO_TOTAL_ANUAL))

#¿Qué sector genera mayor costo ocasional?
psp %>%  
  group_by(SECTOR) %>% 
  summarise(COSTO_TOTAL_ANUAL = round(sum(COSTO_TOTAL_ANUAL) / 1000000)) %>%
  arrange(desc(COSTO_TOTAL_ANUAL))


##PRACTICAR  
#¿Qué pliego tiene menor costo de planilla en el mes de marzo?

#¿Cuántos pensionistas financia el Estado y a cuanto asciende el monto?

#¿Cuál es la remuneración mínima, promedio y máxima de un docente en universidades públicas?
