require(ggplot2)
require(arules)
require(reshape2)
require(stringi)
require(randomForest)
require(mice) # Para estimación y asignación de valores nulos (meteoros)

#Descarte de variables para el análisis
asteroides <- asteroides[,-c(9,10,11)]
meteoros <- meteoros[,-c(6:9)]
colnames(asteroides)
colnames(meteoros)

# Comprobación de valores descartados por el sistema a modo de estadística
descartados <- read.csv('https://raw.githubusercontent.com/jestebango/ds/master/datos/cneos_sentry_removed_objects.csv',stringsAsFactors = F)
descartados$Removed..UTC. <- as.Date(descartados$Removed..UTC.)
nrow(descartados)

table(descartados$anno)
descartados$anno <- as.integer(stringi::stri_extract(descartados$Object.Designation,regex ='\\d{4}'))

# Limpieza de identificadores de asteroides
asteroides$Object.Designation.. <- stri_trim(asteroides$Object.Designation..)
asteroides$Object.Designation.. <- stri_replace(asteroides$Object.Designation..,replacement = '',fixed='(')
asteroides$Object.Designation.. <- stri_replace(asteroides$Object.Designation..,replacement = '',fixed=')')

any(asteroides$Object.Designation.. %in% descartados$Object.Designation)
# Ningún descarte se ha vuelto a considerar como PHA, salvo los renombrados ya tratados

# Examen manual de "outliers"
descartados[descartados$anno==6344,] # Corresponde a 6344 P-L (incluido en 2007)
descartados[descartados$anno==1448,] # Corresponde a 144898 (desc en 2008)
descartados[descartados$anno==1538,] # Corresponde a 153814 (eliminado en 2010)

# CORRECCIONES PREVIAS tras consultar literatura
# 6344 P-L se elimina para la exploración, puesto que se renombró y se eliminó (DUPLICADO)
asteroides[asteroides$Object.Designation..=='2007 RR9',] # No está actualmente incluido
descartados[descartados$Object.Designation=='2007 RR9',] # Pero está descartado con el nombre nuevo
descartados <- descartados[-1527,] # redescubierto en 2007 como 2007 RR9

# 144898 (también 2004 VD17: 2º asteroide con mayor probabilidad de impacto en la escala Turín, de ahí la nomenclatura especial)
# Se actualiza la fecha de descubrimiento a 2004, y se mantiene la nomenclatura especial
descartados[descartados$Object.Designation=='2004 VD17',] # redescubierto en 2007 como 2007 RR9
asteroides[asteroides$Object.Designation..=='2004 VD17',]
asteroides[asteroides$Object.Designation..=='144898',]
descartados[descartados$anno=='1448',]$anno <- 2004

# 153814 (también 2001 WN5) tiene otra entrada en los descartados, por tanto se elimina
descartados[descartados$Object.Designation=='2001 WN5',] # Ya se encuentra eliminado con la otra nomenclatura
descartados <- descartados[-1028,]

ggplot(descartados,aes(x=Removed..UTC.)) + geom_density() + scale_x_date() +ggtitle('Evolución anual de los descartes')
ggplot(descartados,aes(x=anno)) + geom_density() + scale_x_continuous(breaks=seq(1990,2020,5)) + ggtitle('Evolución anual de los descubrimientos')
max(table(descartados$anno)) # 2008 es el año con más descubrimientos de asteroides
rm(descartados)

# --------------------------------------------
# Missing values y examen de valores inusuales
# --------------------------------------------

any(is.na(asteroides$Vinfinity..km.s.))
any(complete.cases(asteroides)==F) # No hay NA intencionados en ninguna variable
summary(asteroides) # Validación visual de valores en rangos razonables (respecto NAs)

dim(meteoros)
length(which(is.na(meteoros$Altitude..km.)))
length(which(is.na(meteoros$Velocity..km.s.)))
length(which(is.na(meteoros$Calculated.Total.Impact.Energy..kt.)))
head(meteoros)
set.seed(37)
meteoros[sample(1:nrow(meteoros),3),]

summary(meteoros)
meteoros[meteoros$Calculated.Total.Impact.Energy..kt.>50,] 
# Valor sospechoso máximo de 440, pero se considera posible en un ángulo muy "horizontal" (se disiparía al atravesar mayor cantidad de atmósfera debido a la inclinación)

#Asignación de valores a los NAs, mediante pmm (ponderación de medias)
pairs(meteoros) 
#uso de quickpred con menos iteraciones por cuestiones de rendimiento
modelo <- mice(meteoros, pred=quickpred(meteoros, minpuc=0.25,exclude=c('Peak.Brightness.Date.Time..UT.','Altitude..km.','Latitude..deg..','Longitude..deg..')))
head(complete(modelo))
any(!complete.cases(complete(modelo)))
head(meteoros)

summary(complete(modelo)$Altitude..km.)
summary(meteoros$Altitude..km.)
summary(complete(modelo)$Velocity..km.s.)
summary(meteoros$Velocity..km.s.)

meteoros$Altitud.sin <- complete(modelo)$Altitude..km.
meteoros$Velocidad.sin <- complete(modelo)$Velocity..km.s.

#--------------------
# Gestión de outliers
#--------------------

# Impactos potenciales
outliers <- asteroides[asteroides$Potential.Impacts.. %in% boxplot.stats(asteroides$Potential.Impacts..)$out,]
summary(asteroides$Potential.Impacts..) 
round(quantile(asteroides$Potential.Impacts..)[1] - (IQR(asteroides$Potential.Impacts..) * 1.5))
round(quantile(asteroides$Potential.Impacts..)[3] + (IQR(asteroides$Potential.Impacts..) * 1.5))
nrow(outliers)
# Estrategia de cambio de escala (se soluciona pero valores menos interpretables)
prueba <- log(asteroides$Potential.Impacts..)
boxplot(prueba)
rm(prueba)

# Probabilidad acumulada transformada a escala logarítmica en apartado transformaciones

# Diámetro estimado
outliers <- asteroides[asteroides$Estimated.Diameter..km. %in% boxplot.stats(asteroides$Estimated.Diameter..km.)$out,]
summary(asteroides$Estimated.Diameter..km.) 
(quantile(asteroides$Estimated.Diameter..km.)[1] - (IQR(asteroides$Estimated.Diameter..km.) * 1.5))
(quantile(asteroides$Estimated.Diameter..km.)[3] + (IQR(asteroides$Estimated.Diameter..km.) * 1.5))
nrow(outliers)
summary(outliers$Estimated.Diameter..km.)

# Brillo
outliers <- asteroides[asteroides$H..mag. %in% boxplot.stats(asteroides$H..mag.)$out,]
summary(asteroides$H..mag.) 
(quantile(asteroides$H..mag.)[1] - (IQR(asteroides$H..mag.) * 1.5))
(quantile(asteroides$H..mag.)[3] + (IQR(asteroides$H..mag.) * 1.5))
nrow(outliers)
summary(outliers$H..mag.)

# Velocidad relativa
outliers <- asteroides[asteroides$Vinfinity..km.s. %in% boxplot.stats(asteroides$Vinfinity..km.s.)$out,]
summary(asteroides$Vinfinity..km.s.) 
(quantile(asteroides$Vinfinity..km.s.,na.rm=T)[1] - (IQR(asteroides$Vinfinity..km.s.,na.rm=T) * 1.5))
(quantile(asteroides$Vinfinity..km.s.,na.rm=T)[3] + (IQR(asteroides$Vinfinity..km.s.,na.rm=T) * 1.5))
nrow(outliers)
summary(outliers$Vinfinity..km.s.)

# Altitud
outliers <- meteoros[meteoros$Altitude..km. %in% boxplot.stats(meteoros$Altitude..km.)$out,]
summary(meteoros$Altitude..km.)
(quantile(meteoros$Altitude..km.,na.rm=T)[1] - (IQR(meteoros$Altitude..km.,na.rm=T) * 1.5))
(quantile(meteoros$Altitude..km.,na.rm=T)[3] + (IQR(meteoros$Altitude..km.,na.rm=T) * 1.5))
nrow(outliers)
summary(outliers$Altitude..km.)

# Energía de impacto en superficie 
outliers <- meteoros[meteoros$Calculated.Total.Impact.Energy..kt. %in% boxplot.stats(meteoros$Calculated.Total.Impact.Energy..kt.)$out,]
summary(meteoros$Calculated.Total.Impact.Energy..kt.)
(quantile(meteoros$Calculated.Total.Impact.Energy..kt.,na.rm=T)[1] - (IQR(meteoros$Calculated.Total.Impact.Energy..kt.,na.rm=T) * 1.5))
(quantile(meteoros$Calculated.Total.Impact.Energy..kt.,na.rm=T)[3] + (IQR(meteoros$Calculated.Total.Impact.Energy..kt.,na.rm=T) * 1.5))
nrow(outliers)
summary(outliers$Calculated.Total.Impact.Energy..kt.)
rm(outliers)

# ----------------------------------
#TRANSFORMACIONES Y DISCRETIZACIONES
# ----------------------------------

# Limpieza por precaución de los nombres para evitar que incluyan espacios al final
asteroides$Object.Designation.. <- stri_trim(asteroides$Object.Designation..)

# Discretización escala de Palermo acumulada (con kmeans)
asteroides$Palermo.Clase <- discretize(asteroides$Palermo.Scale..cum..,method='cluster',labels=c('Nivel Bajo','Nivel Medio','Nivel Alto'),ordered=F)
table(asteroides$Palermo.Clase)

# Separación del intervalo de años de los posibles impactos
limites <- colsplit(asteroides$Year.Range.., "-", names=c('Fecha.Min','Fecha.Max')) 
asteroides$Fecha.Min <- limites[,1]
asteroides$Fecha.Max <- limites[,2]
rm(limites)

# Discretización respecto a la fecha más próxima
asteroides$FechaProx.Clase <- discretize(asteroides$Fecha.Min,method='fixed',categories=c(-Inf,2025,2050,Inf),labels=c('Medio Plazo','Largo Plazo','Muy Largo Plazo'),ordered=F)
table(asteroides$FechaProx.Clase)

# Transformación de la probabilidad a escala logarítmica
asteroides$Probabilidad.Log <- log(asteroides$Impact.Probability..cumulative.)

# Discretización respecto la magnitud absoluta H
asteroides$BrilloH.Clase <- discretize(asteroides$H..mag.,method='frequency',labels=c('Nivel Bajo','Nivel Medio','Nivel Alto'),ordered=F)
ggplot(asteroides,aes(y=asteroides$H..mag.,x=asteroides$BrilloH.Clase)) + geom_boxplot(outlier.color = 'red') + ggtitle('Discretización según magnitud H')
table(asteroides$BrilloH.Clase)

# Discretización de alturas de meteoros

cbind(meteoros,'Altitud.Clase')
indices <- which(meteoros$Altitud.sin>9 & meteoros$Altitud.sin<=18)
meteoros[indices,9] <- 'Troposfera'

indices <- which(meteoros$Altitud.sin>18 & meteoros$Altitud.sin<=50)
meteoros[indices,9] <- 'Estratosfera'

indices <- which(meteoros$Altitud.sin>50 & meteoros$Altitud.sin<=80)
meteoros[indices,9] <- 'Mesosfera'
summary(meteoros$Altitud.Clase)

meteoros$Altitud.Clase <- factor(meteoros$Altitud.Clase)
table(meteoros$Altitud.Clase)


ggplot(meteoros,aes(x=Altitud.Clase,y=Calculated.Total.Impact.Energy..kt.)) + geom_boxplot(outlier.color = 'red') + 
scale_y_continuous(limits=c(0,1)) 

#----------------------
# Datasets sin outliers
#----------------------

# No se eliminan porque los valores son razonables
indicesPI <- which(asteroides$Potential.Impacts.. %in% boxplot.stats(asteroides$Potential.Impacts..)$out)
indicesED <- which(asteroides$Estimated.Diameter..km. %in% boxplot.stats(asteroides$Estimated.Diameter..km.)$out)
indicesH  <- which(asteroides$H..mag. %in% boxplot.stats(asteroides$H..mag.)$out)
indicesV  <- which(asteroides$Vinfinity..km.s. %in% boxplot.stats(asteroides$Vinfinity..km.s.)$out)
indicesTOT <- union(indicesPI,indicesED)
indicesTOT <- union(indicesTOT,indicesH)
indicesTOT <- union(indicesTOT,indicesV)

#Distintos datasets ante posibles análisis de variables individual
asteroides.sin.OPI <- asteroides[-indicesPI,]
asteroides.sin.OED <- asteroides[-indicesED,]
asteroides.sin.OH <- asteroides[-indicesH,]
asteroides.sin.OV <- asteroides[-indicesV,]
asteroides.sin <- asteroides[-indicesTOT,]
rm(indicesPI)
rm(indicesED)
rm(indicesH)
rm(indicesV)
rm(indicesTOT)

#-----------------
# Datasets sin NAs
#-----------------

str(meteoros)
indices1 <- which(is.na(meteoros$Altitude..km.))
meteoros.noNAs.Altitud <- meteoros[-indices1,c(1:6,9)]
indices2 <- which(is.na(meteoros$Velocity..km.s.))
meteoros.noNAs.Velocidad <- meteoros[-indices2,c(1:6,9)]
indices <- union(indices1,indices2)
length(indices)
meteoros.noNAs <- meteoros[-indices,c(1:6,9)]
rm(indices1)
rm(indices2)
rm(indices)
