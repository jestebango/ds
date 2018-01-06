require(ggplot2)

asteroides <- read.csv('https://raw.githubusercontent.com/jestebango/ds/master/datos/cneos_sentry_summary_data.csv')
str(asteroides)
colnames(asteroides)
summary(asteroides)
pairs(asteroides[,3:10])
boxplot(asteroides[,c(2,3)],main='Distribución intervalos y # impactos') # Intervalo y número de impactos en el intervalo
boxplot(asteroides[,4], main='Distribución de probabilidad de impactos')
ggplot(asteroides,aes(x='',y=Impact.Probability..cumulative.)) + geom_boxplot() + ggtitle('Distribución de probabilidades de impacto (acumuladas)') +
   scale_y_continuous(limits=c(0,1E-6))
ggplot(asteroides,aes(x='',y=Estimated.Diameter..km.)) + geom_boxplot() + ggtitle('Distribución de diámetros (km)') + 
   scale_y_continuous(limits=c(0,1E-2))
ggplot(asteroides,aes(x='',y=Vinfinity..km.s.)) + geom_boxplot() + ggtitle('Distribución de velocidades relativas (km/s)')
ggplot(asteroides,aes(x='',y=Palermo.Scale..max..)) + geom_boxplot() + ggtitle('Distribución valores en escala Palermo (máx)')
# No se han publicado datos en la escala de Turín


meteoros <- read.csv('https://raw.githubusercontent.com/jestebango/ds/master/datos/cneos_fireball_data.csv')
str(meteoros)
colnames(meteoros)
summary(meteoros)
# Aproximación a la distribución de longitudes y latitudes antes de procesar los datos (más a la derecha y arriba mayores latitudes y longitudes)
# Latitudes en la parte inferior del gráfico, eventos próximos al ecuador
ggplot(meteoros,aes(x='',y=Altitude..km.)) + geom_boxplot() + ggtitle('Distribución de altitudes (km)')
ggplot(meteoros,aes(x='',y=Velocity..km.s.)) + geom_boxplot() + ggtitle('Distribución de velocidades (km/s)')
ggplot(meteoros,aes(x='',y=Total.Radiated.Energy..J.)) + geom_boxplot() + ggtitle('Distribución de energía radiada (J)') + 
   scale_y_continuous(limits=c(1E8,1E11))
ggplot(meteoros,aes(x='',y=Calculated.Total.Impact.Energy..kt.)) + geom_boxplot() + ggtitle('Distribución de energía calculada para superficie (kt)') + 
 scale_y_continuous(limits=c(0,1))





