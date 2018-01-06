require(ggplot2)
require(lawstat) # Levene test

ggplot(asteroides,aes(x=Palermo.Clase,y=Vinfinity..km.s.)) + geom_boxplot(outlier.colour = 'red',fill=c('green','orange','red')) + ggtitle('Velocidad según grupo de riesgo')
ggplot(asteroides,aes(x=Palermo.Clase,y=Fecha.Min)) + geom_boxplot(outlier.colour = 'red') + ggtitle('Velocidad según grupo de riesgo')

temporal <- asteroides[asteroides$Palermo.Clase=='Nivel Alto',]
ggplot(temporal,aes(x=Palermo.Clase,y=Fecha.Min)) + geom_boxplot(outlier.colour = 'red') + ggtitle('Fecha impacto más inmediato (con Alto riesgo)') +
  scale_y_continuous(minor_breaks = 5,limits=c(2000,2200))
graf <- ggplot(temporal,aes(y=Estimated.Diameter..km.,x=Fecha.Min,color=Vinfinity..km.s.)) + geom_point() + geom_jitter()
graf + scale_x_continuous(limits=c(2000,2150)) + scale_y_continuous(limits=c(0,1)) + ggtitle('Fecha próxima de impacto potencial, diámetro y velocidad')

ggplot(asteroides,aes(x=Palermo.Clase,y=H..mag.,color=BrilloH.Clase)) + geom_point() + geom_jitter() + ggtitle('Brillo según riesgo')


#NORMALIDAD DE LAS VARIABLES

shapiro.test(asteroides$Potential.Impacts..)
shapiro.test(asteroides.sin$Potential.Impacts..)
shapiro.test(asteroides.sin.OED$Potential.Impacts..)
shapiro.test(asteroides.sin.OH$Potential.Impacts..)
shapiro.test(asteroides.sin.OPI$Potential.Impacts..)
shapiro.test(asteroides.sin.OV$Potential.Impacts..)
qqnorm(asteroides$Potential.Impacts..)
qqline(asteroides$Potential.Impacts..)

shapiro.test(asteroides$Vinfinity..km.s.)
shapiro.test(asteroides.sin$Vinfinity..km.s.)
shapiro.test(asteroides.sin.OED$Vinfinity..km.s.)
shapiro.test(asteroides.sin.OH$Vinfinity..km.s.)
shapiro.test(asteroides.sin.OPI$Vinfinity..km.s.)
shapiro.test(asteroides.sin.OV$Vinfinity..km.s.)
qqnorm(asteroides$Vinfinity..km.s.)
qqline(asteroides$Vinfinity..km.s.)

shapiro.test(asteroides$H..mag.)
shapiro.test(asteroides.sin$H..mag.)
shapiro.test(asteroides.sin.OED$H..mag.)
shapiro.test(asteroides.sin.OH$H..mag.)
shapiro.test(asteroides.sin.OPI$H..mag.)
shapiro.test(asteroides.sin.OV$H..mag.)
qqnorm(asteroides$H..mag.)
qqline(asteroides$H..mag.)

shapiro.test(asteroides$Estimated.Diameter..km.)
shapiro.test(asteroides.sin$Estimated.Diameter..km.)
shapiro.test(asteroides.sin.OED$Estimated.Diameter..km.)
shapiro.test(asteroides.sin.OH$Estimated.Diameter..km.)
shapiro.test(asteroides.sin.OPI$Estimated.Diameter..km.)
shapiro.test(asteroides.sin.OV$Estimated.Diameter..km.)
qqnorm(asteroides$Estimated.Diameter..km.)
qqline(asteroides$Estimated.Diameter..km.)

shapiro.test(asteroides$Palermo.Scale..cum..)
shapiro.test(asteroides.sin$Palermo.Scale..cum..)
shapiro.test(asteroides.sin.OED$Palermo.Scale..cum..)
shapiro.test(asteroides.sin.OH$Palermo.Scale..cum..)
shapiro.test(asteroides.sin.OPI$Palermo.Scale..cum..)
shapiro.test(asteroides.sin.OV$Palermo.Scale..cum..)
qqnorm(asteroides$Palermo.Scale..cum..)
qqline(asteroides$Palermo.Scale..cum..)


#Variables de meteoros
shapiro.test(meteoros$Altitude..km.)
shapiro.test(meteoros$Velocity..km.s.)
shapiro.test(meteoros$Calculated.Total.Impact.Energy..kt.)

histogram(asteroides$Estimated.Diameter..km.,breaks=30,xlab='Diámetro estimado',main='Histograma')


#HOMOGENEIDAD DE LA VARIANZA POR GRUPOS DE RIESGO

levene.test(asteroides$Potential.Impacts..,group = asteroides$Palermo.Clase)
levene.test(asteroides$Vinfinity..km.s.,group = asteroides$Palermo.Clase)
levene.test(asteroides$H..mag.,group = asteroides$Palermo.Clase)
levene.test(asteroides$Estimated.Diameter..km.,group = asteroides$Palermo.Clase)

levene.test(meteoros.noNAs.Velocidad$Velocity..km.s.,group = meteoros.noNAs.Velocidad$Altitud.Clase)
levene.test(meteoros$Velocidad.sin,group = meteoros$Altitud.Clase)
levene.test(meteoros$Calculated.Total.Impact.Energy..kt.,group = meteoros$Altitud.Clase)

fligner.test(Potential.Impacts..~Palermo.Clase,data=asteroides)
fligner.test(Vinfinity..km.s.~Palermo.Clase,data=asteroides)
fligner.test(H..mag.~Palermo.Clase,data=asteroides)
fligner.test(Estimated.Diameter..km.~Palermo.Clase,data=asteroides)

fligner.test(Velocity..km.s.~Altitud.Clase,data=meteoros.noNAs.Velocidad)
fligner.test(Velocidad.sin~Altitud.Clase,data=meteoros)
fligner.test(Calculated.Total.Impact.Energy..kt.~Altitud.Clase,data=meteoros)

# ESTADÍSTICAS POR GRUPOS
aggregate(Potential.Impacts..~Palermo.Clase, data=asteroides,FUN=summary)
aggregate(Potential.Impacts..~Palermo.Clase, data=asteroides.sin.OPI,FUN=summary)

aggregate(Vinfinity..km.s.~Palermo.Clase, data=asteroides,FUN=summary)
aggregate(Vinfinity..km.s.~Palermo.Clase, data=asteroides.sin.OV,FUN=summary)

aggregate(H..mag.~Palermo.Clase, data=asteroides,FUN=summary)
aggregate(H..mag.~Palermo.Clase, data=asteroides.sin.OH,FUN=summary)

aggregate(Estimated.Diameter..km.~Palermo.Clase, data=asteroides,FUN=summary)
aggregate(Estimated.Diameter..km.~Palermo.Clase, data=asteroides.sin.OED,FUN=summary)

aggregate(Velocity..km.s.~Altitud.Clase,data=meteoros,FUN=summary)
aggregate(Velocidad.sin~Altitud.Clase,data=meteoros,FUN=summary)

aggregate(Calculated.Total.Impact.Energy..kt.~Altitud.Clase,data=meteoros,FUN=summary)

# COMPARACIÓN DE MEDIAS (Tukey por pares)

TukeyHSD((aov(asteroides$Potential.Impacts..~asteroides$Palermo.Clase)))
TukeyHSD((aov(asteroides.sin.OPI$Potential.Impacts..~asteroides.sin.OPI$Palermo.Clase)))
TukeyHSD((aov(asteroides$Vinfinity..km.s.~asteroides$Palermo.Clase)))
TukeyHSD((aov(asteroides.sin.OPI$Vinfinity..km.s.~asteroides.sin.OPI$Palermo.Clase)))
TukeyHSD((aov(asteroides$H..mag.~asteroides$Palermo.Clase)))
TukeyHSD((aov(asteroides.sin.OPI$H..mag.~asteroides.sin.OPI$Palermo.Clase)))
TukeyHSD((aov(asteroides$Estimated.Diameter..km.~asteroides$Palermo.Clase)))
TukeyHSD((aov(asteroides.sin.OPI$Estimated.Diameter..km.~asteroides.sin.OPI$Palermo.Clase)))

TukeyHSD((aov(meteoros$Velocity..km.s.~meteoros$Altitud.Clase)))
TukeyHSD((aov(meteoros$Velocidad.sin~meteoros$Altitud.Clase)))

TukeyHSD((aov(meteoros$Calculated.Total.Impact.Energy..kt.~meteoros$Altitud.Clase)))

# GRÁFICAS FINALES Y CONCLUSIONES

ggplot(asteroides,aes(x=Palermo.Clase,y=Vinfinity..km.s.)) + geom_boxplot(outlier.colour = 'red',fill=c('green','orange','red')) + ggtitle('Velocidad según grupo de riesgo')

mean(asteroides$Vinfinity..km.s.,na.rm=F)
mean(asteroides.sin.OV$Vinfinity..km.s.,na.rm=F)
mean(asteroides.sin.OED$Estimated.Diameter..km.,na.rm=F)
mean(asteroides$Estimated.Diameter..km.,na.rm=F)

# Relaciones 
# Riesgo con velocidad
asteroidesAlto <- asteroides[asteroides$Palermo.Clase=='Nivel Alto',]
cor(asteroidesAlto$Palermo.Scale..cum..,asteroidesAlto$Vinfinity..km.s.)
cor(asteroides$Palermo.Scale..cum..,asteroides$Vinfinity..km.s.)
cor(asteroidesAlto$Probabilidad.Log,asteroidesAlto$Vinfinity..km.s.)
cor(asteroides$Probabilidad.Log,asteroides$Vinfinity..km.s.)
# Mayor correlación log probabilidad (en lugar de la escala directa) con velocidad relativa en los asteroides de alto riesgo
ggplot(asteroidesAlto,aes(x=Vinfinity..km.s.,y=Probabilidad.Log)) + geom_smooth(method=lm) + geom_point()


# Riesgo con brillo
cor(asteroidesAlto$Palermo.Scale..cum..,asteroidesAlto$H..mag.)
cor(asteroides$Palermo.Scale..cum..,asteroides$H..mag.)
cor(asteroidesAlto$Probabilidad.Log,asteroidesAlto$H..mag.)
cor(asteroides$Probabilidad.Log,asteroides$H..mag.)

ggplot(asteroidesAlto,aes(x=H..mag.,y=Probabilidad.Log)) + geom_smooth(method=lm) + geom_point()

# Diferencia de velocidades asteroides-meteoros
wilcox.test(asteroides$Vinfinity..km.s.,meteoros.noNAs.Velocidad$Velocity..km.s.)
wilcox.test(asteroidesAlto$Vinfinity..km.s.,meteoros.noNAs.Velocidad$Velocity..km.s.)
summary(asteroides$Vinfinity..km.s.,na.rm=T)
summary(asteroidesAlto$Vinfinity..km.s.,na.rm=T)
summary(meteoros.noNAs.Velocidad$Velocity..km.s.)

# Creación de archivos Ready For Analysis (no se incluyen los derivados, se pueden crear con scripts)
write.csv(asteroides,'asteroidesRFA.csv')
write.csv(meteoros,'meteorosRFA.csv')




