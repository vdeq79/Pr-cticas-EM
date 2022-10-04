# Instalación del paquete desde un repositorio en caso de no estar instalado
install.packages("archdata")
# Carga del paquete "archdata" si está instalado
library(archdata)

# Carga y asignación a una variable del conjunto de datos de interés "RBGlass1
data("RBGlass1")
datos<-RBGlass1

# La variable "datos" es un data.frame
# Eliminamos la primera columna del data.frame porque la ubicación del resto 
# de vidrio no aporta nada al ACP (un índice negativo en cualquier objeto de
# R indica que esa dimensión es eliminada)
datos_pca2<-datos[,-1]
# Guardo los datos originales porque la variable datos_pca va a cambiar a lo 
# largo del an?lisis
datos_originales<-datos[,-1]
# La función "head" muestra unos cuantos datos del data.frame aunque trabajamos
# con las 105 muestras de vidrio recogida
head(datos_pca2)

boxplot(datos_pca2,main="Análisis exploratorio de datos",
        xlab="Elementos químicos",
        ylab="% de concentración",
        col=c(1:11))

outlier2<-function(data,na.rm=T){
  H<-1.5*IQR(data)
  
  data[data<quantile(data,0.25,na.rm = T)-H]<-NA
  
  data[data>quantile(data,0.75, na.rm = T)+H]<-NA
  
  data[is.na(data)]<-mean(data, na.rm = T)
  
  H<-1.5*IQR(data)
  
  if (TRUE %in% (data<quantile(data,0.25,na.rm = T)-H) | TRUE %in% (data>quantile(data,0.75,na.rm = T)+H))
    outlier2(data)
  else
    return(data)
}

# A continuación aplicamos esta función a cada una a de las variables
datos_pca2$Mg<-outlier2(datos_pca2$Mg)
datos_pca2$Ca<-outlier2(datos_pca2$Ca)
datos_pca2$K<-outlier2(datos_pca2$K)
datos_pca2$P<-outlier2(datos_pca2$P)
datos_pca2$Mn<-outlier2(datos_pca2$Mn)

# Comparamos los datos originales y los arreglados 
# Esta función divide la salida gráfica en dos columnas
par(mfrow=c(1,2))
# Boxplot de los datos originales
boxplot(datos_originales,main="Datos originales",
        xlab="Elementos qu?micos",
        ylab="% de concentraci?n",
        col=c(1:11))
# Boxplot de los datos corregidos.
boxplot(datos_pca2,main="Datos sin outliers",
        xlab="Elementos qu?micos",
        ylab="% de concentraci?n",
        col=c(1:11))
