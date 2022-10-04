################################################################################
# Ejemplo de ACP con datos de de concentraciones de 11 elementos químicos en   #
# restos de vidrio.                                                            #
# En este estudio se analiza la viabilidad de un ACP y en caso afirmativo se   #
# interpretan sus resultados.                                                  #
################################################################################

# -------------------------------------- #
# PASO 0: carga de unos datos de interés #
# -------------------------------------- #
# El paquete "archdata" tiene datos de concentraciones de 11 elementos químicos
# de 105 muestras de restos de vidrio en Manchester y en Leiceter

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
datos_pca<-datos[,-1]
# Guardo los datos originales porque la variable datos_pca va a cambiar a lo 
# largo del an?lisis
datos_originales<-datos_pca
# La función "head" muestra unos cuantos datos del data.frame aunque trabajamos
# con las 105 muestras de vidrio recogida
head(datos_pca)

# PASO 1: ?tiene sentido un ACP?
# --------------------------------------
# Para responder a esta pregunta se comprueba si existe correlación entre las
# variables con la función "cor" del paquete base, que proporciona la matriz
# de correlaciones R
cor(datos_pca)

# Observando la matriz de datos existe correlación importante entre algunas
# variables, como sodio (NA) y antimonio (Sb) o titanio (Ti) e hierro (Fe)
cor(datos_pca$Na,datos_pca$Sb)
cor(datos_pca$Ti,datos_pca$Fe)

# El contraste de esfericidad de Bartlett permite comprobar si las correlaciones
# son distintas de 0 de modo significativo. La hipótesis nula es que det(R)=1
# La función "cortest.bartlett" del paquete "pysch" realiza este test.
# Esta función trabaja con datos normalizados
# Instalación del paquete desde un repositorio en caso de no estar instalado
install.packages("psych")
# Carga del paquete "psych" si está instalado
library(psych)

# Se normalizan los datos
datos_normalizados<-scale(datos_pca)
# Se hace el test de esfericidad
cortest.bartlett(cor(datos_normalizados))
# Para estos datos se obtiene un test significativo de modo que se rechaza la 
# hip?tesis nula y por tanto los datos no est?n incorrelados

# ------------------------------------------ #
# Paso 2: an?lisis exploratorio de los datos #
# ------------------------------------------ #
# El objetivo es el de localizar outliers que puedan dar lugar a resultados
# erróneos ya que el ACP es muy sensible a valores extremos. Un diagrama de 
# cajas puede dar esta primera información.
boxplot(datos_pca,main="Análisis exploratorio de datos",
        xlab="Elementos químicos",
        ylab="% de concentración",
        col=c(1:11))
# Al obtener el gráfico con estos datos se observa que tanto el Magnesio (Mg),
# el Calcio (Ca) así como el Potasio (K), Fósforo (P) y el Manganeso (Mn) 
# presentan outliers.


# Los outliers deben ser tratados de forma independiente por el investigador, 
# de modo que para el ACP es necesario eliminarlos
# La función outlier definida como sigue elimina los outliers sustituyéndolos
# por los promedios del resto de valores.

# Ejemplo de construcción de una función en R
outlier<-function(data,na.rm=T){
  H<-1.5*IQR(data)
  data[data<quantile(data,0.25,na.rm = T)-H]<-NA
  data[data>quantile(data,0.75, na.rm = T)+H]<-NA
  data[is.na(data)]<-mean(data,na.rm=T)
  data
}
# A continuación aplicamos esta función a cada una a de las variables
datos_pca$Mg<-outlier(datos_pca$Mg)
datos_pca$Ca<-outlier(datos_pca$Ca)
datos_pca$K<-outlier(datos_pca$K)
datos_pca$P<-outlier(datos_pca$P)
datos_pca$Mn<-outlier(datos_pca$Mn)

# Comparamos los datos originales y los arreglados 
# Esta función divide la salida gráfica en dos columnas
par(mfrow=c(1,2))
# Boxplot de los datos originales
boxplot(datos_originales,main="Datos originales",
        xlab="Elementos qu?micos",
        ylab="% de concentraci?n",
        col=c(1:11))
# Boxplot de los datos corregidos.
boxplot(datos_pca,main="Datos sin outliers",
        xlab="Elementos qu?micos",
        ylab="% de concentraci?n",
        col=c(1:11))

# Llegado a este punto se dan las condiciones para realizar un ACP:
# los datos están correlados y no hay 'casos raros'


# ----------- #
# Paso 3: ACP #
# ----------- #

# La función "prcomp" del paquete base de R realiza este análisis
# Pasamos los parámetros "scale" y "center" a TRUE para consideras
# los datos originales normalizados
PCA<-prcomp(datos_pca, scale=T, center = T)

# El el campo "rotation" del objeto "PCA" es una matriz cuyas columnas
# son los coeficientes de las componentes principales, es decir, el
# peso de cada variable en la correspondiente componente principal
PCA$rotation


# En el campo "sdev" del objeto "PCA" y con la función summary aplicada
# al objeto, obtenemos información relevante: desviaciones típicas de 
# cada componente principal, proporción de varianza explicada y acumulada.
PCA$sdev
summary(PCA)

# A continuación hacemos un análisis gráfico de la varianza explicada
# Instalación del paquete desde un repositorio en caso de no estar instalado
install.packages("ggplot2")
# Carga del paquete "ggplot2" si está instalado
library(ggplot2)

# El siguiente gráfico muestra la proporción de varianza explicada
varianza_explicada <- PCA$sdev^2 / sum(PCA$sdev^2)
ggplot(data = data.frame(varianza_explicada, pc = 1:11),
       aes(x = pc, y = varianza_explicada, fill=varianza_explicada )) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,0.6)) + theme_bw() +
  labs(x = "Componente principal", y= " Proporci?n de varianza explicada")

# El siguiente gráfico muestra la proporción de varianza explicada
varianza_acum<-cumsum(varianza_explicada)
ggplot( data = data.frame(varianza_acum, pc = 1:11),
        aes(x = pc, y = varianza_acum ,fill=varianza_acum )) +
  geom_col(width = 0.5) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Proporci?n varianza acumulada")

# -------------------------------------------------------------- #
# Paso 4: selección del número de componentes principales óptimo #
# -------------------------------------------------------------- #
# Existen diferentes métodos:
# 1.- Método del codo (Cuadras, 2007). Ejercicio: buscar información (voluntario)
# 2.- A criterio del investigador que elige un porcentaje mínimo de varianza explicada
# por las componentes principales (no es fiable porque puede dar más de las necesarias).
# 3.- En este caso se utiliza la regla de Abdi et al. (2010). Se promedia las varianzas
# explicadas por la componentes principales y se selecciona aquellas cuya proporción de 
# varianza explicada supera la media.
# En este caso se eligen tan solo tres direcciones principales tal y como se puede ver
PCA$sdev^2
mean(PCA$sdev^2)

# Cada componente principal se obtiene de forma sencilla como combinación lineal de todas
# las variables con los coeficientes que indican las columnas de la matriz de rotación
# Ejercicio voluntario: escribir la expresión analítica de cada componente principal

# -------------------------------------------------------------- #
# Paso 5: Representación gráfica de las componentes principales  #
# -------------------------------------------------------------- #
# El paquete "factoextra" permite la representación de las componentes principales
# junto con las variables y observaciones del análisis de componentes principales.

# Instalación del paquete desde un repositorio en caso de no estar instalado
# El siguiente paquete requiere tener al menos la versión de R 4.0.x
install.packages("factoextra")
# Carga del paquete "factorextra" si está instalado
library("factoextra")

# Esto produce una comparativa entre la primera y segunda componente principal analizando 
# que variables tienen más peso para la definición de cada componente principal
fviz_pca_var(PCA,
             repel=TRUE,col.var="cos2",
             legend.title="Distancia")+theme_bw()

# Esto produce una comparativa entre la primera y tercera componente principal analizando 
# que variables tienen m?s peso para la definición de cada componente principal
fviz_pca_var(PCA,axes=c(1,3),
             repel=TRUE,col.var="cos2",
             legend.title="Distancia")+theme_bw()

# Esto produce una comparativa entre la segunda y tercera componente principal analizando 
# que variables tienen más peso para la definición de cada componente principal
fviz_pca_var(PCA,axes=c(2,3),
             repel=TRUE,col.var="cos2",
             legend.title="Distancia")+theme_bw()






# Es posible también representar las observaciones de los objetos junto con las componentes 
# principales mediante la orden "contrib" de la función "fviz_pca_ind", así como identificar
# con colores aquellas observaciones que mayor varianza explican de las componentes principales
 
# Observaciones en la primera y segunda componente principal
fviz_pca_ind(PCA,col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=TRUE,legend.title="Contrib.var")+theme_bw()

# Observaciones en la primera y tercera componente principal
fviz_pca_ind(PCA,axes=c(1,3),col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=TRUE,legend.title="Contrib.var")+theme_bw()

# Observaciones en la segunda y tercera componente principal
fviz_pca_ind(PCA,axes=c(2,3),col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=TRUE,legend.title="Contrib.var")+theme_bw()


# Representación conjunta de variables y observaciones
# que relaciona visualmente las posibles relaciones entre las
# observaciones, las contribuciones de los individuos a las varianzas de las componentes
# y el peso de las variables en cada componentes principal

# Variables y observaciones en las 1ª  y 2ª componente principal
fviz_pca(PCA,
         alpha.ind ="contrib", col.var = "cos2",col.ind="seagreen",
         gradient.cols = c("#FDF50E", "#FD960E", "#FD1E0E"),
         repel=TRUE,
         legend.title="Distancia")+theme_bw()

# Variables y observaciones en las 1ª  y 3ª componente principal
fviz_pca(PCA,axes=c(1,3),
         alpha.ind ="contrib", col.var = "cos2",col.ind="seagreen",
         gradient.cols = c("#FDF50E", "#FD960E", "#FD1E0E"),
         repel=TRUE,
         legend.title="Distancia")+theme_bw()

# Variables y observaciones en las 1ª  y 2ª componente principal
fviz_pca(PCA,axes=c(1,2),
         alpha.ind ="contrib", col.var = "cos2",col.ind="seagreen",
         gradient.cols = c("#FDF50E", "#FD960E", "#FD1E0E"),
         repel=TRUE,
         legend.title="Distancia")+theme_bw()



# Por último, ya que el objeto de este estudio era reducir la dimensión de las variables
# utilizadas, es posible obtener las coordenadas de los datos originales tipificados en el
# nuevo sistema de referencia.
# De hecho lo tenemos almacenado desde que utilizamos la función prcomp para crear la variable PCA

head(PCA$x)
