## Código poster procesado ##########

# Paquetes para tratar imágenes.
install.packages("jpeg")
install.packages("pixmap")

# Cargamos las librerias.
library(jpeg)
library(pixmap)

# Descargamos la imagen de la libreria.
x <- readJPEG("mandril.jpg")
z <- pixmapRGB(c(x[,,1],x[,,2],x[,,3]), 512,512, bbox=c(-1,-1,1,1))
plot(z)

# Guardamos los colores.
R<- x[,,1]
G<- x[,,2]
B<- x[,,3]
datos<- cbind(R=c(R),G=c(G),B=c(B))


#4.2 Aplicar k-medias y sustituimos en la imagen.
for (k in c(2,5,10))
{ km<- kmeans(datos,k)
datoscom<- km$centers[km$cluster,]
Rcom<- matrix(datoscom[,1],nrow(R),ncol(R))
Gcom<- matrix(datoscom[,2],nrow(G),ncol(G))
Bcom<- matrix(datoscom[,3],nrow(B),ncol(B))
imagen <- pixmapRGB(c(Rcom,Gcom,Bcom),nrow(R),ncol(R))
ECM<- mean((datos-datoscom)^2)
plot(imagen,main=paste("k= ",k),
     xlab=paste("ECM=",round(ECM,4)))
}
