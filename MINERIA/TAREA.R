# 1: INSTALACION DE LA LIBRERIA E IMPORTACION DEL CSV CON LAS COORDENADAS
### Se instala la siguiente librería
install.packages('ggplot2')
library(ggplot2)

### Importacion de los datos 
datos <- read.csv("datos.csv", header = TRUE, sep = ";")
View(datos);

# 2: PRESENTAR UNA GRÁFICA DE LOS PUNTOS EN EL PLANO X-Y (GRÁFICO DE DISPERSIÓN)
###Graficamos en diagrama de dispersión los datos
plot(datos);


# 3: PRESENTAR AL MENOS 2 DIFERENTES AGRUPAMIENTOS JERÁRQUICOS DEL DATASET Y COMENTAR SOBRE ALGUNA COMPARACIÓN O DIFERENCIA.
###Se calcula la distancia de los datos
distancia <- dist(datos)

### Se agrupan y se aplica el metodo de clousterd, y mostramos el dendogramas
dcompleta <- hclust(distancia, method = "complete")
plot(dcompleta)

### Se le aplica color al borde para el estilo del dendograma
abline(h=2 )
rect.hclust(dcompleta, k=4, border = "red")

d2 <- hclust(distancia, method = "ward.D2")
plot(d2)
abline(h=2, col = "brown")
dl2 <- cutree(d2, h=2)
table(dl2)

# 4: SELECCIONAR UN NÚMERO DE CLUSTERS, Y EJECUTAR EL AGRUPAMIENTO POR K-MEANS.

clust_datos <- cutree(dcompleta, h = 3)
table(clust_datos)
km_datos <- kmeans(scale(datos), centers = 3,
                   nstart = 20, iter.max = 50)
km_datos$cluster

# 5: PRESENTAR LOS CENTROIDES DE CADA UNO DE LOS CLUSTERS.
plot(datos, col = km_datos$cluster)
points(km_datos$centers, cex= 2, col= 11,pch = 19)

# 6: CALCULAR LA MEDIA PARA LAS CATEGORÍAS 

install.packages("magrittr") 
install.packages("dplyr")    
library(magrittr) 
library(dplyr)    

segment_customers <- cbind(datos, cluster = clust_datos)
View(segment_customers)

segment_customers %>%
  group_by(cluster) %>%
  summarise_all(mean)
