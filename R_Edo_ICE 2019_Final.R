### Indice Complejidad Estatal 2019    ###
### INE-DERFE-COC-DE-EVALUACION DEMOGRAFICA ###
# Autor: Miguel David Alvarez Hernández
# Ultima versión : 15/04/2019

#### Paquetes ####

library(pacman)
library(dplyr)
p_load(tidyverse,
       ggfortify,
       ggthemes,
       RColorBrewer,
       colorRamps,
       corrplot,
       openxlsx,
       readxl,
       Hmisc,
       cluster,
       factoextra,
       NbClust,
       magrittr,
       FactoMineR,
       PerformanceAnalytics)


#### Setup #################################################

options(scipen=999) # Prevenir notación científica


#### Datos por distrito ######################################

Dto_All_variables <- read_excel("Dto_Variables-Resultados_IC2019.xlsx")
Dto_All_variables <- as.data.frame(Dto_All_variables)
#nombres de columnas
colnames(Dto_All_variables)


#### Numero distritos por estado ############################

#calculamos el numero de distritos por entidad
#View(table(datos_dto["Edo"]))
Dto_por_estado <- as_data_frame(table(Dto_All_variables["Edo"]))
#write.csv(dto_estado, file = "Numero_distritos_2019.csv",row.names=FALSE)

#cambiamos el nombre de la columna
Dto_por_estado <- Dto_por_estado %>% 
  rename(
    "i#CVE_Estado" = Var1,
    "C#Distritos" = n)


#### Agregacion de las variables por estado #################


#se agrupan  por estado sólo las variables a considerar en el IC estatal
#y se cambian los nombres de las columnas 
Datos_estatales <- Dto_All_variables %>% 
  group_by(Edo) %>%
  summarise("C#Tiempo_promedio" = mean(Tiempo_Dto_JLoc),
            "C#Distancia_promedio" = mean(Distancia_Dto_JLoc),
            "C#Densidad" = (sum(Padron_19)/sum(Superficie)),
            "C#Padron" = sum(Padron_19),
            "C#Padron_Loc_Rurales" = sum(Padron_Rurales),
            "C#Secciones_rurales" = sum(Secc_Rurales),
            "C#IC_distrital" = mean(PC1_originales))

#se agregan dos columnas con las claves y acronimos de los estados, 
#y se elimina la primera columna
Datos_estatales$"mS#Estado" <- c("AGS","BC","BCS","CAMP","COAH","COL","CHIS","CHIH","CDMX","DGO","GTO","GRO","HGO","JAL","MEX","MICH","MOR","NAY","NL","OAX","PUE","QRO","Q_ROO","SLP","SIN","SON","TAB","TAMPS","TLAX","VER","YUC","ZAC")
Datos_estatales$"i#CVE_Estado" <- Datos_estatales$Edo
Datos_estatales <- Datos_estatales[,-1]
View(Datos_estatales)

#unimos los dataframes para incorporar el dato de numero de distritos
Datos_estatales2 <- merge(Datos_estatales,Dto_por_estado,by=c("i#CVE_Estado"))

#cambiamos los rownames por la columna mS#Estado
Datos_estatales2 <- Datos_estatales2 %>% 
  remove_rownames %>% 
  column_to_rownames(var="mS#Estado")

#añadimos nuevamente la columna mS#Estado
Datos_estatales2$"mS#Estado" <- Datos_estatales$"mS#Estado"

#ordenamos las columnas
Datos_estatales2 <- Datos_estatales2[c(1,10,9,2:8)]
View(Datos_estatales2)
colnames(Datos_estatales2)

#se guardan los datos en un csv (este archivo se usa en orange)
#write.csv(Datos_estatales2, file = "Edo_Variables_ICE2019.csv",row.names=FALSE)



#### Analisis de correlacion ###############################

#se crea un dataframe sin columnas con categorías o texto, 
datos_estatales3 <- Datos_estatales2[, -c(1,2)]

#Resumen estadistico para todas las variables; para la variable continua obtiene medidas descriptivas de los datos
summary(datos_estatales3)

#matriz de correlacion 
#ver https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
res <- cor(datos_estatales3)
res <- round(res, 2) #se redondea a dos cifras
#View(res)

#guardar la matriz en un txt
#write.table(res,file="matriz_correlacion.txt") # keeps the rownames

#p-values
res2 <- rcorr(as.matrix(datos_estatales3))
#View(res2)

# se crea una funcion para mostrar simultaneamente la matrix de correlación y los p-valores
# flattenCorrMatrix
# ver http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#se imprime el dataframe con la comparacion de las correlaciones
res3 <- flattenCorrMatrix(res2$r, res2$P)
View(res3)


#gráfica de correlaciones 
res <- cor(datos_estatales3)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Insignificant correlations are leaved blank
res2 <- rcorr(as.matrix(datos_estatales3))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

#display a chart of a correlation matrix
# ver http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
chart.Correlation(datos_estatales3, histogram=TRUE, pch=19)



#### Analisis Componentes Principales  ###################################

#aplicamos PCA con centrado y normalizacion de variables
# ver https://www.datacamp.com/community/tutorials/pca-analysis-r
variables_orig.pca <- prcomp(datos_estatales3, center = TRUE,scale. = TRUE)
summary(variables_orig.pca)
print(variables_orig.pca)

#grafica de varianza explicada (abrir mas ventana en caso de error)

plot(variables_orig.pca, type = "l")

fviz_eig(variables_orig.pca)


#grafica de PC1 y PC2
# ver https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

plot(variables_orig.pca$x[,1:2])

autoplot(variables_orig.pca,label = TRUE, data = datos_estatales3 , label.size = 3)

fviz_pca_ind(variables_orig.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE)     # Avoid text overlapping

# Resultados por estado
res.ind <- get_pca_ind(variables_orig.pca)
pca_voriginales <- res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

#Conclusion: El analisis de componentes principales de Orange y R son coincidentes
# por lo tanto se usa los valores de PC1 y PC2 obtenidos de orange

#cargarmos los datos del analisis realizado con HC optimo (4 clases) en Orange
Orange_Clusters_HC_4_ICE2019 <- read_csv("Orange_Clusters_HC_4_ICE2019.csv")
Orange_Clusters_HC_4_ICE2019 <- Orange_Clusters_HC_4_ICE2019 %>% 
  rename(
    "mS#Estado" = Estado,
    "Cluster_HC" = Cluster)
#View(Orange_Clusters_HC_4_ICE2019)

#cargarmos los datos del analisis realizado con Kmeans optimo (9 clases) en Orange
Orange_Clusters_Kmeans_9_ICE2019 <- read_csv("Orange_Clusters_Kmeans_9_ICE2019.csv", 
                                            col_types = cols(PC1 = col_skip(), PC2 = col_skip(), 
                                                             Silhouette = col_skip()))
Orange_Clusters_Kmeans_9_ICE2019 <- Orange_Clusters_Kmeans_9_ICE2019 %>%
  rename(
    "mS#Estado" = Estado,
    "Cluster_Kmeans" = Cluster)
#View(Orange_Clusters_Kmeans_9_ICE2019)

#unimos los dataframes para incorporar el dato de PC1, PC2, cluster subopt y cluster opt
datos_estatales4 <- merge(Datos_estatales2,Orange_Clusters_HC_4_ICE2019,by=c("mS#Estado"))
datos_estatales4 <- merge(datos_estatales4,Orange_Clusters_Kmeans_9_ICE2019,by=c("mS#Estado"))
View(datos_estatales4)

### Resumen de resultados de PCA, clustering e ICE estatal ###########

#se calcula el promedio ponderado (50% para cada PC) como propuesta de ICE estatal

datos_estatales4 <- datos_estatales4 %>% 
  mutate(ICE_estatal = (PC1+PC2)/2)

#se guardan los datos en un csv 
#write.csv(datos_estatales4, file = "Edo_Variables-Resultados_ICE2019.csv",row.names=FALSE)

#cambiamos los rownames por la columna mS#Estado
datos_estatales4 <- datos_estatales4 %>% 
  remove_rownames %>% 
  column_to_rownames(var="mS#Estado")

#añadimos nuevamente la columna mS#Estado
datos_estatales4$"mS#Estado" <- Datos_estatales2$"mS#Estado"

#ordenamos las columnas
datos_estatales4 <- datos_estatales4[c(15,1:14)]
View(datos_estatales4)



#### Analisis de Clusters ##########################################

#calculo y visualizacion de las distancias usadas para el clustering

data <- datos_estatales4[,c(11,12)]

#euclidiana
dist_ori_euc <- get_dist(data,method = "euclidean")
fviz_dist(dist_ori_euc, lab_size = 4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#manhattan
dist_ori_man <- get_dist(data,method = "manhattan")
fviz_dist(dist_ori_man, lab_size = 4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Conclusion: Se decide usar como métrica la norma euclidiana

# K means =====

#se analizan 30 indices con NbClust para determinar la configuracion optima de clusters
# distancia=euclidiana, clusters minimos=2, clusters maximos=10, alllong=calcula todos los indices
resultados <- NbClust(data, diss= NULL, distance= "euclidean", min.nc=2, max.nc=10,
        method = "kmeans", index= "alllong", alphaBeale=0.1)

#muestra solo la parte de las configuraciones optimas por índice
a <-resultados$Best.nc
#write.csv(a, file = "R_Resultados_indicadores_Kmeans.csv",row.names=FALSE)

#histograma
fviz_nbclust(resultados) + theme_minimal()+
  labs(title = NULL,subtitle = NULL,x = "Número de conglomerados k", y = "Frecuencia en los 30 índices")

#muestra los conglomerados de la mejor configuracion
resultados$Best.partition

#Conclusion: According to the majority rule, the best number of clusters is: 9 
#ver http://rstudio-pubs-static.s3.amazonaws.com/265632_3ad9e0b981244e15887677f8dffb39a0.html#average-silhouette-method
#ver https://cedric.cnam.fr/fichiers/art_2579.pdf

#gráficas que muestran el numero optimo de clusters con diferentes metodos
#ver https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/

# Elbow method
fviz_nbclust(data, kmeans, method = "wss") +
  geom_vline(xintercept = 9, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(data, kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# Visualización con 9 clusters
set.seed(123)
km.res4 <- kmeans(data, 9, nstart = 25)
fviz_cluster(km.res4, data = data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal()) +
  labs(title = NULL, subtitle = NULL)



# Hierarchical clustering (Ward method) =====
#ver detalle importante: http://adn.biol.umontreal.ca/~numericalecology/Reprints/Murtagh_Legendre_J_Class_2014.pdf

#se analizan 30 indices con NbClust para determinar la configuracion optima de clusters
# distancia=euclidiana, clusters minimos=2, clusters maximos=10, alllong=calcula todos los indices
resultados2 <- NbClust(data, diss= NULL, distance= "euclidean", min.nc=2, max.nc=10,
                      method = "ward.D2", index= "alllong", alphaBeale=0.1)

#muestra solo la parte de las configuraciones optimas por índice
b <-resultados2$Best.nc
#write.csv(b, file = "R_Resultados_indicadores_HC.csv",row.names=FALSE)

#histograma
fviz_nbclust(resultados2) + theme_minimal()+
  labs(title = NULL,subtitle = NULL,x = "Número de conglomerados k", y = "Frecuencia en los 30 índices")

#muestra los conglomerados de la mejor configuracion
resultados$Best.partition

#Conclusion: According to the majority rule, the best number of clusters is: 4 


# Elbow method (señala que el óptimo son dos clusters)
fviz_nbclust(data, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method (señala que el óptimo son tres clusters)
fviz_nbclust(data, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic (señala que el óptimo son dos clusters)
set.seed(123)
fviz_nbclust(data, hcut, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# Compute hierarchical clustering
res.hc <- data %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Corta en 4 conglomerados
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


# Compute hierarchical clustering on principal components

res.hcpc <- HCPC(data, graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

#graph
fviz_cluster(res.hcpc,
             repel = FALSE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

#muestra los primeros 10 renglones con el dato correspondiente del cluster
head(res.hcpc$data.clust, 10)


###########################################
