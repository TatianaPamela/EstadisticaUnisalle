library(FactoMineR)
library(factoextra)

#Ingreso de datos
gastos <- read.csv("C:/Users/Tatiana/Downloads/gastos.csv", sep=";")
View(gastos)
gastos2=gastos[1:7,2:26]

#Procedimiento de Componentes principales
res.pca=PCA(gastos2)

#Elección del número de dimensiones a estudiar
#Inercia o varianza explicada
round(res.pca$eig,2)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 70))


#Estudio de los individuos activos
fviz_pca_ind(res.pca,
             col.ind = "cos2", # el color va en función de la calidad de la representación
             gradient.cols = c("#800000", "#FFA500", "#008000"),
             repel = TRUE,     # con esto se evita el solapado del texto
            col.ind.sup = "white"  
             )
round(cbind(res.pca$ind$coord[,1:3],res.pca$ind$cos2[,1:3],res.pca$ind$contrib[,1:3]),2)

#Estudio de la nube de variables

fviz_pca_var(res.pca,
             col.var = "contrib", # el color va en función de la contribución al PC
             gradient.cols = c("#800000", "#FFA500", "#008000"),
             repel = TRUE     # con esto se evita el solapado del texto
             )

round(cbind(res.pca$var$coord[,1:3],res.pca$var$cos2[,1:3],res.pca$var$contrib[,1:3]),2)


fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#D2691E", # color para las variables
                col.ind = "#000000"  # colores de las estaciones
)


#Descripción de las dimensiones
dimdesc(res.pca)$Dim.1$quanti
dimdesc(res.pca)$Dim.2$quanti

#Una vuelta a los datos
plot(res.pca, choix = "ind",axes = 2:3,invisible = "ind.sup")
plot(res.pca, choix = "var",axes = 2:3,invisible="quanti.sup")


par(las=2)
plot(gastos[1:7,21],type = "b",axes = F,ylab = "Comunicaciones (euros)",xlab="",bty="o")
axis(2)
axis(1,1:7,rownames(gastos)[1:7])
par(las=0)

par(las=2)
plot(gastos[1:7,4],type = "b",axes = F,ylab = "Leche, queso y huevos (euros)",xlab="",bty="o",)
axis(2)
axis(1,1:7,rownames(gastos)[1:7])
par(las=0)

par(las=2)
plot(gastos[1:7,6],type = "b",axes = F,ylab = "Fruta (euros)",xlab="",bty="o",)
axis(2)
axis(1,1:7,rownames(gastos)[1:7])
par(las=0)

par(las=2)
plot(gastos[1:7,10],type = "b",axes = F,ylab = "Café, té y cacao (euros)",xlab="",bty="o",)
axis(2)
axis(1,1:7,rownames(gastos)[1:7])
par(las=0)

par(las=2)
plot(gastos[1:7,13],type = "b",axes = F,ylab = "Bebidas alcohólicas (euros)",xlab="",bty="o",)
axis(2)
axis(1,1:7,rownames(gastos)[1:7])
par(las=0)


fviz_pca_biplot(res.pca, axes=c(2,3), repel = TRUE,
                col.var = "#D2691E", # color para las variables
                col.ind = "#000000"  # colores de las estaciones
)


library(corrplot)
corrplot(cor(gastos[1:7,2:5]),method = "number",type = "lower")
