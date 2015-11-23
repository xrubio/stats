
library(ggplot2)
library(gridExtra)
library(MASS)

myData <- read.csv('../01_virtual_datasets/vdata.csv', sep=";", header=T)

# compute PCA with data transformed to logarithmic to get rid of orders of magnitude
logData <- log(myData[,1:4])
pcaResults <- princomp(logData, center=T, scale=T)

# plot to check the relevance of the first 2 PC'S
plot(pcaResults)

# get the scores of the data
pcaValues <- as.data.frame(pcaResults$scores)
# put type in pcaValues
pcaValues$tipo <- myData$tipo

g1 <- ggplot(myData, aes(x=boca, y=long, col=tipo)) + geom_point()
g2 <- ggplot(pcaValues, aes(x=Comp.1, y=Comp.2, col=tipo)) + geom_point()
grid.arrange(g1,g2)

# lo mismo pero con plot normal
par(mfrow=c(2,1))
plot(myData$boca, myData$long, col=myData$tipo, main="datos reales")
plot(pcaValues$Comp.1, pcaValues$Comp.2, col=pcaValues$tipo, main="PCA")


# prueba de variable inútil
myData$inutil <- runif(300)
logDataInutil <- log(myData[,1:4])
logDataInutil <- cbind(logDataInutil, log(myData[,6]))

pcaResultsInutil <- princomp(logDataInutil, center=T, scale=T)

# la variable inutil es el PC1, mientras que PC2 es un combo de las otras 4
pcaResultsInutil$loadings

# K-means con PCA Values
measures <- pcaValues[,1:2]
numGroups = 3
myKMeans <- kmeans(measures,numGroups)

amphKMean <- pcaValues
amphKMean$cluster <- myKMeans$cluster
g1 <- ggplot(amphKMean, aes(x=Comp.1, y=Comp.2, colour=factor(cluster))) + geom_point() + facet_grid(~tipo) + ggtitle("pca1_2")

# K-means con datos originales
measures <- myData[,1:4]
numGroups = 3
myKMeans <- kmeans(measures,numGroups)

amphKMean <- pcaValues
amphKMean$cluster <- myKMeans$cluster
g2 <- ggplot(amphKMean, aes(x=Comp.1, y=Comp.2, colour=factor(cluster))) + geom_point() + facet_grid(~tipo) + ggtitle("datos originales")
grid.arrange(g1,g2)

######################## hierarchical clustering

distMeasures <- dist(pcaValues[,1:2])

# UPGMA is also called average
amphClust <- hclust(distMeasures, method="average")
amphClust$labels <- myData$tipo
g1 <- ggdendrogram(amphClust, rotate=T, size=4)

# hclust datos originales
library(ggdendro)  
distMeasures <- dist(myData[,1:4])
amphClust <- hclust(distMeasures, method="average")
amphClust$labels <- myData$tipo
g2 <- ggdendrogram(amphClust, rotate=T, size=4)

grid.arrange(g1,g2)

######################### discriminant analysis
sampleSize = 60
sampleAmph <- pcaValues[sample(nrow(pcaValues), sampleSize),]
sampleAmph <- cbind(sampleAmph[,1:2],sampleAmph$tipo)


amphDA <- qda(tipo~Comp.1+Comp.2, data=sampleAmph, prior=c(1,1,1)/3)
predQda <- predict(amphDA, pcaValues)
pcaValues$class <- predQda$class

g1 <- ggplot(pcaValues, aes(x=Comp.1, y=Comp.2, colour=factor(class))) + geom_point() + facet_grid(~tipo) + ggtitle("pca1_2")

### DA con datos originales

sampleSize = 60
sampleAmph <- myData[sample(nrow(myData), sampleSize),]

amphDA <- qda(tipo~boca+asa+base+long, data=sampleAmph, prior=c(1,1,1)/3)
predQda <- predict(amphDA, myData)
myData$class <- predQda$class

g2 <- ggplot(myData, aes(x=base, y=long, colour=factor(class))) + geom_point() + facet_grid(~tipo) + ggtitle("datos originales")

grid.arrange(g1,g2)


