library(ggplot2)
library(gridExtra)    
library(plyr)
library(MASS)
library(caret)



myData <- read.csv('all.csv', header=T, sep=";")
# only type 2
myData <- subset(myData, TYPE=="2")
# keep just 8 measures and site
myData <- myData[,5:13]


######### EDA #####################
#ggplot(myData, aes(x=exterior_diam, y=rim_w, colour=site)) + geom_point()
# ggplot(myData, aes(x=exterior_diam, fill=site)) + geom_bar() + facet_wrap(~site, ncol=1)
# ggplot(myData, aes(x=rim_w_2, y=protruding_rim, colour=site)) + geom_point() + facet_wrap(~site)

########## PCA #####################

# compute PCA with data transformed to logarithmic to get rid of orders of magnitude
logData <- log(myData[,1:8])
pcaResults <- princomp(logData, center=T, scale=T)

# plot to check the relevance of the first 2 PC'S
plot(pcaResults)

# get the scores of the data
pcaValues <- as.data.frame(pcaResults$scores)
# put type in pcaValues
pcaValues$site <- myData$site

ggplot(pcaValues, aes(x=Comp.1, y=Comp.2, col=site)) + geom_point()

# K-means con PCA Values
measures <- pcaValues[,1:2]
# 3 sites
numGroups = 3
myKMeans <- kmeans(measures,numGroups)

amphKMean <- pcaValues
amphKMean$cluster <- myKMeans$cluster
ggplot(amphKMean, aes(x=Comp.1, y=Comp.2, colour=factor(cluster))) + geom_point() + facet_grid(~site) + ggtitle("pca1_2")

amphKMeanCountPCA <- count(amphKMean, vars=c('site','cluster'))

g1 <- ggplot(amphKMeanCountPCA, aes(y=factor(site), x=factor(cluster), label=freq)) + geom_text() + ggtitle("pca1_2")

# K-means con datos originales
measures2 <- myData[,1:8]
numGroups = 3
myKMeans <- kmeans(measures2,numGroups)

amphKMean <- myData 
amphKMean$cluster <- myKMeans$cluster
amphKMeanCountStd <- count(amphKMean, vars=c('site','cluster'))

g2 <- ggplot(amphKMeanCountStd, aes(y=factor(site), x=factor(cluster), label=freq)) + geom_text() + ggtitle("datos originales")
grid.arrange(g1,g2)

############ Discriminant Analysis  ############################

sampleSize = min(count(myData,'site')$freq)

amph1 <- subset(myData, site=="Las Delicias")
sample1 <- amph1[sample(nrow(amph1), sampleSize),]
amph2 <- subset(myData, site=="Malpica")
sample2 <- amph2[sample(nrow(amph2), sampleSize),]
amph3 <- subset(myData, site=="belén")
sample3 <- amph3[sample(nrow(amph3), sampleSize),]

sample <- rbind(sample1,sample2)
sample <- rbind(sample, sample3)

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)

# plot to check the relevance of the first 2 PC'S
#plot(pcaResultsSample)

# get the scores of the data
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
# put type in pcaValues
pcaValuesSample$site <- sample$site

amphDA <- qda(site~Comp.1+Comp.2, data=pcaValuesSample, prior=c(1,1,1)/3)
predQda <- predict(amphDA, pcaValuesSample)
    
sample$probDelicias<- predQda$posterior[,"Las Delicias"]
sample$probMalpica <- predQda$posterior[,"Malpica"]
sample$probBelen <- predQda$posterior[,"belén"]
pcaValuesSample$class <- predQda$class

#g1 <- ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, colour=factor(class))) + geom_point() + facet_grid(~site) + ggtitle("pca1_2")

#ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, col=interaction(site,class), label=site)) + geom_text(size=5) + theme_bw() + theme(legend.position="top")
confusionMatrix(pcaValuesSample$class, pcaValuesSample$site)

svg('puntitos.svg')    
ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, col=site)) + geom_point() + facet_wrap(~correcto, ncol=1) + ylim(c(-0.3,0.3))
dev.off()

foo <- subset(pcaValuesSample, Comp.1>-1.2)
foo <- subset(foo, Comp.2>-2.1)

svg('fig_dist.svg')    
ggplot(foo, aes(x=Comp.1, y=Comp.2)) + geom_density2d(aes(col=site), alpha=0.3) + geom_point(aes(col=site), size=2) + facet_wrap(~site, ncol=1) + theme(legend.position='none')
dev.off()

