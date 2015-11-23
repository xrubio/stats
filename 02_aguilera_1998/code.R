
# Replication of analysis developed by Antonio Aguilera in: 
# "ANÁLISIS MULTIVARIABLE: UNA NUEVA VÍA PARA LA CARACTERIZACIÓN CERÁMICA", Pyranae, 29, 1998, 117-134

library(ggplot2)
library(ggdendro)
library(gridExtra)
library(plyr)

# PASCUAL1 ANALYSIS

amph <- read.csv('pascual1.csv', sep=";", header=T)

# Some EDA

ggplot(amph, aes(x=height, y=width1, size=diam, color=site)) + geom_jitter()
ggplot(amph, aes(x=height, y=width2, size=diam, color=site)) + geom_jitter()
ggplot(amph, aes(x=width1, y=width2, size=height, color=site)) + geom_jitter()
ggplot(amph, aes(x=width1, y=width2, size=diam, color=height)) + geom_jitter() + facet_grid(~site)

##################### K-MEANS    
measures <- amph[,5:8]
row.names(measures) <- amph$id

numGroups = 4

myKMeans <- kmeans(measures,numGroups)

amphKMean <- amph
amphKMean$cluster <- myKMeans$cluster

# plot differences
svg('kmeans_1.svg', width=7, height=6)
ggplot(amphKMean, aes(x=diam, y=height, colour=region)) + geom_point() + facet_grid(site~cluster)
dev.off()

# compute 16 groups (4 sites x 4 clusters)
amphKMeanCount <- count(amphKMean, vars=c('site','cluster'))

svg('kmeans_2.svg')
ggplot(amphKMeanCount, aes(y=factor(site), x=factor(cluster), label=freq)) + geom_text()
dev.off()

################### CLUSTERING WITH UPGMA

# create a dissimilarity matrix
distMeasures <- dist(measures)


# UPGMA is also called average
amphClust <- hclust(distMeasures, method="average")
amphClust$labels <- amph$site 

svg('hclust.svg', width=14, height=18)
ggdendrogram(amphClust, rotate=T, size=4)
dev.off()


############ Discriminant Analysis
library(MASS)

# random sample 52 from each region        
sampleSize = 52
nAmph <- subset(amph, region=="North")
n <- nAmph[sample(nrow(nAmph), sampleSize),]
sAmph <- subset(amph, region=="South")
s <- sAmph[sample(nrow(sAmph), sampleSize),]
amphSample <- rbind(n,s)

amphDA <- qda(region~height+width1+width2+diam, data=amphSample, prior=c(1,1)/2)

predQda <- predict(amphDA)

amphSample$probNorth <- predQda$posterior[,"North"]
amphSample$probSouth <- predQda$posterior[,"South"]
amphSample$class <- predQda$class

svg('da.svg', width=10, height=10)
ggplot(amphSample, aes(x=diam, y=height, col=interaction(region,class), label=region)) + geom_text(size=5) + theme_bw() + theme(legend.position="top") + scale_colour_manual(values=c("palegreen2", "deeppink3", "indianred2", "springgreen2"), name="reality-predicted")
dev.off()    

# confusion matrix
library(caret)

confusionMatrix(amphSample$class, amphSample$region)

# all data!

predAll <- predict(amphDA, amph)
amph$probNorth <- predAll$posterior[,"North"]
amph$probSouth <- predAll$posterior[,"South"]
amph$class <- predAll$class

svg('da_final.svg', width=10, height=10)
ggplot(amph, aes(x=diam, y=height, col=interaction(region,class), label=region)) + geom_text(size=5) + theme_bw() + theme(legend.position="top") + scale_colour_manual(values=c("palegreen2", "deeppink3", "indianred2", "springgreen2"), name="reality-predicted")
dev.off()

confusionMatrix(amph$class, amph$region)

