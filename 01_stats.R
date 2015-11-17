# sesión del 16/11/15

library(ggplot2)
grupoA <- data.frame(boca=rnorm(5000,10,2))
grupoA$type='grupoA'
grupoB <- data.frame(boca=rnorm(5000,16,2))
grupoB$type='grupoB'

grupos <- rbind(grupoA, grupoB)
# histograma general
ggplot(grupos, aes(x=boca)) + geom_histogram()
# histograma distinto para cada tipo    
ggplot(grupos, aes(x=boca)) + geom_histogram() + facet_grid(~type)
#Sin ggplot
hist(boca$mpg)
# mismo experimento, 4 veces menos desv. standard    
grupoA <- data.frame(boca=rnorm(100,10,0.5))
grupoA$type='grupoA'
grupoB <- data.frame(boca=rnorm(100,14,0.5))
grupoB$type='grupoB'

grupos <- rbind(grupoA, grupoB)
# histograma general
ggplot(grupos, aes(x=boca)) + geom_histogram()
# histograma distinto para cada tipo    
ggplot(grupos, aes(x=boca)) + geom_histogram() + facet_grid(~type)

#n sirve para darle un valor único sin necesidad de volverlo a escribir
n=100
grupoA<- data.frame(boca=rnorm(n,10,2), asa=rnorm(n,3,0.5), base=rnorm(n, 4.5, 0.3), long=rnorm(n,100,3), tipo='A')

# comprobar que no hay negativos (si sale algún TRUE entonces hay negativos
grupoA$boca<0

grupoB<- data.frame(boca=rnorm(n,13,1.5), asa=rnorm(n,5,0.6), base=rnorm(n, 5, 0.1), long=rnorm(n,115,4), tipo='B')

grupoC<- data.frame(boca=rnorm(n,10,0.5), asa=rnorm(n,4,0.3), base=rnorm(n, 5.5, 0.5), long=rnorm(n,110,4.5), tipo='C')

# rbind solo une dos grupos, tenemos que crear un nuevo grupo ejemplo para agregar el tercero
myData <- rbind(grupoA, grupoB)
myData <- rbind(myData, grupoC)

# EDA
ggplot(myData, aes(x=boca, y=long)) + geom_point()
ggplot(myData, aes(x=asa, y=long)) + geom_point()

# correlaciones para ver si los distintos parámetros son independientes (lo son si el valor se acerca a 0)
cor(myData$boca, myData$long)


# ejemplos de correlaciones
foo <- rnorm(1000,170,10)
bar <- foo-100
plot(foo,bar)
cor(foo,bar)

bar <- foo-rnorm(1000,100,30)
plot(bar,foo)
cor(foo,bar)

# correlacion inversa
bar <- -foo
cor(foo,bar)

# test kolmogorov-smirnof para ver si 2 muestras fueron generadas por la misma distribución estadística
medieval <- rnorm(100, 20, 2)
romano <- rnorm(100,5,0.3)
ks.test(medieval, romano)

