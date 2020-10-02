install.packages("tm")
install.packages("tidyverse")
install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("bibliometirx")
library(cvms)
library(tidyverse)
library(tm)
library(NLP)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(RefManageR)
library(bibliometrix)
library(quanteda)
library(ggplot2)
library(ggpubr)

help("bibliometrix")
help("igraph")
help("network")
#seteo el directorio de trabajo
setwd("/home/alrier/Documentos/valor, satisfacción y calidad bibliometria/bib valor, calidad y satisfaccion")
#Importo el archivo
file <- ("/home/alrier/Documentos/valor, satisfacción y calidad bibliometria/bib valor, calidad y satisfaccion/valorcalidad.bib")
#convierto a Dataframe
M <- convert2df(file, dbsource = "scopus", format = "bibtex")
'''Paso los documentos a formato tibble para trabajar un filtro 
por años a partir del año 2018 en adelante, pero es solo para hacer
un análisis más liviano, después puedo retomar el objeto M que es un 
DF y contiene todos los resultados''' 
N1 <- as.tibble(M)
N2<- N1 %>% filter(PY >=2018)
#convierto nuevamente a DF para continuar trabajando sobre mi filtro
M1 <- as.data.frame(N2)
#Hago el biblioanálisis
N3 <- biblioAnalysis(M1, sep = ";")
#agrupo y resumo resultados
'''este sumario y los plots es mejor trabajarlos sobre el archivo 
bibliometrix en formato DF'''
S <- summary(object = N3, k = 20, pause = FALSE)
plot(x = S, k = 20, pause = FALSE)

'''genero la primera red matricial de datos'''
NetMatrix <- biblioNetwork(M1, analysis = "co-occurrences", network = "author_keywords", sep = ";")
'''calculo los indices de similitud (ojo a este procedimiento porque 
a partir de 10 mil observaciones requiere buena RAM del equipo'''
P <- normalizeSimilarity(NetMatrix, type = "association")
'''Grafico mi primera red'''
p1net <- networkPlot(P, n = 20, Title = "co-occurrence network", type = "fruchterman", 
                         labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "walktrap",
                         remove.isolates = F, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)

'''segunda red matricial de datos con su grafica'''
NetMatrix2 <- biblioNetwork(M1, analysis = "co-citation", network = "references", sep = ". ")
'''Grafico mi segunda red''' 
p2net=networkPlot(NetMatrix2, n = 10, Title = "Country Collaboration", type = "fruchterman", labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "spinglass",
                remove.isolates = T, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)

'''Genero una tercera red matricial de datos'''
n <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix3 <- biblioNetwork(n, analysis = "collaboration", network = "countries", sep = ";")
'''ploteo la red'''
p3net=networkPlot(NetMatrix3, n = 10, Title = "Country Collaboration", type = "fruchterman", labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "spinglass",
                remove.isolates = T, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)

#Del total de resultados, extraigo los papers más citados
'''los puedo retirar del sumario o del total de observaciones'''
'''Estraidos del total de observaciones'''
AU <- N3$MostCitedPapers
'''Extraidos del sumario -> de aquí va a estraer los 20 más 
importantes, o el número de observaciones que yo haya pedido a R'''
AU1 <- S$MostCitedPapers
AUT <- AU1[1:2]
View(AUT)
View(AU)

'''hago lo mismo con los países'''
Paises <- N3$Countries
Paises <- S$MostProdCountries
View(Paises)
Paises <- Paises[c(1, 3)]
'''re nombro la columna paises'''
names(Paises)[1] <- "Country"
#'''esto aplica solo para revistas en español'''
#Paises$Country <- c("USA", "Taiwan", "Korea",  "Reino Unido", "Alemania", "Holanda", "Italia", "Canada", "España", "China")
'''quito algonos errores'''
Paises$Freq <- suppressWarnings(as.numeric(Paises$Freq))
'''miro los años de producción'''
Produccion <- S$AnnualProduction
names(Produccion)[1] <- "Year"
'''seteo la segunda columna como numerica'''
Produccion$Articles <- as.numeric(Produccion$Articles)
'''revisemos los keywords'''
key<- S$MostRelKeywords
key <- key[c(1, 2)]
plot(key)
'''graficas y plots'''
Fig1A <- ggplot(Paises, aes(x=reorder(Country, Freq) , y=Freq)) + geom_bar(stat = "identity", fill="blue") + coord_flip() + xlab("Country") + ylab("Frequency")
Fig1B <- ggplot(Produccion, aes(x=Year , y=Articles)) + geom_bar(stat = "identity", fill="blue") + xlab("Year") + ylab("Articles") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggarrange (Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)

'''graficas y plots por citas y palabras clave'''
Fig1A <- ggplot(key, aes(x=1 , y=2)) + geom_bar(stat = "identity", fill="blue") + coord_flip() + xlab("KeyWords") + ylab("Articles")
Fig1B <- ggplot(AUT, aes(x=2 , y=1)) + geom_bar(stat = "identity", fill="blue") + xlab("Paper") + ylab("TotalC") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggarrange (Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)
plot(Fig1A)
#este trabajo de minería se hace sobre el total de observaciones N3.
'''Minería de datos'''
texto = Corpus(VectorSource(N3)) 
'''minuscula (A!=a)'''
discurso=tm_map(texto, tolower)
'''quitamos los espacios en blanco'''
discurso =tm_map(discurso, stripWhitespace)
'''quitamos la puntuacion'''
discurso = tm_map(discurso, removePunctuation)
'''quitamos los numeros'''
discurso = tm_map(discurso, removeNumbers)


'''quitamos palabras genericas y todas las que 
molesten en el resultado. PDT: hay que cambiar la lista de 
palabras que es de otra bibliometría'''
discurso=tm_map(discurso, removeWords, c(stopwords("english"), "zhang", "human", "usa", "kingdom", "korea", "institute", "national", "lee", "china", "univ", "cell", "london", "medical","coronavirus", "gene", "chen", "kong", "hong", "infectious", "animal", "wang", "diseases", "veterinary", "center", "centre", "college", "sciences", "school", "protein", "public", "control", "state", "liu", "clinical", "chinese", "van", "department", "affiliated", "united", "universit", "kim", "viral", "california", "vaccine"))

############### DATA FRAME DE PALABRAS CON SU FRECUENCIA

'''Creamos matriz de letras'''
letras= TermDocumentMatrix(discurso)
findFreqTerms(letras, lowfreq=5)
matrix=as.matrix(letras)

'''lo ordenamos y sumamos las letras de nuestra matriz'''
vector <- sort(rowSums(matrix),decreasing=TRUE) 
'''creamos la data con las palabras y su frecuencia'''
dataletras <- data.frame(word= names(vector),frequencia=vector) 

################ GRAFICAMOS FRECUENCIA DE LAS PALABRAS

barplot(dataletras[1:30,]$freq, las = 2, names.arg = dataletras[1:30,]$word,
        col ="blue", main ="PALABRAS MÁS FRECUENTES", ylab = "Frecuencia de palabras")


############ GRAFICAMOS LA NUBE DE PALABRAS
wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 2,
          max.words=70)
'''organizar la nube de palabras'''
wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 5,
          max.words=150, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(7, "Dark2"))

#Este trabajo de minería se hace sobre los keyWords
'''Minería de datos'''
texto = Corpus(VectorSource(key)) 
'''minuscula (A!=a)'''
discurso=tm_map(texto, tolower)
'''quitamos los espacios en blanco'''
discurso =tm_map(discurso, stripWhitespace)
'''quitamos la puntuacion'''
discurso = tm_map(discurso, removePunctuation)
'''quitamos los numeros'''
discurso = tm_map(discurso, removeNumbers)


'''quitamos palabras genericas y todas las que 
molesten en el resultado. PDT: hay que cambiar la lista de 
palabras que es de otra bibliometría'''
discurso=tm_map(discurso, removeWords, c(stopwords("english"), "zhang", "human", "usa", "kingdom", "korea", "institute", "national", "lee", "china", "univ", "cell", "london", "medical","coronavirus", "gene", "chen", "kong", "hong", "infectious", "animal", "wang", "diseases", "veterinary", "center", "centre", "college", "sciences", "school", "protein", "public", "control", "state", "liu", "clinical", "chinese", "van", "department", "affiliated", "united", "universit", "kim", "viral", "california", "vaccine"))

############### DATA FRAME DE PALABRAS CON SU FRECUENCIA

'''Creamos matriz de letras'''
letras= TermDocumentMatrix(discurso)
findFreqTerms(letras, lowfreq=5)
matrix=as.matrix(letras)

'''lo ordenamos y sumamos las letras de nuestra matriz'''
vector <- sort(rowSums(matrix),decreasing=TRUE) 
'''creamos la data con las palabras y su frecuencia'''
dataletras <- data.frame(word= names(vector),frequencia=vector) 

################ GRAFICAMOS FRECUENCIA DE LAS PALABRAS

barplot(dataletras[1:30,]$freq, las = 2, names.arg = dataletras[1:30,]$word,
        col ="blue", main ="PALABRAS MÁS FRECUENTES", ylab = "Frecuencia de palabras")


  ############ GRAFICAMOS LA NUBE DE PALABRAS
wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 2,
          max.words=70)
'''organizar la nube de palabras'''
wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 5,
          max.words=150, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(7, "Dark2"))

################# Leer manual CVMS para crossmodalidad. 

