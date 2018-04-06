#sudo apt-get install libtiff5-dev
library(ClusterR)
#sudo apt-get install libxml2-dev
library(tm)
#sudo apt-get install  libx11-dev mesa-common-dev libglu1-mesa-dev
library(rgl)

library(cluster)
library(fpc)

pca_dat <- read.table("/home/andrey/Downloads/cars.data", 
                      sep = "" , header = F, na.strings ="", stringsAsFactors= F)[, c("V1", "V6")]
pca_dat <- center_scale(pca_dat)
km <- KMeans_arma(pca_dat, clusters = 3, n_iter = 10, seed_mode = "random_subset", verbose = T, CENTROIDS = NULL)
pr <- predict_KMeans(pca_dat, km)
class(km) = 'matrix'
plot_2d(data = pca_dat, clusters = as.vector(pr), centroids_medoids = as.matrix(km)) 

predict3means <- function(data) {
  pca_dat <- center_scale(pca_dat)
  km <- KMeans_arma(pca_dat, clusters = 3, n_iter = 10, seed_mode = "random_subset", verbose = T, CENTROIDS = NULL)
  pr <- predict_KMeans(pca_dat, km)
}

setwd("/home/andrey/Documents/R/ClusterAnalysis/data")


dictionary <-list()
categories <- c("Phisics", "Programing", "Football")
fileNames <- list("Phisics" = c("dict/1.txt", "dict/2.txt", "dict/3.txt", "4.txt", "5.txt"),
                  "Programing" = c("dict/6.txt", "dict/7.txt", "dict/8.txt", "9.txt", "10.txt"),
                  "Football" = c("dict/11.txt", "dict/12.txt", "dict/13.txt", "14.txt", "15.txt"))

getDictionaryForCategory <- function(category) {
  corpus <- VCorpus(DirSource(paste(category,"dict", sep = "/") , encoding = "UTF-8"), readerControl = list(language = "eng"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus,  removeWords, stopwords("english"))
  corpus <- tm_map(corpus,  removeWords, c("also", "can")) 
  
  
  dtm <- TermDocumentMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))
  dtm <- removeSparseTerms(dtm, 0.3)
  dtmMatrix <- as.matrix(dtm)
  v <- sort(rowSums(dtmMatrix), decreasing=TRUE)
  sort(names(head(v, 50)))
}

for(category in categories){
  dictionary[[category]] <- getDictionaryForCategory(category)
}

getTermsScoreForTextByCategory <- function(fileName, category) {
  ptd <-PlainTextDocument(readLines(file(fileName ,open="r")))
  freq <- as.matrix(termFreq(ptd, control = list(removePunctuation = TRUE,
                                       removeNumbers = TRUE,
                                       content_transformer(tolower),
                                       wordLengths = c(3, Inf))))
  
  mentionedTerms <- subset(freq, rownames(freq) %in% dictionary[[category]])
  sum(mentionedTerms[, 1])
}

#Creating Cluster Matrix
clusterMatrix <- matrix(nrow=15, ncol = 3)
colnames(clusterMatrix) <- categories
rownames(clusterMatrix) <- c(fileNames$Phisics, fileNames$Programing, fileNames$Football)
for(fileName in rownames(clusterMatrix)) {
  fullFilePath <-paste(getCategoryName(fileName), fileName, sep = "/")
  for(category in colnames(clusterMatrix)) {
    clusterMatrix[fileName, category] =  getTermsScoreForTextByCategory(fullFilePath, category)
  }
}


getCategoryName <- function(fileName) {
  if(fileName %in% fileNames$Phisics){categories[1]}
  else if(fileName %in% fileNames$Programing){categories[2]}
  else if(fileName %in% fileNames$Football){categories[3]}
}

pc <- princomp(clusterMatrix[,1:3], cor=TRUE, scores=TRUE)
plot3d(pc$scores[,1:3])

kMeansData <- KMeans_arma(clusterMatrix, clusters = 3, n_iter = 10, seed_mode = "random_subset", verbose = T, CENTROIDS = NULL)

res <- kmeans(clusterMatrix, 3)
clusplot(clusterMatrix, res$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


clusterMatrix <- center_scale(clusterMatrix)
kMeansData <- KMeans_arma(clusterMatrix, clusters = 3, n_iter = 5, seed_mode = "random_subset", verbose = T, CENTROIDS = NULL)
predict_KMeans(clusterMatrix, kMeansData)
