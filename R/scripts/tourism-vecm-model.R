library(tm) #load text mining library
setwd('D:/Research/PAPERS/finance/tourism')

tour <- read.csv(file.choose())
names(tour)
tour_abs <- tour["Abstract"]
length(t(tour_abs))

tour_corp <- VCorpus((VectorSource(t(tour_abs))))

abstracts_corp <- tm_map(tour_corp, stripWhitespace)
abstracts_corp <- tm_map(abstracts_corp, content_transformer(tolower))                          
abstracts_corp <- tm_map(abstracts_corp, removeWords, stopwords("english"))
abstracts_corp <- tm_map(abstracts_corp, removeNumbers)
abstracts_corp <- tm_map(abstracts_corp, removePunctuation)
# abstracts_corp <- tm_map(hc_corp, stemDocument, language = "english")
path <- readline()

path <- file.path(path, 'R')
setwd(path)
getwd()
dir.create('outputs')
# list.dirs()

pathch <- file.path(path, 'outputs')
setwd(pathch)
getwd()

write.csv(summary(abstracts_corp), 'corpus-summary.csv')

adtm <- DocumentTermMatrix(abstracts_corp)
tm::inspect(adtm[10:16, ] )
dim(adtm) # [1]   57 1652
adtm$dimnames

tourdtm <- adtm[, -c(1:7, 47)]
dim(tourdtm) # [1]   57 1644

library(FactoMineR)

tourdf <- data.frame(as.matrix(tourdtm), stringsAsFactors=FALSE)
tourdf1 <- as.matrix(removeSparseTerms(tourdtm, 0.80))
tourdf2 <- data.frame(tourdf1)
CA(data.frame(tourdf2), 2)

tourdf3 <- as.matrix(removeSparseTerms(tourdtm, 0.70))
tourdf4 <- data.frame(tourdf3)
names(tourdf4)
CA(data.frame(tourdf4), 2)

library(psych)

names(tourdf2)
tourismgrowth <- tourdf2[, 'tourism']+tourdf2[, 'growth']
exchangerate <- tourdf2[, 'exchange']+tourdf2[, 'rate']
gdp <- tourdf2[, 'gross']+tourdf2[, 'domestic'] +tourdf2[, 'product'] + tourdf2[, 'gdp']

tourdata <- data.frame(tourdf2, tourismgrowth = tourismgrowth, exchangerate = exchangerate, gdp = gdp)
names(tourdata)

fit <- lm(tourismgrowth ~ economy + exchangerate + gdp + income, data = tourdata)
write.csv(summary(fit)$coefficients, 'summary-lm.csv')

sink('lm-output.txt')
summary(fit)
sink()
getwd()
par(mfrow = c(2, 2))
plot(fit)
