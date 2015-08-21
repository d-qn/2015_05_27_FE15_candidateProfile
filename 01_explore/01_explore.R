library(dplyr)
library(ggplot2)

### SETTINGS and data files

# candidates data file
candidates.file <- "data/candidates_socio-demographics_2015-08-20.csv"

# which columns should be a factor
candidates.factorCols <- c('gender', 'city', 'country', 'language', 'party_short', 'party_REC', 'district')

# residence related data
zipcode.file <- "data/indicators/PLZO_CSV_LV03.csv"
communePortrait.file <- "data/indicators/communesCH_2015_indicators_je-f-21.03.01.csv"
communeClass.file <- "data/indicators/be-b-00.04-rgs-01_urbanRural.csv"


############################################################################################
###		Get data
############################################################################################

## 1. Get candidates data
data.read <- read.csv(candidates.file, header = TRUE, stringsAsFactors = F)

# Have factor where it makes sense
for (col in candidates.factorCols) {
	data.read[,col] <- as.factor(data.read[,col])
}

# uglyhack: some manual corrections for zip code & given city names
zipManualFix <- structure(c(
	7107,	9000,	1898,	1201
	), names=c(
	"7101 Safien Platz", "9001 St.Gallen", "1989 St-Gingolph", "1211 Genève 1"
))
# replace some of the wrong zip code
data.read[match(names(zipManualFix), paste(data.read$zip, data.read$city)),'zip'] <- zipManualFix

## 2. build a table with correspondance between zip code, commune name, and bfs commune code
# get all candidates' unique zip code in Switzerland
qr <- data.frame(zip = unique(data.read %>% filter(country == 'Schweiz') %>% select(zip)))
qr$communeGiven <- as.character(data.read[match(qr$zip, data.read$zip), 'city'])


## 3. Load all data related to residence/commune/zip code
zipCH <- read.csv(zipcode.file, sep =";", header = TRUE, encoding = "latin1")
communePortrait <- read.csv(communePortrait.file, skip = 2, header = TRUE, stringsAsFactors = F, check.names = FALSE)
communeClass <- read.csv(communeClass.file, header = TRUE, check.names = FALSE)

## 4. Populate table of zip code, commune name, and bfs commune code

# Match commune names
qr$communeOfficial <- zipCH[match(qr$zip, zipCH$PLZ), 'Gemeindename']
qr$bfs <- communeClass[match(qr$communeOfficial, communeClass[,2]),1]

# For elements without bfs code, match the commune name given by the candidate
qr[is.na(qr$bfs),'bfs'] <- match(unlist(qr %>% filter(is.na(bfs)) %>% select(communeGiven)), communeClass[,2])

## display the % of commune without bfs code
warning(sum(is.na(qr$bfs)), " given zip does not have a BFS commune code!", "\n", "This represents: ",
	round((sum(is.na(qr$bfs)) / nrow(qr)) * 100, 3), " %of the whole data")
# qr[is.na(qr$bfs),]


############################################################################################
###		Plot
############################################################################################


