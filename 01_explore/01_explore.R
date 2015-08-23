library(dplyr)
library(ggplot2)
library(scales)

### SETTINGS and data files
theme_set(theme_bw())

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

# clean up age
data.read[which(data.read$age < 0),'age'] <- NA


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
	round((sum(is.na(qr$bfs)) / nrow(qr)) * 100, 3), " % of the whole data")
# qr[is.na(qr$bfs),]


############################################################################################
###		Subset candidates data and combine with indicators
############################################################################################

data <- data.read %>% select(one_of(c('ID_user', 'ID_Candidate', 'firstname', 'lastname', 'gender', 'year_of_birth',
	'age', 'zip', 'city', 'country', 'party_short', 'party_REC', 'district', 'language')))

data$bfs <- qr[match(data$zip, qr$zip),'bfs']
# set to NA bfs code for candidates living not in Switzerland
data[which(data$country != "Schweiz"),'bfs'] <- NA

# add rural/countryside BFS classification
data$communeUrbanClass <- communeClass[match(data$bfs, communeClass[,1]), "Régions urbaines / rurales 2000*"]

# add communal portrait data
data$communeBuildingSurface <- communePortrait[match(data$bfs, communePortrait[,1]), "Surfaces d'habitat et d'infrastructure en %"]
data$communeForeigner <- communePortrait[match(data$bfs, communePortrait[,1]), "Etrangers en %"]

############################################################################################
###		Plot
############################################################################################

# gender, age, language, party, urban

#http://stackoverflow.com/questions/4725339/percentage-on-y-lab-in-a-faceted-ggplot-barchart

# gender

gp1 <- ggplot(data = data, aes(x = gender)) + geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + scale_y_continuous(labels = percent)
gp1 + facet_grid(. ~ party_REC)
#gp1 + facet_grid(. ~ language)
#gp1 + facet_grid(language ~ party_REC)

# age
data %>% group_by(party_REC) %>% summarise(mean(age, na.rm = T))
data %>% group_by(party_REC) %>% summarise(quantile(age, 0.9, na.rm = T))
data %>% group_by(party_REC) %>% summarise(median(as.numeric(age), na.rm = T))

ap1 <- ggplot(data = data, aes(x = age)) + geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), binwidth = 5) + theme_bw() + scale_y_continuous(labels = percent)
ap1 + facet_grid(. ~ party_REC)
ap1 + facet_grid(language ~ party_REC)


# urban class
ucp1 <-  ggplot(data = data, aes(x = communeUrbanClass)) + geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + theme_bw() + scale_y_continuous(labels = percent)
ucp1 + facet_grid(. ~ party_REC)
ucp1 + facet_grid(language ~ party_REC)

upp1 <- ggplot(data = data, aes(x = communeBuildingSurface)) + geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), binwidth = 20) + theme_bw() + scale_y_continuous(labels = percent)
upp1 + facet_grid(. ~ party_REC)
upp1 + facet_grid(language ~ party_REC)

# # foreigner
# fcp1 <-  ggplot(data = data, aes(x = communeForeigner)) + geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),
# , binwidth = 10) + theme_bw() + scale_y_continuous(labels = percent)
# fcp1 + facet_grid(. ~ party_REC)
# fcp1 + facet_grid(language ~ party_REC)

