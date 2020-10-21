# traitement des données pour construire ma base données
rm(list=ls())


library(readxl)
X3nd_extraction_financial_data_BLOOMBERG <- read_excel("C:/Users/aziza/Desktop/Session_3/Projet de fin d'études en gestion financière/data_traiement_dans_R/GOOD/3nd_extraction_financial_data_BLOOMBERG.xlsx")
View(X3nd_extraction_financial_data_BLOOMBERG)


#col_name

col_names <- colnames(my_data[1:19])
col_names

# traitement des données
my_data <- data.frame(X3nd_extraction_financial_data_BLOOMBERG)
View(my_data)
#extraction de mes tickers
my_tickers <- my_data$Ticker
length(my_tickers)

#extraction de mes ratings
my_ratings <- my_data$Ratings
length(my_ratings)
View(my_ratings)

dim(my_data)

#extraction de mes dates
my_dates <- my_data$Dates
View(my_dates)
length(my_dates)

#data sans tenir compte des tickers et ratings et Dates 
my_data.1 <- my_data[,4:ncol(my_data)]
View(my_data.1)

is.data.frame(my_data.1)

#conversion des NA en 0

my_data.1[is.na(my_data.1)] <- 0

View(my_data.1)
dim(my_data.1)

#je rajoute les dates à my_data.1

my_data.1 <- cbind(my_dates, my_data.1)
View(my_data.1)


#calcul du nombre de mes variables (X)

############################################
#le +1 devra être enlevé lorsque mes données seront good
###########################################



# à changer de car j'ai now enlever les dates de mmy_data.1
# not need car je l'ai rajouté de nouveau


# en fait j'ai un ticker de moins par rapport à mes données sur chaque firme
nb_var_X <- (ncol(my_data.1)-1)/(length(my_tickers)+1)   # on prend la ligne 1 de my_data, sans tenir compte de la première colonne qui contient les dates et on le divise par lenght(tickers) (par le nombre detitre que l'on a)
nb_var_X


#création de ma base de données
#sélectionnons tous juste la ligne 29 qui est = data au 2018-12-31

data_2018 <- my_data.1[29,]
View(data_2018)
dim(data_2018)    # on doit faire -1 qui correspond à la colonne des dates
                  

# chaque ligne représente un ticker
#chaque colonne représente une variable

data_2018 <- matrix(data_2018[,2:ncol(data_2018)], length(my_tickers)+1, nb_var_X)

# transformons le en data.frame pour une meilleur vu
data_2018 <- data.frame(data_2018)
dim(data_2018)
View(data_2018)

#Donc je peux voir les firmes qui doivent sauter (no data pour aucune variables même si j'ai leurs ratings)
# je peux voir aussi les variables X qui doivent sauter (no data pour aucune firme)


#je peux now rajouter de nouveau les tickers et les ratings pour une meilleur vue de ce que je vais régresser

#mais pour cela comme dans ma data_base y a un ticker sauter je ne vais pas
# pouvoir le record sur un data frame car not same number of column donc 
# solution le mettre dabs ue liste et transformer la liste en data frame


#*******************************************************
## a delete juste pour les besoins de éliminer 1e ligne pour avoir 146
data_2018 <- data_2018[1:nrow(data_2018)-1,]
dim(data_2018)
#*******************************************************

#si même nb de ligne plus besoin de le put dans liste
#data_2018 <- list(my_tickers, my_ratings, data_2018[,])

data_2018 <- data.frame(my_tickers, my_ratings, data_2018[,])
View(data_2018)
# utilisons col_names pour nommer nos colonnes

colnames(data_2018) <- col_names

ncol(data_2018)

names(data_2018)


dput(names(data_2018))

#exporter data_2018 sur excel

as.data.frame(data_2018)
write.csv(data_2018)
head(data_2018)



#transformation des ratings en valeurs numériques qu'on  pourra utiliser dans notre 
# régression

my_ratings




# construction de mes variables finales
# division ou addition entre varaibles pour avoir good variable




## mettre rating en factor (car need de spécifier leur niveau)
# idée from vidéo youtube


# après on fixe la reférence



