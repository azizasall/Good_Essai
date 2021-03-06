# traitement des donn�es pour construire ma base donn�es

library(readxl)
X3nd_extraction_financial_data_BLOOMBERG <- read_excel("C:/Users/aziza/Desktop/Session_3/Projet de fin d'�tudes en gestion financi�re/data_traiement_dans_R/GOOD/3nd_extraction_financial_data_BLOOMBERG.xlsx")
View(X3nd_extraction_financial_data_BLOOMBERG)


# traitement des donn�es
my_data <- data.frame(X3nd_extraction_financial_data_BLOOMBERG)

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

#je rajoute les dates � my_data.1

my_data.1 <- cbind(my_dates, my_data.1)
View(my_data.1)


#calcul du nombre de mes variables (X)

############################################
#le +1 devra �tre enlev� lorsque mes donn�es seront good
###########################################



# � changer de car j'ai now enlever les dates de mmy_data.1
# not need car je l'ai rajout� de nouveau


# en fait j'ai un ticker de moins par rapport � mes donn�es sur chaque firme
nb_var_X <- (ncol(my_data.1)-1)/(length(my_tickers)+1)   # on prend la ligne 1 de my_data, sans tenir compte de la premi�re colonne qui contient les dates et on le divise par lenght(tickers) (par le nombre detitre que l'on a)
nb_var_X


#cr�ation de ma base de donn�es
#s�lectionnons tous juste la ligne 29 qui est = data au 2018-12-31

data_2018 <- my_data.1[29,]
View(data_2018)
dim(data_2018)    # on doit faire -1 qui correspond � la colonne des dates
                  

# chaque ligne repr�sente un ticker
#chaque colonne repr�sente une variable

data_2018 <- matrix(data_2018[,2:ncol(data_2018)], length(my_tickers)+1, nb_var_X)

# transformons le en data.frame pour une meilleur vu
data_2018 <- data.frame(data_2018)
dim(data_2018)
View(data_2018)

#Donc je peux voir les firmes qui doivent sauter (no data pour aucune variables m�me si j'ai leurs ratings)
# je peux voir aussi les variables X qui doivent sauter (no data pour aucune firme)


#je peux now rajouter de nouveau les tickers et les ratings pour une meilleur vue de ce que je vais r�gresser

data_2018 <- data.frame(my_tickers, my_ratings, data_2018)
View(data_2018)
