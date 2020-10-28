
# enlever les variables dans l'environnement ------------------------------
rm(list=ls())


# traitement des données pour construire ma base données ------------------

library(readxl)
X3nd_extraction_financial_data_BLOOMBERG <- read_excel("C:/Users/aziza/Desktop/Session_3/Projet de fin d'études en gestion financière/data_traiement_dans_R/ESSAI_DATA_GIT/3nd_extraction_financial_data_BLOOMBERG.xlsx")
View(X3nd_extraction_financial_data_BLOOMBERG)

# pour voir s'il y a des données manquantes
NAA <- is.na(X3nd_extraction_financial_data_BLOOMBERG) # pour savoir s'il y a des données manquantes
View(NAA) # partout ouu il ya des TRUE c'est des NA

# Méthode en haut trop long (et pas facile a interpreter), je veux juste savoir s'il y'en a ! 
any(is.na(X3nd_extraction_financial_data_BLOOMBERG)) # est-ce qu'il y en a
!any(is.na(X3nd_extraction_financial_data_BLOOMBERG)) # est-ce qu'il n'y en a pas

# nombre de valeurs manquante par ligne
rowSums(is.na(X3nd_extraction_financial_data_BLOOMBERG))


# nombre de valeurs manquante par colonne
colSums(is.na(X3nd_extraction_financial_data_BLOOMBERG))


# en général 999 dans les bases de données c'est pour dire que c'est valeur manquante
# Pour changer 999 en NA
# partout dans (X3nd_extraction_financial_data_BLOOMBERG) ou il y a 999 remplace le mois par NA
# X3nd_extraction_financial_data_BLOOMBERG [X3nd_extraction_financial_data_BLOOMBERG==999] <- NA



# pour supprimer toutes les lignes avec des NA ( na.omit(var_1))
#  var_2 <- na.omit(var_2) # résultat on aura les lignes sans NA



# attach() permet de ne pas devoir faire dataframe$var pour utiliser la variable
# une fois le attache on écrit directement le nom de la varaible exple mean(var1)
# donc ça permet de détacher chaque colonne de la base de données et de la rendre libre
# attach(X3nd_extraction_financial_data_BLOOMBERG)



# traitement des données
my_data <- data.frame(X3nd_extraction_financial_data_BLOOMBERG)
View(my_data)


#col_name
col_names <- colnames(my_data[1:19])

col_names

#extraction de mes tickers
my_tickers <- my_data$Ticker
length(my_tickers)

#extraction de mes ratings
my_ratings <- my_data$Ratings
length(my_ratings)
#View(my_ratings)

dim(my_data)

#extraction de mes dates
my_dates <- my_data$Dates
#View(my_dates)
length(my_dates)

#data sans tenir compte des tickers et ratings et Dates 
my_data.1 <- my_data[,4:ncol(my_data)]
View(my_data.1)

is.data.frame(my_data.1)




# conversion des NA en 0 --------------------------------------------------
# my_data.1[is.na(my_data.1)] <- 0



# savoir si on a des NA ? -------------------------------------------------
# une seule réponse pour voir s'il y a NA dans tout le dataframe
any(is.na(my_data.1))

fix(my_data.1)
View(my_data.1)
dim(my_data.1)






# je rajoute les dates à my_data.1 ----------------------------------------
my_data.1 <- cbind(my_dates, my_data.1)
View(my_data.1)




#calcul du nombre de mes variables (X) -----------------------------------


############################################
#le +1 devra être enlevé lorsque mes données seront good
###########################################



# à changer de car j'ai now enlever les dates de my_data.1
# not need car je l'ai rajouté de nouveau


# en fait j'ai un ticker de moins par rapport à mes données sur chaque firme
nb_var_X <- (ncol(my_data.1)-1)/(length(my_tickers)+1)   # on prend la ligne 1 de my_data, sans tenir compte de la première colonne qui contient les dates et on le divise par lenght(tickers) (par le nombre detitre que l'on a)
nb_var_X







# création de ma base de données ------------------------------------------

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



# Calculer pourcentage de data NA -----------------------------------------

p <- function(x){sum(is.na(x))/length(x)*100}
apply(data_2018, 2, p)
# on voit que pour chaque variable, on a entre 79% et 82% de NA


# chargement library pour NA data -----------------------------------------

library(VIM)

#représentation graphique des NA
# et ça transforme les NA en zéro automatiquement : c'est le package qui le fait
md.pattern(data_2018)
md.pairs(data_2018)
marginplot( data_2018[ , c("X1","X5")])



# Library pour IMPUTE data ------------------------------------------------

library(mice)

impute <- mice(data_2018, m=5, method = "logreg", maxit = 20)

#




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

#View(data_2018)
fix(data_2018)





#exporter data_2018 sur excel
data.frame(data_2018)
is.list(data_2018) #ça retourne TRUE

sapply(data_2018, class) #pour voir ce qui est liste ou pas

my_extract_data <- data.frame(sapply(data_2018, unlist))  # pour corriger problème liste

#write.csv(my_extract_data, file = "data_base_2018") # on fait l'extraction 
#write.csv(my_extract_data, file = "data_base_2018.csv") # si on oublie le .csv le dossier ne sera pas dans le wd

# write.table() aussi existe
#getwd  # pour know mon working directory





# commande pour afficher table de données

#fix(data_2018) # ne pas modifier ici car ça ne va pas rester
#View(data_2018)


# nombre de valeurs manquantes par colonne
colSums(is.na(data_2018))
rowSums(is.na(data_2018))

is.data.frame(data_2018)





# transformation des ratings ----------------------------------------------

#transformation des ratings en valeurs numériques qu'on  pourra utiliser dans notre 
# régression

# nombre de Levels différents que j'ai: pour cela je vais use factor 
class(Ratings) # pour voir classe de ratings

data_2018$Ratings <- factor(data_2018$Ratings) #convertissons Ratings qui est character en factor
is.factor(data_2018$Ratings)



stats::.getXlevels()

get_levels(data_2018$Ratings)  

#je dois faire ça pour 21 ratings diffférents donc le mettre dans une boucle
# pour cela les repertorier et les mettres dans un vecteur
data_2018$Ratings[data_2018$Ratings == "CCC+"] <- 3

fix(data_2018)










# construction de mes variables finales -----------------------------------

# division ou addition entre varaibles pour avoir good variable







# mettre rating en factor -------------------------------------------------

## mettre rating en factor (car need de spécifier leur niveau)
# idée from vidéo youtube


# après on fixe la reférence



