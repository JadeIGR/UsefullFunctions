# List of generic functions for sanity check

#######
# quick look at nb of NAs or missing values in selected colnames rom your DATA file: a df or matrix

ColNames<-c("X","Y")      #ColNames<-c(names(basePts))

findNAs<-sapply(X=ColNames,function(x){
  table(is.na(DATA[,X])|DATA[,X]==""|is.null(DATA[,X]))
})

print(findNAs)

#### attribuer à chaque case vide la valeur NA    OK

fixNAs <- function(db){
  for (i in 1:length(ColNames)){
    for (j in 1:length(ColNames)){
      if (is.null(db[i,j])){
        db[i,j]=NA
      }
    }
  }
}

fixNAs(basePts)
head(basePts)

###TROUVER DOUBLONS     A REVOIR (!)

doublon = function(df){ #retourne le nom des colonnes existant en double & change nom de la col en double
  
  c <- colnames(df)
  nomColonne <- gsub(pattern = "\\.[0123456789]","",x = c)
  db <- c()
  col_double <- c()
  
  for (i in 1:length(nomColonne)) {
    for (j in 1:length(nomColonne)) {
      if(nomColonne[i] == nomColonne[j] & i!=j) {
        db <- c(db,nomColonne[i])
        nomColonne[j] <- paste(nomColonne[j],".1",sep = "")
        
        col_double <- c(col_double,nomColonne[j])
      }
    }
  }
  
  duplic_db <- which(duplicated(db))
  duplic_colDouble <- which(duplicated(col_double))
  rep <- db[-duplic_db]
  rep2 <- col_double[-duplic_colDouble]
  
  reponse<-list("rep1"=rep,"rep2"=rep2)
  print(colnames(df))
  return(reponse)
  
}

supprimer_doublon = function(df){     #supprime la colonne en double
  for(nomCol in doublon(df)[1]){
    for(nomColDouble in doublon(df)[2] ){
      if(df[nomCol] == df[nomColDouble] | grep(pattern = "\\.[:digit:]",  df[nomCol])){
        print (paste("La colonne"), nomCol, ("existait en deux fois. Elle a été supprimée."))
        df_witout_db <- subset(df,nomCol2)
      }
    }
  }
  return (head(df_witout_db))
}

supprimer_doublon(basePts)

#if(grep(pattern="\\.1", nomColonne[j])){
 # nomColonne[j]<- gsub(pattern = "\\.[0123456789]","",nomColonne[j])
 # nomColonne[j]<-paste(nomColonne[j],".2",sep = "")
#}

####Check type des variables

type_is_good = function (db){
  type <-str(db)
}

type_is_good(basePts)
