# List of generic functions for sanity check

#######
# quick look at nb of NAs or missing values in selected colnames rom your DATA file: a df or matrix

ColNames<-c("X","Y")

NAs<-sapply(ColNames,function(x){
  table(is.na(DATA[,x])|DATA[,x]=="")
})

print(NAs)

##CODE FOR REPLACING "" or NA with "MISSING"

x <- which(basePts=="", arr.ind=TRUE)
y <- which(is.na(basePts), arr.ind=TRUE)

na <- function (db){
  y <- which(is.na(db), arr.ind=TRUE)
  x <- which(db=="", arr.ind=TRUE)

  for(i in 1:nrow(x)){
    numLigne1 <- x[i][1]
    numCol1 <- x[i][2]
    numLigne2 <- y[i][1]
    numCol2 <- y[i][2]
  
    db[numLigne1,numCol1] = "MISSING"
    db[numLigne2,numCol2] = "MISSING"
  }  
}

PROBLEME :  #message d'erreur de R :  Error in `[<-.data.frame`(`*tmp*`, numLigne1, numCol1, value = "MISSING") :
    #missing values are not allowed in subscripted assignments of data frames 

##CODE FOR FINDING DOUBLONS IN DATABASE


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

doublon(basePts)

## CODE FOR DELETE DOUBLON IN DATABASE

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
