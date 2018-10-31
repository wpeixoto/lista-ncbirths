# Rotinas auxiliares para fatorizar data frames derivados de BCbirths

NCB_noId = function(ncbvar) {
  ncbvar$X <- NULL
  ncbvar$ID <- NULL  
  return(ncbvar)
}


race_to_etn <- function(racestr, broad=F) {
  names = c(
  "white White N",
  "hispanic Hispanic P",
  "hispanic Hispanic M",
  "hispanic Hispanic S",
  "hispanic Hispanic O",
  "hispanic Hispanic C",
  "black Black N",
  "black Black P",
  "black Black S",
  "other AmericanIndian N",
  "other AmericanIndian S",
  "other Chinese N",
  "other Filipino N",
  "other OtherAsianOrPacific N"
  )
  new_detailed_values = c(
    
      "white",
      "portoriq",
      "mexican",
      "centro-south american",
      "hispanic other",
      "cuban",
      "black Black N",
      "black portoriq",
      "black south american",
      "americanIndian",
      "south americanIndian",
      "chinese",
      "filipino",
      "OtherAsianOrPacific"
    )
    
    new_broad_values = c(
        "white",
        "hisp",
        "hisp",
        "centro-south american",
        "hisp",
        "cuban",
        "black",
        "black",
        "black",
        "americanIndian",
        "south americanIndian",
        "chinese",
        "filipino",
        "OtherAsianOrPacific"
      )
      
  map = setNames(new_detailed_values, names)
  
  return(map[racestr])
  
}

NCB_factorize = function(ncbvar, etn=FALSE) {
  ncbvar$Plural <- factor(ncbvar$Plural, levels=c(1,2,3), labels=c('Single', 'Twins', 'Triplets'))
  ncbvar$Sex <- factor(ncbvar$Sex, levels=c(1,2), labels=c("M", "F"))
  ncbvar$Marital <- factor(ncbvar$Marital, labels=c("Married", "Not Married"))
  ncbvar$RaceMom <- factor(ncbvar$RaceMom, levels=c(1,2,3,4,5,7,8),  # There's no Hawaiian in this base
                           labels=c(
                             "White",
                             "Black",
                             "AmericanIndian",
                             "Chinese",
                             "Hispanic",  # Era "Japanese", mas foi reconsiderado
                             # "Hawaiian",  # Não foram encontrados registros
                             "Filipino",
                             "OtherAsianOrPacific"
                           ))
  # TODO: Check race assignments more thoroughly
  
  # Transformar todas as colunas étnicas em uma só
  if (etn) {
    ncbvar <- cbind(
      ncbvar, 
      Etnicidade = race_to_etn(paste(ncbvar$MomRace, ncbvar$RaceMom, ncbvar$HispMom))
    )
    ncbvar$MomRace <- NULL  # limpar 
    ncbvar$RaceMom <- NULL  # limpar 
    ncbvar$HispMom <- NULL  # limpar 
  } else {
    ncbvar$HispMom <- factor(ncbvar$HispMom)
  }

  
  ncbvar$Smoke <- factor(ncbvar$Smoke, labels = c('N', 'Y'))  # Cuidado com os NAs
  ncbvar$Premie <- factor(ncbvar$Premie, labels=c("N", "Y"))
  ncbvar$Low <- factor(ncbvar$Low, labels=c("N", "Y"))
  
  return(ncbvar)
}


new_ncbirths <- function(ncbvar, etn=F) {
  return(NCB_factorize(NCB_noId(ncbvar), etn))
}