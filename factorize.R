# Rotinas auxiliares para fatorizar data frames derivados de BCbirths

NCB_noId = function(ncbvar) {
  ncbvar$X <- NULL
  ncbvar$ID <- NULL  
  return(ncbvar)
}


NCB_factorize = function(ncbvar) {
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
  
  ncbvar$Smoke <- factor(ncbvar$Smoke, labels = c('N', 'Y'))  # Cuidado com os NAs
  ncbvar$HispMom <- factor(ncbvar$HispMom)
  ncbvar$Premie <- factor(ncbvar$Premie, labels=c("N", "Y"))
  ncbvar$Low <- factor(ncbvar$Low, labels=c("N", "Y"))
  
  return(ncbvar)
}