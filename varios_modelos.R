
# Leitura de dados                                                        ====
NCbirths = read.csv("NCbirths.csv")
ncb = NCbirths

# Variáveis e características                                            ====
QUANTITATIVAS = c("MomAge", "Weeks",  "Gained", "BirthWeightOz")
BINARIAS = c("Sex", "Marital", "Smoke", "Low", "Premie")
CATEGORICAS = c("Plural", "RaceMom",  "HispMom", "MomRace")

source("factorize.R")

ncb <- NCB_noId(ncb)
ncb <- NCB_factorize(ncb)
ncb$BirthWeightGm = NULL  # Descarta variável redundante

for (col in QUANTITATIVAS) {
  fm = as.formula(paste(col, " ~ ."))
  s = summary(lm(fm, data=ncb))  
  print(paste(col, " -- ", s$adj.r.squared))
}

# for (col in BINARIAS) {
#   fm = as.formula(paste(col, " ~ ."))
#   s = summary(glm(fm, data=ncb, family = "binomial")) 
#   print(paste(col, " -- ", s$adj.r.squared))
# }



summary(lm(Weeks ~. -Low -Premie, data=ncb))
summary(lm(Weeks ~ BirthWeightOz + Plural, data=ncb))

# Unificar grupos étnicos em uma só coluna                              ====
unique(paste(ncb$MomRace, ncb$RaceMom, ncb$HispMom))
ncb <- cbind(ncb, Etnicidade = paste(ncb$MomRace, ncb$RaceMom, ncb$HispMom))
ncb$MomRace = NULL
ncb$RaceMom = NULL
ncb$HispMom = NULL

summary(lm(Weeks ~ ., data=ncb))
summary(lm(Gained ~ ., data=ncb))
summary(lm(BirthWeightOz ~ ., data=ncb))


