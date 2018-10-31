
NCbirths = read.csv("NCbirths.csv")
nc_births = NCbirths
# str(nc_births)
# summary(nc_births)

# col.sums = colSums(is.na(nc_births))
# col.sums[col.sums>0]  # Verificação programática de quais colunas têm NAs

nc_births<- cbind(nc_births, somaNulos = rowSums(is.na(nc_births)))
NCB_NAs = nc_births[nc_births$somaNulos > 0,]
NCBnoNAs = nc_births[complete.cases(nc_births),]

# Fatorizar o que der
source("factorize.R")

useEtn = TRUE  # Flag para unificar campos étnicos

nc_births = NCB_noId(nc_births)
nc_births = NCB_factorize(nc_births, useEtn)

#ncb_births_noGainedNA = nc_births[!is.na(nc_births$Gained), ]
# ncb_births_noNAsAtAll = nc_births[ncb_births_noGainedNA$somaNulos == 0, ]
ncb_births_noNAsAtAll = nc_births[nc_births$somaNulos == 0, ]

if (useEtn) {
  COLS_W = c("Plural", "Sex",    "MomAge", "Weeks",  "Marital",
             "Gained", "Smoke", "BirthWeightOz", "Premie", "Etnicidade")
} else {
  COLS_W = c("Plural", "Sex",    "MomAge", "Weeks",  "Marital",
             "RaceMom",  "HispMom", "Gained", "Smoke",
             "BirthWeightOz", "Premie", "MomRace")
}
ncb_weight = nc_births[, COLS_W]
# ncb_weight_noNAs = ncb_births_noGainedNA[, COLS_W]  # ncb_births_noNAsAtAll
ncb_weight_noNAs = ncb_births_noNAsAtAll[, COLS_W]  # 

ncb_weigh_patchedNA = ncb_weight
# ncb_weigh_patchedNA[ncb_weigh_patchedNA$somaNulos >0, "Gained"] = mean(ncb_weigh_patchedNA$Gained, na.rm = T)

ncb_weigh_patchedNA[is.na(ncb_weigh_patchedNA$Gained), "Gained"] = mean(ncb_weigh_patchedNA$Gained, na.rm = T)
ncb_weigh_patchedNA[is.na(ncb_weigh_patchedNA$Smoke), "Smoke"] = "N"

# $Gained[is.na(ncb_weight$Gained)] = mean(ncb_weight$Gained)



# Remover coluna desnecessária
# ncb_births_noGainedNA$somaNulos <- NULL
nc_births$somaNulos <- NULL

alias(lm(BirthWeightOz ~ . - BirthWeightGm, data = new_ncbirths(NCbirths)))
alias(lm(BirthWeightOz ~ . - BirthWeightGm, data = new_ncbirths(NCbirths, T)))

# ncb_weight_noNAs = complete.cases(ncb_weight)

fit_weight_1 = lm(BirthWeightOz ~ ., data=ncb_weight, na.action = na.omit)
sf1 = summary(fit_weight_1)
sf1

sf1.1 = summary(lm(BirthWeightOz ~ ., data=ncb_weigh_patchedNA, na.action = na.omit))
sf1.1

source("NormalCompara.R")

for (name in rownames(sf1$coefficients)) {
  c1 = sf1$coefficients
  c2 = sf1.1$coefficients
  dnormalComp(c1[name, 1], c1[name, 2], c2[name, 1], c2[name, 2], main_title = name)
}
