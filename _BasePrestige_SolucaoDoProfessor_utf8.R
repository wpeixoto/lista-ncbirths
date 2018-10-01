library(car)
data("Prestige")
? Prestige
View(Prestige)
summary(Prestige)

###########################
# an?lise explorat?ria
###########################

prestige <- Prestige
str(prestige)

# variaveis com valores nulos
colSums(is.na(prestige))
# linhas com valores nulos
prestige <- cbind(prestige, somaNulos = rowSums(is.na(prestige)))
prestige[prestige$somaNulos > 0, ]
# tratamento dos nulos
prestige[prestige$somaNulos > 0, "type"] <- "prof"
# verifica??o
colSums(is.na(prestige))
# retira somaNulos
prestige$somaNulos <- NULL
# diferen?a entre tratar e n?o tratar nulos
fit <- lm(prestige ~ . - census, data = Prestige)
summary(fit)
fit <- lm(prestige ~ . - census, data = prestige)
summary(fit)


# resumo
summary(prestige)
summary(prestige[prestige$type=="bc",])
summary(prestige[prestige$type=="prof",])
summary(prestige[prestige$type=="wc",])

boxplot(prestige ~ type, data=prestige)
boxplot(women ~ type, data=prestige)
boxplot(income ~ type, data=prestige)

# Correla??o
pairs(prestige ~ education + income + women + type,
      panel = panel.smooth,
      data = prestige, main = "Base 'Prestige'")

# corrplot
library(corrplot)
par(mfrow = c(2, 2))
corrplot(cor(prestige[,1:4]), method = "number", main="Todos os tipos")
corrplot(cor(prestige[prestige$type=="bc",1:4]), method = "number", main="bc")
corrplot(cor(prestige[prestige$type=="prof",1:4]), method = "number", main="prof")
corrplot(cor(prestige[prestige$type=="wc",1:4]), method = "number", main="wc")


cor(prestige[, 1:4])
for(x in levels(prestige$type)){
  print(x)
  print(cor(prestige[prestige$type == x, 1:4]))
}


###########################
# escolha do modelo
###########################

# tudo menos census
fit <- lm(prestige ~ . - census, data = prestige)
summary(fit)

# come?o pelos significativos, iniciando com o mais significativo
fit1 <- lm(prestige ~ education, data = prestige)
summary(fit1)
fit2 <- lm(prestige ~ education + income, data = prestige)
summary(fit2)
anova(fit1, fit2)

# variacao de women n?o ? significativa
fit3 <- lm(prestige ~ education + income + women, data = prestige)
summary(fit3)
anova(fit1, fit2, fit3)

# varia??o de type ? significativa mas os coeficientes, n?o.
fit4 <- lm(prestige ~ education + income + type, data = prestige)
summary(fit4)
anova(fit1, fit2, fit4)

# e type tem alto fator de infla??o: n?o compensa
vif(fit4) 
# al?m disso, o R2 ? praticamente o mesmo

# intera??es
fit5 <- lm(prestige ~ education * income, data = prestige)
summary(fit5)
anova(fit2,fit5)

fit5$coefficients
predict(fit5,list(education=0,income=0,type="wc"),interval = "conf")

vif(fit5) # n?o compensa a intera??o

# ent?o fechamos o modelo em fit2: prestige ~ education + income

# temos um bom modelo que explica o prest?gio, mas,
# suponha que o estudo ? sobre perstigio em fun??o do g?nero: fit2 n?o ? ?til, voltamos ao fit3
vif(fit3)

# proposta de dois modelos lineares
# retiramos income
fit6 <- lm(prestige ~ women + education, data = prestige)
summary(fit6)
plot(fit6) # a decis?o de classificar newsboy como prof deu a ele potencial de alavancagem
# acrescentamos type
fit7 <- lm(prestige ~ women + education + type, data = prestige)
summary(fit7)
plot(fit7)

anova(fit6,fit7)

vif(fit7)

# E se tirarmos education
fit8 <- lm(prestige ~ women + type, data = prestige)
summary(fit8)
plot(fit8)

#######################
# escolha: fit7
#######################
# considerando que existe alguma varia??o quando type varia de bc para wc
# e que os res?duos se aproximam de uma distribui??o normal quando a 
# vari?vel type ? adicionada


#######################
# stepwise
#######################
library(MASS)
stepw <- stepAIC(fit, direction="both", trace=FALSE)
summary(stepw)

stepw <- step(fit, direction="both", trace=FALSE)
summary(stepw)

#######################
# predi??es
#######################
fit7$coefficients

# modelo geral
x <- c(1, 0, 0, 0, 0) # intercepto
sum(fit7$coefficients*x)

# predi??o pelo modelo geral
x[1] <- 1   # intercepto
x[2] <- 50  # women
x[3] <- 10  # education
x[4] <- 0   # typeprof
x[5] <- 1   # typewc

sum(fit7$coefficients*x)

# predi??o com a fun??o predict
predict(fit7,list(education=10,women=50,type="wc"),interval = "conf")


######################################
# outra possibilidade: income na sa?da
######################################
fit <- lm(income ~ . - census, data = prestige)
summary(fit)

stepw <- step(fit, direction="both", trace=FALSE)
summary(stepw)

cor.test(prestige$prestige, prestige$women)

