library( neuralnet )
library( hydroGOF )
library( leaps )
library( arules )
library(dplyr)

dados <- read.table(
  "C:\\Users\\José\\Desktop\\3Ano\\SRCR\\TP\\TP3\\bank-additional.csv",
  sep=";", header=TRUE)


dados$job <- as.numeric(dados$job)
dados$marital <- as.numeric(dados$marital)
dados$education <- as.numeric(dados$education) 
dados$default <- as.numeric(dados$default)
dados$housing <- as.numeric(dados$housing)
dados$loan <- as.numeric(dados$loan)
dados$contact <- as.numeric(dados$contact)
dados$month <- as.numeric(dados$month)
dados$day_of_week <- as.numeric(dados$day_of_week)
dados$poutcome <- as.numeric(dados$poutcome)
dados$y <- as.numeric(dados$y)

summary(dados)

desvPad <- apply(dados, 2, sd)
View(desvPad)

dat2 <- dados %>% mutate_each_(funs(scale(.) %>% as.vector), 
              vars=c("age","job"))

dados.scaled <- as.data.frame(scale(dados))

dados.scaled$y <- (dados$y)+9


funcao <- y ~ age+job+marital+education+default+housing+loan+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed


selecao2 <- regsubsets(funcao,dados.scaled,nvmax=10)
summary(selecao2)

selecao <- regsubsets(funcao,dados.scaled,method="backward")
summary(selecao)


formula1 <- y ~ nr.employed + pdays + month + euribor3m


treino1 <- dados.scaled[1:3001, ]
teste1 <- dados.scaled[3002:4119, ]


aux1 <- dados.scaled[dados.scaled$y != 1, ]
aux2 <- dados.scaled[dados.scaled$y == 1, ]

ptr1 <- aux1[1:300, ]
ptr2 <- aux2[1:600, ]

pt1 <- aux1[301:451, ]
pt2 <- aux2[601:1500, ]


treino2 <- rbind(ptr1, ptr2)
teste2 <- rbind(pt1, pt2)

rna1 <- neuralnet( formula1,treino1,hidden=c(4,3),lifesign="full", algorithm = 'rprop+' ,threshold=0.05 )

rna2 <- neuralnet( formula1,treino2,hidden=c(4,3),lifesign="full",threshold=0.1 )
rna1B <- neuralnet( formula1,treino1,hidden=c(2,3,2),lifesign="full",threshold=0.07 )


teste1.sub <- subset(teste1,select = c("nr.employed","pdays","month","euribor3m"))
teste2.sub <- subset(teste2,select = c("nr.employed","pdays","month","euribor3m"))

rna1.resultados <- compute(rna1,teste1.sub)
rna1B.resultados <- compute(rna1B,teste1.sub)

rna2.resultados <- compute(rna2,teste2.sub)

resultados1 <- data.frame(atual=teste1$y,previsao=rna1.resultados$net.result)
resultados1B <- data.frame(atual=teste1$y,previsao=rna1B.resultados$net.result)

resultados2 <- data.frame(atual=teste2$y,previsao=rna2.resultados$net.result)

resultados1$previsao <- round(resultados1$previsao,digits = 0)
resultados1B$previsao <- round(resultados1B$previsao,digits = 0)

resultados2$previsao <- round(resultados2$previsao,digits = 0)


rmse(c(teste1$y),c(resultados1$previsao))
rmse(c(teste1$y),c(resultados1B$previsao))

rmse(c(teste2$y),c(resultados2$previsao))

quantosV1 <- resultados1$previsao[resultados1$previsao!=10] 
quantosV1B <- resultados1B$previsao[resultados1B$previsao!=1] 

View(quantosV1)

quantosV2 <- resultados2$previsao[resultados2$previsao!=1] 

View(quantosV2)


diferentes1 <- resultados1[resultados1$previsao != resultados1$atual, ]
diferentes1B <- resultados1B[resultados1B$previsao != resultados1B$atual, ]


qua1d1 <- diferentes1[diferentes1$previsao!=1, ]
qua2d1 <- diferentes1[diferentes1$previsao!=2, ]
qua1d1B <- diferentes1B[diferentes1B$previsao!=1, ]
qua2d1B <- diferentes1B[diferentes1B$previsao!=2, ]
