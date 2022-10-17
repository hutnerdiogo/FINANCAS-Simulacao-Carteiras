#### Caso de error, rode essas duas linhas: ####
#install.packages('zoo')
#install.packages('quantmod')
#install.packages('fPortfolio')

#### Iniciando os pacotes ####
library('quantmod')
library('zoo')
library('fPortfolio')

#### Criando a função ####
#' Basicamente, funções são conjuntos de codigos que são executados, mudando algumas variaveis
#' nesse caso a função chama "get_adj_close_values_from_papers" e quando iniciar ela
#' são passadas variaveis que são declaradas aqui ⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄
get_adj_close_values_from_papers <- function(PAPER, from='2010-12-01', to='2022-10-01'){
  df_intc <- getSymbols(PAPER,
                        src='yahoo',
                        from=from,
                        to=to,
                        periodicity="monthly",
                        auto.assign=FALSE)
  only_close <- df_intc[,6]
  names(only_close) <- PAPER
  return(only_close)
}

risco_retorno_portfolio_ingenuo <- function(retornos){
  portfolio_uniforme <- matrix(rep(1/length(colnames(retornos)),length(colnames(retornos))))
  media_retornos <- matrix(sapply(retornos,mean))
  risco_retornos <- cov(retornos)
  risco_portfolio_uniforme <- t(portfolio_uniforme) %*% risco_retornos %*% portfolio_uniforme
  retorno_portfolio_uniforme <- t(portfolio_uniforme) %*% media_retornos
  output <- matrix(c(risco_portfolio_uniforme^.5, retorno_portfolio_uniforme),nrow=2,ncol=1)
  rownames(output) <- c("risco", "retorno")
  return(output) 
}

price_to_variation <- function(retornos){
  variacao <- (retornos-lag(retornos))/lag(retornos)
  variacao <- variacao[-1]
  return(variacao)
}


#' Resumindo a opera, toda vez que vc chamar rodar esse codigo ->
#' e nesse lugar você colocar o papel (⌄⌄⌄⌄⌄⌄) que deseja, ele irá retornar a coluna
fechamento_ajustado_vale <- get_adj_close_values_from_papers("VALE3.SA")
fechamento_ajustado_vale
#' de fechamento ajustados do papel (da lista de papeis que mandei) para a variavel
#### Montando o banco de dados ####

# Criando a estrutura do banco de dados:
# Basicamente colocando todas as datas entre periodos, nesse caso to pegando
# só o primeiro dia de cada mes, por conta de quando pegamos os valores mensais
# vem o valor de fechamento do primeiro dia do mes

periodo <- seq(as.Date("2010-12-01"), as.Date("2022-10-12"), by = "day")
periodo <- periodo[strftime(periodo, '%d') %in% "01"]
base_dados <- zoo(,order.by = periodo)


# Escreva nesse ativos_desejados os ativos para se construirem o banco de dados
# Exemplo, quer o ativo "VALE3.SA" e "PETR3.SA", pode ir quantos quiser
# ativos_desejados <- c("VALE3.SA", "PETR3.SA") 
# *** TEM QUE ESTAR COM O NOME IGUAL O DA LISTA QUE PASSEI 
ativos_desejados <-  c("%5EBVSP","RRRP3.SA","ALPA4.SA","ABEV3.SA","AMER3.SA",
                       "ARZZ3.SA","ASAI3.SA","AZUL4.SA","B3SA3.SA","BPAN4.SA",
                       "BBSE3.SA","BRML3.SA","BBDC3.SA","BBDC4.SA","BRAP4.SA",
                       "BBAS3.SA","BRKM5.SA","BRFS3.SA","BPAC11.SA","CRFB3.SA",
                       "CCRO3.SA","CMIG4.SA","CIEL3.SA","COGN3.SA","CPLE6.SA",
                       "CSAN3.SA","CPFE3.SA","CMIN3.SA","CVCB3.SA","CYRE3.SA",
                       "DXCO3.SA","ECOR3.SA","ELET3.SA","ELET6.SA","EMBR3.SA",
                       "ENBR3.SA","ENGI11.SA","ENEV3.SA","EGIE3.SA","EQTL3.SA",
                       "EZTC3.SA","FLRY3.SA","GGBR4.SA","GOAU4.SA","GOLL4.SA",
                       "NTCO3.SA","SOMA3.SA","HAPV3.SA","HYPE3.SA","IGTI11.SA",
                       "IRBR3.SA","ITSA4.SA","ITUB4.SA","JBSS3.SA","KLBN11.SA",
                       "RENT3.SA","LWSA3.SA","LREN3.SA","MGLU3.SA","MRFG3.SA",
                       "CASH3.SA","BEEF3.SA","MRVE3.SA","MULT3.SA","PCAR3.SA",
                       "PETR3.SA","PETR4.SA","PRIO3.SA","PETZ3.SA","POSI3.SA",
                       "QUAL3.SA","RADL3.SA","RAIZ4.SA","RDOR3.SA","RAIL3.SA",
                       "SBSP3.SA","SANB11.SA","SMTO3.SA","CSNA3.SA","SLCE3.SA",
                       "SULA11.SA","SUZB3.SA","TAEE11.SA","VIVT3.SA","TIMS3.SA",
                       "TOTS3.SA","UGPA3.SA","USIM5.SA","VALE3.SA","VIIA3.SA",
                       "VBBR3.SA","WEGE3.SA","YDUQ3.SA")

#' Basicamente isso vai listar cada "ativo" em ativos desejados, ou seja, 
#' no caso do VALE3.SA e PETR3.SA, vai rodar o codigo inteiro uma vez para o VALE
#' e depois uma vez inteira para PETR
for (ativo in ativos_desejados){
  fechamento_ativo <- get_adj_close_values_from_papers(ativo)
  base_dados <- merge.zoo(base_dados, fechamento_ativo)
}
# Selic
selic_bruto <- read.csv("SELIC.csv",sep=";",dec=",")
selic_bruto[,1] <- as.Date(selic_bruto[,1],"%d/%m/%Y")
selic <- zoo(selic_bruto[,-1],order.by = selic_bruto[,1])
base_dados <- merge.zoo(base_dados,selic)
base_dados <- base_dados[-1:-11,]
base_dados <- base_dados[-145:-143,]
base_dados[,'selic'] <- base_dados[,'selic']/10

# Salvando o banco de dados 
write.zoo(base_dados, file = "BaseDeDados.zoo", 
          index.name = "Index", row.names = FALSE, col.names = T)

# Para carregar o banco de dados
file <- read.csv("BaseDeDados.zoo",sep=" ")
datas <- file[,1]
file <- file[,-1]
base_2 <- zoo(file,order.by = datas)
#base_dados <- base_2


# Cortando o banco de dados amostral: 
banco_dados_estimacao <- base_dados[1:round(length(base_dados[,5])* 73/100),]


#### Montar Portfolio ####
portfolio_pequeno <- c("BBDC3.SA","CPLE6.SA","ENGI11.SA","HYPE3.SA","TIMS3.SA")
portfolio_medio <- c(portfolio_pequeno, "VIVT3.SA","RADL3.SA","MRFG3.SA","ENBR3.SA","BRML3.SA")
portfolio_grande <- c(portfolio_medio,"DXCO3.SA","GGBR4.SA","PRIO3.SA","CCRO3.SA","CYRE3.SA")

portfolio_pequeno_retornos <- as.timeSeries(price_to_variation(banco_dados_estimacao[,portfolio_pequeno]))
portfolio_medio_retornos <- as.timeSeries(price_to_variation(banco_dados_estimacao[,portfolio_medio]))
portfolio_grande_retornos <- as.timeSeries(price_to_variation(banco_dados_estimacao[,portfolio_grande]))

var_ibov <- as.timeSeries(price_to_variation(banco_dados_estimacao[,"X.5EBVSP"]))
sd(var_ibov)
mean(var_ibov)

##### Calculando a fronteira com o portfolio pequeno #####
spec <- portfolioSpec()
setNFrontierPoints(spec) <- 100
setRiskFreeRate(spec) <- mean(banco_dados_estimacao[,'selic'])


portfolio_pequeno.fronteira <- portfolioFrontier(portfolio_pequeno_retornos,spec)

titulo <-  paste("Analise de portfolio Risco e Retorno \n")
subtitulos <- paste("Ativos: ", paste(portfolio_pequeno,collapse = ', '))

frontierPlot(portfolio_pequeno.fronteira, col = c('blue', 'red'), pch = 20,
             risk="VaR", title = F)

title(main=titulo, 
      sub=subtitulos,
      xlab="Risco",
      ylab="Retorno",
      cex.lab=1.5)


monteCarloPoints(portfolio_pequeno.fronteira, mcSteps = 5000, pch = 20, cex = 0.25,
                 col="Grey")

for (ativo in portfolio_pequeno){
  media <- mean(portfolio_pequeno_retornos[,ativo])
  risco <- sd(portfolio_pequeno_retornos[,ativo])
  paste("Risco: ",risco,"\n Media: ", media, "\n Ativo:", ativo)
  points(risco,media,col="Black")
  text(risco,media,ativo,col="Brown", pos = 2)
}

points(sd(var_ibov),mean(var_ibov),col="Black",pch=19)
text(sd(var_ibov),mean(var_ibov),'Ibovespa',col="Black",pos = 3)

portfolio_pequeno.ingenuo <- risco_retorno_portfolio_ingenuo(portfolio_pequeno_retornos)

points(portfolio_pequeno.ingenuo["risco",],portfolio_pequeno.ingenuo["retorno",],col="Black",pch=19)
text(portfolio_pequeno.ingenuo["risco",],portfolio_pequeno.ingenuo["retorno",],'Carteira Ingenua',col="Black",pos = 3)

portfolio_pequeno.min_risk <- minriskPortfolio(portfolio_pequeno_retornos)
portfolio_pequeno.tangente <- tangencyPortfolio(portfolio_pequeno_retornos)


##### Calculando a fronteira com o portfolio pequeno com codigo proprio#####
tent_portfolio_pequeno.retornos <- matrix(sapply(portfolio_pequeno_retornos, mean))

row.names(tent_portfolio_pequeno.retornos) <- colnames(portfolio_pequeno_retornos)

tent_portfolio_pequeno.covarianca <- cov(portfolio_pequeno_retornos)

tent_portfolio_pequeno.retornos

pesos_brutos <- inv(tent_portfolio_pequeno.covarianca) %*% matrix(c(rep(1,length(colnames(portfolio_pequeno_retornos)))))
pesos <- matrix(,nrow=length(portfolio_pequeno))
rownames(pesos) <- portfolio_pequeno
for (ativo in rownames(pesos_brutos)){
  pesos[ativo,] <- pesos_brutos[ativo,]/sum(pesos_brutos)
}


retorno <- t(pesos) %*% tent_portfolio_pequeno.retornos

risco <- t(pesos) %*% tent_portfolio_pequeno.covarianca %*% pesos

retorno
risco^(.5)

portfolio_pequeno.min_risk
##### Calculando a fronteira com o portfolio medio #####

portfolio_medio.fronteira <- portfolioFrontier(portfolio_medio_retornos,spec)

titulo <-  paste("Analise de portfolio Risco e Retorno \n")
subtitulos <- paste("Ativos: ", paste(portfolio_medio,collapse = ', '))

frontierPlot(portfolio_medio.fronteira, col = c('blue', 'red'), pch = 20,
             risk="VaR", title = F)

title(main=titulo, 
      sub=subtitulos,
      xlab="Risco",
      ylab="Retorno",
      cex.lab=1.5)


monteCarloPoints(portfolio_medio.fronteira, mcSteps = 10000, pch = 20, cex = 0.25,
                 col="Grey")

for (ativo in portfolio_medio){
  media <- mean(portfolio_medio_retornos[,ativo])
  risco <- sd(portfolio_medio_retornos[,ativo])
  paste("Risco: ",risco,"\n Media: ", media, "\n Ativo:", ativo)
  points(risco,media,col="Black")
  text(risco,media,ativo,col="Brown", pos = 2)
}

points(sd(var_ibov),mean(var_ibov),col="Black",pch=19)
text(sd(var_ibov),mean(var_ibov),'Ibovespa',col="Black",pos = 3)

portfolio_medio.ingenuo <- risco_retorno_portfolio_ingenuo(portfolio_medio_retornos)

points(portfolio_medio.ingenuo["risco",],portfolio_medio.ingenuo["retorno",],col="Black",pch=19)
text(portfolio_medio.ingenuo["risco",],portfolio_medio.ingenuo["retorno",],'Carteira Ingenua',col="Black",pos = 3)


portfolio_medio.min_risk <- minriskPortfolio(portfolio_medio_retornos)
portfolio_medio.tangente <- tangencyPortfolio(portfolio_medio_retornos)

##### Calculando a fronteira com o portfolio grande#####

portfolio_grande.fronteira <- portfolioFrontier(portfolio_grande_retornos,spec)

titulo <-  paste("Analise de portfolio Risco e Retorno \n")
subtitulos <- paste("Ativos:", paste(portfolio_grande[1:8],collapse = ', '),
                    '\n',paste(portfolio_grande[9:15],collapse = ', '))

frontierPlot(portfolio_grande.fronteira, col = c('blue', 'red'), pch = 20,
             risk="VaR", title = F)

title(main=titulo, 
      xlab="Risco",
      ylab="Retorno",
      cex.lab=1.5)


monteCarloPoints(portfolio_grande.fronteira, mcSteps = 40000, pch = 20, cex = 0.25,
                 col="Grey")

for (ativo in portfolio_grande){
  media <- mean(portfolio_grande_retornos[,ativo])
  risco <- sd(portfolio_grande_retornos[,ativo])
  paste("Risco: ",risco,"\n Media: ", media, "\n Ativo:", ativo)
  points(risco,media,col="Green",pch=19)
  text(risco,media,ativo,col="Brown", pos = 2,cex=0.5)
}

points(sd(var_ibov),mean(var_ibov),col="Black",pch=19)
text(sd(var_ibov),mean(var_ibov),'Ibovespa',col="Black",pos = 3)

portfolio_grande.ingenuo <- risco_retorno_portfolio_ingenuo(portfolio_grande_retornos)

points(portfolio_grande.ingenuo["risco",],portfolio_grande.ingenuo["retorno",],col="Black",pch=19)
text(portfolio_grande.ingenuo["risco",],portfolio_grande.ingenuo["retorno",],'Carteira Ingenua',col="Black",pos = 3)

portfolio_grande.min_risk <- minriskPortfolio(portfolio_grande_retornos)
portfolio_grande.tangente <- tangencyPortfolio(portfolio_grande_retornos)


#### Fronteiras ####
resultado_pequeno <- matrix(,nrow=length(getPortfolio(portfolio_pequeno.fronteira)$targetRisk[,1]),ncol=7)
colnames(resultado_pequeno) <- c("VaR","mean",portfolio_pequeno)
resultado_pequeno[,"VaR"] <- round(getPortfolio(portfolio_pequeno.fronteira)$targetRisk[,"CVaR"],4)
resultado_pequeno[,"mean"] <- round(getPortfolio(portfolio_pequeno.fronteira)$targetReturn[,"mean"],4)
resultado_pequeno[,portfolio_pequeno] <- round(getPortfolio(portfolio_pequeno.fronteira)$weights,4)
plot(resultado_pequeno[,c("VaR","mean")])

resultado_medio <- matrix(,nrow=length(getPortfolio(portfolio_medio.fronteira)$targetRisk[,1]),ncol=2+length(portfolio_medio))
colnames(resultado_medio) <- c("VaR","mean",portfolio_medio)
resultado_medio[,"VaR"] <- round(getPortfolio(portfolio_medio.fronteira)$targetRisk[,"CVaR"],4)
resultado_medio[,"mean"] <- round(getPortfolio(portfolio_medio.fronteira)$targetReturn[,"mean"],4)
resultado_medio[,portfolio_medio] <- round(getPortfolio(portfolio_medio.fronteira)$weights,4)
plot(resultado_medio[,c("VaR","mean")])

resultado_grande <- matrix(,nrow=length(getPortfolio(portfolio_grande.fronteira)$targetRisk[,1]),ncol=2+length(portfolio_grande))
colnames(resultado_grande) <- c("VaR","mean",portfolio_grande)
resultado_grande[,"VaR"] <- round(getPortfolio(portfolio_grande.fronteira)$targetRisk[,"CVaR"],4)
resultado_grande[,"mean"] <- round(getPortfolio(portfolio_grande.fronteira)$targetReturn[,"mean"],4)
resultado_grande[,portfolio_grande] <- round(getPortfolio(portfolio_grande.fronteira)$weights,4)
plot(resultado_grande[,c("VaR","mean")])


write.table(resultado_pequeno,"resultado_pequeno.csv",sep=';',dec=',')
write.table(resultado_medio,"resultado_medio.csv",sep=';',dec=',')
write.table(resultado_grande,"resultado_grande.csv",sep=';',dec=',')

#### Analise do Ibovespa ####


portfolio_ibov <- c("RRRP3.SA","ALPA4.SA","ABEV3.SA","AMER3.SA",
                 "ARZZ3.SA","ASAI3.SA","AZUL4.SA","B3SA3.SA","BPAN4.SA",
                 "BBSE3.SA","BRML3.SA","BBDC3.SA","BBDC4.SA","BRAP4.SA",
                 "BBAS3.SA","BRKM5.SA","BRFS3.SA","BPAC11.SA","CRFB3.SA",
                 "CCRO3.SA","CMIG4.SA","CIEL3.SA","COGN3.SA","CPLE6.SA",
                 "CSAN3.SA","CPFE3.SA","CMIN3.SA","CVCB3.SA","CYRE3.SA",
                 "DXCO3.SA","ECOR3.SA","ELET3.SA","ELET6.SA","EMBR3.SA",
                 "ENBR3.SA","ENGI11.SA","ENEV3.SA","EGIE3.SA","EQTL3.SA",
                 "EZTC3.SA","FLRY3.SA","GGBR4.SA","GOAU4.SA","GOLL4.SA",
                 "NTCO3.SA","SOMA3.SA","HAPV3.SA","HYPE3.SA","IGTI11.SA",
                 "IRBR3.SA","ITSA4.SA","ITUB4.SA","JBSS3.SA","KLBN11.SA",
                 "RENT3.SA","LWSA3.SA","LREN3.SA","MGLU3.SA","MRFG3.SA",
                 "CASH3.SA","BEEF3.SA","MRVE3.SA","MULT3.SA","PCAR3.SA",
                 "PETR3.SA","PETR4.SA","PRIO3.SA","PETZ3.SA","POSI3.SA",
                 "QUAL3.SA","RADL3.SA","RAIZ4.SA","RDOR3.SA","RAIL3.SA",
                 "SBSP3.SA","SANB11.SA","SMTO3.SA","CSNA3.SA","SLCE3.SA",
                 "SULA11.SA","SUZB3.SA","TAEE11.SA","VIVT3.SA","TIMS3.SA",
                 "TOTS3.SA","UGPA3.SA","USIM5.SA","VALE3.SA","VIIA3.SA",
                 "VBBR3.SA","WEGE3.SA","YDUQ3.SA")

portfolio_ibov.retornos <- as.timeSeries(log(lag(banco_dados_estimacao[,portfolio_ibov])/banco_dados_estimacao[,portfolio_ibov]))
portfolio_ibov.have_retornos <- portfolio_ibov.retornos[,!as.logical(colSums(is.na(portfolio_ibov.retornos)))]
portfolio_ibov.frontiers <- portfolioFrontier(portfolio_ibov.have_retornos,spec)

frontierPlot(portfolio_ibov.frontiers, col = c('blue', 'red'), pch = 20,
             risk="VaR", title = F)

monteCarloPoints(portfolio_ibov.frontiers, mcSteps = 4000, pch = 20, cex = 0.25,
                 col="Grey")


title(main="Analise do Portfolio IBOVESPA", 
      xlab="Risco (Desvio padrão)",
      ylab="Retorno",
      cex.lab=1.5)


for (ativo in colnames(portfolio_ibov.have_retornos)){
  media <- mean(portfolio_ibov.have_retornos[,ativo])
  risco <- sd(portfolio_ibov.have_retornos[,ativo])
  points(risco,media,col="Green",pch=19)
}

points(sd(var_ibov),mean(var_ibov),col="Black",pch=19)
text(sd(var_ibov),mean(var_ibov),'Ibovespa',col="Black",pos = 1)

portfolio_uniforme.uniforme <- risco_retorno_portfolio_ingenuo(portfolio_ibov.have_retornos)
points(portfolio_uniforme.uniforme['risco',],portfolio_uniforme.uniforme['retorno',],col="Black",pch=19)
text(portfolio_uniforme.uniforme['risco',],portfolio_uniforme.uniforme['retorno',],'Carteira Uniforme',col="Black",pos = 3)



portfolio_ibov.min_risk <- minriskPortfolio(portfolio_ibov.have_retornos,spec)
portfolio_ibov.tangente <- tangencyPortfolio(portfolio_ibov.have_retornos,spec)

#### Portfolios Setoriais ####

setores <- read.csv("SetorEconomico.csv",sep=";")
tipo_setores <- unique(setores[,'Setor'])
quantidade_por_setor <- matrix(,nrow=(length(tipo_setores)))
rownames(quantidade_por_setor) <- tipo_setores

for (item in tipo_setores){
  quantidade_bruta_por_setor <- setores[setores[,'Setor'] == item,'Papel']
  quantidade_bruta_por_setor[quantidade_bruta_por_setor == ""] <- NA
  quantidade_bruta_por_setor <- quantidade_bruta_por_setor[quantidade_bruta_por_setor %in% ativos_desejados]
  total <- sum(!is.na(quantidade_bruta_por_setor))
  quantidade_por_setor[item,] <- total
}

nome_setores <- rownames(quantidade_por_setor)[quantidade_por_setor[,1] > 10]
# nome_setores <- c("Materiais Básicos","Bens Industriais","Consumo não Cíclico",
#              "Utilidade Pública","Financeiro")

portfolios <- c()
for (setor in nome_setores){
  name_var <- tolower(paste("portfolio_",iconv(gsub(" ","_",setor,fixed = T),to="ASCII//TRANSLIT"),sep=""))
  portfolios <- c(portfolios, name_var)
  ativos <- setores[setores[,'Setor'] == setor,'Papel']
  ativos[ativos == ""] <- NA
  ativos <- ativos[!is.na(ativos)]
  ativos <- ativos[ativos %in% ativos_desejados]
  portfolio_retorno <- as.timeSeries(log(lag(banco_dados_estimacao[,ativos])/banco_dados_estimacao[,ativos]))
  portfolio_retorno2 <- portfolio_retorno[,!colSums(is.na(portfolio_retorno)) > 1]
  assign(paste(name_var,".retorno",sep=""), portfolio_retorno2)
}

for (portfolio in portfolios){
  assign(paste(portfolio,".ingenua",sep=""),
         risco_retorno_portfolio_ingenuo(get(paste(portfolio,".retorno",sep=""))))
}

for (portfolio in portfolios){
  assign(paste(portfolio,".fronteira",sep=""),
         portfolioFrontier(get(paste(portfolio,".retorno",sep=""))))
}

for (portfolio in portfolios){
  png(file=paste("Graficos/",portfolio,".png",sep=''))
  frontierPlot(get(paste(portfolio,".fronteira",sep="")), col = c('blue', 'red'), pch = 20,
               risk="VaR", title = F)
  
  monteCarloPoints(get(paste(portfolio,".fronteira",sep="")), mcSteps = 4000, pch = 20, cex = 0.25,
                   col="Grey")
  
  title(main=paste(strsplit(portfolio,"_")[[1]][1],"do setor",paste(strsplit(portfolio,"_")[[1]][-1],collapse = ' '), collapse = ' '), 
        xlab="Risco (Desvio padrão)",
        ylab="Retorno",
        cex.lab=1.5)
  
  points(sd(var_ibov),mean(var_ibov),col="Black",pch=19)
  text(sd(var_ibov),mean(var_ibov),'Ibovespa',col="Black",pos = 1)
  
  
  dev.off()
}

#### Analise de todos os portfolios ####


portfolios_min_risks <- c("portfolio_ibov.min_risk","portfolio_grande.min_risk",
                          "portfolio_medio.min_risk","portfolio_pequeno.min_risk")
for (portfolio in portfolios){
  var_name <- paste(portfolio,".minrisk",sep="")
  assign(var_name,
         minriskPortfolio(get(paste(portfolio,".retorno",sep=""))))
  portfolios_min_risks <- c(portfolios_min_risks,var_name)
}

portfolios_tangente <- c("portfolio_ibov.tangente","portfolio_grande.tangente",
                         "portfolio_medio.tangente","portfolio_pequeno.tangente")
for (portfolio in portfolios){
  var_name <- paste(portfolio,".tangente",sep="")
  assign(var_name,
         tangencyPortfolio(get(paste(portfolio,".retorno",sep=""))))
  portfolios_tangente <- c(portfolios_tangente, var_name)
}

#### Menores Riscos ####
rf <- round(mean(banco_dados_estimacao[,'selic']),5)
dtframe_results <- data.frame()
portfolio <- ''
for (portfolio in portfolios_min_risks){
  retorno <- round(getPortfolio(get(portfolio))$targetReturn[['mean']],5)
  risco <- round(getPortfolio(get(portfolio))$targetRisk[['CVaR']],5)
  name <- gsub("_"," ",
               gsub("portfolio_","",
                    gsub(".min_risk","",
                         gsub(".minrisk","",portfolio))))
  sharpe <- round((retorno - rf) / getPortfolio(get(portfolio))$targetRisk[['VaR']],5)
  linha <- c(name,"Risco Minimo",risco,retorno,sharpe)
  dtframe_results <- rbind(dtframe_results,linha) 
}
colnames(dtframe_results) <- c("Nome Portfolio","Tipo","Risco (CVaR)","Retorno","Indice de Sharpe")

dtframe_results

for (portfolio in portfolios_tangente){
  retorno <- round(getPortfolio(get(portfolio))$targetReturn[['mean']],5)
  risco <- round(getPortfolio(get(portfolio))$targetRisk[['CVaR']],5)
  name <- gsub("_"," ",
               gsub("portfolio_","",
                    gsub(".tangente","",portfolio)))
  sharpe <- round((retorno - rf) / getPortfolio(get(portfolio))$targetRisk[['VaR']],5)
  linha <- c(name,"Tangente",risco,retorno,sharpe)
  dtframe_results <- rbind(dtframe_results,linha) 
}

numeros <- c("Risco (CVaR)","Retorno","Indice de Sharpe")
dtframe_results[,numeros] <- as.numeric(as.matrix(dtframe_results[,numeros],ncol=3,nrow=18))
summary(dtframe_results)
write.table(file = "Comparacao_portfolios.csv",dtframe_results,sep=";",dec=',')
