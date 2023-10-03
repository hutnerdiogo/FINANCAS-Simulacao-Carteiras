#### Caso de error, rode essas duas linhas: ####
#install.packages('zoo')
#install.packages('quantmod')
#install.packages('fPortfolio')
#install.packages('ggplot2')

#### Iniciando os pacotes ####
library('quantmod')
library('zoo')
library('fPortfolio')
library('ggplot2')
library('xts')



#### Criando a função ####
#' Basicamente, funções são conjuntos de codigos que são executados, mudando algumas variaveis
#' nesse caso a função chama "get_adj_close_values_from_papers" e quando iniciar ela
#' são passadas variaveis que são declaradas aqui ⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄⌄
get_adj_close_values_from_papers <- function(PAPER, from='2010-12-01', to='2023-10-01'){
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
                       "BBSE3.SA","BBDC3.SA","BBDC4.SA","BRAP4.SA",
                       "BBAS3.SA","BRKM5.SA","BRFS3.SA","BPAC11.SA","CRFB3.SA",
                       "CCRO3.SA","CMIG4.SA","CIEL3.SA","COGN3.SA","CPLE6.SA",
                       "CSAN3.SA","CPFE3.SA","CMIN3.SA","CVCB3.SA","CYRE3.SA",
                       "DXCO3.SA","ECOR3.SA","ELET3.SA","ELET6.SA","EMBR3.SA",
                       "ENGI11.SA","ENEV3.SA","EGIE3.SA","EQTL3.SA",
                       "EZTC3.SA","FLRY3.SA","GGBR4.SA","GOAU4.SA","GOLL4.SA",
                       "NTCO3.SA","SOMA3.SA","HAPV3.SA","HYPE3.SA","IGTI11.SA",
                       "IRBR3.SA","ITSA4.SA","ITUB4.SA","JBSS3.SA","KLBN11.SA",
                       "RENT3.SA","LWSA3.SA","LREN3.SA","MGLU3.SA","MRFG3.SA",
                       "CASH3.SA","BEEF3.SA","MRVE3.SA","MULT3.SA","PCAR3.SA",
                       "PETR3.SA","PETR4.SA","PRIO3.SA","PETZ3.SA","POSI3.SA",
                       "QUAL3.SA","RADL3.SA","RAIZ4.SA","RDOR3.SA","RAIL3.SA",
                       "SBSP3.SA","SANB11.SA","SMTO3.SA","CSNA3.SA","SLCE3.SA",
                       "SUZB3.SA","TAEE11.SA","VIVT3.SA","TIMS3.SA",
                       "TOTS3.SA","UGPA3.SA","USIM5.SA","VALE3.SA","VIIA3.SA",
                       "WEGE3.SA","YDUQ3.SA")

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
portfolio_medio <- c(portfolio_pequeno, "VIVT3.SA","RADL3.SA","MRFG3.SA","MULT3.SA","ELET3.SA")
portfolio_grande <- c(portfolio_medio,"DXCO3.SA","GGBR4.SA","PRIO3.SA","CCRO3.SA","CYRE3.SA")

portfolio_pequeno_retornos <- as.timeSeries(price_to_variation(banco_dados_estimacao[,portfolio_pequeno]))
portfolio_medio_retornos <- as.timeSeries(price_to_variation(banco_dados_estimacao[,portfolio_medio]))
portfolio_grande_retornos <- as.timeSeries(price_to_variation(banco_dados_estimacao[,portfolio_grande]))

var_ibov <- as.timeSeries(price_to_variation(banco_dados_estimacao[,"%5EBVSP"]))
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
                 "BBSE3.SA","BBDC3.SA","BBDC4.SA","BRAP4.SA",
                 "BBAS3.SA","BRKM5.SA","BRFS3.SA","BPAC11.SA","CRFB3.SA",
                 "CCRO3.SA","CMIG4.SA","CIEL3.SA","COGN3.SA","CPLE6.SA",
                 "CSAN3.SA","CPFE3.SA","CMIN3.SA","CVCB3.SA","CYRE3.SA",
                 "DXCO3.SA","ECOR3.SA","ELET3.SA","ELET6.SA","EMBR3.SA",
                 "ENGI11.SA","ENEV3.SA","EGIE3.SA","EQTL3.SA",
                 "EZTC3.SA","FLRY3.SA","GGBR4.SA","GOAU4.SA","GOLL4.SA",
                 "NTCO3.SA","SOMA3.SA","HAPV3.SA","HYPE3.SA","IGTI11.SA",
                 "IRBR3.SA","ITSA4.SA","ITUB4.SA","JBSS3.SA","KLBN11.SA",
                 "RENT3.SA","LWSA3.SA","LREN3.SA","MGLU3.SA","MRFG3.SA",
                 "CASH3.SA","BEEF3.SA","MRVE3.SA","MULT3.SA","PCAR3.SA",
                 "PETR3.SA","PETR4.SA","PRIO3.SA","PETZ3.SA","POSI3.SA",
                 "QUAL3.SA","RADL3.SA","RAIZ4.SA","RDOR3.SA","RAIL3.SA",
                 "SBSP3.SA","SANB11.SA","SMTO3.SA","CSNA3.SA","SLCE3.SA",
                 "SUZB3.SA","TAEE11.SA","VIVT3.SA","TIMS3.SA",
                 "TOTS3.SA","UGPA3.SA","USIM5.SA","VALE3.SA","VIIA3.SA",
                 "VBBR3.SA","WEGE3.SA","YDUQ3.SA")

#elementos_faltantes <- setdiff(portfolio_ibov, unique(colnames(banco_dados_estimacao)))


portfolio_ibov.retornos <- as.timeSeries(log(lag(banco_dados_estimacao[,portfolio_ibov])/banco_dados_estimacao[,portfolio_ibov]))
portfolio_ibov.have_retornos <- portfolio_ibov.retornos[,!as.logical(colSums(is.na(portfolio_ibov.retornos)))]
portfolio_ibov.frontiers <- portfolioFrontier(portfolio_ibov.have_retornos,spec)

frontierPlot(portfolio_ibov.frontiers, col = c('blue', 'red'), pch = 20,
             risk="VaR", title = F)

monteCarloPoints(portfolio_ibov.frontiers, mcSteps = 16000, pch = 20, cex = 0.25,
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
  ativos <- setores[setores[,'Setor'] == setor,'Papel']
  print(setor)
  print(ativos)
}


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

colnames(portfolio_bens_industriais.retorno)
colnames(portfolio_consumo_nao_ciclico.retorno)
colnames(portfolio_materiais_basicos.retorno)
colnames(portfolio_utilidade_publica.retorno)
colnames(portfolio_financeiro.retorno)


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
  
  
  ret_port <- get(paste(portfolio,".retorno",sep=""))
  for (ativo in colnames(ret_port)){
    media <- mean(ret_port[,ativo])
    risco <- sd(ret_port[,ativo])
    paste("Risco: ",risco,"\n Media: ", media, "\n Ativo:", ativo)
    points(risco,media,col="Green",pch=19)
    text(risco,media,ativo,col="Brown", pos = 2,cex=0.5)
  }
  
  
  
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


#### Coletando fronteira eficiente dos setoriais ####

for (portfolio in portfolios){
  fronteira <- get(paste(portfolio,".fronteira",sep=''))
  colunas <- c("CVaR","mean","Sharp",colnames(getWeights(fronteira)))
  resultado <- matrix(round(getPortfolio(fronteira)$targetRisk[,"CVaR"],4),ncol=1)
  resultado <- cbind(resultado, round(getPortfolio(fronteira)$targetReturn[,"mean"],4))
  sharpe <- round((getPortfolio(fronteira)$targetReturn[,"mean"] - rf)/getPortfolio(fronteira)$targetRisk[,"VaR"],4)
  resultado <- cbind(resultado,sharpe)
  resultado <- cbind(resultado, round(getPortfolio(fronteira)$weights,4))
  colnames(resultado) <- colunas
  write.table(resultado,paste(portfolio,".csv",sep=""),sep=';',dec=',')
}

#### Analisando a eficiencia do portfolio ####
portfolio_utilidade_publica.tangente
portfolio_ibov.tangente
portfolio_grande.tangente
portfolio_utilidade_publica.minrisk

banco_dados_avaliacao <- base_dados[round(length(base_dados[,5])* 73/100):length(base_dados[,5]),]
rownames(banco_dados_avaliacao) <- attr(base_dados,"index")[round(length(base_dados[,5])* 73/100):length(base_dados[,5])]
banco_dados_avaliacao_oscilacao <- price_to_variation(banco_dados_avaliacao)
a <- matrix(
            Delt(
              matrix(
                banco_dados_avaliacao, ncol=dim(banco_dados_avaliacao)[2]),
              type= "arithmetic")
            ,ncol=dim(banco_dados_avaliacao)[2])

colnames(a) <- colnames(banco_dados_avaliacao)
rownames(a) <- rownames(banco_dados_avaliacao)
banco_dados_avaliacao_oscilacao <- a[-1,]
banco_dados_avaliacao_oscilacao[,'selic'] <- banco_dados_avaliacao[-1,'selic']

portfolios <- c("portfolio_utilidade_publica.tangente","portfolio_ibov.tangente",
                "portfolio_grande.tangente", "portfolio_utilidade_publica.minrisk")
for (nome_portfolio in portfolios){
  portfolio <- get(nome_portfolio)
  #Considerando somente os pesos que não são 0
  pesos <- getPortfolio(portfolio)$weights[getPortfolio(portfolio)$weights != 0]
  ativos <- names(pesos)
  retornos <- banco_dados_avaliacao_oscilacao[,ativos]
  valor <- 1000
  oscilador_por_ativo <- valor * pesos
  resultados <- matrix(ncol = dim(retornos)[2],nrow=dim(retornos)[1]+1)
  colnames(resultados) <- ativos
  rownames(resultados) <- c("Inicio",rownames(retornos))
  resultados[1,] <- oscilador_por_ativo
  resultados[2,] <- resultados[1,] * (1+retornos[1,])
  for (ind in 2:dim(retornos)[1]+1){
    resultados[ind,] <- resultados[ind-1,] * (1+retornos[ind-1,])
  }
  resultados <- cbind(resultados,valor_carteira = rowSums(resultados),rendimento_acumulado=rowSums(resultados)/1000)
  evolucao <- Delt(resultados[,'valor_carteira'])
  resultados <- cbind(resultados,evolucao = evolucao)
  colnames(resultados) <- c(colnames(resultados)[-dim(resultados)[2]],"evolucao")
  assign(gsub("portfolio","resultado",x = nome_portfolio),resultados)
}

resultados <- c("resultado_ibov.tangente",
                "resultado_grande.tangente",
                "resultado_utilidade_publica.tangente",
                "resultado_utilidade_publica.minrisk")

## Calculando o Ibovespa
retorno_ibov <- banco_dados_avaliacao_oscilacao[,'%5EBVSP']
valor_inicial <- 1000
carteira_ibov <- matrix(,nrow=length(retorno_ibov)+1)
carteira_ibov[1,] <- 1000
carteira_ibov[2,] <- carteira_ibov[1,] * (1 + retorno_ibov[1])
for (ind in 2:length(retorno_ibov)+1){
  carteira_ibov[ind,] <- carteira_ibov[ind-1,] * (1 + retorno_ibov[ind-1])
}
rownames(carteira_ibov) <- c("Inicio",names(retorno_ibov))
colnames(carteira_ibov) <- "Ibovespa"

## Calculando a Selic
retorno_selic <- banco_dados_avaliacao_oscilacao[,'selic']
valor_inicial <- 1000
carteira_selic <- matrix(,nrow=length(retorno_selic)+1)
carteira_selic[1,] <- 1000
carteira_selic[2,] <- carteira_selic[1,] * (1 + retorno_selic[1])
for (ind in 2:length(retorno_selic)+1){
  carteira_selic[ind,] <- carteira_selic[ind-1,] * (1 + retorno_selic[ind-1])
}
rownames(carteira_selic) <- c("Inicio",names(retorno_selic))
colnames(carteira_selic) <- "Selic"



## Gerando Graficos

par(mar=c(4.5,4.5,4.5,4.5))
for (resultado in resultados){
  png(file=paste("Resultado Carteiras/",resultado,".png",sep=''))
  analisado <- get(resultado)
  colnames(a)[-dim(a)[2]]
  valor_data <- analisado[-1,'valor_carteira']
  valor_data <- cbind(valor_data, ibov = carteira_ibov[-1],selic=carteira_selic[-1])
  name <- gsub('_',' ', gsub('resultado_','',resultado))
  matplot(y=valor_data[,1:3],x=as.Date(rownames(valor_data)),
          type='l',
          main=paste("Evolucao da carteira",name), 
          xlab="Tempo",
          ylab="Valor do Portfolio",
          cex=5
  )
  legend("topleft", legend = c("Portfolio","Ibovespa","Selic"), col=1:2, pch=19)
  abline(h=1000, col="red")
  
  dev.off()
}
## Analisando dados Absolutos
todas_carteiras <- matrix(,nrow=dim(get(resultados[1]))[1])
for (resultado in resultados){
  analisado <- get(resultado)
  name <- gsub('_',' ', gsub('resultado_','',resultado))
  todas_carteiras <- cbind(todas_carteiras,name=analisado[,'valor_carteira'])
  colnames(todas_carteiras)[dim(todas_carteiras)[2]] <- name 
}

todas_carteiras <- todas_carteiras[,-1]
matplot(y=todas_carteiras,x=as.Date(c("2019-07-01",rownames(todas_carteiras)[-1])),
        type='l',
        main=paste("Comparacao de carteiras"), 
        xlab="Tempo",
        ylab="Valor dos Portfolios",
        cex=5
)
legend("topleft", legend = colnames(todas_carteiras), col=1:4, pch=19)
abline(h=1000, col="red")


analise_final <- matrix(nrow=length(resultados))
rownames(analise_final) <- colnames(todas_carteiras)

analise_final[,1] <- todas_carteiras[39,]
colnames(analise_final) <- "Retorno carteira R$1000"
analise_final <- cbind(analise_final,Valor_Inicial = todas_carteiras[1,])
analise_final <- cbind(analise_final,Minimo_periodo = colMins(todas_carteiras))
analise_final <- cbind(analise_final,Maximo_periodo = colMaxs(todas_carteiras))
analise_final <- cbind(analise_final,Desvio_padrao_periodo = colSds(todas_carteiras))
analise_final <- cbind(analise_final,Rendimento_Acumulado = todas_carteiras[39,])
analise_final <- analise_final[,c(2,1,3:6)]
variacao <- analise_final[,'Rendimento_Acumulado']/analise_final[,'Valor_Inicial']-1
analise_final <- cbind(analise_final,
                       Rendimento_Acumulado_percentual=variacao)
write.table(analise_final,file="Analise Buy-Hold periodo 01-07-2019 - 01-09-2022.csv",
            sep=';',dec=',')

## Analisando dados de Retornos

retornos_carteiras <- matrix(,nrow=dim(get(resultados[1]))[1]-1)
for (resultado in resultados){
  analisado <- get(resultado)
  name <- gsub('_',' ', gsub('resultado_','',resultado))
  retornos_carteiras <- cbind(retornos_carteiras,analisado[-1,'evolucao'])
  colnames(retornos_carteiras)[dim(retornos_carteiras)[2]] <- name 
}
retornos_carteiras <- retornos_carteiras[,-1]


analise_final_retornos <- matrix(nrow=dim(retornos_carteiras)[2])
rownames(analise_final_retornos) <- colnames(retornos_carteiras)
analise_final_retornos <- cbind(analise_final_retornos,Minimo_periodo = colMins(retornos_carteiras))
analise_final_retornos <- cbind(analise_final_retornos,Maximo_periodo = colMaxs(retornos_carteiras))
analise_final_retornos <- cbind(analise_final_retornos,Desvio_padrao_periodo = colSds(retornos_carteiras))
analise_final_retornos <- analise_final_retornos[,-1]
analise_final_retornos <- cbind(analise_final_retornos,Media_Do_Retorno = colMeans(retornos_carteiras))

correlacao <- matrix(nrow=dim(retornos_carteiras)[2])
rownames(correlacao) <- colnames(retornos_carteiras)

for (coluna in 1:dim(retornos_carteiras)[2]){
 retorno_analisado <- retornos_carteiras[,coluna]
 nome <- colnames(retornos_carteiras)[coluna]
 correlacao[nome,] <- cor(retorno_analisado,retorno_ibov)
}

analise_final_retornos <- cbind(analise_final_retornos,correlacao_com_mercado = correlacao)
colnames(analise_final_retornos)[4] <- "Correlacao com Ibovespa"

sharpe <- (analise_final_retornos[,'Media_Do_Retorno'] - mean(retorno_selic)) / analise_final_retornos[,'Desvio_padrao_periodo']
analise_final_retornos <- cbind(analise_final_retornos,sharpe=sharpe)
analise_final_retornos <- cbind(analise_final_retornos,VaR=colVars(retornos_carteiras))
## Fazendo a regressão, para mais dados
regressoes <- c()
for (resultado in colnames(retornos_carteiras)){
  name <- paste('regressao_',gsub(' ','_',resultado),sep='')
  retorno <- retornos_carteiras[,resultado]
  assign(name,lm(retorno ~ retorno_ibov ))
  regressoes <- c(regressoes,name)
}

alfa_beta <- matrix(,ncol=2,nrow=4)
rownames(alfa_beta) <- colnames(todas_carteiras)
for (reg in regressoes){
  name <- gsub('_',' ',gsub('regressao_','',reg))
  regr <- get(reg)
  significantes <- summary(regr)$coefficients[, "Pr(>|t|)"] < 0.05
  significantes <- as.integer(significantes)
  alfa_beta[name,] <- significantes*coefficients(regr)
}
colnames(alfa_beta) <- c("Alfa","Beta")
analise_final_retornos <- cbind(analise_final_retornos,alfa_beta)
## Voltando as estatisticas
treynor <- (analise_final_retornos[,'Media_Do_Retorno'] - mean(retorno_selic)) / analise_final_retornos[,'Beta']
analise_final_retornos <- cbind(analise_final_retornos,treynor)

write.table(analise_final_retornos,file="Resultado Carteiras/Analise portfolio periodo 01-07-2019 - 01-09-2022.csv",
            sep=';',dec=',')

#
ativos <- list()
for (port in portfolios){
  temp <- list(getPortfolio(get(port))$weights[getPortfolio(get(port))$weights > 0])
  ativos <- c(ativos, temp)
  names(ativos)[length(ativos)] <- port
}

