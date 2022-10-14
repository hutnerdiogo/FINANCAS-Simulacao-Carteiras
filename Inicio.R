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

# Salvando o banco de dados 
write.zoo(base_dados, file = "BaseDeDados.zoo", 
          index.name = "Index", row.names = FALSE, col.names = T)

# Para carregar o banco de dados
file <- read.csv("BaseDeDados.zoo",sep=" ")
datas <- file[,1]
file <- file[,-1]
base_2 <- zoo(file,order.by = datas)


# Cortando o banco de dados amostral: 
banco_dados_estimacao <- base_dados[1:round(length(base_dados[,5])* 73/100),]


#### Montar Portfolio ####
portfolio_pequeno <- c("BBDC3.SA","CPLE6.SA","ENGI11.SA","HYPE3.SA","TIMS3.SA")
portfolio_medio <- c(portfolio_pequeno, "VIVT3.SA","RADL3.SA","MRFG3.SA","ENBR3.SA","BRML3.SA")
portfolio_grande <- c(portfolio_medio,"DXCO3.SA","GGBR4.SA","PRIO3.SA","CCRO3.SA","CYRE3.SA")


par(mfrow=c(3,5))
for (element in portfolio_grande){
  plot(base_dados[,element],main=element)
}

portfolio_pequeno_retornos <- log(lag(base_dados[,portfolio_pequeno])/base_dados[,portfolio_pequeno])
portfolio_medio_retornos <- log(lag(base_dados[,portfolio_medio])/base_dados[,portfolio_medio])
portfolio_grande_retornos <- log(lag(base_dados[,portfolio_grande])/base_dados[,portfolio_grande])

analise_temporaria <- matrix(,nrow=2,ncol=5)
colnames(analise_temporaria) <- portfolio_pequeno
rownames(analise_temporaria) <- c("Risco", "Media")

for (ativo in portfolio_pequeno){
  media = mean(portfolio_pequeno_retornos[,ativo])
  risco = sd(portfolio_pequeno_retornos[,ativo])
  analise_temporaria[,ativo] <- c(risco, media)
}

par(mfrow=c(1,1))
tranposta_analise <- t(analise_temporaria)
plot(tranposta_analise)

cor(portfolio_pequeno_retornos)
fronteira <- portfolioFrontier(portfolio_pequeno_retornos)
