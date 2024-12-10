####### Cliente: TC773 - Leopoldo
####### Início: 12/02/2019
####### Autor: Luiz Henrique

rm(list=ls())
source("Y:/01 - Arquivos Administrativos/Funções ABG.R")
setwd("Y:/02 - Produção Científica - Trabalhos/TC773 - Leopoldo Mandic")

###================================ 
### Instalando e Carregando pacotes
###================================

if(!require(irr)){ install.packages("irr"); require(irr) }
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(psych)){install.packages("psych"); require(psych)}
if(!require(psy)){install.packages("psy"); require(psy)}
if(!require(nFactors)){install.packages("nFactors"); require(nFactors)}
if(!require(DescTools)){install.packages("DescTools"); require(DescTools)}
if(!require(Hmisc)){install.packages("Hmisc"); require(Hmisc)}


###========
### Funções
###========

icc.kappa1 <- function(x, y){
  D <- data.frame(x, y)
  y1<- irr::icc(D, model="twoway", type="agreement", unit="single")
  y2<- irr::icc(D, model="twoway", type="agreement", unit="average")
  k <- kappa2(cbind(x,y), weight = "squared")$value
  c(Val1(D)[3], y1$value, y2$value, k)
}

icc.kappa2 <- function(x, y, z, w, t, s){
  D <- data.frame(x, y, z, w, t, s)
  y1<- irr::icc(D, model="twoway", type="agreement", unit="single")
  y2<- irr::icc(D, model="twoway", type="agreement", unit="average")
  k <- kappam.fleiss(cbind(x, y, z, w, t, s), exact = T, detail = FALSE)$value
  c(Val1(D)[3], y1$value, y2$value, k)
}

icc.kappa3 <- function(x, y, z, w){
  D <- data.frame(x, y, z, w)
  y1<- irr::icc(D, model="twoway", type="agreement", unit="single")
  y2<- irr::icc(D, model="twoway", type="agreement", unit="average")
  k <- kappam.fleiss(cbind(x, y, z, w), exact = T, detail = FALSE)$value
  c(Val1(D)[3], y1$value, y2$value, k)
}

###=======================
### Puxando banco de dados
###======================= 

Dados <- read.csv2("Y:/02 - Produção Científica - Trabalhos/TC773 - Leopoldo Mandic/Dados.csv")
head(Dados); dim(Dados)

###======================
### Construindo variáveis
###======================

Extensao <- Dados$Superior + Dados$Lateral + Dados$Inferior; Dados$Extensao <- Extensao
Comp <- Dados$Mesencefalo + Dados$Ponte.Bulbo + Dados$Cerebelo; Dados$Comp <- Comp

Dados2 <- Dados %>% filter(Avaliador!="GABARITO")

###===================
### Análise descritiva
###===================

###==============
### Intra e Inter
###==============

AV1 <- Dados2 %>% filter(Avaliacao=="AV1")
AV2 <- Dados2 %>% filter(Avaliacao=="AV2")

AV1.2 <- full_join(AV1, AV2, by=c("ID", "Avaliador", "Tempo"))

D.NCR3 <- Dados2 %>% filter(Avaliador=="NCR3")
D.NCR2 <- Dados2 %>% filter(Avaliador=="NCR2")
D.R4 <- Dados2 %>% filter(Avaliador=="R4")
D.R3 <- Dados2 %>% filter(Avaliador=="R3")
D.R2 <- Dados2 %>% filter(Avaliador=="R2")
D.R1 <- Dados2 %>% filter(Avaliador=="R1")

rbind(
icc.kappa1(AV1.2$Superior.x, AV1.2$Superior.y)
,icc.kappa2(D.NCR3$Superior, D.NCR2$Superior, D.R4$Superior, 
           D.R3$Superior, D.R2$Superior, D.R1$Superior)
,icc.kappa1(AV1.2$Inferior.x, AV1.2$Inferior.y)
,icc.kappa2(D.NCR3$Inferior, D.NCR2$Inferior, D.R4$Inferior, 
           D.R3$Inferior, D.R2$Inferior, D.R1$Inferior)
,icc.kappa1(AV1.2$Lateral.x, AV1.2$Lateral.y)
,icc.kappa2(D.NCR3$Lateral, D.NCR2$Lateral, D.R4$Lateral, 
           D.R3$Lateral, D.R2$Lateral, D.R1$Lateral)
,icc.kappa1(AV1.2$Mesencefalo.x, AV1.2$Mesencefalo.y)
,icc.kappa2(D.NCR3$Mesencefalo, D.NCR2$Mesencefalo, D.R4$Mesencefalo, 
           D.R3$Mesencefalo, D.R2$Mesencefalo, D.R1$Mesencefalo)
,icc.kappa1(AV1.2$Ponte.Bulbo.x, AV1.2$Ponte.Bulbo.y)
,icc.kappa2(D.NCR3$Ponte.Bulbo, D.NCR2$Ponte.Bulbo, D.R4$Ponte.Bulbo, 
           D.R3$Ponte.Bulbo, D.R2$Ponte.Bulbo, D.R1$Ponte.Bulbo)
,icc.kappa1(AV1.2$Cerebelo.x, AV1.2$Cerebelo.y)
,icc.kappa2(D.NCR3$Cerebelo, D.NCR2$Cerebelo, D.R4$Cerebelo, 
           D.R3$Cerebelo, D.R2$Cerebelo, D.R1$Cerebelo)
,icc.kappa1(AV1.2$Extensao.x, AV1.2$Extensao.y)
,icc.kappa2(D.NCR3$Extensao, D.NCR2$Extensao, D.R4$Extensao, 
           D.R3$Extensao, D.R2$Extensao, D.R1$Extensao)
,icc.kappa1(AV1.2$Comp.x, AV1.2$Comp.y)
,icc.kappa2(D.NCR3$Comp, D.NCR2$Comp, D.R4$Comp, 
           D.R3$Comp, D.R2$Comp, D.R1$Comp)
,icc.kappa1(AV1.2$Total.x, AV1.2$Total.y)
,icc.kappa2(D.NCR3$Total, D.NCR2$Total, D.R4$Total, 
           D.R3$Total, D.R2$Total, D.R1$Total))

###==================
### solicitação 24/04
###==================

Dados3 <- Dados2 %>% filter(Avaliador!="R1"&Avaliador!="R3")

AV1.B <- Dados3 %>% filter(Avaliacao=="AV1")
AV2.B <- Dados3 %>% filter(Avaliacao=="AV2")

AV1.2.B <- full_join(AV1.B, AV2.B, by=c("ID", "Avaliador", "Tempo"))

rbind(
  icc.kappa1(AV1.2.B$Superior.x, AV1.2.B$Superior.y)
  ,icc.kappa3(D.NCR3$Superior, D.NCR2$Superior, D.R4$Superior, D.R2$Superior)
  
  ,icc.kappa1(AV1.2.B$Inferior.x, AV1.2.B$Inferior.y)
  ,icc.kappa3(D.NCR3$Inferior, D.NCR2$Inferior, D.R4$Inferior, D.R2$Inferior)
  
  ,icc.kappa1(AV1.2.B$Lateral.x, AV1.2.B$Lateral.y)
  ,icc.kappa3(D.NCR3$Lateral, D.NCR2$Lateral, D.R4$Lateral,D.R2$Lateral)
  
  ,icc.kappa1(AV1.2.B$Mesencefalo.x, AV1.2.B$Mesencefalo.y)
  ,icc.kappa3(D.NCR3$Mesencefalo, D.NCR2$Mesencefalo, D.R4$Mesencefalo, D.R2$Mesencefalo)
  
  ,icc.kappa1(AV1.2.B$Ponte.Bulbo.x, AV1.2.B$Ponte.Bulbo.y)
  ,icc.kappa3(D.NCR3$Ponte.Bulbo, D.NCR2$Ponte.Bulbo, D.R4$Ponte.Bulbo, D.R2$Ponte.Bulbo)
  
  ,icc.kappa1(AV1.2.B$Cerebelo.x, AV1.2.B$Cerebelo.y)
  ,icc.kappa3(D.NCR3$Cerebelo, D.NCR2$Cerebelo, D.R4$Cerebelo, D.R2$Cerebelo)
  
  ,icc.kappa1(AV1.2.B$Extensao.x, AV1.2.B$Extensao.y)
  ,icc.kappa3(D.NCR3$Extensao, D.NCR2$Extensao, D.R4$Extensao, D.R2$Extensao)
  
  ,icc.kappa1(AV1.2.B$Comp.x, AV1.2.B$Comp.y)
  ,icc.kappa3(D.NCR3$Comp, D.NCR2$Comp, D.R4$Comp, D.R2$Comp)
  
  ,icc.kappa1(AV1.2.B$Total.x, AV1.2.B$Total.y)
  ,icc.kappa3(D.NCR3$Total, D.NCR2$Total, D.R4$Total, D.R2$Total))

###===========
### Correlação
###===========

D1.GAB <- Dados %>% filter(Avaliacao=="AV1"&Avaliador=="GABARITO")
D1.NCR3 <- Dados %>% filter(Avaliacao=="AV1"&Avaliador=="NCR3")
D1.NCR2 <- Dados %>% filter(Avaliacao=="AV1"&Avaliador=="NCR2")
D1.R4 <- Dados %>% filter(Avaliacao=="AV1"&Avaliador=="R4")
D1.R3 <- Dados %>% filter(Avaliacao=="AV1"&Avaliador=="R3")
D1.R2 <- Dados %>% filter(Avaliacao=="AV1"&Avaliador=="R2")
D1.R1 <- Dados %>% filter(Avaliacao=="AV1"&Avaliador=="R1")

D2.GAB <- Dados %>% filter(Avaliacao=="AV2"&Avaliador=="GABARITO")
D2.NCR3 <- Dados %>% filter(Avaliacao=="AV2"&Avaliador=="NCR3")
D2.NCR2 <- Dados %>% filter(Avaliacao=="AV2"&Avaliador=="NCR2")
D2.R4 <- Dados %>% filter(Avaliacao=="AV2"&Avaliador=="R4")
D2.R3 <- Dados %>% filter(Avaliacao=="AV2"&Avaliador=="R3")
D2.R2 <- Dados %>% filter(Avaliacao=="AV2"&Avaliador=="R2")
D2.R1 <- Dados %>% filter(Avaliacao=="AV2"&Avaliador=="R1")

Corsup.T1 <- cbind(D1.NCR3$Superior, D1.NCR2$Superior, D1.R4$Superior, 
                   D1.R3$Superior, D1.R2$Superior, D1.R1$Superior, D1.GAB$Superior)

Corsup.T2 <- cbind(D2.NCR3$Superior, D2.NCR2$Superior, D2.R4$Superior, 
                   D2.R3$Superior, D2.R2$Superior, D2.R1$Superior, D2.GAB$Superior)

Corinf.T1 <- cbind(D1.NCR3$Inferior, D1.NCR2$Inferior, D1.R4$Inferior, 
                   D1.R3$Inferior, D1.R2$Inferior, D1.R1$Inferior, D1.GAB$Inferior)

Corinf.T2 <- cbind(D2.NCR3$Inferior, D2.NCR2$Inferior, D2.R4$Inferior, 
                   D2.R3$Inferior, D2.R2$Inferior, D2.R1$Inferior, D2.GAB$Inferior)

Corlat.T1 <- cbind(D1.NCR3$Lateral, D1.NCR2$Lateral, D1.R4$Lateral, 
                   D1.R3$Lateral, D1.R2$Lateral, D1.R1$Lateral, D1.GAB$Lateral)

Corlat.T2 <- cbind(D2.NCR3$Lateral, D2.NCR2$Lateral, D2.R4$Lateral, 
                   D2.R3$Lateral, D2.R2$Lateral, D2.R1$Lateral, D2.GAB$Lateral)

Cormes.T1 <- cbind(D1.NCR3$Mesencefalo, D1.NCR2$Mesencefalo, D1.R4$Mesencefalo, 
                   D1.R3$Mesencefalo, D1.R2$Mesencefalo, D1.R1$Mesencefalo, D1.GAB$Mesencefalo)

Cormes.T2 <- cbind(D2.NCR3$Mesencefalo, D2.NCR2$Mesencefalo, D2.R4$Mesencefalo, 
                   D2.R3$Mesencefalo, D2.R2$Mesencefalo, D2.R1$Mesencefalo, D2.GAB$Mesencefalo)

Corpb.T1 <- cbind(D1.NCR3$Ponte.Bulbo, D1.NCR2$Ponte.Bulbo, D1.R4$Ponte.Bulbo, 
                   D1.R3$Ponte.Bulbo, D1.R2$Ponte.Bulbo, D1.R1$Ponte.Bulbo, D1.GAB$Ponte.Bulbo)

Corpb.T2 <- cbind(D2.NCR3$Ponte.Bulbo, D2.NCR2$Ponte.Bulbo, D2.R4$Ponte.Bulbo, 
                   D2.R3$Ponte.Bulbo, D2.R2$Ponte.Bulbo, D2.R1$Ponte.Bulbo, D2.GAB$Ponte.Bulbo)

Corcer.T1 <- cbind(D1.NCR3$Cerebelo, D1.NCR2$Cerebelo, D1.R4$Cerebelo, 
                   D1.R3$Cerebelo, D1.R2$Cerebelo, D1.R1$Cerebelo, D1.GAB$Cerebelo)

Corcer.T2 <- cbind(D2.NCR3$Cerebelo, D2.NCR2$Cerebelo, D2.R4$Cerebelo, 
                   D2.R3$Cerebelo, D2.R2$Cerebelo, D2.R1$Cerebelo, D2.GAB$Cerebelo)

Corext.T1 <- cbind(D1.NCR3$Extensao, D1.NCR2$Extensao, D1.R4$Extensao, 
                   D1.R3$Extensao, D1.R2$Extensao, D1.R1$Extensao, D1.GAB$Extensao)

Corext.T2 <- cbind(D2.NCR3$Extensao, D2.NCR2$Extensao, D2.R4$Extensao, 
                   D2.R3$Extensao, D2.R2$Extensao, D2.R1$Extensao, D2.GAB$Extensao)

Corcom.T1 <- cbind(D1.NCR3$Comp, D1.NCR2$Comp, D1.R4$Comp, 
                   D1.R3$Comp, D1.R2$Comp, D1.R1$Comp, D1.GAB$Comp)

Corcom.T2 <- cbind(D2.NCR3$Comp, D2.NCR2$Comp, D2.R4$Comp, 
                   D2.R3$Comp, D2.R2$Comp, D2.R1$Comp, D2.GAB$Comp)

Cortot.T1 <- cbind(D1.NCR3$Total, D1.NCR2$Total, D1.R4$Total, 
                   D1.R3$Total, D1.R2$Total, D1.R1$Total, D1.GAB$Total)

Cortot.T2 <- cbind(D2.NCR3$Total, D2.NCR2$Total, D2.R4$Total, 
                   D2.R3$Total, D2.R2$Total, D2.R1$Total, D2.GAB$Total)

rbind(
cbind(
(rcorr(Corsup.T1, type="spearman")$r[,7])[-7]
,(rcorr(Corsup.T1, type="spearman")$P[,7])[-7]
,(rcorr(Corsup.T2, type="spearman")$r[,7])[-7]
,(rcorr(Corsup.T2, type="spearman")$P[,7])[-7]),

cbind(
  (rcorr(Corinf.T1, type="spearman")$r[,7])[-7]
  ,(rcorr(Corinf.T1, type="spearman")$P[,7])[-7]
  ,(rcorr(Corinf.T2, type="spearman")$r[,7])[-7]
  ,(rcorr(Corinf.T2, type="spearman")$P[,7])[-7]),

cbind(
  (rcorr(Corlat.T1, type="spearman")$r[,7])[-7]
  ,(rcorr(Corlat.T1, type="spearman")$P[,7])[-7]
  ,(rcorr(Corlat.T2, type="spearman")$r[,7])[-7]
  ,(rcorr(Corlat.T2, type="spearman")$P[,7])[-7]),

cbind(
  (rcorr(Cormes.T1, type="spearman")$r[,7])[-7]
  ,(rcorr(Cormes.T1, type="spearman")$P[,7])[-7]
  ,(rcorr(Cormes.T2, type="spearman")$r[,7])[-7]
  ,(rcorr(Cormes.T2, type="spearman")$P[,7])[-7]),

cbind(
  (rcorr(Corpb.T1, type="spearman")$r[,7])[-7]
  ,(rcorr(Corpb.T1, type="spearman")$P[,7])[-7]
  ,(rcorr(Corpb.T2, type="spearman")$r[,7])[-7]
  ,(rcorr(Corpb.T2, type="spearman")$P[,7])[-7]),

cbind(
  (rcorr(Corcer.T1, type="spearman")$r[,7])[-7]
  ,(rcorr(Corcer.T1, type="spearman")$P[,7])[-7]
  ,(rcorr(Corcer.T2, type="spearman")$r[,7])[-7]
  ,(rcorr(Corcer.T2, type="spearman")$P[,7])[-7]),

cbind(
  (rcorr(Corext.T1, type="spearman")$r[,7])[-7]
  ,(rcorr(Corext.T1, type="spearman")$P[,7])[-7]
  ,(rcorr(Corext.T2, type="spearman")$r[,7])[-7]
  ,(rcorr(Corext.T2, type="spearman")$P[,7])[-7]),

cbind(
  (rcorr(Corcom.T1, type="spearman")$r[,7])[-7]
  ,(rcorr(Corcom.T1, type="spearman")$P[,7])[-7]
  ,(rcorr(Corcom.T2, type="spearman")$r[,7])[-7]
  ,(rcorr(Corcom.T2, type="spearman")$P[,7])[-7]),

cbind(
  (rcorr(Cortot.T1, type="spearman")$r[,7])[-7]
  ,(rcorr(Cortot.T1, type="spearman")$P[,7])[-7]
  ,(rcorr(Cortot.T2, type="spearman")$r[,7])[-7]
  ,(rcorr(Cortot.T2, type="spearman")$P[,7])[-7]))









