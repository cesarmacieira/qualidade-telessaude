####=============================================
#### Trabalho Mariana - Validação de instrumento
####=============================================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
tryCatch({setwd("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-telessaude")},
         error = function(e) { setwd("D:/NESCON/Trabalho - Mariana/qualidade-telessaude") })

####=================================
#### Instalando e carregando pacotes
####=================================
if(!require(openxlsx)){ install.packages("openxlsx"); require(openxlsx)}#Ler e exportar excel
if(!require(purrr)){ install.packages("purrr"); require(purrr)}#Programação funcional
if(!require(tidyverse)){ install.packages("tidyverse"); require(tidyverse)}#Manipulação de dados
if(!require(irr)){ install.packages("irr"); require(irr) }
if(!require(psych)){install.packages("psych"); require(psych)}
if(!require(psy)){install.packages("psy"); require(psy)}
if(!require(nFactors)){install.packages("nFactors"); require(nFactors)}
if(!require(DescTools)){install.packages("DescTools"); require(DescTools)}
#if(!require(Hmisc)){install.packages("Hmisc"); require(Hmisc)}

####=========
#### Funções
####=========
DescritivaCat = function(x){
  tabela = cbind(table(x), prop.table(table(x)))
  colnames(tabela) = c("Freq. Absoluta (N)", "Freq. Relativa (%)")
  return(tabela)
}

DescritivaNum = function(x, more = F) {
  stats = list();
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$Mín. = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Máx. = round(max(clean.x),3)
  t1 = unlist(stats)
  names(t1) = c("N","Média","Variância","D.P.","Mínimo","1ºQ","2ºQ","3ºQ","Máximo")
  t1
}

DescritivaNumMais2Grupos = function(y, z, more = F){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo")
  rownames(tab)= levels(factor(z))
  tab
}

basic.stats = function(x, more = F) {
  stats = list()
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$E.P = round(sd(clean.x)/sqrt(length(clean.x)),3)
  stats$Min = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Max = round(max(clean.x),3)
  t1 = unlist(stats)
  names(t1) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo")
  t1
}

QuiQuadrado_Fisher = function(x, y, type.sum, teste){
  t0 = table(x, y)
  if(type.sum==2) {
    t1 = prop.table(t0, 2)
  } else {
    t1 = prop.table(t0, 1)
  }
  colnames(t0) = paste0("X", 1:dim(t0)[2])
  colnames(t1) = paste0("X", 1:dim(t1)[2])
  t2_aux = cbind(t0, t1)
  t3 = t2_aux[, order(colnames(t2_aux))]
  colnames(t3) = c(rep(c("N", "%"), dim(t3)[2]/2))
  if(teste=="chisq") {
    Valor_p = chisq.test(t0)$p.value
  }
  if(teste=="fisher") {
    Valor_p = fisher.test(t0)$p.value
  } 
  if(teste=="chisq.simulate"){
    Valor_p = chisq.test(t0, simulate.p.value=TRUE, B=10000)$p.value
  }
  t4 = cbind(t3, Valor_p)
  return(t4)
}

KruskalTeste = function(y, z, more = F){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  p_valor = rep(kruskal.test(y~factor(z))$p.value, length(levels(factor(z))))
  tab = cbind(tab, p_valor)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  rownames(tab)= levels(factor(z))
  if(!require(PMCMRplus)){ install.packages("PMCMRplus"); require(PMCMRplus) }
  #CM = posthoc.kruskal.nemenyi.test(y ~ factor(z), dist="Chisq")$p.value
  CM = kwAllPairsNemenyiTest(y ~ factor(z), dist="Chisquare")$p.value
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

FriedmanTeste = function(y, z, id, more = F){
  dados = data.frame(y = y, grupos = as.factor(z), id = id)
  dados_agg = dados %>% select(y,grupos,id) %>% group_by(grupos,id) %>%
    summarize(y = mean(y, na.rm = TRUE)) %>% na.omit()
  tab = matrix(NA, length(levels(factor(dados_agg$grupos))), 10)
  for(i in 1:length(levels(factor(dados_agg$grupos)))){ 
    desc = tapply(dados_agg$y, factor(dados_agg$grupos),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  dados_completos = dados_agg %>%
    pivot_wider(names_from = grupos, values_from = y, names_prefix = "Grupo_") %>% na.omit() %>% 
    pivot_longer(cols = starts_with("Grupo_"), names_to = "grupos", values_to = "y") %>%
    mutate(grupos = gsub("Grupo_", "", grupos)) %>%
    group_by(id) %>% filter(!any(is.na(y))) %>% ungroup()
  p_valor = rep(friedman.test(y ~ grupos | id, data = dados_completos)$p.value, length(levels(factor(dados_agg$grupos))))
  tab = cbind(tab, p_valor)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  rownames(tab)= levels(factor(dados_agg$grupos))
  if(!require(PMCMRplus)){ install.packages("PMCMRplus"); require(PMCMRplus) }
  #CM = pairwise.wilcox.test(dados_completos$media, factor(dados_completos$grupos), p.adjust.method = "bonferroni")$p.value
  CM = frdAllPairsConoverTest(y = dados_completos$y, groups = dados_completos$grupos, 
                              blocks = dados_completos$id, p.adjust.method = 'none')$p.value
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

MannWhitney = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

WilcoxonDependente = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE, paired = TRUE, alternative = "two.sided")$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

AnovaIndepTeste = function(y, z, CM_teste = "bonferroni", more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result = summary(aov(y ~ factor(z)))
  p_valor_anova = anova_result[[1]]$"Pr(>F)"[1]
  #CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  if(CM_teste == "tukey") {
    CM = TukeyHSD(aov(y ~ factor(z)))$`factor(z)`
  } else if(CM_teste == "bonferroni") {
    if(!require(PMCMRplus)){ 
      install.packages("PMCMRplus")
      require(PMCMRplus) 
    }
    CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  }
  tab = cbind(tab, p_valor_anova)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p_ANOVA")
  rownames(tab)= levels(factor(z))
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

AnovaDepTeste = function(y, z, unid_amostral, CM_teste = "tukey", more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result = aov(y ~ factor(z) + Error(factor(unid_amostral)), data = data.frame(y, z, unid_amostral))
  p_valor_anova = summary(anova_result)[[1]]$"Pr(>F)"[1]
  #CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  if(CM_teste == "tukey") {
    CM = TukeyHSD(aov(y ~ factor(z)))$`factor(z)`
  } else if(CM_teste == "bonferroni") {
    if(!require(PMCMRplus)){install.packages("PMCMRplus"); require(PMCMRplus)}
    CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  }
  tab = cbind(tab, p_valor_anova)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p_ANOVA")
  rownames(tab)= levels(factor(z))
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

TesteTpareado = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = TRUE, alternative = "two.sided")$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  return(tab)
}

TesteTindep = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = F)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

TesteDeNormalidade = function(x){
  if(!require(dgof)){ install.packages("dgof"); require(dgof)}#Teste de Kolmogorov-Smirnov
  if(!require(nortest)){ install.packages("nortest"); require(nortest)}#Anderson-Darling
  AndersonDarling = round(ad.test(x)$p.value,3)
  KolmogorovSmirnov = round(ks.test(x, "pnorm", mean(x, na.rm = T), sd(x, na.rm = T))$p.value,3)
  Lilliefors = round(lillie.test(x)$p.value,3)
  CramerVonMises = round(cvm.test(x)$p.value,3)
  if(length(x) > 5000){
    ShapiroWilk = "N > 5000"
    ShapiroFrancia = "N > 5000"
  }else{
    ShapiroWilk = shapiro.test(x)$p.value
    ShapiroFrancia = sf.test(x)$p.value   
  }
  tabela = cbind(AndersonDarling,KolmogorovSmirnov,Lilliefors,CramerVonMises,
                 ShapiroWilk,ShapiroFrancia)
  colnames(tabela) = c('Anderson-Darling','Kolmogorov-Smirnov','Lilliefors','Cramer Von Mises','Shapiro-Wilk','Shapiro Francia')
  #row.names(tabela) = x
  return(tabela)
}

TesteDeNormalidadeGrupos = function(y, z){
  if(!require(dgof)){ install.packages("dgof"); require(dgof)}#Teste de Kolmogorov-Smirnov
  if(!require(nortest)){ install.packages("nortest"); require(nortest)}#Anderson-Darling
  dados = data.frame(y = y, Grupos = as.factor(z))
  if(dim(dados)[1] < 5000){
    result = dados %>% group_by(Grupos)%>% na.omit() %>%
      summarise(ShapiroWilk = round(shapiro.test(y)$p.value,3),
                ShapiroFrancia = round(sf.test(y)$p.value,3),
                AndersonDarling = round(ad.test(y)$p.value,3),
                KolmogorovSmirnov = round(ks.test(y, "pnorm", 
                                                  mean(y, na.rm = T), 
                                                  sd(y, na.rm = T))$p.value,3),
                Lilliefors = round(lillie.test(y)$p.value,3),
                CramerVonMises = round(cvm.test(y)$p.value,3)) %>% na.omit()
  }else{
    result = dados %>% group_by(Grupos) %>% na.omit() %>%
      summarise(ShapiroWilk = "N > 5000",
                ShapiroFrancia = "N > 5000",
                AndersonDarling = round(ad.test(y)$p.value,3),
                KolmogorovSmirnov = round(ks.test(y, "pnorm", 
                                                  mean(y, na.rm = T), 
                                                  sd(y, na.rm = T))$p.value,3),
                Lilliefors = round(lillie.test(y)$p.value,3),
                CramerVonMises = round(cvm.test(y)$p.value,3))
  }
  return(result)
}

HomogeneidadeVariancias = function(y, z){
  if(!require(car)){ install.packages("car"); require(car)}
  valor_p_Levene = leveneTest(y ~ as.factor(z))$`Pr(>F)`[1]
  return(valor_p_Levene)
}

TabelaGEEGama = function(modelo,casasdecimaisExpB=F){
  options(OutDec=",")
  if(casasdecimaisExpB == F){
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = exp(summary(modelo)$coefficients[,1]),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C." = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),3),"; ",
                                        round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),3),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%; ",
                                                    round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%]"),
                        "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  }else{
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = round(exp(summary(modelo)$coefficients[,1]),casasdecimaisExpB),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C." = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),casasdecimaisExpB),"; ",
                                        round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),casasdecimaisExpB),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,casasdecimaisExpB),"%; ",
                                                    round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,casasdecimaisExpB),"%]"),
                        "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  }
  return(Tabela)
}

TabelaGEENormal = function(modelo){
  options(OutDec=",")
  Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                      "β" = summary(modelo)$coefficients[,1],
                      "I.C. (95%)" = paste0("[",round(summary(modelo)$coefficients[,1]-(1.96*summary(modelo)$coefficients[,2]),3),"; ",
                                            round(summary(modelo)$coefficients[,1]+(1.96*summary(modelo)$coefficients[,2]),3),"]"),
                      "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  return(Tabela)
}

TabelaGLMMBeta = function(modelo){
  options(OutDec=",")
  Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients$cond),
                      "β" = summary(modelo)$coefficients$cond[,1],
                      "Exp β" = exp(summary(modelo)$coefficients$cond[,1]),
                      "Alteração" = (exp(summary(modelo)$coefficients$cond[,1]) - 1),
                      "I.C." = paste0("[",round(exp(summary(modelo)$coefficients$cond[,1]-1.96*summary(modelo)$coefficients$cond[,2]),3),"; ",
                                      round(exp(summary(modelo)$coefficients$cond[,1]+1.96*summary(modelo)$coefficients$cond[,2]),3),"]"),
                      "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients$cond[,1]-1.96*summary(modelo)$coefficients$cond[,2])-1)*100,2),"%; ",
                                                  round((exp(summary(modelo)$coefficients$cond[,1]+1.96*summary(modelo)$coefficients$cond[,2])-1)*100,2),"%]"),
                      "Valor-p" = round(summary(modelo)$coefficients$cond[,4],4))
  return(Tabela)
}

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

####=============================
#### Carregando o banco de dados 
####=============================
dados = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-telessaude/questionário para avaliadores final.xlsx", sheet = 3)},
                 error = function(e) {read.xlsx("D:/NESCON/Trabalho - Mariana/qualidade-telessaude/questionário para avaliadores final.xlsx", sheet = 3)})
dados_relevancia = dados %>% filter(Tipo.de.avaliação == 'Relevância') %>% as.data.frame()
dados_clareza = dados %>% filter(Tipo.de.avaliação == 'Clareza') %>% as.data.frame()

####======================
#### Validade de conteúdo
####======================
# Função para calcular o IVC
calcula_ivc = function(data) {
  avaliadores = data %>% select(starts_with("J"))
  
  # Cálculo do IVC item a item (I-IVC)
  i_ivc = rowMeans(avaliadores == 3)
  
  # Cálculo do IVC da escala (S-IVC)
  s_ivc = mean(i_ivc)
  
  # Cálculo do CVR
  n_avaliadores = ncol(avaliadores)
  cvr = apply(avaliadores, 1, function(x) {
    prop = sum(x == 3) / n_avaliadores
    prop * 2 - 1
  })
  
  # Combina os resultados em um data frame
  resultados = data.frame(
    Assunto = data$Assunto,
    Questão = data$Questão,
    I_IVC = i_ivc,
    CVR = cvr
  )
  
  return(list(Resultados = resultados, S_IVC = s_ivc))
}

# Cálculo do IVC para relevância
ivc_relevancia = calcula_ivc(dados_relevancia)
resultados_relevancia = ivc_relevancia$Resultados
s_ivc_relevancia = ivc_relevancia$S_IVC

# Cálculo do IVC para clareza
ivc_clareza = calcula_ivc(dados_clareza)
resultados_clareza = ivc_clareza$Resultados
s_ivc_clareza = ivc_clareza$S_IVC

# Cálculo do IVC para relevância e clareza
ivc_geral = calcula_ivc(dados)
resultados_geral = ivc_geral$Resultados
s_ivc_geral = ivc_geral$S_IVC

# Exibe os resultados
cat("\nResultados de Relevância:\n")
print(resultados_relevancia)
cat("\nS-IVC Relevância:", s_ivc_relevancia, "\n")

cat("\nResultados de Clareza:\n")
print(resultados_clareza)
cat("\nS-IVC Clareza:", s_ivc_clareza, "\n")

cat("\nResultados gerais:\n")
print(resultados_geral)
cat("\nS-IVC geral:", s_ivc_geral, "\n")

####==============
#### Concordância
####==============
require(irr)
kappa2(cbind(dados$J1, dados$J2), weight = "squared")
kappa2(cbind(dados$J1, dados$J2), weight = "squared")
kappa2(cbind(dados$J1, dados$J3), weight = "squared")
kappa2(cbind(dados$J1, dados$J4), weight = "squared")
kappa2(cbind(dados$J1, dados$J5), weight = "squared")
kappa2(cbind(dados$J1, dados$J6), weight = "squared")

kappa2(cbind(dados$J2, dados$J3), weight = "squared")
kappa2(cbind(dados$J2, dados$J4), weight = "squared")
kappa2(cbind(dados$J2, dados$J5), weight = "squared")
kappa2(cbind(dados$J2, dados$J6), weight = "squared")

kappa2(cbind(dados$J3, dados$J4), weight = "squared")
kappa2(cbind(dados$J3, dados$J5), weight = "squared")
kappa2(cbind(dados$J3, dados$J6), weight = "squared")

kappa2(cbind(dados$J4, dados$J5), weight = "squared")
kappa2(cbind(dados$J4, dados$J6), weight = "squared")

kappa2(cbind(dados$J5, dados$J6), weight = "squared")

####================
#### Confiabilidade
####================
# Coeficiente de Kendall
kendall_relevancia = dados_relevancia %>%
  select(starts_with("J")) %>%
  as.matrix() %>%
  kendall()

cat("\nCoeficiente de Concordância de Kendall para Relevância:\n")
print(kendall_relevancia)

kendall_clareza = dados_clareza %>%
  select(starts_with("J")) %>%
  as.matrix() %>%
  kendall()
cat("\nCoeficiente de Concordância de Kendall para Clareza:\n")
print(kendall_clareza)

kendall_geral = dados %>%
  select(starts_with("J")) %>%
  as.matrix() %>%
  kendall()
cat("\nCoeficiente de Concordância de Kendall geral:\n")
print(kendall_geral)

# Alfa de Cronbach
cronbach_relevancia = dados_relevancia %>%
  select(starts_with("J")) %>%
  alpha()
cat("\nAlfa de Cronbach para Relevância:\n")
print(cronbach_relevancia$total)

cronbach_clareza = dados_clareza %>%
  select(starts_with("J")) %>%
  alpha()
cat("\nAlfa de Cronbach para Clareza:\n")
print(cronbach_clareza$total)

cronbach_geral = dados %>%
  select(starts_with("J")) %>%
  alpha()
cat("\nAlfa de Cronbach geral:\n")
print(cronbach_geral$total)



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
### solicita??o 24/04
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
### Correla??o
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









