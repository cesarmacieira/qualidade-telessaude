####=============================
#### Trabalho Mariana - Análises
####=============================
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

####=============================
#### Carregando o banco de dados 
####=============================
dados = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-telessaude/Dados respostas questionários 12-12-2024.xlsx", sheet = 1)},
                 error = function(e) {read.xlsx("D:/NESCON/Trabalho - Mariana/qualidade-telessaude/Dados respostas questionários 12-12-2024.xlsx", sheet = 1)})

####=====================
#### Tratamento de dados
####=====================
dados$País = trimws(tolower(gsub("\\s*\\(.*?\\)", "", dados$`Estado(ou.País.para.extrangeiros)`)))
dados$País = gsub("^m[eé]xico.*", "México", dados$País, ignore.case = TRUE)
dados$País = gsub("^brasil.*", "Brasil", dados$País, ignore.case = TRUE)
dados$País = gsub("^per[uú].*", "Perú", dados$País, ignore.case = TRUE)
dados$País = gsub("^chile.*", "Chile", dados$País, ignore.case = TRUE)

df1 = dados %>% select(País,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,
                       Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,Q25,Q26,Q27,
                       Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42,Q43,Q44,Q45,Q46,Q47,Q48,Q49,
                       Q50,Q51,Q52,Q53,Q54,Q55,Q56,Q57,Q58,Q59,Q60,Q61,Q62)

dicionario_respostas <- list(
  "Sí" = "Sim",
  "No" = "Não",
  "Establecimientos, instalaciones e infraestructura requeridos o involucrados" = "Estabelecimentos, instalações e infraestrutura necessários ou envolvidos",
  "Disponibilidad de especialistas" = "Disponibilidade de especialistas",
  "Personal clínico local" = "Equipe clínica local",
  "Horarios y días de disponibilidad" = "Horários e dias de disponibilidade",
  "Horarios y días de disponibilidad, Horarios y días de indisponibilidad" = "Horários e dias de disponibilidade, Horários e dias de indisponibilidade",
  "Ninguno de los dos" = "Nenhum dos dois"
)

# Função para codificar respostas
codificar_respostas <- function(resposta) {
  return(ifelse(resposta %in% names(dicionario_respostas), 
                dicionario_respostas[[resposta]], 
                resposta))
}

# Aplicar a codificação para todas as colunas começadas com 'Q'
dados_codificados <- df1 %>%
  mutate(across(starts_with("Q"), ~ sapply(., codificar_respostas)))

####====================
#### Análise descritiva
####====================
Tabela1 = do.call(rbind,dados_codificados %>% select(País,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15) %>% map(DescritivaCat))
Tabela2 = do.call(rbind,dados_codificados %>% select(Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,Q25,Q26,Q27) %>% map(DescritivaCat))
Tabela3 = do.call(rbind,dados_codificados %>% select(Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42,Q43,Q44,Q45,Q46,Q47,Q48,Q49,
                                                     Q50,Q51,Q52,Q53,Q54,Q55,Q56,Q57,Q58,Q59,Q60,Q61,Q62) %>% map(DescritivaCat))
write.xlsx(Tabela1 %>% as.data.frame(),'Tabela 1.xlsx', rowNames = T)
write.xlsx(Tabela2 %>% as.data.frame(),'Tabela 2.xlsx', rowNames = T)
write.xlsx(Tabela3 %>% as.data.frame(),'Tabela 3.xlsx', rowNames = T)
