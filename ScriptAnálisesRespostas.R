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

# df1 = dados %>% select(País,Serviço,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,
#                        Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,Q25,Q26,Q27,
#                        Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42,Q43,Q44,Q45,Q46,Q47,Q48,Q49,
#                        Q50,Q51,Q52,Q53,Q54,Q55,Q56,Q57,Q58,Q59,Q60,Q61,Q62)
df1 = dados

dicionario_respostas = list(
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
codificar_respostas = function(resposta) {
  return(ifelse(resposta %in% names(dicionario_respostas), 
                dicionario_respostas[[resposta]], 
                resposta))
}

# Aplicar a codificação para todas as colunas começadas com 'Q'
dados_codificados = df1 %>%
  mutate(across(starts_with("Q"), ~ sapply(., codificar_respostas)))

dados_BR_MX = dados_codificados %>% filter(País == 'Brasil' | País == 'México')

####====================
#### Análise descritiva
####====================
Tabela1 = do.call(rbind,dados_codificados %>% select(País,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15) %>% map(DescritivaCat))
Tabela2 = do.call(rbind,dados_codificados %>% select(Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,Q25,Q26,Q27) %>% map(DescritivaCat))
Tabela3 = do.call(rbind,dados_codificados %>% select(Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42,Q43,Q44,Q45,Q46,Q47,Q48,Q49,
                                                     Q50,Q51,Q52,Q53,Q54,Q55,Q56,Q57,Q58,Q59,Q60,Q61,Q62) %>% map(DescritivaCat))
# write.xlsx(Tabela1 %>% as.data.frame(),'Tabela 1.xlsx', rowNames = T)
# write.xlsx(Tabela2 %>% as.data.frame(),'Tabela 2.xlsx', rowNames = T)
# write.xlsx(Tabela3 %>% as.data.frame(),'Tabela 3.xlsx', rowNames = T)

####===========================
#### Comparações e associações
####===========================
table(dados_codificados$Q2,dados_codificados$País)
prop.table(table(dados_codificados$Q2,dados_codificados$País))
#QuiQuadrado_Fisher(dados_codificados$Q2,dados_codificados$País,'1','fisher')
Tabela4 = rbind(QuiQuadrado_Fisher(dados_codificados$Q1,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q3,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q4,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q5,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q6,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q7,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q8,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q9,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q10,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q11,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q12,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q13,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q14,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q15,dados_codificados$País,'2','fisher'))
#write.xlsx(Tabela4 %>% as.data.frame(),'Tabela 4.xlsx', rowNames = T)

Tabela4.1 = rbind(QuiQuadrado_Fisher(dados_BR_MX$Q1,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q3,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q4,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q5,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q6,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q7,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q8,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q9,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q10,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q11,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q12,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q13,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q14,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q15,dados_BR_MX$País,'2','fisher'))
#write.xlsx(Tabela4.1 %>% as.data.frame(),'Tabela 4.1.xlsx', rowNames = T)

table(dados_codificados$Q17,dados_codificados$País)
prop.table(table(dados_codificados$Q17,dados_codificados$País))
#QuiQuadrado_Fisher(dados_codificados$Q2,dados_codificados$País,'1','fisher')
Tabela5 = rbind(QuiQuadrado_Fisher(dados_codificados$Q16,dados_codificados$País,'2','fisher'),
                #QuiQuadrado_Fisher(dados_codificados$Q17,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q18,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q19,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q20,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q21,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q22,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q23,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q24,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q25,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q26,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q27,dados_codificados$País,'2','fisher'))
#write.xlsx(Tabela5 %>% as.data.frame(),'Tabela 5.xlsx', rowNames = T)

Tabela5.1 = rbind(QuiQuadrado_Fisher(dados_BR_MX$Q16,dados_BR_MX$País,'2','fisher'),
                  #QuiQuadrado_Fisher(dados_BR_MX$Q17,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q18,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q19,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q20,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q21,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q22,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q23,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q24,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q25,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q26,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q27,dados_BR_MX$País,'2','fisher'))
#write.xlsx(Tabela5.1 %>% as.data.frame(),'Tabela 5.1.xlsx', rowNames = T)

table(dados_codificados$Q2,dados_codificados$País)
prop.table(table(dados_codificados$Q2,dados_codificados$País))
#QuiQuadrado_Fisher(dados_codificados$Q2,dados_codificados$País,'1','fisher')
Tabela6 = rbind(QuiQuadrado_Fisher(dados_codificados$Q28,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q29,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q30,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q31,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q32,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q33,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q34,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q35,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q36,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q37,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q38,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q39,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q40,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q41,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q42,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q43,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q44,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q45,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q46,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q47,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q48,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q49,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q50,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q51,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q52,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q53,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q54,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q55,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q56,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q57,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q58,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q59,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q60,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q61,dados_codificados$País,'2','fisher'),
                QuiQuadrado_Fisher(dados_codificados$Q62,dados_codificados$País,'2','fisher'))
#write.xlsx(Tabela6 %>% as.data.frame(),'Tabela 6.xlsx', rowNames = T)

Tabela6.1 = rbind(QuiQuadrado_Fisher(dados_BR_MX$Q28,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q29,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q30,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q31,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q32,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q33,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q34,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q35,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q36,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q37,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q38,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q39,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q40,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q41,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q42,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q43,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q44,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q45,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q46,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q47,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q48,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q49,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q50,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q51,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q52,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q53,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q54,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q55,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q56,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q57,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q58,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q59,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q60,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q61,dados_BR_MX$País,'2','fisher'),
                  QuiQuadrado_Fisher(dados_BR_MX$Q62,dados_BR_MX$País,'2','fisher'))
#write.xlsx(Tabela6.1 %>% as.data.frame(),'Tabela 6.1.xlsx', rowNames = T)

####========================
#### Criação de indicadores
####========================
dados_codificados$Q3_num = case_when(dados_codificados$Q3 == 'Disponibilidade de especialistas' ~ 0.6,
                                     dados_codificados$Q3 == 'Equipe clínica local' ~ 0.4,
                                     dados_codificados$Q3 == 'Estabelecimentos, instalações e infraestrutura necessários ou envolvidos' ~ 1)
dados_codificados$Q7_num = 
  case_when(dados_codificados$Q7 == 'Nenhum dos dois' ~ 0,
            dados_codificados$Q7 == 'Horários e dias de disponibilidade' | dados_codificados$Q7 == 'Horários e dias de indisponibilidade' ~ 0.5,
            dados_codificados$Q7 == 'Horários e dias de disponibilidade, Horários e dias de indisponibilidade' ~ 1)

dados_ind = dados_codificados %>%
  mutate(across(c(Q1,Q2,Q4,Q5,Q6,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,
                  Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,Q25,Q26,Q27,
                  Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42,Q43,Q44,Q45,Q46,Q47,Q48,Q49,
                  Q50,Q51,Q52,Q53,Q54,Q55,Q56,Q57,Q58,Q59,Q60,Q61,Q62), ~ ifelse(. == "Sim", 1, 0), .names = "{.col}_num"))

# Função para categorizar os valores
categorizar <- function(valor, max_valor) {
  percentual <- (valor / max_valor) * 100
  categoria <- ifelse(is.na(percentual), NA, ifelse(percentual < 60, "Baixa", ifelse(percentual <= 79, "Média", "Alta")))
  factor(categoria, levels = c("Baixa", "Média", "Alta"), ordered = TRUE)
}

dados_ind = dados_ind %>%
  mutate(Planejamento_Servico = rowSums(across(c(Q1_num,Q2_num,Q3_num,Q4_num,Q5_num,Q6_num,Q7_num,Q8_num,Q9_num,
                                                 Q10_num,Q11_num,Q12_num,Q13_num,Q14_num,Q15_num))),
         Gestao_Riscos = rowSums(across(c(Q16_num,Q17_num,Q18_num,Q19_num,Q20_num,Q21_num,Q22_num,Q23_num,Q24_num,Q25_num,Q26_num,Q27_num))),
         Gestao_Tecnologia = rowSums(across(c(Q28_num,Q29_num,Q30_num,Q31_num,Q32_num,Q33_num,Q34_num,Q35_num,Q36_num,Q37_num,Q38_num,
                                              Q39_num,Q40_num,Q41_num,Q42_num,Q43_num,Q44_num,Q45_num,Q46_num,Q47_num,Q48_num,Q49_num,
                                              Q50_num,Q51_num,Q52_num,Q53_num,Q54_num,Q55_num,Q56_num,Q57_num,Q58_num,Q59_num,Q60_num,Q61_num,Q62_num))),
         Geral = rowSums(across(c(Planejamento_Servico,Gestao_Riscos,Gestao_Tecnologia))),
         Planejamento_Servico_cat = categorizar(Planejamento_Servico, 15),
         Gestao_Riscos_cat = categorizar(Gestao_Riscos, 12),  
         Gestao_Tecnologia_cat = categorizar(Gestao_Tecnologia, 35),
         Geral_cat = categorizar(Geral, 62))
dados_ind %>% select(Planejamento_Servico,Gestao_Riscos,Gestao_Tecnologia,Geral)
#write.xlsx(dados_ind %>% as.data.frame(), 'Dados dos indicadores por serviço.xlsx', rowNames = F)

####=========================
#### Análise dos indicadores
####=========================
Tabela7 = do.call(rbind,dados_ind %>% select(Planejamento_Servico,Gestao_Riscos,Gestao_Tecnologia,Geral) %>% map(DescritivaCat))
#write.xlsx(Tabela7 %>% as.data.frame(),'Tabela 7.xlsx', rowNames = T)

Tabela8 = do.call(rbind,dados_ind %>% select(Planejamento_Servico,Gestao_Riscos,Gestao_Tecnologia,Geral) %>% map(DescritivaNum))
#write.xlsx(Tabela8 %>% as.data.frame(),'Tabela 8.xlsx', rowNames = T)

dados_ind %>% select(Planejamento_Servico,Gestao_Riscos,Gestao_Tecnologia,Geral) %>% map(TesteDeNormalidade)
Tabela8.1 = rbind(AnovaIndepTeste(dados_ind$Planejamento_Servico,dados_ind$País)$tabela,
                  KruskalTeste(dados_ind$Gestao_Riscos,dados_ind$País)$tabela,
                  KruskalTeste(dados_ind$Gestao_Tecnologia,dados_ind$País)$tabela,
                  AnovaIndepTeste(dados_ind$Geral,dados_ind$País)$tabela)
#write.xlsx(Tabela8.1 %>% as.data.frame(),'Tabela 8.1.xlsx', rowNames = T)

Tabela9 = do.call(rbind,dados_ind %>% select(Planejamento_Servico_cat,Gestao_Riscos_cat,Gestao_Tecnologia_cat,Geral_cat) %>% map(DescritivaCat))
#write.xlsx(Tabela9 %>% as.data.frame(),'Tabela 9.xlsx', rowNames = T)

####===========================================
#### Comparações e associações dos indicadores
####===========================================
Tabela10 = rbind(QuiQuadrado_Fisher(dados_ind$Planejamento_Servico_cat,dados_ind$País,'2','fisher'),
                 QuiQuadrado_Fisher(dados_ind$Gestao_Riscos_cat,dados_ind$País,'2','fisher'),
                 QuiQuadrado_Fisher(dados_ind$Gestao_Tecnologia_cat,dados_ind$País,'2','fisher'),
                 QuiQuadrado_Fisher(dados_ind$Geral_cat,dados_ind$País,'2','fisher'))
#write.xlsx(Tabela10 %>% as.data.frame(),'Tabela 10.xlsx', rowNames = T)

dados_ind_BR_MX = dados_ind %>% filter(País == 'Brasil' | País == 'México')
Tabela10.1 = rbind(QuiQuadrado_Fisher(dados_ind_BR_MX$Planejamento_Servico_cat,dados_ind_BR_MX$País,'2','fisher'),
                 QuiQuadrado_Fisher(dados_ind_BR_MX$Gestao_Riscos_cat,dados_ind_BR_MX$País,'2','fisher'),
                 QuiQuadrado_Fisher(dados_ind_BR_MX$Gestao_Tecnologia_cat,dados_ind_BR_MX$País,'2','fisher'),
                 QuiQuadrado_Fisher(dados_ind_BR_MX$Geral_cat,dados_ind_BR_MX$País,'2','fisher'))
#write.xlsx(Tabela10.1 %>% as.data.frame(),'Tabela 10.1.xlsx', rowNames = T)