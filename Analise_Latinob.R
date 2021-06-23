
setwd("C:/Users/Duda/Desktop/DissertacaoCods")
require(dplyr)
require(vctrs)
require(lmerTest)
require(lme4)
require(foreign)
require(haven)
require(MASS)
require(tidyverse)

# Análise Latinobarómetro:
# Carregar bases:
latino2018 <- readRDS("Latinobarometro_2018_Esp_R_v20190303.rds")             
latino2018br <- filter(latino2018, IDENPA == 76)

load("Latinobarometro2017Eng_v20180117.rdata")
latino2017br <- filter(Latinobarometro2017Eng_v20180117, idenpa == 76)

load("Latinobarometro2016Eng_v20170205.rdata")
latino2016br <- filter(Latinobarometro2016Eng_v20170205, idenpa == 76)

load("Latinobarometro_2015_Eng.rdata")
latino2015br <- filter(Latinobarometro_2015_Eng, idenpa == 76)

load("Latinobarometro2013Eng.rdata")
latino2013br <- filter(Latinobarometro2013Eng, idenpa == 76)

latino2011 <- read_dta("Latinobarometro_2011_eng.dta")
latino2011br <- filter(latino2011, idenpa == 76)

latino2009 <- read_dta("Latinobarometro_2009_datos_eng_v2014_06_27.dta")
latino2009br <- filter(latino2009, idenpa == 76)

latino2008 <- read_dta("Latinobarometro_2008_datos_eng_v2014_06_27.dta")
latino2008br <- filter(latino2008, idenpa == 76)

latino2006 <- read_dta("Latinobarometro_2006_datos_eng_v2014_06_27.dta")
latino2006br <- filter(latino2006, idenpa == 76)


---

# 2006:
latino2006br$p22st_b # Eficácia Externa
latino2006br$p32st_c # Confiança nos jornais
latino2006br$p35st_c # Confiança na TV
latino2006br$p35st_d # Confiança na comissão eleitoral
latino2006br$p37st_aa # Freqyencia com que fala sobre política
latino2006br$p37st_ba # Assinou/assinaria petição
latino2006br$p39st # Acha que eleiçõs são justas ou corrompidas?
latino2006br$p40nf # votou
latino2006br$p47st # ideologia
latino2006br$p67st_a # frequencia com que assiste notícias na tv
latino2006br$p67st_b # frequencia com que assiste notícias no jornal
latino2006br$reeduc1 # Educação
latino2006br$s7 # Idade
latino2006br$s11 # anos de estudo
latino2006br$s1 # Renda subjetiva 


# 2008:
latino2008br$p25st # Eficácia externa
latino2008br$p31st_d # Confiança nos jornais
latino2008br$p31st_e # Confiança na televisão
latino2008br$p53st # Votou
latino2008br$p56st # Ideologia
latino2008br$p82st_c # participar de protestos
latino2008br$p82st_d # participar de boicotes
latino2008br$p83st_c # contactou governante
latino2008br$p83st_b # contactou servidor municipal
latino2008br$p83st_d # contactou membro do congresso
latino2008br$p83st_e # contactou a mídia
latino2008br$p95st_a # como se informa sobre política: familia
latino2008br$p95st_b # como se informa sobre política: amigos
latino2008br$p95st_c # como se informa sobre política: povo do trabalho
latino2008br$p95st_d # como se informa sobre política: povo do estudo
latino2008br$p95st_e # como se informa sobre política: radio
latino2008br$p95st_f # como se informa sobre política: jornal/revista
latino2008br$p95st_g # como se informa sobre política: internet
latino2008br$p95st_h # como se informa sobre política: tv
latino2008br$p95st_i # como se informa sobre política: outro
latino2008br$p95st_j # como se informa sobre política: nenhum
latino2008br$s9 # Idade
latino2008br$s15 # Educacao
latino2008br$s2 # renda subjetiva

# 2009:
latino2009br$p39st # Eleições são limpas ou objeto de fraude
latino2009br$p22st # Eficácia externa



# 2015:
latino2015br$P19N.H # Confiança nas eleições
latino2015br$P21ST.B # Participou de protesto autorizado
latino2015br$P21ST.c # Participou de protesto não autorizado
latino2015br$P21N.E # Reclamou nas mídias sociais
latino2015br$P24STM # Votou
latino2015br$P26STM # Eleições são justas ou fraudulentas
latino2015br$P28STGBS # Eficácia interna
latino2015br$P30STGBS # Identifica partido
latino2015br$P49ST.E # Qualidade do trabalho das instituições eleitorais
latino2015br$S4 # Renda subjetiva
latino2015br$S6 # Classe social
latino2015br$S13 # Idade
latino2015br$S19 # Escolaridade


# 2018:
latino2018br <- mutate(latino2018br, part1 = P19ST.A + P19ST.B + P19ST.C + P19ST.D +
                         P19ST.E + P19ST.F + P19ST.G + P19ST.H + P19NC.I + P19NC.J +
                         P19F.K + P19ST.L)             

latino2018br <- mutate(latino2018br, part2 = ifelse(P21STGBS.A == 95, 0,
                                                    ifelse(P21STGBS.A == 96, 0,
                                                    ifelse(P21STGBS.A == 76001, 1,
                                                    ifelse(P21STGBS.A == 76003, 1,
                                                    ifelse(P21STGBS.A == 76004, 1, 
                                                    ifelse(P21STGBS.A == 76005, 1,
                                                    ifelse(P21STGBS.A == 76007, 1,
                                                    ifelse(P21STGBS.A == 76008, 1,
                                                    ifelse(P21STGBS.A == 76009, 1,
                                                    ifelse(P21STGBS.A == 76019, 1,
                                                    ifelse(P21STGBS.A == 76026, 1,
                                                    ifelse(P21STGBS.A == 76028, 1,
                                                    ifelse(P21STGBS.A == 76032, 1,
                                                    ifelse(P21STGBS.A == 76033, 1,
                                                    ifelse(P21STGBS.A == 76034, 1,
                                                    ifelse(P21STGBS.A == 76035, 1,
                                                    ifelse(P21STGBS.A == 76036, 1,
                                                    ifelse(P21STGBS.A == 76037, 1, "NA"
                                                    )))))))))))))))))))  

count(latino2018br, P15STGBSC.H)
count(latino2018br, P16NC.D)
quant <- count(latino2018br, P21STGBS.A)
count(latino2018br, part2)
count(latino2018br, part1)
count(latino2018br, P22ST)
latino2018$P19ST.H

latino2018br$P22ST <- as.numeric(latino2018br$P22ST)
latino2018br$EDAD <- as.numeric(latino2018br$EDAD)
latino2018br$S10 <- as.numeric(latino2018br$S10)
latino2018br$S1 <- as.numeric(latino2018br$S1)
latino2018br$P14ST <- as.numeric(latino2018br$P14ST)
latino2018br$P15STGBSC.H <- as.numeric(latino2018br$P15STGBSC.H)
latino2018br$P16NC.D <- as.numeric(latino2018br$P16NC.D)
latino2018br$part1 <- as.numeric(latino2018br$part1)
latino2018br$part2 <- as.numeric(latino2018br$part2)
latino2018br$P19ST.H <- as.numeric(latino2018br$P19ST.H)
latino2018br$P19ST.G <- as.numeric(latino2018br$P19ST.G)
latino2018br$P19NC.I <- as.numeric(latino2018br$P19NC.I)
latino2018br$P19F.K <- as.numeric(latino2018br$P19F.K)
latino2018br$P19NC.J <- as.numeric(latino2018br$P19NC.J)
latino2018br$S12M.E <- as.numeric(latino2018br$S12M.E)
latino2018br$S12M.F <- as.numeric(latino2018br$S12M.F)


# Selecionar e renomear variáveis de interesse:
latinosel <- data.frame("ideologia" = latino2018br$P22ST,
                        "idade" = latino2018br$EDAD,
                        "educacao" = latino2018br$S10,
                        "renda" = latino2018br$S1,             
                        "efext" = latino2018br$P14ST,            
                        "confiancaeleicoes" = latino2018br$P15STGBSC.H,
                        "confiancamidia" = latino2018br$P16NC.D,
                        "quantmeios" = latino2018br$part1,
                        "votoucandidatobin" = latino2018br$part2,
                        "infointernet" = latino2018br$P19ST.G,
                        "infoFacebook" = latino2018br$P19NC.I,
                        "infotv" = latino2018br$P19ST.H,
                        "infoYouTube" = latino2018br$P19F.K,
                        "infoTwitter" = latino2018br$P19NC.J,
                        "WhatsApp" = latino2018br$S12M.E,
                        "Instagram" = latino2018br$S12M.F)

# Recodificar variáveis:  
latinosel$ideologia <- dplyr::recode(latinosel$ideologia,
                              "0" = "1", "1" = "2", "2" = "3", "3" = "4", 
                              "4" = "5", "5" = "6", "6" = "7", "7" = "8",
                              "8" = "9", "9" = "10", "10" = "11", "-1" = "NA",
                              "-2" = "NA", "-3" = "NA",  "-4" = "NA", "-5" = "NA",
                              "-8" = "NA")

latinosel$idade[latinosel$idade == -2] <- NA

educ <- c(-1, -2)
latinosel$educacao[latinosel$educacao %in% educ] <- NA

renda <- c(-1,-2,-4)
latinosel$renda[latinosel$renda %in% renda] <- NA
latinosel$renda <- dplyr::recode(latinosel$renda,
                          "1" = "5", "2" = "4", "3" = "3", "4" = "2", 
                          "5" = "1", "NA" = "NA")

latinosel$confiancaeleicoes <- dplyr::recode(latinosel$confiancaeleicoes, 
                                      "1" = "4", "2" = "3", "3" = "2", "4" = "1", 
                                      "-1" = "NA", "-2" = "NA", "-3" = "NA",  "-4" = "NA")

latinosel$confiancamidia <- dplyr::recode(latinosel$confiancamidia, 
                                   "1" = "4", "2" = "3", "3" = "2", "4" = "1", 
                                   "-1" = "NA", "-2" = "NA")

latinosel$efext <- dplyr::recode(latinosel$efext, 
                          "1" = "0", "2" = "1", "-1" = "NA")

latinosel <- filter(latinosel, idade <=70)
latinosel <- filter(latinosel, idade >=18)


# Transformar variáveis em numéricas:
latinosel$ideologia <- as.numeric(as.character(latinosel$ideologia))
latinosel$renda <- as.numeric(as.character(latinosel$renda))
latinosel$efext <- as.numeric(as.character(latinosel$efext))
latinosel$confiancaeleicoes <- as.numeric(as.character(latinosel$confiancaeleicoes))
latinosel$confiancamidia <- as.numeric(as.character(latinosel$confiancamidia))

# Testar correlação entre as variáveis da base: 
summary(latinosel)
cor(latinosel)

# Regressões:

# VD votou candidato:
reglatino1 <- glm(votoucandidatobin ~ confiancaeleicoes + ideologia + idade + renda + 
                     educacao + efext, data = latinosel, family = "binomial")
summary(reglatino1)
car::vif(reglatino1)

# Diagnóstico:
mydata1 <- data.frame(latinosel$ideologia, latinosel$confiancaeleicoes,
                      latinosel$idade, latinosel$renda, latinosel$educacao, 
                      latinosel$votoucandidatobin, latinosel$efext)

mydata1 <- na.omit(mydata1)
predictors <- colnames(mydata1)
# Bind the logit and tidying the data for plot
probabilities <- predict(reglatino1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

mydata <- lapopagreg %>%
  dplyr::select_if(is.numeric) 

require(tidyr)
mydata1 <- mydata1 %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


count(latinosel, votoucandidatobin)



