
require(readxl)

basesp <- read_excel("BaseSPOnda1.xlsx")
basegrafico <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha3")
basegraficohaddad <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha4")
basegraficobolsonaro <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha5")


count(basesp,TERCALIVRE)
count(basesp,JOVEMPAN)
count(basesp,UOL)
count(basesp,BRASILPARALELO)
count(basesp,BRASIL247)
count(basesp,RECORD)
count(basesp,FOLHA)
count(basesp,GLOBO)

# Ajustar base para plotar gráfico:
df2 <- tidyr::pivot_longer(basegrafico, cols=c('Globo', 'Record', 'Folha',
                                               'UOL', 'Tercalivre', 'Jovempan',
                                               'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")
head(df2)

windowsFonts(Times=windowsFont("Times New Roman"))
geral <- ggplot(df2, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  labs(title="Geral - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                           labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                                      "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                                      "UOL"))+
    theme(text=element_text(family="Times"))+
  guides(fill=FALSE, color=FALSE)


# Contar uso a partir de escolha de candidato:
count(basesp, VOTOUPRESIDENTE2TURNO)
tl <- count(basesp,TERCALIVRE, VOTOUPRESIDENTE2TURNO)
jp <- count(basesp,JOVEMPAN, VOTOUPRESIDENTE2TURNO)
uol <- count(basesp,UOL, VOTOUPRESIDENTE2TURNO)
bp <- count(basesp,BRASILPARALELO, VOTOUPRESIDENTE2TURNO)
b247 <- count(basesp,BRASIL247, VOTOUPRESIDENTE2TURNO)
rec <- count(basesp,RECORD, VOTOUPRESIDENTE2TURNO)
fsp <- count(basesp,FOLHA, VOTOUPRESIDENTE2TURNO)
glob <- count(basesp,GLOBO, VOTOUPRESIDENTE2TURNO)


# Gráfico Haddad:
df3 <- tidyr::pivot_longer(basegraficohaddad, cols=c('Globo', 'Record', 'Folha',
                                               'UOL', 'Tercalivre', 'Jovempan',
                                               'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")

windowsFonts(Times=windowsFont("Times New Roman"))
haddad <- ggplot(df3, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  labs(title="Eleitores de Haddad - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  guides(fill=FALSE, color=FALSE)

# Gráfico Bolsonaro:
df4 <- tidyr::pivot_longer(basegraficobolsonaro, cols=c('Globo', 'Record', 'Folha',
                                               'UOL', 'Tercalivre', 'Jovempan',
                                               'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")

windowsFonts(Times=windowsFont("Times New Roman"))
bolsonaro <- ggplot(df4, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  labs(title="Eleitores de Bolsonaro - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo:")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")

grid.arrange(geral, haddad, bolsonaro, nrow=3)


basesp$IDEOLOGIA <- as.numeric(basesp$IDEOLOGIA) 
basesp$IDADE <- as.numeric(basesp$IDADE)
basesp$CONFIANCAGRANDEMIDIA <- as.numeric(basesp$CONFIANCAGRANDEMIDIA) 
basesp$TERCALIVRE <- as.factor(basesp$TERCALIVRE) 
basesp$JOVEMPAN <- as.factor(basesp$JOVEMPAN) 
basesp$UOL <- as.factor(basesp$UOL) 
basesp$BRASILPARALELO <- as.factor(basesp$BRASILPARALELO) 
basesp$BRASIL247 <- as.factor(basesp$BRASIL247) 
basesp$RECORD <- as.factor(basesp$RECORD) 
basesp$FOLHA <- as.factor(basesp$FOLHA) 
basesp$GLOBO <- as.factor(basesp$GLOBO) 
basesp$VOTOUMUNICIPAL <- as.numeric(basesp$VOTOUMUNICIPAL)
basesp$VOTOUMUNICIPAL <- as.factor(basesp$VOTOUMUNICIPAL)
basesp$VOTOUCANDIDATOMUNICIPAL <- as.numeric(basesp$VOTOUCANDIDATOMUNICIPAL)
basesp$VOTOUCANDIDATOMUNICIPAL <- as.factor(basesp$VOTOUCANDIDATOMUNICIPAL)
basesp$VOTOUCANDIDATO <- as.numeric(basesp$VOTOUCANDIDATO) 
basesp$VOTOUCANDIDATO <- as.factor(basesp$VOTOUCANDIDATO) 
basesp$VOTOU <- as.numeric(basesp$VOTOU) 
basesp$VOTOU <- as.factor(basesp$VOTOU) 
basesp$VOTOU <- as.numeric(basesp$VOTOU) 
basesp$VOTOUBOLSONARO <- as.numeric(basesp$VOTOUBOLSONARO) 


basesp2 <- filter(basesp, VOTOUPRESIDENTE2TURNO != "NA")
basesp2$VOTOUPRESIDENTE2TURNO <- as.factor(basesp2$VOTOUPRESIDENTE2TURNO)



# Modelos de regressão:

# Terça Livre:
regsp1 <- polr(TERCALIVRE ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                           SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
                        data = basesp, Hess = T)
summary(regsp1)
(ctable <- coef(summary(regsp1)))
psp1 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp1))
(ci <- confint(regsp1))
exp(cbind(OR = coef(regsp1), ci))


# Jovem Pan:
regsp2 <- polr(JOVEMPAN ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp2)
(ctable <- coef(summary(regsp2)))
psp2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp2))

# UOL: 
regsp3 <- polr(UOL ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SEXOFEMININO + SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp3)
(ctable <- coef(summary(regsp3)))
psp3 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp3))

# Brasil Paralelo:
regsp4 <- polr(BRASILPARALELO ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA + 
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp4)
(ctable <- coef(summary(regsp4)))
psp4 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp4))

# Brasil 247:
regsp5 <- polr(BRASIL247 ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SEXOFEMININO + SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp5)
(ctable <- coef(summary(regsp5)))
psp5 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp5))
(ci <- confint(regsp5))
exp(cbind(OR = coef(regsp5), ci))



# Rede Record:
regsp6 <- polr(RECORD ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp6)
(ctable <- coef(summary(regsp6)))
psp6 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp6))

# Folha de São Paulo:
regsp7 <- polr(FOLHA ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp7)
(ctable <- coef(summary(regsp7)))
psp7 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp7))

# Rede Globo:
regsp8 <- polr(GLOBO ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp8)
(ctable <- coef(summary(regsp8)))
psp8 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp8))
(ci <- confint(regsp8))
exp(cbind(OR = coef(regsp8), ci))

# Votou nas eleições municipais:
regsp9 <- glm(VOTOUMUNICIPAL ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                  SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
               data = basesp, family = binomial)
summary(regsp9)


# Votou em candidato nas eleições municipais:
regsp9.2 <- glm(VOTOUCANDIDATOMUNICIPAL ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
              data = basesp, family = binomial)
summary(regsp9.2)
(ci <- confint(regsp9.2))
exp(cbind(OR = coef(regsp9.2), ci))

# Votou nas eleições presidenciais:
regsp10 <- glm(VOTOU ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                  SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
               data = basesp, family = binomial)
summary(regsp10)

# Votou em candidato:
regsp11 <- glm(VOTOUCANDIDATO ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                  SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
               data = basesp, family = binomial)
summary(regsp11)
vif(regsp11)

count(basesp,TERCALIVRE)
count(basesp,JOVEMPAN)
count(basesp,UOL)
count(basesp,BRASILPARALELO)
count(basesp,BRASIL247)
count(basesp,RECORD)
count(basesp,FOLHA)
count(basesp,GLOBO)

regsp2EX <- polr(JOVEMPAN ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                   SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp2EX)
(ctable <- coef(summary(regsp2EX)))
psp2EX <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp2EX))

regsp4EX <- polr(BRASILPARALELO ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                   SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
                 data = basesp, Hess = T)
summary(regsp4EX)
(ctable <- coef(summary(regsp4EX)))
psp4EX <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp4EX))



cor(select(basesp, CONFIANCAGRANDEMIDIA, VOTOUBOLSONARO, INTERESSE, IDADE, RENDA, SOFISTICACAO, IDEOLOGIA, APOIOPARTIDO))

cor(select(basesp, CONFIANCAGRANDEMIDIA, INTERESSE, IDADE, RENDA, SOFISTICACAO, IDEOLOGIA, APOIOPARTIDO))

cor.test(basesp$VOTOUBOLSONARO, basesp$IDEOLOGIA)

count(basesp, conf)


# Contar uso a partir do escore de confiança:
basegraficozerotrust <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha8")
basegraficotentrust <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha7")

count(basesp, CONFIANCAGRANDEMIDIA)

tl2 <- count(basesp,TERCALIVRE, CONFIANCAGRANDEMIDIA)
jp2 <- count(basesp,JOVEMPAN, CONFIANCAGRANDEMIDIA)
uol2 <- count(basesp,UOL, CONFIANCAGRANDEMIDIA)
bp2 <- count(basesp,BRASILPARALELO, CONFIANCAGRANDEMIDIA)
b2472 <- count(basesp,BRASIL247, CONFIANCAGRANDEMIDIA)
rec2 <- count(basesp,RECORD, CONFIANCAGRANDEMIDIA)
fsp2 <- count(basesp,FOLHA, CONFIANCAGRANDEMIDIA)
glob2 <- count(basesp,GLOBO, CONFIANCAGRANDEMIDIA)


# Gráfico nenhuma confiança:
df5 <- tidyr::pivot_longer(basegraficozerotrust, cols=c('Globo', 'Record', 'Folha',
                                                     'UOL', 'Tercalivre', 'Jovempan',
                                                     'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")

windowsFonts(Times=windowsFont("Times New Roman"))
zerotrust <- ggplot(df5, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  scale_y_continuous(limits = c(0, 100))+
  labs(title="Nenhuma Confiança - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  guides(fill=FALSE, color=FALSE)

# Gráfico total confiança:
df6 <- tidyr::pivot_longer(basegraficotentrust, cols=c('Globo', 'Record', 'Folha',
                                                        'UOL', 'Tercalivre', 'Jovempan',
                                                        'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")

windowsFonts(Times=windowsFont("Times New Roman"))
tentrust <- ggplot(df6, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  scale_y_continuous(limits = c(0, 100))+
  labs(title="Total Confiança - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  guides(fill=FALSE, color=FALSE)

grid.arrange(zerotrust, tentrust, nrow=2)

# Adicionar interação com voto em Bolsonaro:
# Rede Globo:
regsp8ex <- polr(GLOBO ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp8ex)
(ctable <- coef(summary(regsp8ex)))
psp8ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp8ex))

(ci <- confint(regsp8))
exp(cbind(OR = coef(regsp8), ci))


# Folha de São Paulo:
regsp7ex <- polr(FOLHA ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp7ex)
(ctable <- coef(summary(regsp7ex)))
psp7ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp7ex))

# Brasil Paralelo:
regsp4ex <- polr(BRASILPARALELO ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA + 
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp4ex)
(ctable <- coef(summary(regsp4ex)))
psp4ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp4ex))

# Brasil 247:
regsp5ex <- polr(BRASIL247 ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SEXOFEMININO + SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp5ex)
(ctable <- coef(summary(regsp5ex)))
psp5ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp5ex))

(ci <- confint(regsp5))
exp(cbind(OR = coef(regsp5), ci))

# Jovem Pan:
regsp2ex <- polr(JOVEMPAN ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
data = basesp, Hess = T)
summary(regsp2ex)
(ctable <- coef(summary(regsp2ex)))
psp2ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp2ex))

basesp2 <- basesp
basesp2$JOVEMPAN <- as.numeric(as.character(basesp2$JOVEMPAN))

mod2<-lm(JOVEMPAN ~ CONFIANCAGRANDEMIDIA + VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
           SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
         data = basesp2)

vif(mod2)


regsp2ex2 <- polr(JOVEMPAN ~ CONFIANCAGRANDEMIDIA + VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                   SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
                 data = basesp, Hess = T)
summary(regsp2ex2)
(ctable <- coef(summary(regsp2ex2)))
psp2ex2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp2ex2))


# Terça Livre:
regsp1ex <- polr(TERCALIVRE ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp1ex)
(ctable <- coef(summary(regsp1ex)))
psp1ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp1ex))

basesp2$TERCALIVRE <- as.numeric(as.character(basesp2$TERCALIVRE))
mod3<-lm(TERCALIVRE ~ CONFIANCAGRANDEMIDIA + VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
           SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
         data = basesp2)

vif(mod3)

# Rede Record:
regsp6ex <- polr(RECORD ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp6ex)
(ctable <- coef(summary(regsp6ex)))
psp6ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp6ex))

regsp6ex2 <- polr(RECORD ~ CONFIANCAGRANDEMIDIA + VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                   SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
                 data = basesp, Hess = T)
summary(regsp6ex2)
(ctable <- coef(summary(regsp6ex2)))
psp6ex2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp6ex2))





# UOL:
regsp3ex <- polr(UOL ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SEXOFEMININO + SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp3ex)
(ctable <- coef(summary(regsp3ex)))
psp3ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp3ex))

