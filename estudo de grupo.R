#GRUPO DE ESTUDO 27/04/2021 ===================================================

#Estudo sobre causalidade =====================================================

#0. Livro utilizado como refer?ncia ===========================================

# AGRESTI, Alan; FINLAY, Barbara. 
      #M?todos estat?sticos para as ci?ncias sociais. Penso Editora, 2012.
      #Cap?tulos 9 e 10

# QUEST?O DE EXEMPLO: A ESQUERDA PROTESTA MAIS DO QUE A DIREITA?

#1. Carregando pacotes e importando banco de dados ============================

# Caso queira baixar o pacote 'Tidyverse' e 'rio' no seu R
install.packages('tidyverse')
install.packages('rio')

#pacotes que utilizo

library(tidyverse) # como voc? viu, tive problemas com esse pacote.
                    # tive conflitos com os pacotes do Rbase.

library(rio) # utilizo para fun??o 'import'

library(memisc)# voc? j? conhece
library(descr) # voc? j? conhece

# Estou usando para este exerc?cio o LAPOP2019 - BRASIL
lapop <- import('~/TESE - DANIEL/BASE DE DADOS DOUTORADO/LAPOP/Lapop2019.RData')

#2. Separando e renomeando as vari?veis de interesse ==========================

teste <- lapop %>% select(l1, ed, q10new, prot3, q1) %>% 
  rename(ideologia = l1,
         educa??o = ed,
         renda = q10new,
         protesto = prot3,
         sexo = q1) #aqui usei o 'pipe [%.%]', mas pode utilizar um recurso dif.
                    #A ideia do 'pipe' significa 'jogar pra frente'.

#2.1. Recodificando ===========================================================

teste$protesto <- recode(teste$protesto, 1 <- 1, 0 <- 2)
teste$ideologia <- recode(teste$ideologia, 1 <- 10, 2 <- 9, 3 <- 8, 4 <- 7,
                          5 <- 6, 6 <- 5, 7 <- 4, 8 <- 3, 9 <- 2, 10 <- 1)

#3. Primeiras an?lises [tabela de conting?ncia, correla??o e regress?o linear] ======

# A ideia aqui ? verificar como as vari?veis est?o relacionadas.
# AGRESTI & FINLAY (2010) afirmam que a an?lise da causalidade obedece 3 crit?rios:
# A. Deve haver associa??o entre as vari?veis x e y;
# B. Existe uma ordem apropriada no tempo (x causa y)
# C. Deve-se eliminar explica??es alternativas.

table(teste$ideologia, teste$protesto)

cor.test(teste$ideologia, teste$protesto, method = "pearson")

mod1 <- glm(protesto ~ ideologia, data = teste, family = binomial(link = logit))
summary(mod1)

#3.1. faixas de ideologia =====================================================

# AGRESTI & FINLAY (2010) pontuam que fixar constantes ajuda a identificar
# qual aspecto ? mais importante para determinar a causalidade. Isso ser?
# realizado para as outras vari?veis que ser?o incluidas no modelo.

teste$ideologia1 <- cut(teste$ideologia, c(0, 3, 7, 10), 
                       labels = c("direita", "misto", "esquerda"))

mod1.1 <- glm(protesto ~ ideologia1, data = teste, family = binomial(link = logit))
summary(mod1.1)

#4. Controlando pela vari?vel renda ===========================================
table(teste$renda, teste$ideologia, teste$protesto) #var dependente por ?ltimo,
# assim voc? consegue ver o relacionamento de renda e ideologia dentro de cada
# da vari?vel protesto.

mod2 <- glm(protesto ~ ideologia + renda, data = teste, 
            family = binomial(link = logit))
summary(mod2)

#4.1. Criando as faixas de renda ===============================================

teste$renda1 <- cut(teste$renda, c(00, 06, 13, 16), 
                       labels = c("at? 1 sal?rio", "at? 2 sal?rios",
                                  "mais de 2 sal?rios"))

mod2.1 <- glm(protesto ~ ideologia + renda1, data = teste, 
              family = binomial(link = logit))
summary(mod2.1)

#5. Controlando pela educa??o =================================================
table(teste$educa??o, teste$ideologia, teste$protesto)

mod3 <- glm(protesto ~ ideologia + educa??o, data = teste, 
            family = binomial(link = logit))
summary(mod3)

#5.1. Criando as faixas de educa??o ===========================================

teste$educa??o1 <- cut(teste$educa??o, c(0, 4, 8, 11, 17), 
                       labels = c("prim?rio", "fundamental", "m?dio", 
                                  "superior"))

mod3.1 <- glm(protesto ~ ideologia + educa??o1, data = teste, 
              family = binomial(link = logit))
summary(mod3.1)

#6. Incluindo educa??o e renda ================================================

mod4 <- glm(protesto ~ ideologia + renda + educa??o, data = teste, 
            family = binomial(link = logit))
summary(mod4)

mod4a <- glm(protesto ~ ideologia + renda + educa??o + renda:educa??o, data = teste, 
            family = binomial(link = logit))
# achei legal a ideia da intera??o entre renda e educa??o [renda:educa??o]. 
# Essa intera??o acrescenta um valor explicativo para renda e educa??o separadas.
summary(mod4a)

mod4.1 <- glm(protesto ~ ideologia + renda1 + educa??o1, data = teste,
             family = binomial(link = logit)) # com faixas
summary(mod4.1)

#7. Controlando por sexo ======================================================

mod5 <- glm(protesto ~ ideologia + sexo, data = teste,
            family = binomial(link = logit))
summary(mod5)

#8. Incluindo todas as vari?veis no modelo ====================================

mod6 <- glm(protesto ~ ideologia + sexo + renda + educa??o, data = teste,
            family = binomial(link = logit))
summary(mod6)

mod6.1 <- glm(protesto ~ sexo + ideologia1 + renda1 + educa??o1, data = teste,
             family = binomial(link = logit))
summary(mod6.1)

mod6.2 <- glm(protesto ~ ideologia + 
               sexo + renda + educa??o + renda:ideologia + 
               educa??o:ideologia + 
               educa??o:renda, data = teste,
              family = binomial(link = logit)) # com as intera??es
summary(mod6.2)

# Neste exerc?cio eu percebi que os anos de estudo ? um forte preditor.
# Que a ideologia n?o ? um forte preditor, mas quando eu analiso a rela??o
# entre protesto e ideologia, o ano de 2019 revela um protagonismo da direita.
# Que a renda ? importante para explicar o protesto, mas s? ? verdadeira quando
# controlada pela educa??o.

# Esquema:

# Educa??o --> Renda --> Protesto