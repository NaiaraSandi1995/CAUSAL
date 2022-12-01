#GRUPO DE ESTUDO 27/04/2021 ===================================================

#Estudo sobre causalidade =====================================================

#0. Livro utilizado como referência ===========================================

# AGRESTI, Alan; FINLAY, Barbara. 
      #Métodos estatísticos para as ciências sociais. Penso Editora, 2012.
      #Capítulos 9 e 10

# QUESTÃO DE EXEMPLO: A ESQUERDA PROTESTA MAIS DO QUE A DIREITA?

#1. Carregando pacotes e importando banco de dados ============================

# Caso queira baixar o pacote 'Tidyverse' e 'rio' no seu R
install.packages('tidyverse')
install.packages('rio')

#pacotes que utilizo

library(tidyverse) # como você viu, tive problemas com esse pacote.
                    # tive conflitos com os pacotes do Rbase.

library(rio) # utilizo para função 'import'

library(memisc)# você já conhece
library(descr) # você já conhece

# Estou usando para este exercício o LAPOP2019 - BRASIL
lapop <- import('~/TESE - DANIEL/BASE DE DADOS DOUTORADO/LAPOP/Lapop2019.RData')

#2. Separando e renomeando as variáveis de interesse ==========================

teste <- lapop %>% select(l1, ed, q10new, prot3, q1) %>% 
  rename(ideologia = l1,
         educação = ed,
         renda = q10new,
         protesto = prot3,
         sexo = q1) #aqui usei o 'pipe [%.%]', mas pode utilizar um recurso dif.
                    #A ideia do 'pipe' significa 'jogar pra frente'.

#2.1. Recodificando ===========================================================

teste$protesto <- recode(teste$protesto, 1 <- 1, 0 <- 2)
teste$ideologia <- recode(teste$ideologia, 1 <- 10, 2 <- 9, 3 <- 8, 4 <- 7,
                          5 <- 6, 6 <- 5, 7 <- 4, 8 <- 3, 9 <- 2, 10 <- 1)

#3. Primeiras análises [tabela de contingência, correlação e regressão linear] ======

# A ideia aqui é verificar como as variáveis estão relacionadas.
# AGRESTI & FINLAY (2010) afirmam que a análise da causalidade obedece 3 critérios:
# A. Deve haver associação entre as variáveis x e y;
# B. Existe uma ordem apropriada no tempo (x causa y)
# C. Deve-se eliminar explicações alternativas.

table(teste$ideologia, teste$protesto)

cor.test(teste$ideologia, teste$protesto, method = "pearson")

mod1 <- lm(protesto ~ ideologia, data = teste)
summary(mod1)

#3.1. faixas de ideologia =====================================================

# AGRESTI & FINLAY (2010) pontuam que fixar constantes ajuda a identificar
# qual aspecto é mais importante para determinar a causalidade. Isso será
# realizado para as outras variáveis que serão incluidas no modelo.

teste$ideologia1 <- cut(teste$ideologia, c(0, 3, 7, 10), 
                       labels = c("direita", "misto", "esquerda"))

mod1.1 <- lm(protesto ~ ideologia1, data = teste)
summary(mod1.1)

#4. Controlando pela variável renda ===========================================
table(teste$renda, teste$ideologia, teste$protesto) #var dependente por último,
# assim você consegue ver o relacionamento de renda e ideologia dentro de cada
# da variável protesto.

mod2 <- lm(protesto ~ ideologia + renda, data = teste)
summary(mod2)

#4.1. Criando as faixas de renda ===============================================

teste$renda1 <- cut(teste$renda, c(00, 06, 13, 16), 
                       labels = c("até 1 salário", "até 2 salários",
                                  "mais de 2 salários"))

mod2.1 <- lm(protesto ~ ideologia + renda1, data = teste)
summary(mod2.1)

#5. Controlando pela educação =================================================
table(teste$educação, teste$ideologia, teste$protesto)

mod3 <- lm(protesto ~ ideologia + educação, data = teste)
summary(mod3)

#5.1. Criando as faixas de educação ===========================================

teste$educação1 <- cut(teste$educação, c(0, 4, 8, 11, 17), 
                       labels = c("primário", "fundamental", "médio", 
                                  "superior"))

mod3.1 <- lm(protesto ~ ideologia + educação1, data = teste)
summary(mod3.1)

#6. Incluindo educação e renda ================================================

mod4 <- lm(protesto ~ ideologia + renda + educação, data = teste)
summary(mod4)

mod4a <- lm(protesto ~ ideologia + renda + educação + renda:educação, data = teste)
# achei legal a ideia da interação entre renda e educação [renda:educação]. 
# Essa interação acrescenta um valor explicativo para renda e educação separadas.
summary(mod4a)

mod4.1 <- lm(protesto ~ ideologia + renda1 + educação1, data = teste) # com faixas
summary(mod4.1)

#7. Controlando por sexo ======================================================

mod5 <- lm(protesto ~ ideologia + sexo, data = teste)
summary(mod5)

#8. Incluindo todas as variáveis no modelo ====================================

mod6 <- lm(protesto ~ ideologia + sexo + renda + educação, data = teste)
summary(mod6)

mod6.1 <- lm(protesto ~ sexo + ideologia1 + renda1 + educação1, data = teste)
summary(mod6.1)

mod6.2 <- lm(protesto ~ ideologia + 
               sexo + renda + educação + renda:ideologia + 
               educação:ideologia + 
               educação:renda, data = teste) # com as interações
summary(mod6.2)

# Neste exercício eu percebi que os anos de estudo é um forte preditor.
# Que a ideologia não é um forte preditor, mas quando eu analiso a relação
# entre protesto e ideologia, o ano de 2019 revela um protagonismo da direita.
# Que a renda é importante para explicar o protesto, mas só é verdadeira quando
# controlada pela educação.

# Esquema:

# Educação --> Renda --> Protesto