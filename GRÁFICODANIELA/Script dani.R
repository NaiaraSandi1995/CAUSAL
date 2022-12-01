
#Gráfico de barras simples
library(tidyr)
library(ggplot2)



Dani <-ggplot (ID_M, aes(y=ID, x = ano)) + 
  geom_bar(stat= "identity", alpha = 0.5) + #Criação do objeto com o banco
  facet_wrap(~ ID_M$CIDADE, nrow = 2) # Organização de todos os gráficos 
#em uma única projeção, com 2 linhas 

Dani + labs(y ="Índice de Democratização", x= "Ano") +  #Colocar títulos no rótulos
  theme_classic() +  theme(axis.text.x = 
                             element_text(angle = 90, 
                         vjust = 0, size= 7,  hjust = 1)) #Colocar o tema, 
#E ajustar os angulos dos anos

