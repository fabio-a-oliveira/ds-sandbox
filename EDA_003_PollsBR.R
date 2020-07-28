# initialization

library(pollingBR)
library(dplyr)
library(ggplot2)

# data wrangling

polls_all <- president(year=2018,state="BR")

polls <- polls_all %>%
  filter(turno == 1 & tipo_id != 3) %>%
  select(pesquisa_id,candidato,percentual,data_pesquisa,qtd_entrevistas,tipo,instituto,cenario_descricao,condicao) %>%
  transmute(id=as.factor(pesquisa_id), 
            instituto=as.factor(instituto),
            candidato=as.factor(candidato),
            percentual=as.numeric(percentual),
            data=as.Date(data_pesquisa),
            amostras=as.integer(qtd_entrevistas),
            tipo=as.factor(tipo),
            cenario=as.factor(cenario_descricao),
            condicao=as.factor(condicao))
  
polls %>% ggplot(aes(x=data,y=percentual)) +
  geom_point()
