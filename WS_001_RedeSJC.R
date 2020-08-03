# Housekeeping -----------------------------------------------------------------

library('tidyverse')
library('rvest')

# Read data --------------------------------------------------------------------

url <- "https://www.redesaojose.com.br/imovel/apartamento-de-65-m-bosque-dos-eucaliptos-sao-jose-dos-campos-a-venda-por-r-300-000/AP3063-FE"
page <- read_html(url)

nodes <- html_nodes(page,"table")

nodes
