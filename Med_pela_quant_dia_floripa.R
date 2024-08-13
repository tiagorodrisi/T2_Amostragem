if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(writexl)) {
  install.packages("writexl")
  library(writexl)
}

library(readxl)
Leads_Zap <- read_excel("Leads Zap.xlsx")
View(Leads_Zap)

# Filtrando os dados para a cidade de Florianópolis
dados_florianopolis <- subset(Leads_Zap, imovel_cidade == "Florianópolis")

# Convertendo a coluna data_criado_em para o formato Date
dados_florianopolis$data_criado_em <- as.Date(dados_florianopolis$data_criado_em, format="%d-%m-%Y")

# Agrupando os dados pelo código do imóvel e calcular as estatísticas necessárias
dados_agrupados <- dados_florianopolis %>%
  group_by(codigo_do_imovel) %>%
  summarise(
    data_min = min(data_criado_em),
    data_max = max(data_criado_em),
    dias_entre_datas = if_else(data_min == data_max, 1, as.numeric(difftime(data_max, data_min, units = "days"))),
    quant_Leads = n(),
    med_Leads_por_dia = quant_Leads / if_else(dias_entre_datas == 0, 1, dias_entre_datas)
  ) %>%
  arrange(codigo_do_imovel)


# Media Populacional da media de Leads por dia de cada imovel

med_pop <- mean(dados_agrupados$Med_Leads_por_dia)

# Desvio padrão

desvio_padrão <- sd(dados_agrupados$med_Leads_por_dia)
desvio_padrão

# Tamanho da amostral
n = 14

# Número de amostras a serem geradas
num_amostras <- 1000

# Gerando amostras aleatórias e calculando as médias
set.seed(2024)  # Definida uma semesnte
valores_media_amostral <- replicate(num_amostras, {
  amostra <- sample(dados_agrupados$med_Leads_por_dia, n, replace = FALSE)
  mean(amostra)
})
print(valores_media_amostral)

# Media das 1000 amostras geradas
med_med_amostral <- mean(valores_media_amostral)

# Retirada da amostra 1 aleatória de tamanho n
set.seed(100)  # Definindo semente
amostra_1 <- sample(dados_agrupados$med_Leads_por_dia, n, replace = FALSE)

# Retirada da amostra 2 aleatória de tamanho n
set.seed(200)  # Definindo semente
amostra_2 <- sample(dados_agrupados$med_Leads_por_dia, n, replace = FALSE)

# Retirada da amostra 2 aleatória de tamanho n
set.seed(300)  # Definindo semente
amostra_3 <- sample(dados_agrupados$med_Leads_por_dia, n, replace = FALSE)

# Media amostra 1
med_amostra_1 <- mean(amostra_1)

# Media amostra 2
med_amostra_2 <- mean(amostra_2)

# Media amostra 3
med_amostra_3 <- mean(amostra_3)


# Calculando a margem de erro para um nível de confiança de 94%
nivel_conf <- 0.94
z <- qnorm(1 - (1 - nivel_conf) / 2)
margem_erro <- z * (desvio_padrão / sqrt(n))
print(margem_erro)


# Verificar se a média da amostra 1 está dentro do intervalo de confiança
intervalo_conf_inferior <- med_pop - margem_erro
intervalo_conf_superior <- med_pop + margem_erro

dentro_intervalo_amostra_1 <- (med_amostra_1 >= intervalo_conf_inferior) && (med_amostra_1 <= intervalo_conf_superior)

dentro_intervalo_amostra_2 <- (med_amostra_2 >= intervalo_conf_inferior) && (med_amostra_2 <= intervalo_conf_superior)

dentro_intervalo_amostra_3 <- (med_amostra_3 >= intervalo_conf_inferior) && (med_amostra_3 <= intervalo_conf_superior)
