library(ggplot2)
library(tidyverse)
library(gganimate)

#Importar o banco de dados
pib_mundo <- read.csv2("countries_gdp_hist.csv")

#Dar uma olhada no banco de dados com str(), length(), sumary(), head()

#Queremos olhar para o Pib dos paises da America do Sul ao longo dos anos,
#então vamos utilizar a coluna year(), total_gdp_million, intermediate_region
#e country_name

#Aqui filtramos só o que utilizariamos do banco de dados
america_do_sul <- pib_mundo[pib_mundo$intermediate_region == "South America",
                            c("year","total_gdp_million","country_name")]

#Unique para ver se o nome dos paises está certo, e também precisamos
#transformar em factor e numeric algumas colunas.

america_do_sul <- america_do_sul %>%
  mutate(country_name = recode(country_name,
                               "Venezuela (Bolivarian Republic of)"
                               = "Venezuela",
                               "Bolivia (Plurinational State of)" =
                                  "Bolivia"))
america_do_sul$country_name <- factor(america_do_sul$country_name)
america_do_sul$total_gdp_million <- as.numeric((america_do_sul$total_gdp_million))


#Rank dos maiores PIBs para que quando mude o ano os paises subam e desçam
america_do_sul <- america_do_sul %>%
  arrange(year, desc(total_gdp_million)) %>%
  group_by(year) %>%
  mutate(rank = factor(row_number(), levels = 1:length(country_name)))

#Criar o grafico no GGplot2

grafico <- ggplot(america_do_sul, aes(x= reorder(country_name, total_gdp_million),
                                      y = total_gdp_million,
                                      fill = country_name)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "PIB do Pais em: {closest_state}",
       x = "Pais", y = "Milhões",
       fill = "") +
  theme( plot.title = element_text(hjust = 0.5),
         axis.text.x = element_text(size = 10),
         panel.grid.major = element_line(size = 0.5,
                                         color = "lightblue",
                                         linetype = "blank"),
         
         panel.grid.minor = element_line(size = 0.5,
                                         color = "lightblue",
                                         linetype = "blank")) +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal() +
# O gráfico não está alterando de colocação os paises com maior e menor PIB
#quando joga para o gganimate animar
transition_states(
  america_do_sul$year,
  transition_length = 5,
  state_length = 5
)

grafico

anim_save("PIB.gif")



 
