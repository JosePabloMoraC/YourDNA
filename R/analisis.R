library(dplyr)
library(ggplot2)
encuesta <- readr::read_csv2("Data\\YDYS_R.csv", show_col_types = FALSE)

#Cuadro de barras

table(encuesta$D.D.1a)


encuesta |>
  count(D.D.1a) |>
  mutate(freq = n / sum(n)*100) |>
  ggplot(aes(x = D.D.1a, y= freq)) +
  geom_bar(stat="identity", alpha= 0.40, colour="Black") +
  theme_classic() +
  labs(
    x = "Edad",
    y = "Frecuencia"
  )


encuesta |>
  count(D.D.1a) |>
  mutate(freq = n / sum(n)*100)





