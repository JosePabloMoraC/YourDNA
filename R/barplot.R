library(dplyr)
library(ggplot2)

#' Grafico de barras simple
#'
#' @export
barras_simple <- function(datos, pregunta, titulo = NULL,
                          nombreEjeX = NULL, nombreEjeY = NULL, tamanoTituloEje = NULL,
                          tamanoEje = NULL, tamanoTitulo = NULL) {
  datos |>
    count(!!rlang::ensym(pregunta)) |>
    mutate(freq = n / sum(n)*100) |>
    ggplot(aes(x = !!rlang::ensym(pregunta), y= freq)) +
    geom_bar(stat="identity", alpha= 0.60, colour="Black") +
    geom_text(aes(label = n), position=position_dodge(width=0.9), vjust=-0.6) +
    theme_classic() +
    ggtitle(titulo) +
    labs(
      x = nombreEjeX,
      y = nombreEjeY
    ) + theme(axis.title = element_text(size = tamanoTituloEje ),
              axis.text = element_text(size = tamanoEje),
              plot.title = element_text(size = tamanoTitulo))
}

#' @export
barras_agrupadas <- function(datos, pregunta, clasificarPor,
                             titulo = NULL,
                             nombreEjeX = NULL, nombreEjeY = NULL,
                             nombreLegenda = NULL,
                             tamanoTituloEje = NULL,
                             tamanoEje = NULL,
                             tamanoTitulo = NULL,
                             cantidad = NULL,
                             tamanoCantidad = NULL){
  gp <- datos |>
    count(!!rlang::ensym(clasificarPor),
          !!rlang::ensym(pregunta)) |>
    mutate(freq = n / sum(n) * 100) |>
    ggplot(aes(x = !!rlang::ensym(pregunta),
               y = freq, fill = !!rlang::ensym(clasificarPor))) +
    geom_bar(stat = "identity",
             alpha = 0.60,
             colour = "Black") +
    ggtitle(titulo) +
    #geom_text(aes(label = n), position=position_dodge(width=0.9), vjust=-0.25) +
    theme_classic() +
    labs(x = nombreEjeX,
         y = nombreEjeY,
         fill = nombreLegenda) +
    theme(axis.title = element_text(size = tamanoTituloEje ),
          axis.text = element_text(size = tamanoEje),
          plot.title = element_text(size = tamanoTitulo))

  resumen = datos |>
    count(pregunta) |>
    mutate(freq = round((n / sum(n)*100), 2)) |>
    mutate(tot = paste(freq, "% ", "(", n, ")", sep = ""))

  if(length(cantidad) == 0){
    # Lo que importa es que funciona
  } else if (cantidad == "arriba"){
    gp = gp + geom_text(data = resumen, aes(label = n, fill = NULL, vjust = -.6, size = tamanoCantidad)
    )
  } else if (cantidad == "centro"){
    gp = gp + geom_text(aes(label = n), position = "stack", vjust = 1.2, size = tamanoCantidad)
  } else if (cantidad == "completo"){
    gp = gp + geom_text(data = resumen, aes(label = tot, fill = NULL, vjust = -.6, size = tamanoCantidad))
  }

  gp


}

#' @export
barras_separadas <- function(datos, pregunta, clasificarPor,
                             titulo = NULL,
                             nombreEjeX = NULL, nombreEjeY = NULL, nombreLegenda = NULL,
                             tamanoTituloEje = NULL,
                             tamanoEje = NULL,
                             tamanoTitulo = NULL,
                             cantidad = NULL,
                             tamanoCantidad = 2.1){
  gp <- datos |>
    count(!!rlang::ensym(clasificarPor),
          !!rlang::ensym(pregunta)) |>
    mutate(freq = round((n / sum(n)*100),1)) |>
    ggplot(aes(x = !!rlang::ensym(pregunta),
               y = freq, fill = !!rlang::ensym(clasificarPor),
               by = !!rlang::ensym(clasificarPor))) +
    geom_bar(stat="identity",
             alpha= 0.60,
             colour="Black",
             position = position_dodge(width = .9)) +
    ggtitle(titulo) +
    #geom_text(aes(label = n), position=position_dodge(width=0.9), vjust=-0.25) +
    theme_classic() +
    labs(x = nombreEjeX,
         y = nombreEjeY,
         fill = nombreLegenda) +
    theme(axis.title = element_text(size = tamanoTituloEje ),
          axis.text = element_text(size = tamanoEje),
          plot.title = element_text(size = tamanoTitulo))

  if(length(cantidad) == 0){
    # Lo que importa es que funciona
  } else if (cantidad == "arriba"){
    gp = gp + geom_text(aes(label = n), size = tamanoCantidad, position=position_dodge(.9), vjust=-0.6)

  }
  gp
}

#' @export
barras_secciones <- function(datos, pregunta, clasificarPor,
                             titulo = NULL,
                             nombreEjeX = NULL, nombreEjeY = NULL,
                             nombreLegenda = NULL,
                             tamanoTituloEje = NULL,
                             tamanoEje = NULL,
                             tamanoTitulo = NULL,
                             tamanoCantidad = 3){
  datos |> tidyr::drop_na(!!rlang::ensym(pregunta)) |>
    count(!!rlang::ensym(clasificarPor),
          !!rlang::ensym(pregunta)) |>
    group_by(!!rlang::ensym(clasificarPor)) |>
    mutate(freq = n / sum(n)*100) |>
    ggplot(aes(x = !!rlang::ensym(pregunta),
               y= freq, fill = !!rlang::ensym(pregunta))) +
    facet_grid(!!rlang::ensym(~clasificarPor)) +
    geom_bar(stat="identity", alpha= 0.60, colour="Black") +
    ggtitle(titulo) +
    theme_bw() +
    geom_text(aes(label = n), size = tamanoCantidad, position=position_dodge(.9), vjust=-0.6) +
    labs(
      x = nombreEjeX,
      y = nombreEjeY,
      fill = nombreLegenda
    ) +
    theme(axis.text.x=element_blank(),
          axis.title = element_text(size = tamanoTituloEje ),
          axis.text = element_text(size = tamanoEje),
          plot.title = element_text(size = tamanoTitulo))

}


