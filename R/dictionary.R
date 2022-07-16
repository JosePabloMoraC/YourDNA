#' Question dictionary
#'
#' @description Displays information from the questions in the "Your DNA, Your Say" survey.
#'
#' @param code Character, the code corresponding to the question.
#'
#' @export
#'
#' @examples
#' dictionary("q_1.1.1a")
#'
dictionary <- function(code){
  text = paste("Code:",
               code,
               "\n",
               "Section:",
               readSection(code),
               "\n\n",
               "Question:",
               readQuestion(code),
               "\n\n",
               readOption(code)
         )

  cat(text)
}


# Helper function for dictionary()
readSection <- function(code) {
  number <- questions |>
            dplyr::filter(
            codigo == code
            ) |>
            dplyr::select(2) |>
            as.character()


  textSection <- sections |>
        dplyr::filter(
          codigo == number
          ) |>
          dplyr::select(seccion)

  as.character(textSection)

}

# Helper function for dictionary()
readQuestion <- function(code){
  textQuestion <- questions |>
            dplyr::filter(
              codigo == code
              ) |>
              dplyr::select(pregunta, tipo_datos, Observacion)
  if(is.na(textQuestion[1,3]) == FALSE){
    observation = paste(textQuestion[1,3], "\n")
  } else {
    observation = NULL
  }

  paste(observation,
        textQuestion[1,1],
        "\n\n",
        "*** Type of data:",
        textQuestion[1,2],
        "***")
}

# Helper function for dictionary()
readOption <- function(code){
  optionGroup <- options |>
        dplyr::filter(
          codigo == code
          )

  textOption = paste("Answers:", "\n")

  if(nrow(optionGroup > 0)){
    for(i in 1:nrow(optionGroup)) {
      textOption =
        paste(
          textOption,
          ">",
          as.character(optionGroup[i,2]),
          "\n"
        )
      if (is.na(optionGroup[i,3]) == FALSE) {
        textOption =
          paste(
            textOption,
            " ",
            as.character(optionGroup[i,3]),
            "\n"
          )
      }
    }
  } else {
    textOption =
      paste(
        textOption,
        'There are not options to select. This may be a "natural language" answer.'
      )
  }
  textOption
}


#' View sections
#'
#' @description Shows all possible sections for questions (e.g. online footprint, data access by others, etc.)
#'
#' @export
#'
#' @examples
viewSections <- function(){
  knitr::kable(sections, format = "markdown")
}


#' View questions
#'
#' @description Shows the questions of the "Your DNA, Your Say" survey and its related code.
#' @export
#'
#' @examples
viewQuestions <- function(){
  table <- questions |>
           dplyr::select(codigo, pregunta)
  print(table, n=80)

}





