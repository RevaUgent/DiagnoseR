##-------------- DIAGNOSTIC TESTS

dx_ui <- function(id) {
  
  tagList(
  radioGroupButtons(
    inputId = NS(id, "level"),
    label = "Main group",
    choices = list("LEVEL 0" = "level0",
                   "LEVEL 1" = "level1"),
    justified = TRUE
  ),
  selectInput(
    inputId = NS(id, "disorder"),
    label = "Disorder",
    choices = NULL
  ),
  selectInput(
    inputId = NS(id, "patpop"),
    label = "Patient population",
    choices = NULL
  ),
  selectInput(
    inputId = NS(id, "tests"),
    label = "Test",
    choices = NULL
  ),
  materialSwitch(
    inputId = NS(id, "onlygood"),
    label = "Se/Sp > 0.5",
    right = TRUE
  ),
  pickerInput(
    inputId = NS(id, "lim_prev"),
    label = "Limit the prevalence",
    choices = c("ALL", "5%", "10%", "20%")
  ),
  sliderInput(
    inputId = NS(id, "pat_prev"),
    label = "Pre-test probability",
    value = 50,
    min = 1,
    max = 99
  ))
}

