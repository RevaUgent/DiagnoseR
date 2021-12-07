##++++++++++++++++++++++++++++ Packages ++++++++++++++++++++++++++++++++++++++
library(shiny)
library(openxlsx)
library(tidyr)
library(dplyr)
library(shinyWidgets)
library(flexdashboard)
library(ggplot2)
library(scales)
library(plotly)
library(shinydashboard)
library(waiter)
library(scales)
library(highcharter)
library(purrr) # to work with list columns
library(fontawesome)
library(extrafont)
library(rsvg)
library(stringr)
library(googlesheets4)
#library(waffle)
#library(hrbrthemes)

##++++++++++++++++++++++++++++ SCRIPTS +++++++++++++++++++++++++++++++++++++++
source("www/helpers.R")
source("www/nomogrammer.R")


##++++++++++++++++++++++++++++ MODULES +++++++++++++++++++++++++++++++++++++++
source("www/1_info.R")


##+++++++++++++++++++++++++++ Load data ++++++++++++++++++++++++++++++++++++++
# tests <- tidyr::pivot_longer(
#   openxlsx::read.xlsx("www/tests.xlsx"),
#   cols = c(level0, level1),
#   names_to = "level",
#   values_to = "disease"
# ) %>%
#   rowwise() %>%
#   dplyr::mutate(prev = round((tp + fp) / (tp + fp + tn + fn), 3),
#                 se = round((tp) / (tp + fp), 3),
#                 sp = round((tn) / (tn + fn), 3),
#                 lr_pos = round(min(se,0.999)/(1-min(sp,0.999)),2),
#                 lr_neg = round((1-min(sp,0.999))/min(se,0.999),3))

allinfo <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTBnv4lcKomAFux1P8Sl9nq8TQa8PSvllNN6KH2TrOLB4G4FCtLdepS4ugY8sVoX7luK20tYcoz6LE6/pub?gid=1859884833&single=true&output=csv")
colnames(allinfo) <- c("Time", "study", "year", "level0", "level1", 
                       "population", "test", "reference", "tp", 
                       "fp", "fn", "tn", "maxscore", "score", 
                       "doi", "name", "country")

## Create test database
tests <- tidyr::pivot_longer(
  allinfo[c("study", "year", "level0", "level1", 
            "population", "test", "reference", "tp", 
            "fp", "fn", "tn", "maxscore", "score")],
  cols = c(level0, level1),
  names_to = "level",
  values_to = "disease"
) %>%
  rowwise() %>%
  dplyr::mutate(prev = round((tp + fp) / (tp + fp + tn + fn), 3),
                se = round((tp) / (tp + fp), 3),
                sp = round((tn) / (tn + fn), 3),
                lr_pos = round(min(se,0.999)/(1-min(sp,0.999)),2),
                lr_neg = round((1-min(sp,0.999))/min(se,0.999),3))

## Create pooled database
tests_pooled <- tests %>%
    dplyr::group_by(level, disease, population, test) %>%
    dplyr::summarise(
      tp = sum(tp),
      fp = sum(fp),
      tn = sum(tn),
      fn = sum(fn)
    ) %>%
    rowwise() %>%
    dplyr::mutate(prev = round((tp + fp) / (tp + fp + tn + fn), 3),
                  se = round((tp) / (tp + fp), 3),
                  sp = round((tn) / (tn + fn), 3),
                  lr_pos = min(se,0.999)/(1-min(sp,0.999)),
                  lr_neg =(1-min(sp,0.999))/min(se,0.999))

##+++++++++++++++++++++++++++ HELPERS ++++++++++++++++++++++++++++++++++++++
myGauge <- function(id, label, value) {
  tagList(tags$label(`for` = id,
                     label),
          tags$meter(id = id,
                     value = value))
}

col_stops <- data.frame(
  q = c(0.5, 0.7, 0.85),
  c = c('#e74c3c', '#f39c12', '#00a65a'),
  stringsAsFactors = FALSE
)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##            UI ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##+++++++++++++++++++++++++++ Graphical UI +++++++++++++++++++++++++++++++++++
ui <- navbarPage(
  title =
    HTML(
      "<a href='https://www.ugent.be'><img src='ugent_logo.png' height='30px'></a>&nbsp;&nbsp;&nbsp; <span style='color:#1E64C8; font-size:1.1em;'>BackpocketPhysio &rsaquo; DiagnoseR<span>"
    ),
  windowTitle = "BackpocketPhysio › Diagnosis",
  useShinydashboard(),
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            TAB: INFO ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel(
    "Info",
    icon = icon("info"),
    InfoUI(),
    downloadButton(
      "download", 
      label = "Download .csv"
    )
  ),
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            TAB: DIAGNOSTIC TESTS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel(
    "Diagnostic tests",
    icon = icon("vial"),
    sidebarLayout(
      fluid = TRUE,
      sidebarPanel(
        # style = "position:fixed;width:22%;",
        radioGroupButtons(
          inputId = "level",
          label = "Main group",
          choices = list("LEVEL 0" = "level0",
                         "LEVEL 1" = "level1"),
          justified = TRUE
        ),
        selectInput(
          inputId = "disorder",
          label = "Patient population",
          choices = NULL
        ),
        selectInput(
          inputId = "patpop",
          label = "Disorder",
          choices = NULL
        ),
        selectInput(
          inputId = "tests",
          label = "Test",
          choices = NULL
        ),
        # materialSwitch(
        #   inputId = "onlygood",
        #   label = "Se/Sp > 0.5",
        #   right = TRUE
        # ),
        # pickerInput(
        #   inputId = "lim_prev",
        #   label = "Limit the prevalence",
        #   choices = c("ALL", "5%", "10%", "20%")
        # ),
        sliderInput(
          inputId = "pat_prev",
          label = "Pre-test probability",
          value = 50,
          min = 1,
          max = 99
        )
      ),
      mainPanel(tabsetPanel(
        tabPanel(
          "Summary",
          h1("Summary of diagnostic accuracy over all studies"),
          p("The following numbers give an overview of the pooled sensitivity and pooled specificity of the selected test,
            and provides an overview of the pooled prevalence of the disease of interest across all studies. In total, ",
            textOutput("nstudies", inline = T),
            "studies were retrieved from the entire dataset with information on the diagnostic test of interest."
          ),
          fluidRow(
            column(
              h3("Sensitivity"),
              highchartOutput("se"),
              width = 4,
              align = "center"
            ),
            column(
              h3("Specificity"),
              highchartOutput("sp"),
              width = 4,
              align = "center"
            ),
            column(
              h3("Prevalence"),
              highchartOutput("prev"),
              width = 4,
              align = "center"
            )
          ),
          h1("How can we translate these numbers?"),
          p(
            "The following graph depicts the number of TP, FN, TN, and FP tests in a patient population of 100 patients based
             on an occurance that is identical to the pooled prevalence."
          ),
          HTML(
            "<h4><i class='fa fa-user-injured' style='color:#D35400'></i>&nbsp; Patients with the disorder that you can identify</h4>"
          ),
          HTML(
            "<h4><i class='fa fa-user-injured' style='color:#F89101'></i>&nbsp; Patients with the disorder that you will not identify</h4>"
          ),
          HTML(
            "<h4><i class='fa fa-user' style='color:#3CAB48'></i>&nbsp; Patients without the disorder that you can identify</h4>"
          ),
          HTML(
            "<h4><i class='fa fa-user' style='color:#FFCD01'></i>&nbsp; Patients without the disorder that you will wrongfully will classify</h4>"
          ),
          fluidRow(column(
            highchartOutput2("prev_item"),
            width = 12,
            align = "center"
          ))
        ),
        tabPanel(
          "Quality",
          h1("Overall study quality reported in systematic reviews"),
          p(
            "The following chart provides information on the overall study quality. The original scores were recalculated as
            percentages (%). For example, if a study achieved a score of 3 on a total of 7 for its risk of bias assessment, the
            score of 3/7 is recalculated as 43%."
          ),
          highchartOutput("qualityplot"),
          h1("Distribution of accuracy measures"),
          p(
            "The following chart provides information on the distribution of the prevalence in each study and the resulting accuracy measures."
          ),
          highchartOutput("bubbleplot")
        ),
        tabPanel(
          "Nomogram",
          h1("Probabilities for your patient"),
          p(
            "Based on the given pre-test probability (in the side bar), the post-test probabilities are automatically calculated and
             displayed as in the nomogram. the results of each calculation is displayed in each box.
             Both the post-test probability of disease are given for a negative and positive test based on your given clinical judgement.
             A patient with, for example, more clinical symptoms that match the disorder, can be given a higher pre-test probability.
             If you are unsure about the pre-test probability, you could use the pooled prevalence."
          ),
          h2("Did I identify the disorder in my patient?"),
          p(
            "Based on your given pre-test probability, the probability that your patients effectively has the disorder is calculated based on your specific given
             pre-test probabilty. You can change the pre-test probability on the left side to evaluate its effect."
          ),
          fluidRow(
            column(
              align = "center",
              width = 12,
              valueBoxOutput("post_test_pos", width = 6),
              valueBoxOutput("post_test_neg", width = 6)
            ),
            p(
              "Change the pre-test probability in the left side-bar to evaluate the probability that your patient has the disorder.
               Hereafter, you can find a typical nomogram that allows you to visually interpret the effect of the test on the evidence-based
               decision making process."
            ),
            plotOutput("nomogram")
          )
        )
      ))
    )
  ),
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            TAB: MY PATIENT ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel(
    "My patient",
    icon = icon("user-md"),
    sidebarLayout(
      sidebarPanel(
        fluid = TRUE,
        selectInput(
          inputId = "patpop_list",
          label = "Patient population",
          choices = NULL
        ),
        selectInput(
          inputId = "mypat_dis_1",
          label = "Disorder (1)",
          choices = NULL
        ),
        sliderInput(
          inputId = "mypat_prob_1",
          label = "Pre-test probability disorder (1)",
          value = 50,
          min = 0,
          max = 100
        ),
        selectInput(
          inputId = "mypat_dis_2",
          label = "Disorder (2)",
          choices = NULL
        ),
        sliderInput(
          inputId = "mypat_prob_2",
          label = "Pre-test probability disorder (2)",
          value = 50,
          min = 0,
          max = 100
        ),
        selectInput(
          inputId = "mypat_dis_3",
          label = "Disorder (3)",
          choices = NULL
        ),
        sliderInput(
          inputId = "mypat_prob_3",
          label = "Pre-test probability disorder (3)",
          value = 50,
          min = 0,
          max = 100
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Overview of most suitable tests",
            h1(
              "Most suitable tests to detect",
              textOutput("name_mypat_1", inline = T)
            ),
            DT::dataTableOutput("mypat_db_1"),
            h1(
              "Most suitable tests to detect",
              textOutput("name_mypat_2", inline = T)
            ),
            DT::dataTableOutput("mypat_db_2"),
            h1(
              "Most suitable tests to detect",
              textOutput("name_mypat_3", inline = T)
            ),
            DT::dataTableOutput("mypat_db_3")
          ),
          
        tabPanel(
          title = "Results of your patient",
          h1("Identify probability of disease for your patient"),
          p(
            "In the list below, you can select the tests you have performed on your patient. It provides you with information on the
          probability of your patient having or not having the disease when you would combine tests. When selecting a combined test set,
          it does not make sense to select the individual tests as well."
          ),
          fluidRow(
            column(width = 12, h2("Which test would you like to select?")),
            column(width = 4,
                   h3("Test: ",textOutput("name_mypat_1_T", inline = TRUE)),
                   radioGroupButtons(
                     inputId = "mypat_test_1",
                     choices = "",
                     direction = "vertical", size = "lg", justified = TRUE
                   ),
                   radioGroupButtons(
                     inputId = "mypat_test1_res",
                     label = "Test result:", 
                     choices = c(`<i class='fa fa-question'></i>` = "unkn", `<i class='fa fa-plus'></i>` = "pos", 
                                 `<i class='fa fa-minus'></i>` = "neg"),
                     justified = TRUE
                   ),
                   highchartOutput2("mypat_test1_graph")
                   ),
            column(width = 4,
                   h3("Test: ",textOutput("name_mypat_2_T", inline = TRUE)),
                   radioGroupButtons(
                     inputId = "mypat_test_2",
                     choices = "",
                     direction = "vertical", size = "lg", justified = TRUE
                   ),
                   radioGroupButtons(
                     inputId = "mypat_test2_res",
                     label = "Test result:", 
                     choices = c(`<i class='fa fa-question'></i>` = "unkn", `<i class='fa fa-plus'></i>` = "pos", 
                                 `<i class='fa fa-minus'></i>` = "neg"),
                     justified = TRUE
                   ),
                   highchartOutput2("mypat_test2_graph")
            ),
            column(width = 4,
                   h3("Test: ",textOutput("name_mypat_3_T", inline = TRUE)),
                   radioGroupButtons(
                     inputId = "mypat_test_3",
                     choices = "",
                     direction = "vertical", size = "lg", justified = TRUE
                   ),
                   radioGroupButtons(
                     inputId = "mypat_test3_res",
                     label = "Test result:", 
                     choices = c(`<i class='fa fa-question'></i>` = "unkn", `<i class='fa fa-plus'></i>` = "pos", 
                                 `<i class='fa fa-minus'></i>` = "neg"),
                     justified = TRUE
                   ),
                   highchartOutput2("mypat_test3_graph")
            )
          )
         
          # fluidRow(
          #   checkboxGroupButtons(
          #     inputId = "test_list",
          #     label = "Test you wish to perform",
          #     choices = c("None"),
          #     checkIcon = list(
          #       yes = tags$i(class = "fa fa-check-square",
          #                    style = "color: steelblue"),
          #       no = tags$i(class = "fa fa-square-o",
          #                   style = "color: steelblue")
          #     )
          #   )
          # ),
          # fluidRow(
          #   # actionButton(inputId = "testbutton", label = "Calculate"),
          #   h3("How likely does my patient have the disorder?"),
          #   # column(
          #   #   h3("NPV"),
          #   #   gaugeOutput("mypat_npv_s"),
          #   #   width = 4,
          #   #   align = "center"
          #   # ),
          #   column(
          #     h4("My patients tests positive on all tests:"),
          #     gaugeOutput("mypat_ppv_s"),
          #     width = 6,
          #     align = "center"
          #   ),
          #   column(
          #     h4("My patients tests positive on one of the tests:"),
          #     gaugeOutput("mypat_ppv_p"),
          #     width = 6,
          #     align = "center"
          #   )
          # )
        )
      ))
    )
  ),
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            TAB: ALL DATA ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("All data",
           icon = icon("table"),
           DT::dataTableOutput("dt_tests")),
  
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            TAB: PARTCIPATE ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("Participate",
           icon = icon("hands-helping")
           # HTML('<b>','Feedback','</b>'),
           # tags$iframe(src = 'https://forms.gle/fm9THiUFyhMCjdj69',
           #             width = '100%',
           #             height = 1000,
           #             frameborder = 0,
           #             marginheight = 0)
           )
)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##            SERVER ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##+++++++++++++++++++++++++++ SERVER FXS +++++++++++++++++++++++++++++++++++++

server <- function(input, output, session) {
  
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            OVERALL FUNCTIONS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## Filter pooled Se and Sp #####
  diag <- reactive({
    req(input$disorder)
    req(input$patpop)
    req(input$level)
    req(input$tests)
    
    ## filter pooled dataset
    tests_pooled_filter <- tests_pooled %>%
      filter(
        disease == input$disorder,
        population == input$patpop,
        level == input$level,
        test == input$tests
      )
    
    ## create empty list to save results
    diag <- list()
    
    ## prevalence
    diag[["prev"]] <- tests_pooled_filter$prev
    
    ## sensitivity
    diag[["se"]] <- tests_pooled_filter$se
    
    ## specificity
    diag[["sp"]] <- tests_pooled_filter$sp
    
    return(diag)
  })
  
  
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            TAB: DIAGNOSTIC TESTS ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## +++Input for each level #####
  level <- reactive({
    level <- dplyr::filter(tests, level == input$level)
    return(level)
  })
  
  observeEvent(level(), {
    choices <- unique(level()$disease)
    updateSelectInput(inputId = "disorder", choices = choices)
  })
  
  disorder_lvl <- reactive({
    req(input$disorder)
    disorder_lvl <- filter(level(), disease == input$disorder)
    return(disorder_lvl)
  })
  
  observeEvent(disorder_lvl(), {
    choices <- unique(disorder_lvl()$population)
    updateSelectInput(inputId = "patpop", choices = choices)
  })
  
  patpop_lvl <- reactive({
    req(input$patpop)
    patpop_lvl <- filter(disorder_lvl(), population == input$patpop)
    return(patpop_lvl)
  })
  
  observeEvent(patpop_lvl(), {
    choices <- unique(patpop_lvl()$test)
    updateSelectInput(inputId = "tests", choices = choices)
  })
  
  data <- reactive({
    req(input$tests)
    data <- filter(disorder_lvl(), test == input$tests)
    return(data)
  })
  
  ## +++Output #####
  ## ++++++Text output update #####
  output$nstudies <- renderText({
    nrow(data())
    
  })
  
  ## ++++++Create gauges #####
  ## +++++++++Se #####
  output$se <- renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = round(diag()$se, 3) * 100,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>%
      hc_size(height = 300)
  })
  
  ## +++++++++Sp #####
  output$sp <- renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = round(diag()$sp, 3) * 100,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>%
      hc_size(height = 300)
  })
  
  ## +++++++++Prev #####
  output$prev <- renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        #stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = round(diag()$prev, 3) * 100,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>%
      hc_size(height = 300)
  })
  
  ## ++++++Create itemplot #####
  ## +++++++++Prev #####
  output$prev_item <- renderHighchart2({
    ## save input
    Prev <- diag()$prev
    Sp <- diag()$sp
    Se <- diag()$se
    
    PAT <- round(100 * Prev)
    HEALTH <- 100 - PAT
    
    TP <- round(Se * PAT)
    TN <- round(Sp * HEALTH)
    FN <- PAT - TP
    FP <- HEALTH - TN
    
    df2 <- tibble(
      type = c("TP", "FN", "TN", "FP"),
      amount = c(TP, FN, TN, FP),
      faico = c("user-injured", "user-injured", "user", "user"),
      col = c("#D35400", "#F89101", "#3CAB48", "#FFCD01")
    )
    
    df2 <- df2 %>%
      mutate(
        uri = map2_chr(faico, col, ~ fa_to_png_to_datauri(.x, fill = .y)),
        marker = map(uri, ~ list(
          symbol = str_glue("url({data_uri})", data_uri = .x)
        ))
      )
    
    hchart(
      df2,
      "item",
      hcaes(name = type, y = amount),
      name = "Test result",
      showInLegend = TRUE
    ) %>%
      hc_plotOptions(# avoid hide series due bug
        series = list(point = list(events = list(
          legendItemClick = JS("function(e) {e.preventDefault() }")
        )))) %>%
      hc_legend(labelFormat =  '{name} <span style="opacity: 0.4">{y}</span>') %>%
      hc_colors(pull(df2, col))
    
  })
  
  ## ++++++Study Quality #####
  quality <- reactive({
    qual_dat <- data() %>%
      mutate(
        Score = round(score / maxscore, 3) * 100,
        Study = paste0(study, " (", year, ")")
      )
    
    ## +++++++++Bullet plot #####
    hchart(qual_dat,
           "bullet",
           hcaes(x = Study, y = Score, target = 100),
           color = "black",
           name = "Quality score (%)") %>%
      hc_chart(inverted = TRUE) %>%
      hc_yAxis(
        min = 0,
        max = 100,
        gridLineWidth = 0,
        plotBands = list(
          list(
            from = 0,
            to = 50,
            color = "#e74c3c"
          ),
          list(
            from = 50,
            to = 80,
            color = "#f39c12"
          ),
          list(
            from = 80,
            to = 100,
            color = "#00a65a"
          )
        )
      ) %>%
      hc_xAxis(gridLineWidth = 15,
               gridLineColor = "white") %>%
      hc_plotOptions(series = list(
        pointPadding = 0.25,
        pointWidth = 15,
        borderWidth = 0,
        targetOptions = list(width = '200%')
      )) %>%
      hc_size(height = 300)
    
  })
  
  output$qualityplot <- renderHighchart({
    quality()
  })
  
  
  ## +++++++++Bubble plot #####
  bubble <- reactive({
    bubble_dat <- data() %>%
      mutate(
        nsize = tp+fp+tn+fn,
        study = paste0(study, " (", year, ")")
      ) %>%
      rename(Prevalence = prev, Sensitivity = se, Specificity = sp) %>%
      pivot_longer(cols = c(Prevalence, Sensitivity, Specificity))
    
    hchart(bubble_dat,
           'scatter', 
           hcaes(x = name, y = value, size = nsize, group = study),
           maxSize = "10%"
    ) %>%
      hc_chart(inverted = TRUE) %>%
      hc_yAxis(
        min = 0,
        max = 1,
        gridLineWidth = 0,
        title = list(text = "")) %>%
      hc_xAxis(
        title = list(text = ""))
    
  })
  
  output$bubbleplot <- renderHighchart({
    bubble()
  })
  
  ## ++++++Create Nomogram #####
  output$nomogram <- renderPlot({
    pl_nomo <- nomogrammer(
      Prevalence = input$pat_prev / 100,
      Sens = diag()$se,
      Spec = diag()$sp
    ) + scale_color_manual(values = c("#e74c3c", "#00a65a"))
    return(pl_nomo)
  })
  
  ## +++++++++Item PPV and NPV #####
  post_test <- reactive({
    ## Calculate post test probability
    
    prior_prob  <-  input$pat_prev / 100
    prior_odds  <- odds(prior_prob)
    sensitivity <- min(diag()$se, 0.99999)
    specificity <- min(diag()$sp, 0.99999)
    PLR <- sensitivity / (1 - specificity)
    NLR <- (1 - sensitivity) / specificity
    post_odds_pos  <- prior_odds * PLR
    post_odds_neg  <- prior_odds * NLR
    post_prob_pos  <- post_odds_pos / (1 + post_odds_pos)
    post_prob_neg  <- post_odds_neg / (1 + post_odds_neg)
    
    return(list("post_prob_pos" = post_prob_pos,
                "post_prob_neg" = post_prob_neg))
  })
  
  output$post_test_pos <- renderValueBox({
    valueBox(
      paste0(round(post_test()$post_prob_pos * 100), " %"),
      color = "green",
      subtitle = "Probability that your patient has the disorder after a positive test",
      width = 6
    )
  })
  
  output$post_test_neg <- renderValueBox({
    valueBox(
      paste0(round(post_test()$post_prob_neg * 100), " %"),
      color = "red",
      subtitle = "Probability that your patient has the disorder after a negative test",
      width = 6
    )
  })
  
  
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            TAB: MY PATIENT ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## +++Update input for each level #####
  mypat_level <- reactive({
    mypat_level <- dplyr::filter(tests_pooled, level == "level1")
    return(mypat_level)
  })
  
  observeEvent(mypat_level(), {
    choices <- unique(mypat_level()$disease)
    updateSelectInput(inputId = "patpop_list", choices = choices)
  })
  
  mypat_disorder_lvl <- reactive({
    req(input$patpop_list)
    mypat_disorder_lvl <- filter(mypat_level(), disease == input$patpop_list)
    return(mypat_disorder_lvl)
  })
  
  ## DISORDER 1
  
  observeEvent(mypat_disorder_lvl(), {
    choices <- unique(mypat_disorder_lvl()$population)
    updateSelectInput(inputId = "mypat_dis_1", choices = choices)
  })
  
  mypat_dat_1 <- reactive({
    req(input$mypat_dis_1)
    mypat_dat_1 <- filter(mypat_disorder_lvl(), population == input$mypat_dis_1)
    return(mypat_dat_1)
  })
  
  output$name_mypat_1 <- renderText({
    req(input$mypat_dis_1)
    input$mypat_dis_1
    
  })
  
  output$name_mypat_1_T <- renderText({
    req(input$mypat_dis_1)
    input$mypat_dis_1
    
  })
  
  observeEvent(input$mypat_dis_1, {
    req(mypat_dat_1())
    choices <- unique(mypat_dat_1()$test)
    updateRadioGroupButtons(inputId = "mypat_test_1", choices = choices, session = session)
  })
  
  ## DISORDER 2
  
  observeEvent(mypat_disorder_lvl(), {
    choices <- unique(mypat_disorder_lvl()$population)
    updateSelectInput(inputId = "mypat_dis_2", choices = choices)
  })
  
  mypat_dat_2 <- reactive({
    req(input$mypat_dis_2)
    mypat_dat_2 <- filter(mypat_disorder_lvl(), population == input$mypat_dis_2)
    return(mypat_dat_2)
  })
  
  output$name_mypat_2 <- renderText({
    req(input$mypat_dis_2)
    input$mypat_dis_2
    
  })
  
  output$name_mypat_2_T <- renderText({
    req(input$mypat_dis_2)
    input$mypat_dis_2
    
  })
  
  observeEvent(input$mypat_dis_2, {
    req(mypat_dat_2())
    choices <- unique(mypat_dat_2()$test)
    updateRadioGroupButtons(inputId = "mypat_test_2", choices = choices, session = session)
  })
  
  ## DISORDER 3
  
  observeEvent(mypat_disorder_lvl(), {
    choices <- unique(mypat_disorder_lvl()$population)
    updateSelectInput(inputId = "mypat_dis_3", choices = choices)
  })
  
  mypat_dat_3 <- reactive({
    req(input$mypat_dis_3)
    mypat_dat_3 <- filter(mypat_disorder_lvl(), population == input$mypat_dis_3)
    return(mypat_dat_3)
  })
  
  output$name_mypat_3 <- renderText({
    req(input$mypat_dis_3)
    input$mypat_dis_3
    
  })
  
  output$name_mypat_3_T <- renderText({
    req(input$mypat_dis_3)
    input$mypat_dis_3
    
  })
  
  observeEvent(input$mypat_dis_3, {
    req(mypat_dat_3())
    choices <- unique(mypat_dat_3()$test)
    updateRadioGroupButtons(inputId = "mypat_test_3", choices = choices, session = session)
  })
  
  # +++ Output
  # ++++++Which tests should I select? #####
  # +++++++++Table with ranked tests #####
  
  ## DISORDER 1
  
  mypat_db_1 <- eventReactive(mypat_dat_1(),{

    mypat_db_1 <- mypat_dat_1() %>%
      ungroup() %>%
      mutate(ord = -(se + sp),
             lr_pos = round(lr_pos, 2),
             lr_neg = round(lr_neg, 2)) %>%
      arrange(ord) %>%
      select(Test = test, Sensitvity = se, Specificity = sp, Prevalence = prev,
             `Positive LR` = lr_pos, `Negative LR` = lr_neg)
    
    return(mypat_db_1)
  })
  
  output$mypat_db_1 <- DT::renderDataTable({
    ## add colors
    clrs <- round(seq(255, 40, length.out = length(seq(0,1,0.05)) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    
    ## DT
    mypat_db_1() %>%
      DT::datatable(data = .,
                    style = "bootstrap",
                    options = list(pageLength = 4,
                                   dom = 't')) %>%
      DT::formatStyle(
        'Sensitvity',
        #backgroundColor = DT::styleInterval(seq(0,1,0.05), clrs),
        background = DT::styleColorBar(seq(0,1,0.05), "#00a65a"),
        backgroundSize = '95% 95%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'Specificity',
        background = DT::styleColorBar(seq(0,1,0.05), "#00a65a"),
        backgroundSize = '95% 95%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  ## DISORDER 2
  
  mypat_db_2 <- eventReactive(mypat_dat_2(),{
    
    mypat_db_2 <- mypat_dat_2() %>%
      ungroup() %>%
      mutate(ord = -(se + sp),
             lr_pos = round(lr_pos, 2),
             lr_neg = round(lr_neg, 2)) %>%
      arrange(ord) %>%
      select(Test = test, Sensitvity = se, Specificity = sp, Prevalence = prev,
             `Positive LR` = lr_pos, `Negative LR` = lr_neg)
    
    return(mypat_db_2)
  })
  
  output$mypat_db_2 <- DT::renderDataTable({
    ## add colors
    clrs <- round(seq(255, 40, length.out = length(seq(0,1,0.05)) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    
    ## DT
    mypat_db_2() %>%
      DT::datatable(data = .,
                    style = "bootstrap",
                    options = list(pageLength = 4,
                                   dom = 't')) %>%
      DT::formatStyle(
        'Sensitvity',
        #backgroundColor = DT::styleInterval(seq(0,1,0.05), clrs),
        background = DT::styleColorBar(seq(0,1,0.05), "#00a65a"),
        backgroundSize = '95% 95%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'Specificity',
        background = DT::styleColorBar(seq(0,1,0.05), "#00a65a"),
        backgroundSize = '95% 95%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  ## DISORDER 3
  
  mypat_db_3 <- eventReactive(mypat_dat_3(),{
    
    mypat_db_3 <- mypat_dat_3() %>%
      ungroup() %>%
      mutate(ord = -(se + sp),
             lr_pos = round(lr_pos, 2),
             lr_neg = round(lr_neg, 2)) %>%
      arrange(ord) %>%
      select(Test = test, Sensitvity = se, Specificity = sp, Prevalence = prev,
             `Positive LR` = lr_pos, `Negative LR` = lr_neg)
    
    return(mypat_db_3)
  })
  
  output$mypat_db_3 <- DT::renderDataTable({
    ## add colors
    clrs <- round(seq(255, 40, length.out = length(seq(0,1,0.05)) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    
    ## DT
    mypat_db_3() %>%
      DT::datatable(data = .,
                    style = "bootstrap",
                    options = list(pageLength = 4,
                                   dom = 't')) %>%
      DT::formatStyle(
        'Sensitvity',
        #backgroundColor = DT::styleInterval(seq(0,1,0.05), clrs),
        background = DT::styleColorBar(seq(0,1,0.05), "#00a65a"),
        backgroundSize = '95% 95%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'Specificity',
        background = DT::styleColorBar(seq(0,1,0.05), "#00a65a"),
        backgroundSize = '95% 95%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  

  # +++++++++Results of each test #####
  output$mypat_test1_graph <- renderHighchart2({
    
    # input$mypat_test_1 <- "ROM"
    # input$mypat_test1_res <- "pos"
    
    tmp <- tests_pooled %>%
      filter(level == "level1", test == input$mypat_test_1, 
             disease == input$patpop_list, 
             population == input$mypat_dis_1)
    
    se <- min(tmp$se, 0.999)
    sp <- min(tmp$sp,0.999)
    pre <- (input$mypat_prob_1/100)
    
    prior_odds  <- odds(pre)
    PLR <- se / (1 - sp)
    NLR <- (1 - se) / sp
    post_odds_pos  <- prior_odds * PLR
    post_odds_neg  <- prior_odds * NLR
    post_prob_pos  <- post_odds_pos / (1 + post_odds_pos)
    post_prob_neg  <- post_odds_neg / (1 + post_odds_neg)
    
    if (input$mypat_test1_res == "pos") {
      
      ## Create dataset
      df <- tibble(pre = pre, post = post_prob_pos)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
      
    }
    
    if (input$mypat_test1_res == "neg") {
      ## Create dataset
      df <- tibble(pre = pre, post = post_prob_neg)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
    }
    
    if (input$mypat_test1_res == "unkn") {
      ## Create dataset
      df <- tibble(pre = pre, post = pre)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
    }

    hc
  })
  
  output$mypat_test2_graph <- renderHighchart2({
    
    # input$mypat_test_1 <- "ROM"
    # input$mypat_test1_res <- "pos"
    
    tmp <- tests_pooled %>%
      filter(level == "level1", test == input$mypat_test_2, 
             disease == input$patpop_list, 
             population == input$mypat_dis_2)
    
    se <- min(tmp$se, 0.999)
    sp <- min(tmp$sp,0.999)
    pre <- (input$mypat_prob_2/100)
    
    prior_odds  <- odds(pre)
    PLR <- se / (1 - sp)
    NLR <- (1 - se) / sp
    post_odds_pos  <- prior_odds * PLR
    post_odds_neg  <- prior_odds * NLR
    post_prob_pos  <- post_odds_pos / (1 + post_odds_pos)
    post_prob_neg  <- post_odds_neg / (1 + post_odds_neg)
    
    if (input$mypat_test2_res == "pos") {
      
      ## Create dataset
      df <- tibble(pre = pre, post = post_prob_pos)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
      
    }
    
    if (input$mypat_test2_res == "neg") {
      ## Create dataset
      df <- tibble(pre = pre, post = post_prob_neg)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
    }
    
    if (input$mypat_test2_res == "unkn") {
      ## Create dataset
      df <- tibble(pre = pre, post = pre)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
    }
    
    hc
  })
  
  output$mypat_test3_graph <- renderHighchart2({
    
    # input$mypat_test_1 <- "ROM"
    # input$mypat_test1_res <- "pos"
    
    tmp <- tests_pooled %>%
      filter(level == "level1", test == input$mypat_test_3, 
             disease == input$patpop_list, 
             population == input$mypat_dis_3)
    
    se <- min(tmp$se, 0.999)
    sp <- min(tmp$sp,0.999)
    pre <- (input$mypat_prob_3/100)
    
    prior_odds  <- odds(pre)
    PLR <- se / (1 - sp)
    NLR <- (1 - se) / sp
    post_odds_pos  <- prior_odds * PLR
    post_odds_neg  <- prior_odds * NLR
    post_prob_pos  <- post_odds_pos / (1 + post_odds_pos)
    post_prob_neg  <- post_odds_neg / (1 + post_odds_neg)
    
    if (input$mypat_test3_res == "pos") {
      
      ## Create dataset
      df <- tibble(pre = pre, post = post_prob_pos)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
      
    }
    
    if (input$mypat_test3_res == "neg") {
      ## Create dataset
      df <- tibble(pre = pre, post = post_prob_neg)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
    }
    
    if (input$mypat_test3_res == "unkn") {
      ## Create dataset
      df <- tibble(pre = pre, post = pre)
      df_l <- pivot_longer(df, cols = c(pre, post))
      df_l$name <- factor(df_l$name, c("pre", "post"), c("Pre", "Post"))
      
      ## create graph
      hc <- df_l %>%
        hchart('column', hcaes(x = name, y = value*100)) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_colors(c("black", "black")) %>%
        hc_yAxis(title = list(text = "Probability"),
                 min = 0,
                 max = 100,
                 plotBands = list(
                   list(
                     from = 80,
                     to = 100,
                     color = hex_to_rgba("green", 0.1),
                     label = list(text = "Present", rotation = -90, align = "left"),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 20,
                     to = 80,
                     color = hex_to_rgba("orange", 0.1),
                     label = list(text = "Undecided", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   ),
                   list(
                     from = 0,
                     to = 20,
                     color = hex_to_rgba("red", 0.1),
                     label = list(text = "Absent", rotation = -90),
                     # the zIndex is used to put the label text over the grid lines 
                     zIndex = 1
                   )
                 )) %>%
        hc_tooltip(valueDecimals = 1, valueSuffix = "/100%") %>%
        hc_plotOptions(series = list(colorByPoint = TRUE))
    }
    
    hc
  })
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##            TAB: ALL DATA ####
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## OUTPUT (TAB:: ALL DATA)
  output$dt_tests = DT::renderDataTable({
    tests %>%
      DT::datatable(data = .,
                    extensions = 'Buttons', 
                    options = list(
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}"),
                      columnDefs = list(list(className = 'dt-center', targets = 0:4))
                    ))
    
  })
  
  ## +++Download function #####
  output$download <- downloadHandler(
    filename = function() {
      paste0("alldata.csv")
    },
    content = function(file) {
      write.csv2(tests, file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
