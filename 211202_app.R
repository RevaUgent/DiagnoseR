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
source("www/helpers.R")
source("www/nomogrammer.R")
library(highcharter)
library(purrr) # to work with list columns
library(fontawesome)
library(extrafont)
library(rsvg)
library(stringr)
#library(waffle)
#library(hrbrthemes)


##+++++++++++++++++++++++++++ Load data ++++++++++++++++++++++++++++++++++++++
tests <- tidyr::pivot_longer(
  openxlsx::read.xlsx("www/tests.xlsx"),
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
##+++++++++++++++++++++++++++ Graphical UI +++++++++++++++++++++++++++++++++++
ui <- navbarPage(
  title =
    HTML(
      "<a href='https://www.ugent.be'><img src='ugent_logo.png' height='30px'></a>&nbsp;&nbsp;&nbsp; <span style='color:#1E64C8; font-size:1.1em;'>BackpocketPhysio &rsaquo; DiagnoseR<span>"
    ),
  windowTitle = "BackpocketPhysio › Diagnosis",
  useShinydashboard(),
  tabPanel(
    "Info",
    icon = icon("info-circle"),
    h2("BackpocketPhysio project"),
    HTML(
      "<p>The BackpocketPhysio project tries to translate science within the Physiotherapist to the daily practice.
       The current BackpocketPhysio › Diagnosis APP aims to set-up a datbase that includes all diagnostics tests that are
       applied in a physiotherapy practice. The dataset with all information can also be downloaded from the website,
      and is free to use by anyone.</p> <br>
      <p></p>"
    ),
    HTML(
      "<p>The project is coordinated by the Department of Rehabilation Sciences at Ghent University.</p>"
    ),
    
    
    HTML("<h2><i class='fa fa-vial'></i>&nbsp; Diagnostic Tests</h2>"),
    p(
      "Translating diagnostic accuracy studies into a physio's practice can be a difficult task. By design, many diagnostic studies report mainly the Sensitivity (Sn) and Specificity (Sp) of the evaluated diagnostic tests.
      Unfortunatly both Se and Sp are difficult to interpret in a daily practitc. The basic idea of diagnostic accuracy studies is to
      identify patients with a particular disorder in a pool of individuals. The presence of a particular disease is often noted as
      D+, while its absence is often noted as D-. A similar approach is used for a positive (T+) and negative (T-) test. Se and Sp are
      primarely based on the diagnosis (D) of the patient, and an evaluation of the proportion of tests (T) that are correct and in accordance
      with the accordance with the D. This implies we need to have information on the disease status of the patient, which is only the
      case in a scientific study. However, in practice, we are unaware of the disease status (D+ or D-) of the patient, and only have
      information on the test (T+ or T-). For this purpose, we can use the positive and negative predictive value (PPV and NPV), which
      can be immediatly be translated into a clinical practice as, for example, the PPV can be translated immediatly as the probability
      that your patient actually has the disease. One of the major drawback of the PPV and NPV is that they are depending on the a priori
      probability of having the disease. This is the reason why the PPV and NPV are often useless in the context of a scientific article
      as the a priori probability of being sick is often overestimated (e.g., in diagnostic studies the number of patients often equals
      the number of non-patients). In contrast the Se and Sp is insensible to the a priori probability, which explains their popularity in
      the scientific literature."
    ),
    
    
    HTML("<h2><i class='fa fa-user-md'></i>&nbsp; My Patients</h2>"),
    p(
      "This tab focuses on the translation of test combinations in your practice. It allows you to select multiple test that you want to perform in practice,
      and check what the probability is of your patients haveing or not having the disease when all performed tests are negative or at least one test is negative."
    ),
    
    HTML("<h2><i class='fa fa-table'></i>&nbsp; All data</h2>"),
    HTML(
      "<p>In the <b>All Data</b> tab, you will find all the studies that are included in the current dataset. If you wish to include more studies,
                            you can always fill in the following <a href='https://forms.gle/fm9THiUFyhMCjdj69'>survey</a></p>"
    ),
    downloadButton("download", label = "Download .csv"),
  ),
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
          label = "Disorder",
          choices = NULL
        ),
        selectInput(
          inputId = "patpop",
          label = "Patient population",
          choices = NULL
        ),
        selectInput(
          inputId = "tests",
          label = "Test",
          choices = NULL
        ),
        materialSwitch(
          inputId = "onlygood",
          label = "Se/Sp > 0.5",
          right = TRUE
        ),
        pickerInput(
          inputId = "lim_prev",
          label = "Limit the prevalence",
          choices = c("ALL", "5%", "10%", "20%")
        ),
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
          p(
            "The following numbers give an overview of the pooled sensitivity and pooled specificity of the selected test,
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
          "Patient",
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
  tabPanel(
    "My patient",
    icon = icon("user-md"),
    sidebarLayout(
      sidebarPanel(
        fluid = TRUE,
        selectInput(
          inputId = "disorder_list",
          label = "Disorder",
          choices = NULL
        ),
        selectInput(
          inputId = "patpop_mypat",
          label = "Patient population",
          choices = NULL
        ),
        sliderInput(
          inputId = "pat_prev_mypat",
          label = "Pre-test probability",
          value = 50,
          min = 0,
          max = 100
        )
      ),
      mainPanel(tabsetPanel(
        tabPanel(title = "Which test should I select?",
                 h1("Table with tests"),
                 DT::dataTableOutput("mypat_db")),
        tabPanel(
          title = "Patient",
          h1("Identify probability of disease for your patient"),
          p(
            "In the list below, you can select the tests you have performed on your patient. It provides you with information on the
          probability of your patient having or not having the disease when you would combine tests. When selecting a combined test set,
          it does not make sense to select the individual tests as well."
          ),
          fluidRow(
            checkboxGroupButtons(
              inputId = "test_list",
              label = "Test you wish to perform",
              choices = c("None"),
              checkIcon = list(
                yes = tags$i(class = "fa fa-check-square",
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-square-o",
                            style = "color: steelblue")
              )
            )
          ),
          fluidRow(
            # actionButton(inputId = "testbutton", label = "Calculate"),
            h3("How likely does my patient have the disorder?"),
            # column(
            #   h3("NPV"),
            #   gaugeOutput("mypat_npv_s"),
            #   width = 4,
            #   align = "center"
            # ),
            column(
              h4("My patients tests positive on all tests:"),
              gaugeOutput("mypat_ppv_s"),
              width = 6,
              align = "center"
            ),
            column(
              h4("My patients tests positive on one of the tests:"),
              gaugeOutput("mypat_ppv_p"),
              width = 6,
              align = "center"
            )
          )
          # fluidRow(
          #   h3("My patients tests positive on one of the tests:"),
          #   column(
          #     h3("NPV"),
          #     gaugeOutput("mypat_npv_p"),
          #     width = 4,
          #     align = "center"
          #   ),
          #   column(
          #     h3("My patients tests positive on one of the tests:"),
          #     gaugeOutput("mypat_ppv_p"),
          #     width = 4,
          #     align = "center"
          #   )
          # )
        )
      ))
    )
  ),
  tabPanel("All data",
           icon = icon("table"),
           DT::dataTableOutput("dt_tests"))
)

##+++++++++++++++++++++++++++ SERVER FXS +++++++++++++++++++++++++++++++++++++
server <- function(input, output, session) {
  ## LIMIT BASED ON INPUT
  tests_pooled <- reactive({
    if (input$lim_prev == "ALL") {
      tests <- tests %>%
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
    } ## END IF
    
    if (input$lim_prev != "ALL") {
      req(input$pat_prev)
      lim_id <-
        c(0.05, 0.1, 0.2)[(c("5%", "10%", "20%") == input$lim_prev)]
      tests <- tests %>%
        dplyr::mutate(prev = (tp + fp) / (tp + fp + tn + fn)) %>%
        dplyr::filter(prev < (input$pat_prev + 0.05) | prev > (input$pat_prev - 0.05)) %>%
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
    } ## END IF
    
    return(tests)
    
  })
  
  ## SELECT INPUT AND UPDATE CHOICES FOR EACH LEVEL (TAB:: DIAGNOSTIC TESTS)
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
  
  ## SELECT INPUT AND UPDATE CHOICES FOR EACH LEVEL (TAB:: MY PATIENT)
  observe({
    tests_level1 <- dplyr::filter(tests, level == "level1")
    choices <- unique(tests_level1$disease)
    updateSelectInput(inputId = "disorder_list", choices = choices)
  })
  
  disorder_mypat <- reactive({
    req(input$disorder_list)
    disorder_mypat <- filter(tests, disease == input$disorder_list)
    return(disorder_mypat)
  })
  
  observeEvent(disorder_mypat(), {
    choices <- unique(disorder_mypat()$population)
    updateSelectInput(inputId = "patpop_mypat", choices = choices)
  })
  
  patpop_mypat_dat <- reactive({
    req(input$patpop_mypat)
    patpop_mypat_dat <-
      filter(disorder_mypat(), population == input$patpop_mypat)
    return(patpop_mypat_dat)
  })
  
  observeEvent(patpop_mypat_dat(), {
    choices <- unique(patpop_mypat_dat()$test)
    updateCheckboxGroupButtons(session = session,
                               inputId = "test_list",
                               choices = choices)
  })
  
  # observeEvent(input$test_list, {
  #   choices <- input$test_list
  #   updatePrettyCheckboxGroup(
  #     session = session, inputId = "test_positives",
  #     choices = choices, inline = TRUE,
  #   )
  # })
  
  
  ## CALCULATIONS (TAB:: MY PATIENT)
  
  ## ARRANGE TESTS
  mypat_db <- reactive({
    req(input$disorder_list)
    ## COMBINED SE AND SP
    tmp <- filter(
      tests_pooled(),
      level == "level1",
      disease == input$disorder_list
    ) %>%
      mutate(prev = round(prev,3),
             se = round(se,3),
             sp = round(sp,3),
             lr_pos = round(lr_pos,2),
             lr_neg = round(lr_neg,2)) %>%
      ungroup() %>%
      mutate(ord = -(se + sp)) %>%
      arrange(ord) %>%
      select(Test = test, Sensitvity = se, Specificity = sp, Prevalence = prev,
             `Positive LR` = lr_pos, `Negative LR` = lr_neg)
  })
  
  ## OUTPUT (TAB:: MY PAT)
  output$mypat_db = DT::renderDataTable({
    ## colors
    clrs <- round(seq(255, 40, length.out = length(seq(0,1,0.05)) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    #datatable(df) %>% formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
    
    ## DT
    mypat_db() %>%
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
                    )) %>%
      DT::formatStyle(
        'Sensitvity',
        #backgroundColor = DT::styleInterval(seq(0,1,0.05), clrs),
        background = DT::styleColorBar(seq(0,1,0.05), "#00a65a"),
        backgroundSize = '90% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'Specificity',
        background = DT::styleColorBar(seq(0,1,0.05), "#00a65a"),
        backgroundSize = '80% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  ## CALCULATE COMBINED SENSITIVITY/SPECIFICTY
  post_test_mypat <- reactive({
    req(input$disorder_list)
    req(input$test_list)
    ## COMBINED SE AND SP
    tmp <- filter(
      tests_pooled(),
      test %in% input$test_list,
      level == "level1",
      disease == input$disorder_list
    )
    se_p <- 1
    sp_p <- 1
    se_s <- 1
    sp_s <- 1
    
    for (i in 1:nrow(tmp)) {
      ## combined
      se_s <- tmp[i, "se"] * se_s
      sp_p <- tmp[i, "sp"] * sp_p
      
      se_p <- se_p * (1 - tmp[i, "se"])
      sp_s <- sp_s * (1 - tmp[i, "sp"])
    }
    
    ## after loop
    se_p <- 1 - se_p
    sp_s <- 1 - sp_s
    
    ## Calculate post test probability for SERIAL
    prior_prob  <-  input$pat_prev_mypat / 100
    prior_odds  <- odds(prior_prob)
    sensitivity <- min(se_s, 0.99999)
    specificity <- min(sp_s, 0.99999)
    PLR <- sensitivity / (1 - specificity)
    NLR <- (1 - sensitivity) / specificity
    post_odds_pos  <- prior_odds * PLR
    post_odds_neg  <- prior_odds * NLR
    post_prob_pos_s  <- post_odds_pos / (1 + post_odds_pos)
    post_prob_neg_s  <- post_odds_neg / (1 + post_odds_neg)
    
    ## Calculate post test probability for PARALLEL
    prior_prob  <-  input$pat_prev_mypat / 100
    prior_odds  <- odds(prior_prob)
    sensitivity <- min(se_p, 0.99999)
    specificity <- min(sp_p, 0.99999)
    PLR <- sensitivity / (1 - specificity)
    NLR <- (1 - sensitivity) / specificity
    post_odds_pos  <- prior_odds * PLR
    post_odds_neg  <- prior_odds * NLR
    post_prob_pos_p  <- post_odds_pos / (1 + post_odds_pos)
    post_prob_neg_p  <- post_odds_neg / (1 + post_odds_neg)
    
    post_test_mypat <-
      list(
        "post_prob_pos_s" = post_prob_pos_s,
        "post_prob_neg_s" = post_prob_neg_s,
        "post_prob_pos_p" = post_prob_pos_p,
        "post_prob_neg_p" = post_prob_neg_p
      )
    
    return(post_test_mypat)
  })
  
  ## OUTPUT TEXT
  output$nstudies <- renderText({
    nrow(data())
    
  })
  
  ## CALCULATE SENSITIVITY/SPECIFICITY/PREVALENCE
  diag <- reactive({
    req(input$disorder)
    req(input$patpop)
    req(input$level)
    req(input$tests)
    
    ## filter pooled dataset
    tests_pooled_filter <- tests_pooled() %>%
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
  
  ## CREATE GAUGES
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
  
  ## ITEM PLOTS
  # output$se_item <- renderHighchart({
  #
  #   df.item <- tibble(
  #     type = c("Patient", "Healthy"),
  #     amount = c(round(diag()$se * 100), round((1-diag()$se) * 100)),
  #     col = c("green", "red")
  #   )
  #
  #   hchart(
  #     df.item,
  #     "item",
  #     hcaes(name = type, y = amount),
  #     name = "What I eat",
  #     showInLegend = TRUE
  #   ) %>%
  #     hc_legend(
  #       labelFormat =  '{name} <span style="opacity: 0.4">{y}</span>'
  #     ) %>%
  #     hc_colors(pull(df.item, col)) %>%
  #     hc_size(height = 300)
  #
  # })
  
  # output$sp_item <- renderHighchart({
  #
  #   df.item <- tibble(
  #     type = c("Patient", "Healthy"),
  #     amount = c(round(diag()$sp * 100), round((1-diag()$sp) * 100)),
  #     col = c("green", "red")
  #   )
  #
  #   hchart(
  #     df.item,
  #     "item",
  #     hcaes(name = type, y = amount),
  #     name = "What I eat",
  #     showInLegend = TRUE
  #   ) %>%
  #     hc_legend(
  #       labelFormat =  '{name} <span style="opacity: 0.4">{y}</span>'
  #     ) %>%
  #     hc_colors(pull(df.item, col)) %>%
  #     hc_size(height = 300)
  #
  # })
  #
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
  
  # output$prev_item <- renderPlot({
  #
  #   ## save input
  #   Prev <- diag()$prev
  #   Sp <- diag()$sp
  #   Se <- diag()$se
  #
  #   PAT <- round(100*Prev)
  #   HEALTH <- 100-PAT
  #
  #   TP <- round(Se*PAT)
  #   TN <- round(Sp*HEALTH)
  #   FN <- PAT - TP
  #   FP <- HEALTH - TN
  #
  #   df2 <- tibble(
  #     type = c("TP", "FN", "TN", "FP"),
  #     amount = c(TP, FN, TN, FP),
  #     faico = c("user-injured", "user-injured", "user", "user"),
  #     col = c("#D35400", "#F89101", "#3CAB48", "#FFCD01")
  #   )
  #
  #   pl <- df2 %>%
  #     ggplot(aes(label = type, values = amount)) +
  #     geom_pictogram(n_rows = 10, aes(colour = col), flip = TRUE, make_proportional = FALSE) +
  #     scale_color_identity(
  #       name = NULL,
  #       values = c("TP" = "#D35400", "TN" = "#3CAB48","FP" = "#FFCD01", "FN" = "#F89101"),
  #       labels = c(paste0("TP = ", TP), paste0("TN = ", TN), paste0("FP = ", FP), paste0("FN = ", FN))
  #     ) +
  #     scale_label_pictogram(
  #       name = NULL,
  #       values = c("user-injured", "user", "user", "user-injured"),
  #       labels = c(paste0("TP = ", TP), paste0("TN = ", TN), paste0("FP = ", FP), paste0("FN = ", FN))
  #     ) +
  #     coord_equal() +
  #     theme_ipsum_rc(grid="") +
  #     theme_enhance_waffle() +
  #     theme(legend.key.height = unit(2.25, "line")) +
  #     theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))
  #
  #   pl
  #
  # })
  
  ## PATIENTSPECIFIC PPV and NPV
  output$mypat_ppv_s <- renderGauge({
    gauge(
      value = round(post_test_mypat()$post_prob_pos_s, 3) * 100,
      abbreviateDecimals = 2,
      symbol = "%",
      min = 0,
      max = 100,
      sectors = gaugeSectors(
        success = c(80, 100),
        warning = c(50, 80),
        danger = c(0, 50)
      )
    )
  })
  
  output$mypat_npv_s <- renderGauge({
    gauge(
      value = round(post_test_mypat()$post_prob_neg_s, 3) * 100,
      abbreviateDecimals = 2,
      symbol = "%",
      min = 0,
      max = 100,
      sectors = gaugeSectors(
        success = c(80, 100),
        warning = c(50, 80),
        danger = c(0, 50)
      )
    )
  })
  
  output$mypat_ppv_p <- renderGauge({
    gauge(
      value = round(post_test_mypat()$post_prob_pos_p, 3) * 100,
      abbreviateDecimals = 2,
      symbol = "%",
      min = 0,
      max = 100,
      sectors = gaugeSectors(
        success = c(80, 100),
        warning = c(50, 80),
        danger = c(0, 50)
      )
    )
  })
  
  output$mypat_npv_p <- renderGauge({
    gauge(
      value = round(post_test_mypat()$post_prob_neg_p, 3) * 100,
      abbreviateDecimals = 2,
      symbol = "%",
      min = 0,
      max = 100,
      sectors = gaugeSectors(
        success = c(80, 100),
        warning = c(50, 80),
        danger = c(0, 50)
      )
    )
  })
  
  ## CREATE NOMOGRAM
  output$nomogram <- renderPlot({
    pl_nomo <- nomogrammer(
      Prevalence = input$pat_prev / 100,
      Sens = diag()$se,
      Spec = diag()$sp
    ) + scale_color_manual(values = c("#e74c3c", "#00a65a"))
    return(pl_nomo)
  })
  
  ## OVERALL QUALITY OF INCLUDED STUDIES
  quality <- reactive({
    qual_dat <- data() %>%
      mutate(
        Score = round(score / maxscore, 3) * 100,
        Study = paste0(study, " (", year, ")")
      )
    
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
  
  
  ## OVERALL QUALITY OF INCLUDED STUDIES
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
  
  
  ## POST-TEST PROBABILITIES
  
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
  
  ## OUTPUT (TAB:: ALL DATA)
  output$dt_tests = DT::renderDataTable({
    tests
  })
  
  ## DOWNLOAD
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
