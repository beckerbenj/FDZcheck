

#' Run FDZ checks.
#'
#' Run data checks of the FDZ at IQB.
#'
#' @import shiny
#'
#'@export
runFDZcheck <- function(...) {
  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "sandstone"),
    shinyjs::useShinyjs(),
    
    #titlePanel("FDZ at IQB Data Check"),
    div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
        tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 'FDZ at IQB Data Check'),
        p('Perform automated data checks required for publishing data at the FDZ at IQB.')
    ),
    "This is a small application that performs the automated data checks required by the",
    a(href = "https://www.iqb.hu-berlin.de/fdz/", "FDZ at IQB.", target = "_blank"),
    "For further documentation on the performed checks and instructions on proper data cleaning and appropriate software tools, see, for example the documentation at",
    a(href = "https://beckerbenj.github.io/eatFDZ/articles/FDZ_Data_Cleaning.html", "eatFDZ", target = "_blank"),
    
    # --------------------------------------------
    h2("Data Upload"),
    fileInput("upload", "Please upload a single data file in SPSS format (.sav).", accept = ".sav"),
    #dataTableOutput("full_data"),
    
    
    # --------------------------------------------
    shinyjs::hidden(div(id = "encoding_checks",
                        h2("Encoding checks"),
                        "Variable names should not contain any special characters, such as \"\U00DF\" or \"\U00E4\". Violating characters found in: ",
                        verbatimTextOutput("encoding_names"),
                        
                        "Variable and value labels should not contain any special characters, such as \"\U00DF\" or \"\U00E4\". Violating characters found in the following variables: ",
                        verbatimTextOutput("encoding_labels"),
    )),
    
    # --------------------------------------------
    shinyjs::hidden(div(id = "varLabel_checks",
                        h2("Variable label check"),
                        "All variables should contain a variable label to enhance understandability for secondary users. The following variables are missing a variable label:",
                        verbatimTextOutput("missing_varLabels"),
    )),
    
    # --------------------------------------------
    shinyjs::hidden(div(id = "valLabel_checks",
                        h2("Value label checks"),
                        "Categorical variables should have value labels to enhance understandability for secondary users.",
                        "The following value labels are missing:",
                        verbatimTextOutput("missing_labels"),
                        "The following value labels are do not occur in the data:",
                        verbatimTextOutput("empty_labels"),
    )),
    
    # --------------------------------------------
    shinyjs::hidden(div(id = "missings_checks",
                        h2("Missing tag checks"),
                        "Missing values should be tagged as such. Please specify the range used for missing values.",
                        textInput("missing_range_min", "", value = -99),
                        textInput("missing_range_max", "", value = -50),
                        "The following values are labeled and within the missing range, but lack a missing tag:",
                        verbatimTextOutput("missing_range"),
                        "Missing values should be tagged as such. Please specify the wording which describes missing values in the value labels. Multiple wordings can be separted by the \"|\" operator.",
                        textInput("missing_regex", "", value = "missing|not reached|omitted"),
                        "The following values have value labels using the missing value wording, but lack a missing tag:",
                        verbatimTextOutput("missing_regex"),
    )),
    
    # --------------------------------------------
    shinyjs::hidden(div(id = "id_checks",
                        h2("ID check"),
                        "One (or a combination of multiple) identifier variable(s) should be unique per row.",
                        textInput("id", "ID Variable"),
                        "The following rows have missings on the identifier variable:",
                        verbatimTextOutput("miss_id"),
                        "The following entries in the identifier variable are not unique:",
                        verbatimTextOutput("double_id"),
    )),
    
    # --------------------------------------------
    shinyjs::hidden(div(id = "sdc_checks",
                        h2("Statistical Disclosure Control"),
                        "The identity of study participants should not be retrievable via disclosed, personal information. On variables, which are potentially useful for re-identifying study participants, categories should be as frequent to not allow re-identification (frequencies > 5).",
                        textInput("sdc_variables", "Please specify the variables potentially suitable for re-identification, separated by \",\":", value = "age, sex, country"),
                        verbatimTextOutput("sdc")
    )),
    # tbd: Codebook
  )
  
  server <- function(input, output, session) {
    data <- reactive({
      req(input$upload)
      eatGADS::import_spss(input$upload$datapath)
    })
    observeEvent(input$upload, {
      shinyjs::show("encoding_checks")
      shinyjs::show("varLabel_checks")
      shinyjs::show("valLabel_checks")
      shinyjs::show("missings_checks")
      shinyjs::show("id_checks")
      shinyjs::show("sdc_checks")
    })
    
    clean_encoding_data <- reactive({
      eatGADS::fixEncoding(data())
    })
    
    output$full_data <- renderDataTable({
      data()$dat
    })
    
    # encoding checks  
    output$encoding_names <- renderPrint({
      nam <- eatGADS::namesGADS(data())
      fixed_nam <- eatGADS::namesGADS(clean_encoding_data())
      nam[nam != fixed_nam]
    })
    
    output$encoding_labels <- renderPrint({
      all_differences <- eatGADS::equalGADS(data(), clean_encoding_data())
      all_differences$meta_data_differences
    })
    
    # id checks  
    id_vec <- reactive({
      suppressMessages(id_gads <- eatGADS::extractVars(data(), vars = input$id))
      eatGADS::extractData2(id_gads, convertMiss = TRUE)[[1]]
    })
    output$miss_id <- renderPrint({
      fehlendeIDs <- which(is.na(id_vec()))
      fehlendeIDs
    })
    
    output$double_id <- renderPrint({
      #if(!is.null(id_vec())) browser()
      doppelteIDs <- id_vec()[duplicated(id_vec())]
      doppelteIDs
    })
    
    # missing variable labels
    output$missing_varLabels <- renderPrint({
      varLabels <- unique(eatGADS::extractMeta(data())[, c("varName", "varLabel")])
      varLabels[is.na(varLabels$varLabel), "varName"]
    })
    
    # missing value labels
    output$missing_labels <- renderPrint({
      out <- eatGADS::checkMissingValLabels(data())
      out2 <- out[!sapply(out, is.null)]
      # only display the missing labels per variable
      lapply(out2, function(x) x$missing_labels)
    })
    # empty value labels
    output$empty_labels <- renderPrint({
      out <- eatGADS::checkEmptyValLabels(data())
      out[!sapply(out, is.null)]
    })
    
    # missing tags
    output$missing_range <- renderPrint({
      missing_range <- input$missing_range_min:input$missing_range_min
      suppressMessages(checked_dat <- eatGADS::checkMissingsByValues(data(), missingValues = missing_range))
      changed_vars <- eatGADS::equalGADS(data(), checked_dat)$meta_data_differences
      changed_meta <- eatGADS::extractMeta(data(), changed_vars)
      changed_meta[changed_meta$value %in% missing_range, ]
    })
    
    output$missing_regex <- renderPrint({
      suppressMessages(checked_dat <- eatGADS::checkMissings(data(), missingLabel = input$missing_regex))
      changed_vars <- eatGADS::equalGADS(data(), checked_dat)$meta_data_differences
      changed_meta <- eatGADS::extractMeta(data(), changed_vars)
      changed_meta[grepl(input$missing_regex, changed_meta$valLabel), ]
    })
    
    # disclosure control
    output$sdc <- renderPrint({
      sdc_vars <- strsplit(input$sdc_variables, ", ")[[1]]
      exclude_vars <- setdiff(eatGADS::namesGADS(data()), sdc_vars)
      out <- eatFDZ::sdc_check(input$upload$datapath, exclude = exclude_vars)
      out[out$exclude == FALSE, c("variable", "nKatOhneMissings", "nValid", "nKl5")]
    })
    
    
  }
  
  shinyApp(ui, server)
}
