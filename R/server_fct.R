server_fct <- function(input, output, session){
  ## build fixes : start ##
  Target.ID <- NULL
  Genotype <- NULL
  locus <- NULL
  accept <- NULL
  p_value <- NULL
  selected_ <- NULL
  meta <- NULL
  pop <- NULL
  . <- NULL
  lat <- NULL
  lon <- NULL
  aims_example <- NULL
  ## build fixes : end ##
  if(!exists("db_list")) db_list <- get("db_list", envir = -2)
  if(!exists("reporting_panel")) reporting_panel <- get("reporting_panel", envir = -2)
  
  observeEvent(input$analyse, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("analyse", result())
  })
  
  ## USER INTERFACE
  output$analysis <- renderUI({
    res <- result()
    if(is.null(res)){
      fluidPage(
        h3("Upload valid AIMs profile"),
        HTML("<p>Upload valid file first: It must contain a column named <tt>Target.ID</tt> 
           containing marker (locus) names, and <tt>Genotype</tt> with the genotypes as e.g. 
           <tt>AA</tt> or <tt>AC</tt></p>"),
        HTML("You can download a sample file showing the necessary columns and data structure here:"),
        downloadLink("download_sample", "Download")
      )
    }
    else{
      fluidPage(
        fluidRow(HTML(paste0("<p><b>Analysis of file:</b> ",input$profile_file$name,":")),
                 actionLink("show_profile", label = "Show profile"),
                 icon("new-window", lib = "glyphicon"),
                 HTML("</p>")
                 ),
        fluidRow(tags$h2("Graphics")),
        fluidRow(
          column(width = 6, uiOutput("barplot_panel") ),
          column(width = 6, uiOutput("map_panel") )
        ),
        fluidRow(tags$h2("Tables")),
        fluidRow(div(style = "opacity = 0.5;", DTOutput("result_table"))),
        fluidRow(tags$h2("Likelihood ratios")),
        fluidRow(DTOutput("lr_list"))
      )
    }
  })
  
  ## 
  
  output$download_sample <- downloadHandler(
    filename <- function(){
      paste("aims_example", "csv", sep = ".")
    },
    contentType = "text/csv",
    content = function(file) {
      aims_example <- read_csv(system.file("deployable_app", "aims_example.csv", package = "genogeographer"), col_types = "cc")
      write_csv(aims_example, path = file)
    }
  )
  
  ## REACTIVES
  
  observeEvent(input$show_profile, {
    A1 <- NULL
    A2 <- NULL
    showModal(modalDialog(
      title = paste("Uploaded profile:",input$profile_file$name),
      renderDT(Profile() %>% mutate(genotype = paste0(A1, A2)) %>% 
                 select_("locus", "genotype") %>% 
               DT::datatable(rownames=FALSE, filter = "bottom", selection = 'none',
               extensions = 'Buttons', options = list(
                 dom = 'Blfrtip',
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
               ))),
      footer = modalButton("Close"),
      easyClose = TRUE
      ))
    })
  
  ### ANALYSIS
  
  observeEvent(input$reset,{
    runjs("history.go(0)")
  })
  
  output$uploaded_profile <- renderTable({
    if (is.null(input$profile_file)) return(NULL)
    Profile()
  })
  
  db <- reactiveValues(db = NULL)
  barplot_selected <- reactiveValues(which = NULL)
  bar_ranges <- reactiveValues(x = NULL, y = NULL)
  map_ranges <- reactiveValues(x = NULL, y = NULL)
  LR_lists <- reactiveValues(pop = NULL, meta = NULL)
  
  observe({
    db$db <- db_list[[ifelse(is.null(input$snp_set), 1, input$snp_set)]]
    barplot_selected$which <- NULL
  })
  
  output$dbs <- renderUI({
    selectInput(inputId = "snp_set", label = "Select frequency database:", choices = names(db_list))
  })
  
  
  output$fileUploaded <- reactive({
    return(nrow(Dataset())>0)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  Dataset <- reactive({
    if(is.null(input$profile_file)) { # User has not uploaded a file yet
      return(tibble())
    }
    ## data <- read.csv(input$profile_file$datapath, header = TRUE, stringsAsFactors = FALSE) %>% as_tibble()
    data <- rio::import(input$profile_file$datapath) %>% as_tibble()
    if(!all(c("Target.ID", "Genotype") %in% names(data))) return(tibble())
    ## Arguments specific for RGA files
    data %>% select(locus = Target.ID, Genotype) %>% 
      extract(col = Genotype, into = c("A1","A2"), regex = "(.{1})(.{1})", remove = TRUE) %>% 
      arrange(locus)
  })
  
  Profile <- reactive({
    profile <- Dataset()
    if(is.null(profile)) return(NULL) ## No profile
    if(!is.null(input$profile_file) & nrow(profile)==0){
      showModal(
        modalDialog(title = "Error in reading file",
                    fluidPage(
                      helpText("File not correct format"),
                      helpText("See the example file for correct columns")
                      )
        ))
      shinyjs::reset("profile_file")
      return(NULL) ## No profile
    }
    if(nrow(profile)==0) return(NULL) ## No profile
    updateSelectizeInput(session, "lr_meta", selected = NULL)
    updateSelectizeInput(session, "lr_pop", selected = NULL)
    ## shinyjs::reset("lr_meta")
    ## shinyjs::reset("lr_pop")
    profile
  })
  
  result <- eventReactive(list(input$profile_file,input$analyse),{
    bar_ranges$x <- NULL
    bar_ranges$y <- NULL
    profile <- Profile()
    if(is.null(profile)) return(NULL)
    profile <- profile %>% as_tibble() %>% profile_AA_x0(df = db$db)
    tilt_ <- if(is.null(input$tilt)) FALSE else (input$tilt == "adjust")
    result_pop <- genogeo(profile = profile, df = db$db, CI = input$CI, min_n = input$min_n, tilt = tilt_)
    ### Fixed clusters based on STRUCTURE analysis
    result_meta <- genogeo(profile = profile, df = db$db, CI = input$CI, min_n = input$min_n, grouping = "meta", tilt = tilt_)
    ##
    list(pop = result_pop, meta = result_meta)
  })
  
  output$side_pvalue <- renderUI({
    res <- result()
    if(is.null(res)) return(NULL)
    largest_p_value <- res[[input$meta]] %>% filter(accept)
    if(nrow(largest_p_value)==0) largest_p_value_text <- paste0("All ",ifelse(input$meta == "meta", "meta-", ""),"populations rejected")
    else{
      largest_p_value <- largest_p_value %>% top_n(n = 1, wt = p_value) %>% select(p_value, 2) %>% mutate(p_value = round(p_value, 3))
      largest_p_value_ <- paste0(largest_p_value$p_value, " (",largest_p_value[[2]],")")
      largest_p_value_text <- paste0("<b>DB-score (largest p-value):</b><br/>", largest_p_value_)
    }
    verticalLayout(
      h4("Reporting"),
      HTML(paste0("<p>",largest_p_value_text,"</p>"))
    )
  })
  
  ### MAP
  
  output$map_panel <- renderUI({
    div(style = paste0("position:relative; ",
                       "height: ",0.8*session$clientData$output_barplot_width,"px;"),
        withSpinner(plotOutput("map",
                               dblclick = "map_dblclick",
                               brush = brushOpts(id = "map_brush", 
                                                 delay = 800, 
                                                 resetOnNew = TRUE, 
                                                 direction = "xy"),
                               hover = hoverOpts(id = "map_hover", delay = 100, delayType = "debounce"),
                               click = clickOpts(id = "map_click"),
                               width = "100%"), type = 4),
        uiOutput("hover_map"))
  })
  
  observeEvent(input$map_click, {
    res <- result()
    if(is.null(res)) return(NULL)
    clicked_pop <- nearPoints(res[[input$meta]], input$map_click, threshold = 10, maxpoints = 1)[[1]]
    if(length(clicked_pop)==0) return(NULL)
    ## Already selected
    lr_pop <- input[[paste0("lr_",input$meta)]]
    if(is.null(lr_pop)) return(NULL)
    ## Remove
    if(clicked_pop %in% lr_pop) lr_pop <- lr_pop[lr_pop!=clicked_pop]
    ## Add
    else lr_pop <- c(lr_pop,clicked_pop)
    updateSelectizeInput(session, paste0("lr_",input$meta), selected = lr_pop)
  })
  
  ### BARPLOT
  
  output$barplot_panel <- renderUI({
    div(style = paste0("position:relative; ",
                       "height: ",0.8*session$clientData$output_barplot_width,"px;"),
        plotOutput("barplot", 
                   dblclick = "barplot_dblclick",
                   brush = brushOpts(id = "barplot_brush", 
                                     delay = 800, 
                                     resetOnNew = TRUE, 
                                     direction = "xy"), 
                   hover = hoverOpts(id = "barplot_hover", delay = 100, delayType = "debounce"),
                   width = "100%"), 
        uiOutput("hover_barplot"))
  })
  
  observeEvent(input$barplot_brush, {
    res <- result()
    if(is.null(res)) return(NULL)
    barplot_selected$which <- brushedPoints(df = res[[input$meta]], brush = input$barplot_brush, allRows = TRUE) %>% 
      select(1, selected_)
  })
  
  ## TABLES
  
  output$result_table <- renderDT({
    res <- result()[[input$meta]]
    if(is.null(barplot_selected$which)) return(result_table(res))
    result_table(res %>% inner_join(barplot_selected$which, by = names(res)[1]), .filter = "selected_")
  })
  
  ## LR calculations and controls
  
  output$LR_select <- renderUI({
    res <- result()
    if(is.null(res)) return(NULL)
    meta_pvalue <- res$meta %>% filter(accept) %>% top_n(n = 1, wt = p_value) %>% pull(meta)
    pop_pvalue <- res$pop %>% filter(accept) %>% top_n(n = 1, wt = p_value) %>% pull(pop)
    verticalLayout(
      checkboxGroupInput(inputId = "LR_accept", 
                         label = "LRs with two rejected populations:", 
                         choiceNames = list("Allow (LRs may be misleading)"), choiceValues = list("allow")),
      conditionalPanel("input.meta == 'meta'",
                       selectizeInput(inputId = "lr_meta", 
                                      label = "Metapopulations for LR", 
                                      choices = paste(res$meta$meta), 
                                      selected = unique(c(meta_pvalue,input$lr_meta)),
                                      multiple = TRUE,
                                      options = list(plugins = list("remove_button", "drag_drop")), 
                                      width = "100%")
      ),
      conditionalPanel("input.meta == 'pop'",
                       selectizeInput(inputId = "lr_pop", 
                                      label = "Populations for LR", 
                                      choices = paste(res$pop$pop),
                                      selected = unique(c(pop_pvalue, input$lr_pop)),
                                      multiple = TRUE,
                                      options = list(plugins = list("remove_button", "drag_drop")), 
                                      width = "100%")
      )
    )
  })
  
  output$lr_list <- renderDT({
    accepted_ <- if(is.null(input$LR_accept)) FALSE else (input$LR_accept == "allow")
    LR_list(result = result()[[input$meta]], lr_pops = input[[paste0("lr_",input$meta)]], CI = input$CI, accepted = accepted_)
  })
  
  observeEvent(input$lr_pop,{ ## Can't delete most probable (based on z_score)
    res <- result()
    if(is.null(res)) return(NULL)
    max_score_pop <- res$pop %>% filter(accept) %>% top_n(n = 1, wt = p_value) %>% pull(var = 1) ## Highest p-value
    lr_pop <- input$lr_pop
    lr_pop <- unique(c(max_score_pop, lr_pop))
    updateSelectizeInput(session, "lr_pop", selected = lr_pop)
  })
  
  observeEvent(input$lr_meta,{ ## Can't delete most probable (based on z_score)
    res <- result()
    if(is.null(res)) return(NULL)
    max_score_meta <- res$meta %>% filter(accept) %>% top_n(n = 1, wt = p_value) %>% pull(var = 1) ## Highest p-value
    lr_meta <- input$lr_meta
    lr_meta <- unique(c(max_score_meta, lr_meta))
    updateSelectizeInput(session, "lr_meta", selected = lr_meta)
  })
  
  observeEvent(input$result_table_rows_selected,{ ## Can't delete most probable (based on z_score)
    res <- result()
    if(is.null(res)) return(NULL)
    if(input$meta == "meta"){
      lr_meta <- input$lr_meta
      lr_meta <- unique(c(res[[input$meta]][input$result_table_rows_selected,1], lr_meta))
      updateSelectizeInput(session, "lr_meta", selected = lr_meta)
    }
    else if(input$meta == "pop"){
      lr_pop <- input$lr_pop
      lr_pop <- unique(c(res[[input$meta]][input$result_table_rows_selected,1], lr_pop))
      updateSelectizeInput(session, "lr_pop", selected = lr_pop)
    }
    else return(NULL)
  })
  
  ##
  
  ## PLOTS
  
  plot_bars <- reactive({
    res <- result()
    if(is.null(res)) return(NULL)
    return(error_bar_plot(data = res[[input$meta]]) + coord_cartesian(xlim = bar_ranges$x, ylim = bar_ranges$y))
  })
  
  observeEvent(input$barplot_dblclick, {
    brush <- input$barplot_brush
    if (!is.null(brush)) {
      bar_ranges$x <- c(brush$xmin, brush$xmax)
      bar_ranges$y <- c(brush$ymin, brush$ymax)
      barplot_selected$which <- NULL
    } else {
      bar_ranges$x <- NULL
      bar_ranges$y <- NULL
      barplot_selected$which <- NULL
    }
  })
  
  output$barplot <- renderPlot({ 
    plot_bars() + theme_bw(base_size = 16)
  },height = function() {
    0.8*session$clientData$output_barplot_width
  })
  
  plot_map <- reactive({
    res <- result()
    if(is.null(res)) return(NULL)
    ## SELECTED POINTS IN BAR-PLOT
    res_ <- res[[input$meta]]  
    if(is.null(barplot_selected$which)) res_ <- res_ %>% mutate(selected_ = FALSE)
    else res_ <- inner_join(res_, barplot_selected$which, by = names(res_)[1])
    ## LR populations
    if(is.null(input[[paste0("lr_", input$meta)]])) res_ <- res_ %>% mutate(LR_listing = "No")
    else res_ <- res_ %>% mutate(LR_listing = ifelse(.[[1]] %in% input[[paste0("lr_",input$meta)]], "Yes", "No"))
    ## 
    res_ <- subset(res_, !(is.na(lat) | is.na(lon))) ## Discard pops with no location
    map_plot(res_)
  })
  
  observeEvent(input$map_dblclick, {
    brush <- input$map_brush
    if (!is.null(brush)) {
      map_ranges$x <- c(brush$xmin, brush$xmax)
      map_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      map_ranges$x <- NULL
      map_ranges$y <- NULL
    }
  })
  
  output$map <- renderPlot({
    plot_map() + 
      coord_cartesian(xlim=map_ranges$x, ylim=map_ranges$y, expand = FALSE) + 
      theme_bw(base_size = 16)
  },height = function() {
    0.8*session$clientData$output_map_width
  })
  
  ## ADDITIONAL PLOTS
  
  output$hover_map <- renderUI({
    res <- result()
    if(is.null(res)) return(NULL)
    hover <- input$map_hover
    if(is.null(hover)) return(NULL)
    point <- nearPoints(res[[input$meta]], hover, threshold = 5, maxpoints = 1)
    tool_tip(hover = hover, point = point)
  })
  
  output$hover_barplot <- renderUI({
    res <- result()
    if(is.null(res)) return(NULL)
    hover <- input$barplot_hover
    if(is.null(hover)) return(NULL)
    point <- nearPoints(res[[input$meta]], hover, threshold = 5, maxpoints = 1)
    tool_tip(hover = hover, point = point)
  })
  
  ### RETURN pdf REPORT
  # 
  output$report_panel <- renderUI({
    res <- result()
    if(!reporting_panel) return(verticalLayout())
    if(is.null(res)) return(verticalLayout())
    if(!requireNamespace("tidyverse")) return(verticalLayout(helpText("Needs `tidyverse` for generating report!")))
    verticalLayout(
      h4("Report"),
      textInput(inputId = "name", label = "Name of analyst", width = "100%", 
                placeholder = "Name as to appear in report", value = input$name),
      radioButtons(inputId = 'format', label = 'Report format', choices = c('PDF', 'HTML', 'Word'),
                   inline = TRUE, selected = input$format),
      withBusyIndicatorUI(downloadButton(outputId = "report_download", label = paste0("Download (",input$format,")"), class = "btn-primary")),
      hr()
    )
  })
  # 

  output$report_download = downloadHandler(
    filename = function(){
      paste(sub("\\.[[:alnum:]]*.$","",input$profile_file$name), sep =".",
            switch(
              input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
    },
    content = function(file) {
      withProgress(message = "Generating report..", {
      ## out <- make_report()
      src <- list(rmd_file = normalizePath(system.file("deployable_app", "aims_report.Rmd", package = "genogeographer")))
      ## logo=normalizePath('kulogo_small.png'))
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src$rmd_file, 'aims_report_.Rmd', overwrite = TRUE)
      out <- rmarkdown::render(input = 'aims_report_.Rmd', output_format = switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ),
        params = list(
          set_file = input$profile_file$name,
          set_author = input$name,
          set_output = input$format
          )
        )
      })
      file.rename(out, file)
    }
    )
  # pdf REPORT
}