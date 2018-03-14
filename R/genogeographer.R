#'
#' genogeographer: Methods for analysing forensic Ancestry Informative Markers
#'
#' The genogeographer package provides:
#' genogeo()
#' 
#' @section genogeo functions:
#' See ?genogeo
#'
#' @docType package
#' @name genogeographer
#' @importFrom dplyr starts_with distinct distinct_ filter filter_ select select_ mutate mutate_ mutate_if mutate_at rename
#' @importFrom dplyr top_n desc pull funs rowwise ungroup arrange arrange_ case_when bind_rows sample_n bind_cols
#' @importFrom dplyr vars group_by group_by_ summarise n everything between group_vars
#' @importFrom dplyr full_join inner_join right_join anti_join semi_join left_join
#' @importFrom forcats fct_reorder
#' @importFrom tidyr unnest nest crossing extract
#' @importFrom magrittr "%>%"
#' @importFrom readr write_csv read_csv
#' @importFrom leaflet colorNumeric
#' @importFrom knitr kable
#' @importFrom rio import
#' @importFrom rmarkdown render pdf_document html_document word_document
#' @importFrom stats optimize pnorm qnorm rbeta rbinom rpois sd setNames weighted.mean median
#' @importFrom utils data packageDescription packageVersion read.csv
#' @importFrom DT formatStyle styleEqual datatable DTOutput renderDT
#' @importFrom tibble tibble as_tibble
#' @importFrom shiny shinyApp tags wellPanel HTML sidebarLayout sidebarPanel fluidPage observeEvent renderUI h3 mainPanel span
#' @importFrom shiny uiOutput column div downloadHandler renderTable observe selectInput reactive outputOptions verticalLayout modalButton
#' @importFrom shiny downloadLink reactiveValues eventReactive checkboxGroupInput conditionalPanel selectizeInput renderPlot modalDialog
#' @importFrom shiny fluidRow plotOutput brushOpts clickOpts hoverOpts nearPoints updateSelectizeInput brushedPoints textInput showModal
#' @importFrom shiny radioButtons downloadButton hr withProgress h4 titlePanel fileInput sliderInput actionButton icon helpText actionLink
#' @importFrom shinyjs runjs useShinyjs hidden
#' @importFrom shinycssloaders withSpinner
#' @importFrom ggplot2 ggplot aes labs geom_point guides geom_polygon scale_colour_manual scale_shape_manual scale_fill_manual
#' @importFrom ggplot2 geom_errorbarh scale_x_reverse map_data coord_cartesian theme_bw
#' @importFrom grDevices col2rgb rgb
NULL 