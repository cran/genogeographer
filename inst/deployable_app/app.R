library(genogeographer)

#' The object `db_list` should be initialised prior to lauching
#' For the settings at Department of Forensic Medicine, Section of Forensic Genetics, 
#' University of Copenhagen, Copenhagen, Denmark we collected population data that is 
#' stored in a file "RGA_db.RData". 
#' It contains three databases `KK164`, `KK164_seldin` and `KK164_kidd` generated using `pops_to_DB()`
#' on a spreadsheet data containing the necessary columns and information. 
#' Below, we use these data to construct the required object (Note the naming can be arbritary):

load("RGA_db.RData")
db_list <- list("All 164 AIMs" = KK164, "Seldin (123 AIMs)" = KK164_seldin, "Kidd (55 SNPs)" = KK164_kidd)
reporting_panel <- FALSE

shinyApp(
  ui = genogeographer:::ui_fct(),
  server = function(input, output, session){
    genogeographer:::server_fct(input = input, output = output, session = session)
    }
)

