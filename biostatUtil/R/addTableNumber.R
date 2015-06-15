#' Add table number to caption
addTableNumber <- function(caption, default.table_counter_str = "Table %s: ") {
  # generate table number ...
  if (!is.na(caption)) {
    tc <- getOption("table_counter")
    if (is.numeric(tc)) {
      tc <- tc + 1		
    } else {
      tc <- 1
    }
    options(table_counter=tc)
    caption_template <- getOption("table_counter_str", default.table_counter_str)
    caption <- paste(sprintf(caption_template,as.character(tc)), caption, sep="")
  } 
  return(caption)
}