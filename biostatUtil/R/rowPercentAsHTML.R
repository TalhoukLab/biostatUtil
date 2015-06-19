#' Row percentages in HTML
#' @export
rowPercentAsHTML <- function(
  t, 
  show.count=FALSE, # whether to show the count
  digits=4,
  column.names=NULL,
  html.table.border=0,
  caption=NA,
  transpose=FALSE, # i.e. write this single row as a single column, useful if there are many columns in a single row
  banded.rows=FALSE,
  css.class.name.odd="odd",
  css.class.name.even="even",
  ...
) {
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  table.values <- rowPercent(t, pretty.text = TRUE, keep = TRUE, digits = digits)
  # header
  if (!is.null(column.names)) {
    colnames(table.values) <- column.names
  } else {
    column.names <- names(t)
  }
  result <- paste("<table border=",html.table.border,">",ifelse(is.na(caption),"",paste("<caption style='",TABLE.CAPTION.STYLE,"'>",add.table.number(caption),"</caption>",sep="")),sep="")
  if (transpose) {
    tr.classes <- c() # tr classes for banded rows
    if (banded.rows) {
      for (i in 1:length(t)) {
        tr.classes <- c(tr.classes,paste("class='",ifelse(i%%2==0,css.class.name.even,css.class.name.odd),"'",sep=""))
      }
    } else {
      tr.classes <- ""
    }
    result <- paste(result,"<tr><th style='",col.th.style,"'></th><th style='",col.th.style,"'>",sep="")
    if (show.count) {
      result <- paste(result,"count (%)</th></tr>",sep="")
      result <- paste(result,paste(paste("<tr ",tr.classes,"><th style='",row.th.style,"'>",column.names,"</th><td>",paste(t," (",table.values[2,],")",sep=""),"</td></tr>",sep=""),collapse=""),sep="")
    } else {
      result <- paste(result,"count</th></tr>",sep="")
      result <- paste(result,paste(paste("<tr ",tr.classes,"><th style='",row.th.style,"'>",column.names,"</th><td>",table.values[2,],"</td></tr>",sep=""),collapse=""),sep="")
    }
  } else {
    result <- paste(result,"<tr><th style='",col.th.style,"'>",paste(column.names,collapse=paste("</th><th style='",col.th.style,"'>",sep="")),"</th></tr>",sep="")
    
    # print values
    if (show.count) {
      result <- paste(result,"<tr><td>",paste(paste(t," (",table.values[2,],")",sep=""),collapse="</td><td>"),"</td></tr>",sep="")
    } else {
      result <- paste(result,"<tr><td>",paste(table.values[2,],collapse="</td><td>"),"</td></tr>",sep="")
    }
  }
  result <- paste(result,"</table>",sep="")
  return(result)
}