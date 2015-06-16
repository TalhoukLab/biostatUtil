#' Change table to HTML format
#' @export
tableAsHTML <- function(
  t, 
  row.names=NULL,
  column.names=NULL,
  caption=NA,
  html.table.border=0,
  banded.rows=FALSE,
  css.class.name.odd="odd",
  css.class.name.even="even"
) {
  th.style <- "border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;"
  result <- paste("<table border=",html.table.border,">",ifelse(is.na(caption),"",paste("<caption style='",TABLE.CAPTION.STYLE,"'>",add.table.number(caption),"</caption>",sep="")),sep="")
  # print header
  if (!is.null(row.names)) {
    rownames(t) <- row.names
  } else {
    row.names <- rownames(t)
  }
  if (!is.null(column.names)) {
    colnames(t) <- column.names
  } else {
    column.names <- colnames(t)
  }
  result <- paste(result,"<tr><th style='",th.style,"'></th><th style='",th.style,"'>",paste(column.names,collapse=paste("</th><th style='",th.style,"'>",sep="")),"</th></tr>",sep="")
  for (i in 1:nrow(t)) {
    tr.class <- ifelse(banded.rows,paste(" class='",ifelse(i%%2==0,css.class.name.even,css.class.name.odd),"'",sep=""),"")
    result <- paste(result,"<tr",tr.class,"><th>",row.names[i],"</th><td>",paste(t[i,],collapse="</td><td>"),"</td></tr>",sep="")
  }
  result <- paste(result,"</table>",sep="")
  return(result)
}