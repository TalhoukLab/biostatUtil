#' Plot detailed Kaplan-Meier curve
#' @export
plotKMDetail <- function(input.data,
                           surv.formula,
                           main.text, 
                           xlab.text, 
                           ylab.text,
                           line.name,
                           ten.years.surv.95CI,
                           event.count,
                           line.color,
                           obs.survyrs,
                           line.pattern = NULL,
                           line.width = NULL,
                           legend.pos = "bottomleft", # legend position keyword
                           file.name = "no.file",
                           file.width = 7,
                           file.height = 7,
                           show.test = "single", # show single or the reference group value (for pairwise comparison)
                           single.test.type = "logrank", # the test to show if specified show.test="single". 
                           # the possible choices are logrank, wilcoxon, taroneware, all
                           round.digits.p.value, # number of digits for p-value
                           grey.scale = FALSE,
                           show.single.test.pos, ...) {
  
  var.name <- deparse(surv.formula[[3]]) # this should be the biomarker name
  # the deparse() function is used to make sure var.name is a string
  
  #print(var.name)
  log.rank.p.values    <- c() # p-values to be returned
  wilcox.p.values      <- c() # p-values to be returned 		
  tarone.ware.p.values <- c() # p-values to be returned
  
  fit <- survival::survfit(surv.formula, data = input.data)
  if (file.name != "no.file") {
    # do not generate a file if "no.file" is specified
    file.name.len <- nchar(file.name)
    if (file.name.len > 4) {
      file.name.extension <- tolower(substr(file.name,
                                            file.name.len - 2, file.name.len))
      if (file.name.extension == "pdf") {
        #pdf(file=file.name)
        cairo_pdf(file = file.name, width = file.width, height = file.height) # good for unicode character in e.g. line.name
      } else if (file.name.extension %in% c("wmf", "emf", "wmz", "emz")) {
        win.metafile(file = file.name, width = file.width, height = file.height)	
      } else if (file.name.extension %in% c("tif")) { # does not with with tiff since only check last three character!!!
        tiff(file = file.name, width = file.width * 100, height = file.height * 100)
      } else {
        # unknown extension ... do nothing
        file.name <- "no.file"
      }
    }
  } 
  which.strata.have.cases <- table(input.data[, var.name]) > 0 # in case some strata do not have any cases
  if (grey.scale) {
    # gray scale plot
    if (is.null(line.pattern)) {
      line.pattern <- c(1:length(line.name))[which.strata.have.cases]
    }
    if (is.null(line.width)) {
      line.width <- 1
    }
    plot(fit,
         lty = line.pattern,  
         lwd = line.width,
         main = main.text,
         xlab = xlab.text,
         ylab = ylab.text, ...
    )
    if (legend.pos == "top") {
      l1 <- legend(x = (max(fit$time, na.rm = TRUE) -
                          min(fit$time, na.rm = TRUE)) / 2, y = 0.99, # i.e. top 1% ... since survival plot always start at 100% survival
                   legend = line.name,
                   lty = line.pattern,
                   lwd = line.width,
                   box.lty = 0,
                   cex = 0.8)
    } else {
      l1 <- legend(legend.pos,
                   legend = line.name,
                   lty = line.pattern,
                   lwd = line.width,
                   box.lty = 0,
                   cex = 0.8)
    }
  } else {
    # color plot
    if (is.null(line.pattern)) {
      line.pattern <- 1
    }
    if (is.null(line.width)) {
      line.width <- 1
    }
    plot(fit,
         col = line.color[which.strata.have.cases],
         lty = line.pattern,
         lwd = line.width,
         main = main.text,
         xlab = xlab.text,
         ylab = ylab.text, ...
    )
    if (legend.pos == "top") {
      l1 <- legend(x = (max(fit$time, na.rm = TRUE) -
                          min(fit$time,na.rm = TRUE)) / 2, y = 0.99, # i.e. top 1% ... since survival plot always start at 100% survival
                   legend = line.name,
                   lty = line.pattern,
                   lwd = line.width,
                   col = line.color,
                   box.lty = 0,
                   cex = 0.8)   
    } else {
      l1 <- legend(legend.pos,
                   legend = line.name,
                   lty = line.pattern,
                   lwd = line.width,
                   col = line.color,
                   box.lty = 0,
                   cex = 0.8)
    }
  }
  # there seems to be need for the y-axis adjustment depending on the file.height ...
  dy <- 0.02 * (file.height - 7) / (12 - 7) # determined empirically
  y.pos <- l1$rect$h - dy
  if (legend.pos == "top") {
    y.pos <- l1$rect$top + dy	   
  }
  l2 <- legend(
    x = l1$rect$w + l1$rect$left,
    y = y.pos,
    legend = ten.years.surv.95CI,
    title = paste0(obs.survyrs, "yr 95% CI"),
    title.col = 1,
    box.lty = 0,
    cex = 0.8)
  
  l3 <- legend(
    x = l1$rect$w + l1$rect$left + l2$rect$w,
    y = y.pos,#l1$rect$h-dy,
    legend = event.count,
    title = "Events/N",
    title.col = 1,
    box.lty = 0,
    cex = 0.8)
  box()
  if (show.test == "single") {
    log.rank.test     <- survival::survdiff(surv.formula, data = input.data,
                                            rho = 0)
    gehan.wilcox.test <- survival::survdiff(surv.formula, data = input.data,
                                            rho = 1)
    tarone.ware.test  <- survival::survdiff(surv.formula, data = input.data,
                                            rho = 0.5) # http://courses.nus.edu.sg/course/stacar/internet/st3242/handouts/notes2.pdf
    p.value <- getPval(log.rank.test)
    log.rank.p.values <- p.value
    p.value <- round(p.value, digits = round.digits.p.value)
    gehan.wilcox.p.value <- getPval(gehan.wilcox.test)
    wilcox.p.values <- gehan.wilcox.p.value
    gehan.wilcox.p.value <- round(gehan.wilcox.p.value, digits = round.digits.p.value)
    tarone.ware.p.value <- getPval(tarone.ware.test)
    tarone.ware.p.values <- tarone.ware.p.value
    tarone.ware.p.value <- round(tarone.ware.p.value, digits = round.digits.p.value)
    text(
      x = l1$rect$w + l1$rect$left + l2$rect$w + 1.3*l3$rect$w,
      y = show.single.test.pos, # position of the test statistics on plot
      paste0(
        ifelse(sum(single.test.type %in% c("logrank",   "all")) >= 1,
               paste0("Log-Rank p=",  
                      sprintf(paste0("%.",round.digits.p.value,"f"),
                              p.value             ), "\n"), ""),
        ifelse(sum(single.test.type %in% c("wilcoxon",  "all")) >= 1,
               paste0("Wilcoxon p=",
                      sprintf(paste0("%.", round.digits.p.value, "f"),
                              gehan.wilcox.p.value), "\n"), ""),
        ifelse(sum(single.test.type %in% c("taroneware","all")) >= 1,
               paste0("Tarone-Ware p=",
                      sprintf(paste0("%.", round.digits.p.value, "f"),
                              tarone.ware.p.value ), "\n"), "")),
      adj = c(0, 0),
      cex = 0.8)
  } else if (show.test != "none") {
    # assume show.test shows the reference group index
    legend.txt <- c()
    value.names <- names(table(input.data[, var.name]))
    for (value.name in value.names) {
      if (value.name == show.test) {
        # this is the reference group
        legend.txt <- c(legend.txt, "reference group")
      } else {
        # construct data
        temp.d <- input.data[input.data[, var.name] == show.test |
                               input.data[, var.name] == value.name, ]
        if(sum(input.data[, var.name] == value.name, na.rm = TRUE) == 0) {
          # no case in this group
          p.value <- NA 
          w.p.value <- NA
          t.p.value <- NA
        } else {
          # calculate log rank p-values
          p.value   <- getPval(survival::survdiff(surv.formula, data = temp.d, rho = 0 ))
          log.rank.p.values    <- c(log.rank.p.values,    p.value)
          p.value   <- round(p.value,  digits = round.digits.p.value)
          
          w.p.value <- getPval(survival::survdiff(surv.formula, data = temp.d, rho = 1 ))
          wilcox.p.values      <- c(wilcox.p.values,      w.p.value)
          w.p.value <- round(w.p.value, digits = round.digits.p.value)
          
          t.p.value <- getPval(survival::survdiff(surv.formula, data = temp.d, rho = 0.5))
          tarone.ware.p.values <- c(tarone.ware.p.values, t.p.value)
          t.p.value <- round(t.p.value, digits = round.digits.p.value)
        }
        new.txt <- paste0(
          ifelse("logrank"    %in% single.test.type, paste0(p.value,  " / "), ""),
          ifelse("wilcoxon"   %in% single.test.type, paste0(w.p.value," / "), ""),
          ifelse("taroneware" %in% single.test.type,       t.p.value,     ""))
        if (endsWith(new.txt, " / ")) {
          new.txt <- substr(new.txt, 0, nchar(new.txt) - 3)
        }
        legend.txt <- c(legend.txt, new.txt)
      }
    }
    
    legend.title <- paste0(
      ifelse("logrank"    %in% single.test.type, "Log-Rank / ",  ""),
      ifelse("wilcoxon"   %in% single.test.type, "Wilcoxon / ",  ""),
      ifelse("taroneware" %in% single.test.type, "Tarone-Ware ", ""))
    if (endsWith(legend.title, " / ")) {
      legend.title <- substr(legend.title, 0, nchar(legend.title) - 2)
      }
    legend.title <- paste0(legend.title, "P-values")
    
    l4 <- legend(x = l1$rect$w + l2$rect$w + l3$rect$w, y = y.pos,#y=l1$rect$h,
                 legend = legend.txt,
                 #text.col=line.color,
                 title = legend.title,
                 title.col = 1,
                 box.lty = 0,
                 cex = 0.8)
  }
  if (file.name != "no.file") {
    # do not generate a file if "no.file" is specified
    dev.off()
  }
  return(list(
    "log.rank.p.values" = log.rank.p.values,
    "wilcox.p.values" = wilcox.p.values
  ))	
}