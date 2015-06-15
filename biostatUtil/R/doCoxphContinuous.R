#' coxph model continuous
#' @export
doCoxphContinuous <- function(
  input.d, 
  var.names, 
  var.descriptions,
  var.names.surv.time   = c("os.yrs",  "dss.yrs",  "rfs.yrs"  ), # variable names of survival time
  var.names.surv.status = c("os.sts",  "dss.sts",  "rfs.sts"  ), # variable names of survival status
  event.codes.surv      = c("os.event","dss.event","rfs.event"), # event coding of survival status variable
  surv.descriptions     = c("OS",      "DSS",      "RFS"      ), # description of survival endpoint
  use.firth=1, # the percentage of censored cases before using the Firth method for Cox regression, 1 means NEVER use
  stat.test="waldtest", # can be "logtest", "waldtest", "sctest" (log rank)
  round.digits.p.value=4, # number of digits for p-value
  caption=NA, # caption for table
  html.table.border=0,
  banded.rows=FALSE,
  css.class.name.odd="odd",
  css.class.name.even="even") {
  return(do.coxph.generic(
    input.d         = input.d, 
    var.names       = var.names, 
    var.descriptions= var.descriptions,
    var.ref.groups  = NULL, # a list of reference group, if NULL, assume ALL variables are binary/continuous
    var.names.surv.time   = var.names.surv.time,
    var.names.surv.status = var.names.surv.status,
    event.codes.surv      = event.codes.surv,
    surv.descriptions     = surv.descriptions,
    use.firth             = use.firth,
    stat.test             = stat.test,
    round.digits.p.value  = round.digits.p.value,
    caption               = caption,
    html.table.border     = html.table.border,
    banded.rows           = banded.rows,
    css.class.name.odd    = css.class.name.odd,
    css.class.name.even   = css.class.name.even))
}