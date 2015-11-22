# Function to wrap text
wrap.labels <- function(x, len)
{wrap.it <- function(x, len)
  {
  sapply(x, function(y) paste(strwrap(y, len),
                              collapse = "\n"),
         USE.NAMES = FALSE)
  }

  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}
