cutRange <- function(levs) {
  sapply(levs[-length(levs)], function(x) {
    lo <- levs[which(levs <= x)]
    lo.rng <- paste0("[", paste(unique(c(min(lo), max(lo))), collapse = ", "), "]")
    hi <- levs[which(levs > x)]
    hi.rng <- paste0("[", paste(unique(c(min(hi), max(hi))), collapse = ", "), "]")
    res <- paste(lo.rng, "vs.", hi.rng)
    return(res)
  })
}