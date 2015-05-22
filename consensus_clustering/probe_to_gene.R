library(hgu133plus2.db)
dat <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/Tothill.csv")

# Probe to Gene annotation attempt
x <- hgu133plus2SYMBOL
mapped_probes <- mappedkeys(x)
lookup <- data.frame(gene = unlist(as.list(x[mapped_probes])))
mapped_genes <- match(dat$UNIQID, row.names(lookup))
compare_map <- data.frame(raw = dat$NAME, mapped = lookup[mapped_genes,]) %>%
  mutate(raw = as.character(raw),
         mapped = as.character(mapped),
         check = ifelse(raw != "---" | !is.na(mapped), "YES", "NO"))
sum(compare_map$check == "YES", na.rm = T)

# xx <- unlist(as.list(hgu133plus2ALIAS2PROBE))
# lookup <- data.frame(probe = xx, gene = names(xx))
# mapped_genes <- match(Tothill.raw$UNIQID, lookup$probe)
# compare_map <- data.frame(raw = Tothill.raw$NAME, mapped = lookup[mapped_genes, "gene"]) %>%
#   mutate(raw = as.character(raw),
#          mapped = as.character(mapped),
#          check = ifelse(raw != "---" | !is.na(mapped), "YES", "NO"))
# sum(compare_map$check == "YES", na.rm = T)