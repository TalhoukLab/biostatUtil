#' Process mass spectrometry data
#' 
#' Process mass spectrometry data for filtering out contaminant samples,
#' manipulating variables, removing duplicates, and more.
#' 
#' @param psm PSM file
#' @param protein Protein file
#' @param g character vector of sample groups
#' @param sample.id character vector for sample IDs. Other of samples must match
#'   that in the psm raw data.
#' @param path file path to save processed peptide file
#' @param save logical; should the peptide file be saved?
#' @return A processed data frame to be used by \code{ms_analyze}.
#' @author Derek Chiu
#' @export
ms_process <- function(psm, protein, g, sample.id, path, save = TRUE) {
  # Make raw data file column names into R column names
  psm <- psm %>% set_colnames(make.names(colnames(.)))
  protein <- protein %>% set_colnames(make.names(colnames(.)))
  
  # Variables to keep
  orig.sample.id <- grep("^X[0-9]", names(psm), value = TRUE)
  psmKeepVars <-
    c("Annotated.Sequence", "Modifications", "Number.of.Protein.Groups",
      "Number.of.Proteins", "Master.Protein.Accessions", "Protein.Accessions",
      "Confidence", "Reporter.Quan.Result.ID", "Quan.Info", "PSM.Ambiguity",
      orig.sample.id)
  
  # Select relevant columns for analysis
  # Rename original sample labels to reflect sample grouping
  # Filter out: 
  #   - non-unique peptides
  #   - no quan values
  #   - insufficient data: at least 2 trt values in each group
  #   - Master Protein Accession missing or 'sp'
  pep <- psm %>%
    select(one_of(psmKeepVars)) %>% 
    set_colnames(plyr::mapvalues(colnames(.), orig.sample.id, sample.id)) %>% 
    filter_(.dots = list(lazyeval::interp(
      ~NOP == 1 & !grepl("NoQuanValues", QI) &
        sapply(g, function(y) apply(.[, grep(y, names(.))], 1, function(x)
          sum(!is.na(x)) >= 2)) %>% 
        apply(., 1, function(z) ifelse(all(z), TRUE, FALSE)) &
        (is.na(MPA) | MPA != "sp"),
      NOP = quote(Number.of.Proteins), QI = quote(Quan.Info),
      MPA = quote(Master.Protein.Accessions)
    )))
  
  pro <- pro %>% 
    select_("Accession", "Description", "MW.in.kDa")
  
  # For each Reporter.Quan.Result.ID in pep, remove duplicates for 4 vars
  pep <- pep %>% 
    group_by_("Reporter.Quan.Result.ID") %>% 
    do(remove_dup(., c("Annotated.Sequence", "Modifications",
                       "Master.Protein.Accessions", "Protein.Accessions")))
  
  # Parse peptide accession and merge the protein descriptions with the peptide file
  pep.r <- pep %>% 
    mutate_(.dots = setNames(list(lazyeval::interp(
      ~sapply(strsplit(as.character(MPA), ';'), '[', 1),
      MPA = quote(Master.Protein.Accessions))), "Accession")) %>% 
    mutate_(.dots = setNames(list(lazyeval::interp(
      ~sapply(strsplit(as.character(A), ' | '), '[', 1),
      A = quote(Accession))), "Accession")) %>% 
    merge(pro, by = "Accession") %>% 
    mutate_(.dots = setNames(list(
      lazyeval::interp(~sub(".*?GN=(.*?)( .*|$)", "\\1", D),
                       D = quote(Description)), # Get the gene name out
      lazyeval::interp(~toupper(sub('.*?\\.(.*?)(\\..*|$)','\\1', AS)),
                       AS = quote(Annotated.Sequence)),   # Parse the peptide column for amino acids
      lazyeval::interp(~sub('(.*?)( OS=.*|$)','\\1', D),
                       D = quote(Description))),  # Filter information from Description
      c("Gene", "Sequence", "Descriptions"))) %>%    
    filter_(.dots = list(lazyeval::interp(
      ~!grepl("Keratin", quote(D)) &
        !grepl("sp", quote(A), ignore.case = FALSE) &
        !grepl("ribosomal", quote(D)),  # Remove specific proteins (typically contaminants from other sources)
      D = quote(Descriptions), A = quote(Accession))))
  if (save)
    readr::write_csv(pep.r, path = path)
  
  # Raw, log2, and vsn transformed expression data
  raw <- pep.r[, sample.id]
  l2 <- log2(raw) %>% 
    set_colnames(paste("l2", names(.), sep = "_"))
  vsn <- limma::normalizeVSN(raw, verbose = FALSE) %>% 
    set_colnames(paste("vsn", colnames(.), sep = "_"))
  return(list(pep = pep.r, raw = raw, l2 = l2, vsn = vsn))
}