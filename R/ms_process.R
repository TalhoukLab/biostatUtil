#' Process mass spectrometry data
#'
#' Process mass spectrometry data for filtering out contaminant samples,
#' manipulating variables, removing duplicates, and more.
#'
#' @param psm PSM file
#' @param protein Protein file
#' @param treatment character vector of treatment groups
#' @param samples character vector of sample names to keep
#' @param sample.id character vector for sample IDs. Order of samples must match
#'   that in the psm raw data.
#' @param path file path to save return element `pep`
#' @param ... additional arguments to `ms_condition`
#' @return A list with the following elements
#' \item{pep}{processed data frame to be used by `ms_summarize`}
#' \item{raw}{raw data values}
#' \item{l2}{log2 raw data values}
#' \item{vsn}{vsn raw data values}
#' @family Mass Spectrometry functions
#' @author Derek Chiu
#' @export
ms_process <- function(psm, protein, treatment, samples = NULL,
                       sample.id = NULL, path = NULL, ...) {
  if (!requireNamespace("limma", quietly = TRUE)) {
    stop("Package \"limma\" is required. Please install it.",
         call. = FALSE)
  }
  # Make raw data file column names into R column names
  psm <- psm %>% magrittr::set_colnames(make.names(colnames(.)))
  protein <- protein %>% magrittr::set_colnames(make.names(colnames(.)))

  # Variables to keep
  samples <- samples %||% grep("^X[0-9]", names(psm), value = TRUE)
  if (is.null(sample.id)) {
    ns <- seq_along(samples)
    sample.id <- paste0("X", ceiling(ns * 2 / max(ns)),
                        "_", seq_len(max(ns) / 2))
    assertthat::assert_that(all(purrr::map_lgl(treatment,
                                               ~ any(grepl(.x, sample.id)))))
  }
  psmKeepVars <-
    c("Annotated.Sequence", "Modifications", "Number.of.Protein.Groups",
      "Number.of.Proteins", "Master.Protein.Accessions", "Protein.Accessions",
      "Confidence", "Reporter.Quan.Result.ID", "Quan.Info", "PSM.Ambiguity",
      samples)

  # Select relevant columns for analysis
  # Rename original sample labels to reflect sample grouping
  # Filter out:
  #   - non-unique peptides
  #   - no quan values
  #   - insufficient data: use ms_condition()
  #   - Master Protein Accession missing or "sp"
  pep <- psm %>%
    dplyr::select(dplyr::one_of(psmKeepVars)) %>%
    dplyr::rename_at(samples, ~ sample.id) %>%
    dplyr::filter(
      .data$Number.of.Proteins == 1,
      !grepl("NoQuanValues", .data$Quan.Info),
      is.na(.data$Master.Protein.Accessions) |
        .data$Master.Protein.Accessions != "sp"
    ) %>%
    magrittr::extract(ms_condition(., treatment = treatment, ...), )

  pro <- protein[c("Accession", "Description", "MW.in.kDa")]

  # For each Reporter.Quan.Result.ID in pep, remove duplicates for 4 vars
  pep <- pep %>%
    dplyr::group_by(.data$Reporter.Quan.Result.ID) %>%
    dplyr::do(remove_dup(., c("Annotated.Sequence", "Modifications",
                              "Master.Protein.Accessions", "Protein.Accessions")))

  # Parse peptide accession and merge the protein descriptions with the peptide file
  pep <- pep %>%
    dplyr::mutate(Accession = purrr::map_chr(strsplit(as.character(.data$Master.Protein.Accessions), ";"), `[`, 1)) %>%  # Strip ";"
    dplyr::mutate(Accession = purrr::map_chr(strsplit(as.character(.data$Accession), " | "), `[`, 1)) %>%  # Strip " | "
    merge(pro, by = "Accession") %>%  # Merge with protein set on Accession
    dplyr::mutate(
      Gene = sub(".*?GN=(.*?)( .*|$)", "\\1", .data$Description),  # Get the gene name out
      Sequence = toupper(sub(".*?\\.(.*?)(\\..*|$)", "\\1", .data$Annotated.Sequence)),  # Parse the peptide column for amino acids
      Descriptions = sub("(.*?)( OS=.*|$)", "\\1", .data$Description)  # Filter information from Description
    ) %>%
    dplyr::filter(
      !grepl("Keratin", .data$Descriptions),
      !grepl("sp", .data$Accession, ignore.case = FALSE),
      !grepl("ribosomal", .data$Descriptions)
    )  # Remove specific proteins (typically contaminants from other sources)
  if (!is.null(path))
    readr::write_csv(pep, path = path)

  # Raw, log2, and vsn transformed expression data
  raw <- pep[, sample.id]
  l2 <- log2(raw) %>%
    magrittr::set_colnames(paste("l2", names(.), sep = "_"))
  vsn <- limma::normalizeVSN(raw, verbose = FALSE) %>%
    magrittr::set_colnames(paste("vsn", colnames(.), sep = "_"))
  rlang::dots_list(pep, raw, l2, vsn, .named = TRUE)
}
