# Parse all Rd files in package

Parse all Rd files in a package and return metadata as a data frame.

## Usage

``` r
parse_rd(file = NULL, tags = c("name", "title", "desc", "details"))
```

## Arguments

- file:

  file path to save table

- tags:

  sections to extract from Rd files. Missing entries labelled as `NA`

## Value

a table with information on a package's documented objects (functions
and data). Rows are objects, and columns are tags.

## Details

The function requires that the working directory is set to the package's
root directory.

## Author

Derek Chiu

## Examples

``` r
if (FALSE) { # \dontrun{
tab <- parse_rd()
str(tab)
} # }
```
