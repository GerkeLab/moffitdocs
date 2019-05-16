
# ---- Documents ----

#' Create New Document
#'
#' Creates a new R Markdown document of the requested type. There are currently
#' three templates, one for reports where the default is HTML based on the HTML
#' vignette template, and another with a Moffitt-styled [xaringan] theme. There
#' is also a [radix] template for stunning HTML-only reports. In all cases, the
#' document and supporting files are added to a directory with the name given by
#' the file.
#'
#' @examples
#' \dontrun{
#' doc_new("my_report.Rmd", "doc")
#' doc_new("my_slides.Rmd", "slides")
#' }
#'
#' @param path Path to the location of the new document
#' @param type Type of document to create
#' @export
doc_new <- function(path, type = c("doc", "slides", "radix")) {
  type <- match.arg(type)
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }
  file_path <- switch(
    type,
    "doc"    = rmarkdown::draft(path, template = "default", package = "grkmisc", edit = FALSE),
    "slides" = rmarkdown::draft(path, template = "moffitt-xaringan", package = "grkmisc", edit = FALSE),
    "radix"  = rmarkdown::draft(path, template = "grk-radix", package = "grkmisc", edit = FALSE)
  )
  if (type == "radix" && !requireNamespace("radix", quietly = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      rlang::abort("Please install 'remotes' with `install.packages(\"remotes\")`")
    }
    message("Installing radix from rstudio/radix...")
    remotes::install_github("radix/rstudio")
  }
  if (requireNamespace("rstudioapi", quietly = TRUE))
    rstudioapi::navigateToFile(file_path) else file_path
}
