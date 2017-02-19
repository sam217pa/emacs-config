#' ---
#' title: ""
#' author: "Samuel BARRETO"
#' date:
#' output:
#'   html_document:
#'     highlight: tango
#'     theme: flatly
#'     code_folding: hide
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     fig_width: 10
#'     fig_height: 6.18
#' ---

### ---------- include = FALSE ---------------------------------------------
library(knitr)
opts_chunk$set(
  cache = FALSE, dev = 'png', include = TRUE,
  fig.path  = "graphics/", echo = TRUE, warning = FALSE,
  error = FALSE, message = FALSE, global.par = TRUE
)

## /*
rmarkdown::render((buffer-file-name), encoding = "UTF-8")
## */

### ---------- Setup -------------------------------------------------------
