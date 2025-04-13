#' @keywords internal
"_PACKAGE"

#' Advanced JSON Manipulation Tools for R
#'
#' @description
#' The `jsonr` package provides tools for advanced manipulation of JSON data in R.
#' It offers functions for adding elements to specific paths in JSON objects, inserting
#' properties into nested array elements, and other JSON manipulation tasks.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{add_json_element}}: Add an element to a structure in a JSON object or file
#'   \item \code{\link{insert_json_property}}: Insert a property into elements of a nested JSON array
#' }
#'
#' @docType _PACKAGE
#' @name jsonr-package
#' @aliases jsonr
NULL

.onLoad <- function(libname, pkgname) {
  # Package startup code (if needed)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to jsonr ", utils::packageVersion("jsonr"))
}
