#' Check if a JSON string or file is valid
#'
#' This function checks if a JSON string or file is valid according to JSON syntax.
#'
#' @param json_input Either a JSON string or a path to a JSON file
#' @param is_file Logical indicating whether the input is a file path
#'
#' @return Logical indicating whether the JSON is valid
#' @keywords internal
is_valid_json <- function(json_input, is_file = FALSE) {
  # Check if jsonlite is available
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required. Please install it with install.packages('jsonlite').")
  }
  
  # If input is a file path, read the file
  if (is_file) {
    if (!file.exists(json_input)) {
      return(FALSE)
    }
    
    tryCatch({
      json_input <- readLines(json_input, warn = FALSE)
      json_input <- paste(json_input, collapse = "\n")
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # Check if the JSON is valid
  tryCatch({
    jsonlite::fromJSON(json_input)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Get a nested value from a JSON object using a path
#'
#' @param json_data A JSON object (as an R list)
#' @param path Path to the value using dot notation (e.g., "person.address.city")
#' @param default Default value to return if the path does not exist
#'
#' @return The value at the specified path or the default value if the path does not exist
#' @keywords internal
get_nested_value <- function(json_data, path, default = NULL) {
  # Split the path into components
  path_components <- unlist(strsplit(path, "\\."))
  
  # Traverse the JSON structure
  current <- json_data
  for (component in path_components) {
    if (!is.list(current) || !(component %in% names(current))) {
      return(default)
    }
    current <- current[[component]]
  }
  
  return(current)
}

#' Format JSON for pretty printing
#'
#' This function formats a JSON object or string for pretty printing.
#'
#' @param json_input Either a JSON object (as an R list) or a JSON string
#'
#' @return A formatted JSON string
#' @keywords internal
pretty_json <- function(json_input) {
  # Check if jsonlite is available
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required. Please install it with install.packages('jsonlite').")
  }
  
  # If input is a string, parse it first
  if (is.character(json_input)) {
    tryCatch({
      json_data <- jsonlite::fromJSON(json_input, simplifyVector = FALSE)
    }, error = function(e) {
      stop("Invalid JSON string: ", e$message)
    })
  } else if (is.list(json_input)) {
    json_data <- json_input
  } else {
    stop("Invalid input: json_input must be either a JSON string or a valid JSON object (as an R list).")
  }
  
  # Convert to pretty JSON
  pretty_str <- jsonlite::toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
  
  return(pretty_str)
}

#' Compare two JSON structures and return differences
#'
#' This function compares two JSON structures and returns a list of differences.
#'
#' @param json1 First JSON object (as an R list)
#' @param json2 Second JSON object (as an R list)
#' @param path Current path in the JSON structure (used for recursion)
#'
#' @return A list of differences, each entry containing the path and the values in both structures
#' @keywords internal
compare_json <- function(json1, json2, path = "") {
  differences <- list()
  
  # Function to add a difference
  add_difference <- function(p, v1, v2) {
    differences <<- c(differences, list(list(
      path = p,
      value1 = v1,
      value2 = v2
    )))
  }
  
  # Case 1: Different types
  if (class(json1) != class(json2)) {
    add_difference(path, json1, json2)
    return(differences)
  }
  
  # Case 2: Both are lists (objects or arrays)
  if (is.list(json1) && is.list(json2)) {
    # If one has names and the other doesn't, they're different types (object vs array)
    if (is.null(names(json1)) != is.null(names(json2))) {
      add_difference(path, json1, json2)
      return(differences)
    }
    
    # If both are objects (have names)
    if (!is.null(names(json1))) {
      # Check for keys in json1 that aren't in json2
      for (key in names(json1)) {
        new_path <- if (path == "") key else paste0(path, ".", key)
        if (!(key %in% names(json2))) {
          add_difference(new_path, json1[[key]], NULL)
        } else {
          # Recursively compare nested structures
          differences <- c(differences, compare_json(json1[[key]], json2[[key]], new_path))
        }
      }
      
      # Check for keys in json2 that aren't in json1
      for (key in names(json2)) {
        if (!(key %in% names(json1))) {
          new_path <- if (path == "") key else paste0(path, ".", key)
          add_difference(new_path, NULL, json2[[key]])
        }
      }
    }
    # If both are arrays (no names)
    else {
      # Compare arrays
      max_length <- max(length(json1), length(json2))
      for (i in 1:max_length) {
        new_path <- if (path == "") paste0("[", i, "]") else paste0(path, "[", i, "]")
        
        if (i <= length(json1) && i <= length(json2)) {
          # Both arrays have this index
          differences <- c(differences, compare_json(json1[[i]], json2[[i]], new_path))
        } else if (i <= length(json1)) {
          # Only json1 has this index
          add_difference(new_path, json1[[i]], NULL)
        } else {
          # Only json2 has this index
          add_difference(new_path, NULL, json2[[i]])
        }
      }
    }
  }
  # Case 3: Atomic values
  else if (json1 != json2) {
    add_difference(path, json1, json2)
  }
  
  return(differences)
}