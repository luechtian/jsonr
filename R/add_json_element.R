#' Add an element to a structure in a JSON object or file
#' 
#' @description 
#' This function allows adding a new element to an existing structure in a JSON object
#' or file, even in deeply nested structures. The path to the target structure can be 
#' specified using dot notation (e.g., "person.address"). The position of the new 
#' element can be optionally specified.
#'
#' @param json_input Either a path to a JSON file or an already loaded JSON object (as an R list)
#' @param path Path to the target structure using dot notation (e.g., "person" or "person.address")
#' @param element_name Name of the element to be added
#' @param element_value Value of the element (can be any type: string, number, boolean, list, NULL)
#' @param position Optional: Position at which the element should be inserted (1-based).
#'                If NULL or not specified, the element will be added at the end.
#' @param output_file_path Optional: Path for the output file. If NULL and json_input is a file path,
#'                         the input file will be overwritten. Ignored if json_input is an object.
#' @param create_path Optional: If TRUE, missing path components will be created. 
#'                    If FALSE, an error will be thrown if the path doesn't exist. (Default: FALSE)
#'
#' @return The modified JSON object (as an R list)
#' @export
#'
#' @examples
#' # Example JSON as a string
#' json_str <- '{
#'   "person": {
#'     "name": "John Doe",
#'     "age": 30
#'   }
#' }'
#' 
#' # Convert to R list
#' json_data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
#' 
#' # Add an email element to the "person" structure
#' updated_json <- add_json_element(json_data, "person", "email", "john@example.com")
#' 
#' # The result should include the new email element in the person object
#' jsonlite::toJSON(updated_json, pretty = TRUE, auto_unbox = TRUE)
#' 
#' # Add an element to a nested path that doesn't exist yet
#' updated_json2 <- add_json_element(json_data, "person.contact.phone", 
#'                                  "mobile", "+1234567890", 
#'                                  create_path = TRUE)
add_json_element <- function(
    json_input,              # Path to a JSON file or an already loaded JSON object
    path,                    # Path to the target structure using dot notation
    element_name,            # Name of the element to be added
    element_value,           # Value of the element
    position = NULL,         # Position at which the element should be inserted
    output_file_path = NULL, # Path for the output file
    create_path = FALSE      # If TRUE, missing path components will be created
) {
  # Check if jsonlite is available
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required. Please install it with install.packages('jsonlite').")
  }
  
  # Process the input based on type (file path or JSON object)
  if (is.character(json_input)) {
    if (file.exists(json_input)) {
      # Case 1: Input is a file path to a JSON file
      json_file_path <- json_input
      
      # Read the JSON file and convert to R list
      tryCatch({
        json_data <- jsonlite::fromJSON(json_file_path, simplifyVector = FALSE)
      }, error = function(e) {
        stop("Error reading JSON file: ", e$message)
      })
      
      # Determine the output path for the modified file
      if (is.null(output_file_path)) {
        output_file_path <- json_file_path  # Overwrite the original file if no output path is specified
      }
      is_file_input <- TRUE
    } else {
      # Invalid file path
      stop("File not found: ", json_input, ". Please provide a valid file path.")
    }
  } else if (is.list(json_input)) {
    # Case 2: Input is already a JSON object (as an R list)
    json_data <- json_input
    is_file_input <- FALSE
  } else {
    # Invalid input type
    stop("Invalid input: json_input must be either a path to a JSON file or a valid JSON object (as an R list).")
  }
  
  # Split the path into components
  path_components <- unlist(strsplit(path, "\\."))
  
  # Handle empty path (meaning add to the root level)
  if (path == "") {
    path_components <- character(0)
  }
  
  # Helper function to add an element at a specific position
  add_element_at_position <- function(target_list, element_name, element_value, position = NULL) {
    # If no position is specified, simply add at the end
    if (is.null(position)) {
      target_list[[element_name]] <- element_value
      return(target_list)
    }
    
    # Names of existing properties
    structure_names <- names(target_list)
    
    # Check if the position is valid
    if (!is.numeric(position) || position < 1 || position > length(structure_names) + 1) {
      stop(paste0("Invalid position: ", position, ". Position must be between 1 and ", 
                  length(structure_names) + 1, "."))
    }
    
    # Case 1: Insert at the beginning
    if (position == 1) {
      new_structure <- list()
      new_structure[[element_name]] <- element_value  # New element first
      
      # Add all existing elements after
      for (name in structure_names) {
        new_structure[[name]] <- target_list[[name]]
      }
      
      return(new_structure)
    } 
    # Case 2: Insert at the end
    else if (position == length(structure_names) + 1) {
      target_list[[element_name]] <- element_value
      return(target_list)
    } 
    # Case 3: Insert in the middle
    else {
      new_structure <- list()
      
      # Add elements before the position
      for (i in 1:(position-1)) {
        name <- structure_names[i]
        new_structure[[name]] <- target_list[[name]]
      }
      
      # Add the new element at the desired position
      new_structure[[element_name]] <- element_value
      
      # Add the remaining elements
      for (i in position:length(structure_names)) {
        name <- structure_names[i]
        new_structure[[name]] <- target_list[[name]]
      }
      
      return(new_structure)
    }
  }
  
  # Recursive function to update a nested path
  update_nested_path <- function(data, path_components, name, value, pos = NULL, create = FALSE) {
    # Base case: target reached
    if (length(path_components) == 0) {
      return(add_element_at_position(data, name, value, pos))
    }
    
    # Current path component
    current <- path_components[1]
    
    # Check if the current component exists
    if (!(current %in% names(data))) {
      if (create) {
        # Create the missing structure
        data[[current]] <- list()
      } else {
        stop(paste0("Path component '", current, "' not found. Use create_path = TRUE to create missing path components."))
      }
    }
    
    # Check if the current component is an object (and not a scalar or array)
    if (!is.list(data[[current]]) || is.null(names(data[[current]]))) {
      if (create) {
        # If it's not a list (e.g., it's a scalar value), make it a list
        data[[current]] <- list()
      } else {
        stop(paste0("Path component '", current, "' is not an object and cannot have sub-structures."))
      }
    }
    
    # Recursive call with the remaining path components
    data[[current]] <- update_nested_path(data[[current]], path_components[-1], name, value, pos, create)
    
    return(data)
  }
  
  # Apply the recursive function
  modified_data <- update_nested_path(json_data, path_components, element_name, element_value, position, create_path)
  
  # Save the result to a file if the input was a file
  if (is_file_input && !is.null(output_file_path)) {
    # Convert the modified data structure back to JSON
    # pretty=TRUE for human-readable formatting with indentation
    # auto_unbox=TRUE to remove unnecessary brackets for single values
    tryCatch({
      write(jsonlite::toJSON(modified_data, pretty = TRUE, auto_unbox = TRUE), output_file_path)
    }, error = function(e) {
      stop("Error writing JSON file: ", e$message)
    })
    
    # Create an informative output message with position information (if specified)
    position_info <- if (!is.null(position)) {
      paste0(" at position ", position)
    } else {
      ""
    }
    
    # Print a confirmation message
    message("Element '", element_name, "' was added to path '", path, "'", 
            position_info, " and saved to '", output_file_path, "'.")
  }
  
  # Return the modified JSON object (as an R list)
  return(modified_data)
}