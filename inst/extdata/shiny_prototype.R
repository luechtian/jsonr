library(shiny)
library(jsonlite)
library(shinyAce)
library(DT)
library(shinyjs)
library(jsonr)

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  titlePanel("JSON Schema Transformer"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 # Input section
                 h4("Input Data"),
                 fileInput("jsonFile", "Upload JSON File", 
                           accept = c("application/json", ".json")),
                 
                 # Or use example
                 selectInput("sampleData", "Or use sample data:",
                             c("None" = "none", 
                               "Product Catalog" = "product", 
                               "User Profile" = "user", 
                               "API Response" = "api")),
                 
                 # Transformation Rules section
                 hr(),
                 h4("Transformation Rules"),
                 
                 # Predefined transformation templates
                 selectInput("transformTemplate", "Templates:",
                             c("Custom" = "custom", 
                               "Rename Fields" = "rename", 
                               "Flatten Nested Fields" = "flatten",
                               "Add Metadata" = "metadata",
                               "Array Operations" = "array_ops")),
                 
                 # Saved rules management
                 hr(),
                 h4("Saved Rules"),
                 selectInput("savedRules", "Load saved rule:", choices = c("None" = "none")),
                 
                 textInput("ruleName", "Rule name:"),
                 textAreaInput("ruleDescription", "Description:", height = "60px"),
                 actionButton("saveRule", "Save Current Rule", class = "btn-success"),
                 
                 # Transform button
                 hr(),
                 checkboxInput("prettyOutput", "Pretty formatted output", TRUE),
                 actionButton("transformBtn", "Transform", class = "btn-primary btn-lg btn-block"),
                 
                 # Debug options
                 hr(),
                 checkboxInput("debugMode", "Debug Mode", FALSE),
                 div(id = "debugOptions", style = "display: none;",
                     checkboxInput("verboseLogging", "Verbose Logging", FALSE),
                     checkboxInput("showErrorDetails", "Show Detailed Errors", TRUE),
                     actionButton("testBtn", "Run Test", class = "btn-info")
                 )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Editor",
                 fluidRow(
                   column(12,
                          h4("Rules Editor"),
                          aceEditor("rulesEditor", mode = "json", theme = "chrome", height = "250px",
                                    value = '[\n  {\n    "operation": "add_element",\n    "path": "",\n    "name": "metadata",\n    "value": {"processed": true}\n  }\n]')
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Input JSON"),
                          aceEditor("inputJson", mode = "json", theme = "chrome", height = "400px",
                                    value = '{\n  "Upload a JSON file or select a sample"\n}')
                   ),
                   column(6,
                          div(style = "display: flex; justify-content: space-between; align-items: center;",
                              h4("Output JSON"),
                              actionButton("clearOutputBtn", "Clear Output", class = "btn-warning btn-sm")
                          ),
                          aceEditor("outputJson", mode = "json", theme = "chrome", height = "400px",
                                    readOnly = TRUE),
                          div(style = "margin-top: 10px",
                              downloadButton("downloadJson", "Download Transformed JSON"),
                              actionButton("copyBtn", "Copy to Clipboard", icon = icon("clipboard"))
                          )
                   )
                 )
        ),
        
        tabPanel("Rule Management",
                 h3("Saved Rules"),
                 DT::dataTableOutput("savedRulesTable"),
                 br(),
                 actionButton("deleteRuleBtn", "Delete Selected Rule", class = "btn-danger")
        ),
        
        tabPanel("Documentation",
                 h3("Rule Operations Reference"),
                 
                 h4("add_element"),
                 p("Adds a new element to a JSON object at a specified path."),
                 tags$pre(
                   '{
  "operation": "add_element",
  "path": "user.profile",       // Path to the target object
  "name": "location",           // Name of the new element
  "value": "Berlin",            // Value of the new element
  "position": 2                 // Optional: Position to insert (1-based)
}'
                 ),
                 br(),
                 
                 h4("insert_property"),
                 p("Inserts a property into each element of an array."),
                 tags$pre(
                   '{
  "operation": "insert_property",
  "array_path": ["products"],   // Path to the array as array of strings
  "property_name": "status",    // Name of the property to insert
  "property_value": "active",   // Value or function to compute value
  "position_type": "after",     // "first", "last", "after", "before", "position"
  "position_ref": "name"        // Reference property or position number
}'
                 ),
                 br(),
                 
                 h4("modify_array"),
                 p("Modifies elements in an array using various operations."),
                 tags$pre(
                   '{
  "operation": "modify_array",
  "path": "user.profile.interests", // Path to the array
  "action": "append",               // "append", "prepend", "insert", "replace", "remove"
  "value": "cooking",               // Value to add/insert/replace with
  "position": 2,                    // Optional: Position for insert/replace/remove
  "filter": {"_function_": "return elem === \'hiking\';"}  // Optional: Filter function
}'
                 ),
                 
                 h3("Dynamic Values with Functions"),
                 p("You can use functions to compute values dynamically:"),
                 tags$pre(
                   '"property_value": {"_function_": "return elem.name.toUpperCase();"}

// Safe property access with error handling
"property_value": {"_function_": "return elem.details && elem.details.specs ? elem.details.specs.cpu : null;"}

// Using the index in array operations
"property_value": {"_function_": "return \'Item \' + (index + 1);"}

// Using system time
"property_value": {"_function_": "return Sys.time();"}'
                 )
        ),
        
        tabPanel("Log", 
                 verbatimTextOutput("transformLog"))
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Initialize reactive values
  logs <- reactiveVal(character())
  rules <- reactiveVal(list())
  
  # Debug flag
  observe({
    if (input$debugMode) {
      shinyjs::show("debugOptions")
    } else {
      shinyjs::hide("debugOptions")
    }
  })
  
  # Add a log entry
  addLog <- function(message, isDebug = FALSE) {
    if (isDebug && !input$verboseLogging) {
      return() # Skip debug messages unless verbose logging is enabled
    }
    
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    prefix <- if (isDebug) "[DEBUG] " else ""
    current <- logs()
    logs(c(paste(timestamp, "-", prefix, message), current))
  }
  
  # Display logs
  output$transformLog <- renderText({
    paste(logs(), collapse = "\n")
  })
  
  # Clear output button
  observeEvent(input$clearOutputBtn, {
    updateAceEditor(session, "outputJson", value = "")
    addLog("Output cleared")
  })
  
  # Load sample data based on selection
  observeEvent(input$sampleData, {
    sample_type <- input$sampleData
    
    if (sample_type == "none") {
      return()
    }
    
    json_content <- switch(sample_type,
                           "product" = '{
  "products": [
    {
      "id": "P001",
      "name": "Laptop Pro",
      "details": {
        "specs": {
          "cpu": "Intel i7",
          "ram": "16GB",
          "storage": "1TB SSD"
        }
      },
      "pricing": {
        "retail": 1299.99,
        "sale": 1199.99
      }
    },
    {
      "id": "P002",
      "name": "Smartphone X",
      "details": {
        "specs": {
          "cpu": "Snapdragon 888",
          "ram": "8GB",
          "storage": "256GB"
        }
      },
      "pricing": {
        "retail": 899.99,
        "sale": 849.99
      }
    }
  ]
}',
                           "user" = '{
  "user": {
    "id": "U12345",
    "name": "Max Mustermann",
    "email": "max@example.com",
    "profile": {
      "age": 28,
      "occupation": "Software Developer",
      "interests": ["coding", "hiking", "photography"]
    }
  }
}',
                           "api" = '{
  "meta": {
    "status": "success",
    "code": 200,
    "timestamp": "2023-06-01T12:00:00Z"
  },
  "data": {
    "items": [
      {
        "id": "item1",
        "title": "First Item",
        "created_at": "2023-05-20T10:30:00Z"
      },
      {
        "id": "item2",
        "title": "Second Item",
        "created_at": "2023-05-15T14:45:00Z"
      }
    ]
  }
}'
    )
    
    updateAceEditor(session, "inputJson", value = json_content)
    addLog(paste("Loaded sample data:", sample_type))
  })
  
  # Load template based on selection
  observeEvent(input$transformTemplate, {
    template_type <- input$transformTemplate
    
    if (template_type == "custom") {
      return()  # Don't overwrite if custom is selected
    }
    
    template_content <- switch(template_type,
                               "rename" = '[
  {
    "operation": "add_element",
    "path": "products.0",
    "name": "product_name",
    "value": null,
    "position": 2
  },
  {
    "operation": "add_element",
    "path": "products.1",
    "name": "product_name",
    "value": null,
    "position": 2
  },
  {
    "operation": "insert_property",
    "array_path": ["products"],
    "property_name": "product_name",
    "property_value": {"_function_": "return elem.name;"},
    "position_type": "after",
    "position_ref": "id"
  }
]',
                               "flatten" = '[
  {
    "operation": "insert_property",
    "array_path": ["products"],
    "property_name": "cpu",
    "property_value": {"_function_": "return elem.details && elem.details.specs ? elem.details.specs.cpu : null;"},
    "position_type": "after",
    "position_ref": "name"
  },
  {
    "operation": "insert_property",
    "array_path": ["products"],
    "property_name": "ram",
    "property_value": {"_function_": "return elem.details && elem.details.specs ? elem.details.specs.ram : null;"},
    "position_type": "after",
    "position_ref": "cpu"
  },
  {
    "operation": "insert_property",
    "array_path": ["products"],
    "property_name": "price",
    "property_value": {"_function_": "return elem.pricing ? elem.pricing.retail : null;"},
    "position_type": "after",
    "position_ref": "ram"
  }
]',
                               "metadata" = '[
  {
    "operation": "add_element",
    "path": "",
    "name": "metadata",
    "value": {
      "processed": true,
      "timestamp": {"_function_": "return Sys.time();"},
      "version": "1.0"
    }
  },
  {
    "operation": "insert_property",
    "array_path": ["products"],
    "property_name": "discount_pct",
    "property_value": {"_function_": "if(elem.pricing && elem.pricing.retail && elem.pricing.sale) return round((elem.pricing.retail - elem.pricing.sale) / elem.pricing.retail * 100, 1); else return 0;"},
    "position_type": "after",
    "position_ref": "pricing"
  }
]',
                               "array_ops" = '[
  {
    "operation": "modify_array",
    "path": "user.profile.interests",
    "action": "append",
    "value": "cooking"
  },
  {
    "operation": "modify_array",
    "path": "user.profile.interests",
    "action": "insert",
    "value": "gaming",
    "position": 2
  },
  {
    "operation": "modify_array",
    "path": "user.profile.interests",
    "action": "remove",
    "filter": {"_function_": "return elem === \'hiking\';"}
  },
  {
    "operation": "insert_property",
    "array_path": ["user", "profile", "interests"],
    "property_name": "display_name",
    "property_value": {"_function_": "return elem.charAt(0).toUpperCase() + elem.slice(1);"}
  }
]'
    )
    
    updateAceEditor(session, "rulesEditor", value = template_content)
    addLog(paste("Loaded template:", template_type))
  })
  
  # Handle JSON file upload
  observeEvent(input$jsonFile, {
    file <- input$jsonFile
    
    if (is.null(file)) {
      return()
    }
    
    ext <- tools::file_ext(file$datapath)
    
    if (ext != "json") {
      showNotification("Please upload a JSON file (.json)", type = "error")
      return()
    }
    
    # Read and validate JSON
    tryCatch({
      json_content <- readLines(file$datapath)
      json_content <- paste(json_content, collapse = "\n")
      json_test <- fromJSON(json_content) # Just to validate
      
      updateAceEditor(session, "inputJson", value = json_content)
      addLog(paste("Loaded file:", file$name))
    }, error = function(e) {
      showNotification(paste("Invalid JSON format:", e$message), type = "error")
      addLog(paste("Error loading file:", e$message))
    })
  })
  
  # Safely access nested properties in a JSON object
  safeGet <- function(obj, path, default = NULL) {
    # If obj is NULL or path is empty, return default
    if (is.null(obj) || path == "") {
      return(default)
    }
    
    # Split path into components
    parts <- unlist(strsplit(path, "\\."))
    
    current <- obj
    for (part in parts) {
      # Check if part is a number (array index)
      if (grepl("^\\d+$", part)) {
        idx <- as.integer(part) + 1  # Convert to 1-based index
        
        # Check if current is a list and has enough elements
        if (!is.list(current) || length(current) < idx) {
          return(default)
        }
        
        current <- current[[idx]]
      } else {
        # Check if current has the property
        if (!is.list(current) || !(part %in% names(current))) {
          return(default)
        }
        
        current <- current[[part]]
      }
    }
    
    return(current)
  }
  
  # Safely set a nested value in a JSON object
  safeSet <- function(obj, path, value) {
    # If path is empty, return the value (replace the whole object)
    if (path == "") {
      return(value)
    }
    
    # Split path into components
    parts <- unlist(strsplit(path, "\\."))
    
    # Function to recursively set value at path
    setNestedValue <- function(current, parts_left, value) {
      if (length(parts_left) == 0) {
        return(value)
      }
      
      part <- parts_left[1]
      remaining_parts <- parts_left[-1]
      
      # Check if part is a number (array index)
      if (grepl("^\\d+$", part)) {
        idx <- as.integer(part) + 1  # Convert to 1-based index
        
        # Ensure current is a list
        if (!is.list(current)) {
          current <- list()
        }
        
        # Ensure current has enough elements
        while (length(current) < idx) {
          current[[length(current) + 1]] <- NULL
        }
        
        # Update the element
        current[[idx]] <- setNestedValue(current[[idx]], remaining_parts, value)
        return(current)
      } else {
        # Ensure current is a list
        if (!is.list(current)) {
          current <- list()
        }
        
        # Update the property
        current[[part]] <- setNestedValue(current[[part]], remaining_parts, value)
        return(current)
      }
    }
    
    # Apply the recursive function
    return(setNestedValue(obj, parts, value))
  }
  
  # Helper function to apply add_element with proper positioning and error handling
  applyAddElement <- function(json_obj, path, name, value, position = NULL) {
    addLog(paste("Applying add_element to path:", path, "with name:", name), isDebug = TRUE)
    
    # If path is empty, add to root
    if (path == "") {
      if (is.null(position)) {
        # No position specified, just add at the end
        json_obj[[name]] <- value
        return(json_obj)
      } else {
        # Add at specific position
        if (!is.numeric(position) || position < 1 || (length(names(json_obj)) > 0 && position > length(names(json_obj)) + 1)) {
          addLog(paste("Warning: Invalid position", position, "for root level. Using end position."))
          json_obj[[name]] <- value
          return(json_obj)
        }
        
        # Create a new object with properties in the right order
        new_obj <- list()
        
        # Handle empty object case
        if (length(names(json_obj)) == 0) {
          new_obj[[name]] <- value
          return(new_obj)
        }
        
        current_names <- names(json_obj)
        
        # Add properties before position
        if (position > 1) {
          for (i in 1:(position-1)) {
            new_obj[[current_names[i]]] <- json_obj[[current_names[i]]]
          }
        }
        
        # Add the new property
        new_obj[[name]] <- value
        
        # Add remaining properties
        if (position <= length(current_names)) {
          for (i in position:length(current_names)) {
            new_obj[[current_names[i]]] <- json_obj[[current_names[i]]]
          }
        }
        
        return(new_obj)
      }
    }
    
    # Parse the path
    path_parts <- unlist(strsplit(path, "\\."))
    if (length(path_parts) == 0) {
      addLog(paste("Warning: Invalid path format:", path), isDebug = TRUE)
      return(json_obj)
    }
    
    # Get the target object
    target_obj <- safeGet(json_obj, path)
    
    # If target is NULL or not a list, create it
    if (is.null(target_obj) || !is.list(target_obj)) {
      addLog(paste("Warning: Path", path, "does not point to an object. Creating empty object."))
      target_obj <- list()
    }
    
    # Add element with position
    if (is.null(position)) {
      # No position, add at end
      target_obj[[name]] <- value
    } else {
      # Add at position
      if (!is.numeric(position) || position < 1 || position > length(names(target_obj)) + 1) {
        addLog(paste("Warning: Invalid position", position, "for path", path, ". Using end position."))
        target_obj[[name]] <- value
      } else {
        # Create a new object with properties in the right order
        new_obj <- list()
        
        # Handle empty object case
        if (length(names(target_obj)) == 0) {
          new_obj[[name]] <- value
          target_obj <- new_obj
        } else {
          target_names <- names(target_obj)
          
          # Add properties before position
          if (position > 1) {
            for (i in 1:(position-1)) {
              new_obj[[target_names[i]]] <- target_obj[[target_names[i]]]
            }
          }
          
          # Add the new property
          new_obj[[name]] <- value
          
          # Add remaining properties
          if (position <= length(target_names)) {
            for (i in position:length(target_names)) {
              new_obj[[target_names[i]]] <- target_obj[[target_names[i]]]
            }
          }
          
          target_obj <- new_obj
        }
      }
    }
    
    # Update the original object
    json_obj <- safeSet(json_obj, path, target_obj)
    return(json_obj)
  }
  
  # Evaluate a function string safely
  evaluateFunction <- function(func_code, context = list()) {
    # Create a safer environment for function evaluation
    safe_env <- new.env(parent = baseenv())
    
    # Add necessary global functions
    safe_env$Sys.time <- Sys.time
    safe_env$paste <- paste
    safe_env$paste0 <- paste0
    safe_env$round <- round
    safe_env$toupper <- toupper
    safe_env$tolower <- tolower
    
    # Add context variables
    for (name in names(context)) {
      safe_env[[name]] <- context[[name]]
    }
    
    # Create the function with proper error handling
    result <- NULL
    tryCatch({
      # Create function parameters based on provided context
      params <- paste(names(context), collapse = ", ")
      func <- eval(parse(text = paste0("function(", params, ") {", func_code, "}")), envir = safe_env)
      
      # Call function with context parameters
      args <- as.list(context)
      result <- do.call(func, args)
    }, error = function(e) {
      addLog(paste("Error in function evaluation:", e$message))
    })
    
    return(result)
  }

  # Transform JSON based on rules
  observeEvent(input$transformBtn, {
    req(input$inputJson)
    
    # Clear previous output
    updateAceEditor(session, "outputJson", value = "")
    
    tryCatch({
      # Parse input JSON
      input_json <- fromJSON(input$inputJson, simplifyVector = FALSE)
      
      # Parse transformation rules
      rules_json <- input$rulesEditor
      transform_rules <- fromJSON(rules_json, simplifyVector = FALSE)
      
      # Apply rules one by one (improved implementation)
      output_json <- input_json
      
      for (rule_index in seq_along(transform_rules)) {
        rule <- transform_rules[[rule_index]]
        
        addLog(paste("Processing rule", rule_index, ":", rule$operation), isDebug = TRUE)
        
        if (rule$operation == "add_element") {
          # Validate required fields
          if (!all(c("path", "name") %in% names(rule))) {
            addLog(paste("Warning: Missing required fields for add_element in rule", rule_index))
            next
          }
          
          # Handle function value
          value <- rule$value
          if (is.list(value) && "_function_" %in% names(value)) {
            func_code <- value[["_function_"]]
            addLog(paste("Evaluating function:", func_code), isDebug = TRUE)
            value <- evaluateFunction(func_code, list(root = input_json))
          }
          
          # Add the element with proper positioning
          path <- rule$path
          name <- rule$name
          position <- if ("position" %in% names(rule)) rule$position else NULL
          
          # Use our helper function
          output_json <- applyAddElement(output_json, path, name, value, position)
          
          addLog(paste("Applied add_element:", name, "to path:", path, 
                       if(!is.null(position)) paste("at position", position) else ""))
        } 
        # Modified section from shiny_prototype.R to properly handle function-based property values
        # Replace the existing code around line 580 with this implementation
        
        else if (rule$operation == "insert_property") {
          # Validate required fields
          if (!all(c("array_path", "property_name") %in% names(rule))) {
            addLog(paste("Warning: Missing required fields for insert_property in rule", rule_index))
            next
          }
          
          # Extract parameters
          array_path_input <- rule$array_path
          # Convert different formats to character vector
          array_path <- NULL
          
          if (is.character(array_path_input)) {
            # Case 1: String with dots
            if (length(array_path_input) == 1) {
              array_path <- unlist(strsplit(array_path_input, "\\."))
            } 
            # Case 2: Already a character vector
            else {
              array_path <- array_path_input
            }
          } 
          # Case 3: JSON-Array was loaded as a list
          else if (is.list(array_path_input)) {
            array_path <- as.character(unlist(array_path_input))
          }
          
          # Check if we have a valid character vector
          if (is.null(array_path) || length(array_path) == 0) {
            addLog("Error: Could not convert array_path to a valid character vector")
            next
          }
          
          property_name <- rule$property_name
          property_value <- rule$property_value
          position_type <- if("position_type" %in% names(rule)) rule$position_type else "last"
          position_ref <- if ("position_ref" %in% names(rule)) rule$position_ref else NULL
          
          # Debug logging
          if (input$debugMode) {
            addLog(paste("Debug - Original array_path:", 
                         if(is.character(array_path_input)) paste(array_path_input, collapse=", ") 
                         else "not a character vector"), isDebug = TRUE)
            addLog(paste("Debug - Converted array_path:", paste(array_path, collapse=", ")), isDebug = TRUE)
            addLog(paste("Debug - property_name:", property_name), isDebug = TRUE)
            addLog(paste("Debug - position_type:", position_type), isDebug = TRUE)
            if (!is.null(position_ref)) addLog(paste("Debug - position_ref:", position_ref), isDebug = TRUE)
          }
          
          # Handle function value
          # This is the key change: properly convert string functions to actual functions
          filter_fn <- NULL
          if (is.list(property_value) && "_function_" %in% names(property_value)) {
            func_code <- property_value[["_function_"]]
            addLog(paste("Function code is:", func_code), isDebug = TRUE)
            
            # Create an actual function that can be passed to insert_json_property
            property_value <- function(elem, index, parent, parent_index, root, ...) {
              # Create a safe environment for evaluation
              safe_env <- new.env(parent = baseenv())
              
              # Add necessary global functions
              safe_env$paste <- paste
              safe_env$paste0 <- paste0
              safe_env$round <- round
              safe_env$names <- names
              safe_env$length <- length
              
              # Add context variables to environment
              safe_env$elem <- elem
              safe_env$index <- index
              safe_env$parent <- parent
              safe_env$parent_index <- parent_index
              safe_env$root <- root
              
              # Evaluate the function body in this environment
              result <- NULL
              tryCatch({
                result <- eval(parse(text = func_code), envir = safe_env)
              }, error = function(e) {
                addLog(paste("Error in function evaluation:", e$message))
                result <- paste("Error:", e$message) # Fallback value
              })
              
              return(result)
            }
            
            addLog("Created dynamic function for property value", isDebug = TRUE)
          }
          
          # Apply transformation with careful error handling
          tryCatch({
            addLog("Attempting to call insert_json_property...", isDebug = TRUE)
            result <- insert_json_property(
              json_data = output_json,
              array_path = array_path,
              property_name = property_name,
              property_value = property_value,
              position_type = position_type,
              position_ref = position_ref,
              filter_fn = filter_fn,
              verbose = input$debugMode
            )
            
            # Only update if successful
            if (!is.null(result)) {
              output_json <- result
              addLog(paste("Applied insert_property:", property_name, "to array path:", 
                           paste(array_path, collapse = " > ")))
            } else {
              addLog("Warning: insert_json_property returned NULL", isDebug = TRUE)
            }
          }, error = function(e) {
            addLog(paste("Error in insert_json_property:", e$message))
          })
        }
        else if (rule$operation == "modify_array") {
          # Validate required fields
          if (!all(c("path", "action") %in% names(rule))) {
            addLog(paste("Warning: Missing required fields for modify_array in rule", rule_index))
            next
          }
          
          # Extract parameters
          path <- rule$path
          action <- rule$action
          value <- if ("value" %in% names(rule)) rule$value else NULL
          position <- if ("position" %in% names(rule)) rule$position else NULL
          filter <- if ("filter" %in% names(rule)) rule$filter else NULL
          
          # Handle function value
          if (is.list(value) && "_function_" %in% names(value)) {
            func_code <- value[["_function_"]]
            value <- evaluateFunction(func_code, list(root = input_json))
          }
          
          # Apply the transformation
          output_json <- modify_array(
            output_json,
            path,
            action,
            value,
            position,
            filter
          )
          
          addLog(paste("Applied modify_array:", action, "on path:", path))
        } 
        else if (rule$operation == "custom") {
          # Validiere erforderliche Felder
          if (!("code" %in% names(rule))) {
            addLog(paste("Warning: Missing required field 'code' for custom operation in rule", rule_index))
            next
          }
          
          # Konvertiere JSON zu JavaScript-String für JS-eval
          json_str <- toJSON(output_json, auto_unbox = TRUE)
          
          # Bereite den Code vor
          js_code <- paste0("(", rule$code, ")(", json_str, ")")
          
          # Führe den JavaScript-Code aus
          tryCatch({
            result_json <- NULL
            
            # Verwende V8-Engine wenn verfügbar, sonst vereinfachte Lösung
            if (requireNamespace("V8", quietly = TRUE)) {
              ctx <- V8::v8()
              result_json <- ctx$eval(js_code)
            } else {
              # Füge temporäre JavaScript-Funktion hinzu
              temp_js_file <- tempfile(fileext = ".js")
              writeLines(paste0("var result = ", js_code, ";\nconsole.log(JSON.stringify(result));"), temp_js_file)
              
              # Führe mit node.js aus, wenn verfügbar
              result_text <- system2("node", args = c(temp_js_file), stdout = TRUE)
              if (length(result_text) > 0) {
                result_json <- fromJSON(result_text[1], simplifyVector = FALSE)
              }
              
              # Lösche temporäre Datei
              unlink(temp_js_file)
            }
            
            if (!is.null(result_json)) {
              output_json <- result_json
              addLog(paste("Applied custom transformation with code"))
            } else {
              addLog(paste("Warning: Custom transformation returned NULL"))
            }
          }, error = function(e) {
            addLog(paste("Error in custom transformation:", e$message))
          })
        }
        else {
          addLog(paste("Warning: Unknown operation:", rule$operation))
        }
      }
      
      # Format and display the output
      output_json_str <- if (input$prettyOutput) {
        toJSON(output_json, pretty = TRUE, auto_unbox = TRUE)
      } else {
        toJSON(output_json, auto_unbox = TRUE)
      }
      
      updateAceEditor(session, "outputJson", value = output_json_str)
      addLog("Transformation completed successfully")
      
    }, error = function(e) {
      error_msg <- paste("Error during transformation:", e$message)
      showNotification(error_msg, type = "error")
      addLog(error_msg)
      
      if (input$showErrorDetails) {
        # Show error details in the output
        error_detail <- paste0('{\n  "error": "', e$message, '",\n  "stacktrace": "', 
                               paste(capture.output(traceback()), collapse = "\\n"), '"\n}')
        updateAceEditor(session, "outputJson", value = error_detail)
      }
    })
  })
  
  # Test button for debugging
  observeEvent(input$testBtn, {
    if (input$debugMode) {
      addLog("Running test function", isDebug = TRUE)
      
      # Create a test JSON object
      test_json <- fromJSON('{
        "user": {
          "id": "U12345",
          "name": "Max Mustermann",
          "email": "max@example.com",
          "profile": {
            "age": 28,
            "occupation": "Software Developer",
            "interests": ["coding", "hiking", "photography"]
          }
        }
      }', simplifyVector = FALSE)
      
      # Test insert_json_property with array of primitives
      addLog("Testing insert_json_property with array of primitives", isDebug = TRUE)
      
      # Test case 1: Transform array items to objects
      test_result <- insert_json_property(
        test_json,
        array_path = c("user", "profile", "interests"),
        property_name = "display_name",
        property_value = {"_function_": "return elem.charAt(0).toUpperCase() + elem.slice(1);"}
      )
      
      result_json <- toJSON(test_result, pretty = TRUE, auto_unbox = TRUE)
      addLog("Test result:", isDebug = TRUE)
      addLog(result_json, isDebug = TRUE)
      
      # Show test results in output
      updateAceEditor(session, "outputJson", value = result_json)
      showNotification("Test completed, see log for details", type = "message")
    }
  })
  
  # Save the current rule
  observeEvent(input$saveRule, {
    req(input$ruleName, input$rulesEditor)
    
    tryCatch({
      # Parse rules to validate JSON
      rule_content <- fromJSON(input$rulesEditor, simplifyVector = FALSE)
      
      # Create rule object
      new_rule <- list(
        id = paste0("rule_", format(Sys.time(), "%Y%m%d%H%M%S")),
        name = input$ruleName,
        description = input$ruleDescription,
        created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        content = input$rulesEditor
      )
      
      # Add to saved rules
      current_rules <- rules()
      rules(c(current_rules, list(new_rule)))
      
      # Update the dropdown
      updateRulesDropdown()
      
      showNotification("Rule saved successfully", type = "message")
      addLog(paste("Saved rule:", input$ruleName))
      
      # Clear inputs
      updateTextInput(session, "ruleName", value = "")
      updateTextAreaInput(session, "ruleDescription", value = "")
      
    }, error = function(e) {
      showNotification(paste("Error saving rule:", e$message), type = "error")
      addLog(paste("Error saving rule:", e$message))
    })
  })
  
  # Update rules dropdown
  updateRulesDropdown <- function() {
    saved_rules <- rules()
    
    if (length(saved_rules) > 0) {
      rule_names <- sapply(saved_rules, function(r) r$name)
      rule_ids <- sapply(saved_rules, function(r) r$id)
      
      choices <- c("None" = "none")
      for (i in seq_along(rule_names)) {
        choices[rule_names[i]] <- rule_ids[i]
      }
      
      updateSelectInput(session, "savedRules", choices = choices)
    }
  }
  
  # Load a saved rule
  observeEvent(input$savedRules, {
    rule_id <- input$savedRules
    
    if (rule_id != "none") {
      saved_rules <- rules()
      
      # Find the selected rule
      selected_rule <- NULL
      for (rule in saved_rules) {
        if (rule$id == rule_id) {
          selected_rule <- rule
          break
        }
      }
      
      if (!is.null(selected_rule)) {
        # Load the rule content into the editor
        updateAceEditor(session, "rulesEditor", value = selected_rule$content)
        addLog(paste("Loaded rule:", selected_rule$name))
      }
    }
  })
  
  # Display saved rules table
  output$savedRulesTable <- renderDT({
    saved_rules <- rules()
    
    if (length(saved_rules) > 0) {
      # Create data frame from saved rules
      df <- data.frame(
        Name = sapply(saved_rules, function(r) r$name),
        Description = sapply(saved_rules, function(r) r$description),
        Created = sapply(saved_rules, function(r) r$created),
        ID = sapply(saved_rules, function(r) r$id),
        stringsAsFactors = FALSE
      )
      
      datatable(df, selection = 'single', options = list(pageLength = 5))
    } else {
      datatable(data.frame(Message = "No saved rules"), options = list(dom = 't'))
    }
  })
  
  # Delete selected rule
  observeEvent(input$deleteRuleBtn, {
    selected <- input$savedRulesTable_rows_selected
    saved_rules <- rules()
    
    if (!is.null(selected) && length(saved_rules) >= selected) {
      # Get the rule to delete
      rule_to_delete <- saved_rules[[selected]]
      
      # Remove it from the list
      saved_rules <- saved_rules[-selected]
      rules(saved_rules)
      
      # Update the dropdown
      updateRulesDropdown()
      
      showNotification(paste("Rule deleted:", rule_to_delete$name), type = "message")
      addLog(paste("Deleted rule:", rule_to_delete$name))
    } else {
      showNotification("Please select a rule to delete", type = "warning")
    }
  })
  
  # Download transformed JSON
  output$downloadJson <- downloadHandler(
    filename = function() {
      paste0("transformed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      # Write the transformed JSON to the file
      writeLines(input$outputJson, file)
      addLog("Downloaded transformed JSON")
    }
  )
  
  # Copy to clipboard button
  observeEvent(input$copyBtn, {
    # In a real app, we'd use JavaScript to copy to clipboard
    showNotification("Copied to clipboard (simulated)", type = "message")
    addLog("Copied output to clipboard (simulated)")
  })
  
  # Initialize the app
  observeEvent(1, {
    addLog("Application initialized")
  }, once = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)