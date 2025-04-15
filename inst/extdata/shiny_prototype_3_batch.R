library(shiny)
library(shinydashboard)
library(shinyAce)
library(shinyWidgets)
library(DT)
library(jsonlite)
library(jsonr)  # JSON manipulation library
library(tools)
library(shinyjs)  # For additional JavaScript functionality

# Set maximum file size (in MB)
options(shiny.maxRequestSize = 100 * 1024^2)  # 100MB

# ============================================
# Helper functions
# ============================================

# Helper function to create a temporary processing directory
create_temp_dir <- function(prefix = "batch_process_") {
  temp_dir <- file.path(tempdir(), paste0(prefix, format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  return(temp_dir)
}

# Helper function to check if a path exists and is writable
is_valid_write_path <- function(path) {
  # Check if the path exists
  if (!dir.exists(path)) {
    return(list(valid = FALSE, message = "Directory does not exist"))
  }
  
  # Check if it's writable
  test_file <- file.path(path, ".test_write_permission")
  result <- tryCatch({
    write("test", test_file)
    file.remove(test_file)
    list(valid = TRUE, message = "Path is valid and writable")
  }, error = function(e) {
    list(valid = FALSE, message = paste("Path exists but is not writable:", e$message))
  })
  
  return(result)
}

# ============================================
# Helper functions for rule storage
# ============================================

# Set up storage directory
setupRuleStorage <- function() {
  # Use a fixed path in the user directory
  app_data_dir <- file.path(path.expand("~"), "jsonr_rules")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(app_data_dir)) {
    dir.create(app_data_dir, recursive = TRUE)
  }
  
  return(app_data_dir)
}

# Save a rule to a file
saveRule <- function(rule_name, rule_content, rule_storage_dir) {
  # Create a safe filename
  safe_name <- gsub("[^a-zA-Z0-9_-]", "_", rule_name)
  rule_file <- file.path(rule_storage_dir, paste0(safe_name, ".json"))
  
  # Save the rule to a file
  writeLines(rule_content, rule_file)
  
  return(rule_file)
}

# Load all saved rules
loadRules <- function(rule_storage_dir) {
  # Find all JSON files in the rule directory
  rule_files <- list.files(rule_storage_dir, pattern = "\\.json$", full.names = TRUE)
  
  # Load each rule
  rules <- list()
  for (file in rule_files) {
    # Extract rule names from filenames
    rule_name <- basename(file)
    rule_name <- sub("\\.json$", "", rule_name)
    
    # Read the content
    content <- paste(readLines(file, warn = FALSE), collapse = "\n")
    
    # Add to the list
    rules[[rule_name]] <- content
  }
  
  return(rules)
}

# Delete a rule
deleteRule <- function(rule_name, rule_storage_dir) {
  # Create a safe filename
  safe_name <- gsub("[^a-zA-Z0-9_-]", "_", rule_name)
  rule_file <- file.path(rule_storage_dir, paste0(safe_name, ".json"))
  
  # Delete the file if it exists
  if (file.exists(rule_file)) {
    file.remove(rule_file)
    return(TRUE)
  }
  
  return(FALSE)
}

# ============================================
# Function to process JSON files
# ============================================

process_json_file <- function(input_file, output_file, transformation_rules) {
  # Read the input JSON
  tryCatch({
    # Read the JSON file
    json_data <- fromJSON(input_file, simplifyVector = FALSE)
    
    # Convert transformation rules from text to JSON
    rules <- fromJSON(transformation_rules, simplifyVector = FALSE)
    
    # Apply rules
    result <- json_data
    for (rule in rules) {
      if (rule$operation == "add_element" && all(c("path", "name") %in% names(rule))) {
        # Extract values from the rule
        path <- rule$path
        name <- rule$name
        value <- rule$value
        position <- if ("position" %in% names(rule)) rule$position else NULL
        
        # Replace placeholder for timestamp
        if (is.character(value) && value == "timestamp_placeholder") {
          value <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        }
        
        # Apply add_json_element
        result <- add_json_element(
          result, 
          path, 
          name, 
          value,
          position = position,
          create_path = TRUE
        )
      } 
      else if (rule$operation == "insert_property" && all(c("array_path", "property_name") %in% names(rule))) {
        # Extract parameters
        array_path <- rule$array_path
        property_name <- rule$property_name
        
        # Determine the property_value from the rule
        raw_property_value <- if ("property_value" %in% names(rule)) {
          rule$property_value
        } else if ("value" %in% names(rule)) {
          # Fallback for compatibility
          rule$value
        } else {
          NULL  # Fallback value if none is found
        }
        
        # Check if it's a function
        is_function <- FALSE
        function_code <- NULL
        
        if (is.list(raw_property_value) && length(raw_property_value) > 0 && "_function_" %in% names(raw_property_value)) {
          is_function <- TRUE
          function_code <- raw_property_value[["_function_"]]
          cat("Function code detected:", function_code, "\n")
        }
        
        # Convert array_path to the correct format
        if (is.character(array_path) && length(array_path) == 1) {
          array_path <- unlist(strsplit(array_path, "\\."))
        } else if (is.list(array_path)) {
          # If array_path is provided as a list, convert to vector
          array_path <- as.character(unlist(array_path))
        }
        
        # Extract additional parameters
        position_type <- if("position_type" %in% names(rule)) rule$position_type else "last"
        position_ref <- if ("position_ref" %in% names(rule)) rule$position_ref else NULL
        
        # Create a filter_fn if necessary
        filter_fn <- NULL
        if ("filter" %in% names(rule) && !is.null(rule$filter)) {
          filter_code <- rule$filter
          if (is.list(filter_code) && "_function_" %in% names(filter_code)) {
            filter_code <- filter_code[["_function_"]]
            filter_fn <- function(elem, index, parent, parent_index, root) {
              # Create a safe environment for evaluation
              safe_env <- new.env(parent = baseenv())
              
              # Add necessary global functions
              safe_env$paste <- paste
              safe_env$paste0 <- paste0
              safe_env$names <- names
              safe_env$length <- length
              safe_env$c <- c
              safe_env$list <- list
              
              # Add context variables
              safe_env$elem <- elem
              safe_env$index <- index
              safe_env$parent <- parent
              safe_env$parent_index <- parent_index
              safe_env$root <- root
              
              # Evaluation
              filter_result <- FALSE
              tryCatch({
                filter_result <- eval(parse(text = filter_code), envir = safe_env)
              }, error = function(e) {
                cat("Error in function evaluation (filter):", e$message, "\n")
              })
              
              return(filter_result)
            }
          }
        }
        
        # Create the right property_value function if required
        if (is_function) {
          property_value <- function(elem, index, parent, parent_index, root) {
            # Create a safe environment for evaluation
            safe_env <- new.env(parent = baseenv())
            
            # Add necessary global functions
            safe_env$paste <- paste
            safe_env$paste0 <- paste0
            safe_env$round <- round
            safe_env$names <- names
            safe_env$length <- length
            safe_env$c <- c
            safe_env$list <- list
            safe_env$ifelse <- ifelse
            safe_env$is.null <- is.null
            safe_env$is.na <- is.na
            safe_env$return <- return
            
            # Add context variables
            safe_env$elem <- elem
            safe_env$index <- index
            safe_env$parent <- parent
            safe_env$parent_index <- parent_index
            safe_env$root <- root
            
            # Evaluation
            result_value <- NULL
            tryCatch({
              result_value <- eval(parse(text = function_code), envir = safe_env)
              cat("Function evaluation successful, result:", result_value, "\n")
            }, error = function(e) {
              cat("Error in function evaluation:", e$message, "\n")
              # Try with a simpler version
              tryCatch({
                # Try to evaluate with try environment
                result_value <<- eval(parse(text = paste("tryCatch({", function_code, "}, error=function(e) NULL)")))
              }, error = function(e2) {
                cat("Simplified evaluation also failed:", e2$message, "\n")
              })
            })
            
            return(result_value)
          }
        } else {
          # Use the value directly
          property_value <- raw_property_value
        }
        
        # Enable verbose mode for debugging
        verbose <- TRUE
        
        # Apply insert_json_property
        tryCatch({
          cat("Calling insert_json_property with the following parameters:\n")
          cat("- array_path:", paste(array_path, collapse=", "), "\n")
          cat("- property_name:", property_name, "\n")
          cat("- position_type:", position_type, "\n")
          cat("- is property_value a function?", is.function(property_value), "\n")
          
          result <- insert_json_property(
            json_data = result,
            array_path = array_path,
            property_name = property_name,
            property_value = property_value,
            position_type = position_type,
            position_ref = position_ref,
            filter_fn = filter_fn,
            verbose = verbose
          )
          
          cat("insert_json_property executed successfully\n")
        }, error = function(e) {
          cat("Error in insert_json_property:", e$message, "\n")
        })
      }
      # Additional rule types can be added here
    }
    
    # Write the transformed JSON to the output file
    write_json(result, output_file, pretty = TRUE, auto_unbox = TRUE)
    return(list(success = TRUE, message = "File processed successfully"))
  }, error = function(e) {
    cat("Error processing the file:", e$message, "\n")
    return(list(success = FALSE, message = paste("Error processing the file:", e$message)))
  })
}

# ============================================
# UI Definition
# ============================================
ui <- dashboardPage(
  dashboardHeader(title = "Batch JSON Processor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload & Processing", tabName = "fileUpload", icon = icon("upload")),
      menuItem("Results & Download", tabName = "results", icon = icon("download")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    # JavaScript for custom functions
    useShinyjs(),
    
    tabItems(
      # File Upload Tab
      tabItem(tabName = "fileUpload",
              fluidRow(
                box(
                  title = "Upload Files", width = 6, status = "primary",
                  fileInput("uploadFiles", "Select JSON files to process",
                            multiple = TRUE,
                            accept = c("application/json", ".json")),
                  helpText("You can select multiple files (up to 100MB total)"),
                  
                  hr(),
                  
                  # Rule selection
                  div(
                    h4("Transformation Rules"),
                    selectInput("ruleSet", "Select rule:",
                                choices = c("Default Rules" = "default",
                                            "Custom Rules" = "custom")),
                    
                    conditionalPanel(
                      condition = "input.ruleSet == 'custom'",
                      aceEditor("customRules", mode = "json", theme = "chrome", height = "200px",
                                value = '[\n  {\n    "operation": "add_element",\n    "path": "",\n    "name": "metadata",\n    "value": {"processed": true, "timestamp": "timestamp_placeholder"}\n  }\n]')
                    ),
                    
                    hr()
                  ),
                  
                  radioButtons("outputOption", "Output Option:",
                               choices = c("Download as ZIP" = "download",
                                           "Save to Directory" = "directory")),
                  
                  conditionalPanel(
                    condition = "input.outputOption == 'directory'",
                    textInput("outputDir", "Output Directory Path:", 
                              value = path.expand("~/processed_json")),
                    actionButton("validatePath", "Validate Path")
                  ),
                  
                  hr(),
                  
                  actionButton("processFiles", "Process Files", 
                               icon = icon("play"))
                ),
                
                box(
                  title = "Upload Status", width = 6, status = "info",
                  uiOutput("uploadStatus"),
                  hr(),
                  verbatimTextOutput("processingLog"),
                  conditionalPanel(
                    condition = "input.processFiles != 0",
                    progressBar(id = "progressBar", value = 0, display_pct = TRUE)
                  )
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                box(
                  title = "Processing Results", width = 12, status = "success",
                  DT::dataTableOutput("resultsTable"),
                  hr(),
                  downloadButton("downloadAllResults", "Download All Results (ZIP)"),
                  actionButton("openOutputDir", "Open Output Directory")
                )
              ),
              fluidRow(
                box(
                  title = "File Preview", width = 12, status = "info",
                  fluidRow(
                    column(6,
                           h4("Original JSON"), 
                           selectInput("previewOriginal", "Select File:", choices = NULL),
                           aceEditor("originalPreview", mode = "json", theme = "chrome", 
                                     height = "400px", readOnly = TRUE)
                    ),
                    column(6,
                           h4("Processed JSON"), 
                           selectInput("previewProcessed", "Select File:", choices = NULL),
                           aceEditor("processedPreview", mode = "json", theme = "chrome", 
                                     height = "400px", readOnly = TRUE)
                    )
                  )
                )
              )
      ),
      
      # Settings Tab
      tabItem(tabName = "settings",
              fluidRow(
                box(
                  title = "General Settings", width = 6,
                  numericInput("maxRequestSize", "Maximum Upload Size (MB):", 
                               value = 100, min = 1, max = 1000),
                  actionButton("applySettings", "Apply Settings"),
                  hr(),
                  h4("Default Output Directory"),
                  textInput("defaultOutputDir", "Default Path:", 
                            value = path.expand("~/processed_json")),
                  actionButton("validateDefaultPath", "Validate & Save Default Path")
                ),
                
                box(
                  title = "Default Transformation Rules", width = 6,
                  aceEditor("defaultRules", mode = "json", theme = "chrome", height = "200px",
                            value = '[\n  {\n    "operation": "add_element",\n    "path": "",\n    "name": "processed_by",\n    "value": "Batch JSON Processor"\n  },\n  {\n    "operation": "add_element",\n    "path": "",\n    "name": "processed_at",\n    "value": "timestamp_placeholder"\n  }\n]'),
                  actionButton("saveDefaultRules", "Save Default Rules")
                )
              ),
              
              # Rule Management (moved to settings)
              fluidRow(
                box(
                  title = "Rule Management", width = 12, status = "primary",
                  fluidRow(
                    column(4,
                           textInput("ruleName", "Rule Name:"),
                           actionButton("saveRuleBtn", "Save Current Rule"),
                           actionButton("loadRuleBtn", "Load Saved Rule"),
                           actionButton("deleteRuleBtn", "Delete Rule")
                    ),
                    column(8,
                           uiOutput("savedRulesList")
                    )
                  )
                )
              )
      )
    )
  )
)

# ============================================
# Server Logic
# ============================================
server <- function(input, output, session) {
  # Reactive values to store state
  values <- reactiveValues(
    uploadedFiles = NULL,
    processingResults = NULL,
    outputDirectory = NULL,
    processingLog = character(),
    defaultRules = NULL,
    lastProcessedBatch = NULL,
    savedRules = NULL,
    ruleToDelete = NULL
  )
  
  # Initialize the rule storage directory
  rule_storage_dir <- setupRuleStorage()
  
  # Default transformation rules
  observeEvent(1, {
    values$defaultRules <- '[\n  {\n    "operation": "add_element",\n    "path": "",\n    "name": "processed_by",\n    "value": "Batch JSON Processor"\n  },\n  {\n    "operation": "add_element",\n    "path": "",\n    "name": "processed_at",\n    "value": "timestamp_placeholder"\n  }\n]'
    
    # Set default rules in the editor
    updateAceEditor(session, "defaultRules", value = values$defaultRules)
    
    # Load saved rules
    values$savedRules <- loadRules(rule_storage_dir)
    
    addLog(paste("App initialized. Rule storage directory:", rule_storage_dir))
  }, once = TRUE)
  
  # Log function
  addLog <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    values$processingLog <- c(paste(timestamp, "-", message), values$processingLog)
  }
  
  # Display processing log
  output$processingLog <- renderText({
    paste(values$processingLog, collapse = "\n")
  })
  
  # Process file uploads
  observeEvent(input$uploadFiles, {
    # Check if files were uploaded
    if (is.null(input$uploadFiles)) {
      return()
    }
    
    # Store the uploaded files
    values$uploadedFiles <- input$uploadFiles
    
    # Update the upload status
    addLog(paste("Uploaded:", nrow(values$uploadedFiles), "files."))
  })
  
  # Display upload status
  output$uploadStatus <- renderUI({
    if (is.null(values$uploadedFiles)) {
      return(div(
        h4("No files uploaded yet."),
        p("Upload JSON files to begin.")
      ))
    } else {
      files_info <- values$uploadedFiles
      total_size <- sum(files_info$size) / 1024^2  # in MB
      
      return(div(
        h4(paste("Uploaded:", nrow(files_info), "files.")),
        p(paste("Total size:", round(total_size, 2), "MB")),
        tags$ul(
          lapply(1:nrow(files_info), function(i) {
            file_info <- files_info[i, ]
            tags$li(
              paste0(file_info$name, " (", round(file_info$size / 1024, 1), " KB)")
            )
          })
        )
      ))
    }
  })
  
  # Validate output directory path
  observeEvent(input$validatePath, {
    req(input$outputDir)
    path_result <- is_valid_write_path(input$outputDir)
    
    if (path_result$valid) {
      showNotification(paste("Valid path:", input$outputDir), type = "message")
      values$outputDirectory <- input$outputDir
    } else {
      # Try to create the directory if it doesn't exist
      if (!dir.exists(input$outputDir)) {
        dir_created <- dir.create(input$outputDir, recursive = TRUE, showWarnings = FALSE)
        if (dir_created) {
          showNotification(paste("Directory created:", input$outputDir), type = "message")
          values$outputDirectory <- input$outputDir
        } else {
          showNotification(paste("Error:", path_result$message), type = "error")
        }
      } else {
        showNotification(paste("Error:", path_result$message), type = "error")
      }
    }
  })
  
  # Process files button
  observeEvent(input$processFiles, {
    req(values$uploadedFiles)
    
    # Disable the process button during processing
    shinyjs::disable("processFiles")
    
    # Create temporary directories for processing
    temp_dir <- create_temp_dir()
    input_dir <- file.path(temp_dir, "input")
    output_dir <- file.path(temp_dir, "output")
    dir.create(input_dir, recursive = TRUE)
    dir.create(output_dir, recursive = TRUE)
    
    # Store the temporary directory for later
    values$lastProcessedBatch <- list(
      temp_dir = temp_dir,
      input_dir = input_dir,
      output_dir = output_dir
    )
    
    # Clear previous results
    values$processingResults <- NULL
    
    # Copy uploaded files to the input directory
    files_info <- values$uploadedFiles
    for (i in 1:nrow(files_info)) {
      file.copy(files_info$datapath[i], file.path(input_dir, files_info$name[i]))
    }
    
    # Get transformation rules
    rules <- if (input$ruleSet == "custom") {
      input$customRules
    } else {
      values$defaultRules
    }
    
    # Validate the rules - this is important
    tryCatch({
      test_rules <- fromJSON(rules, simplifyVector = FALSE)
      addLog("Transformation rules successfully validated")
    }, error = function(e) {
      addLog(paste("ERROR: Invalid JSON rules:", e$message))
      showNotification("Invalid JSON rules! Check the syntax.", type = "error")
      shinyjs::enable("processFiles")
      return()
    })
    
    # Replace placeholder for timestamp
    rules <- gsub("timestamp_placeholder", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), rules)
    
    # For progress tracking
    total_files <- nrow(files_info)
    
    # Process each file
    results <- data.frame(
      original_file = character(),
      processed_file = character(),
      status = character(),
      message = character(),
      stringsAsFactors = FALSE
    )
    
    # Log processing start
    addLog(paste("Starting to process", total_files, "files"))
    
    for (i in 1:total_files) {
      # Update progress
      updateProgressBar(session, "progressBar", value = (i - 1) / total_files * 100)
      
      # Get file info
      file_info <- files_info[i, ]
      input_file <- file.path(input_dir, file_info$name)
      output_file <- file.path(output_dir, file_info$name)
      
      # Log processing step
      addLog(paste("Processing file", i, "of", total_files, ":", file_info$name))
      
      # Process the file
      process_result <- process_json_file(input_file, output_file, rules)
      
      # Add to results
      results <- rbind(results, data.frame(
        original_file = file_info$name,
        processed_file = file_info$name,
        status = if (process_result$success) "Success" else "Error",
        message = process_result$message,
        stringsAsFactors = FALSE
      ))
      
      # Log result
      addLog(paste("  Result:", process_result$message))
    }
    
    # Update progress to 100%
    updateProgressBar(session, "progressBar", value = 100)
    
    # Process output based on user selection
    if (input$outputOption == "directory") {
      req(values$outputDirectory)
      
      # Copy processed files to the output directory
      for (i in 1:nrow(results)) {
        if (results$status[i] == "Success") {
          file.copy(
            file.path(output_dir, results$processed_file[i]),
            file.path(values$outputDirectory, results$processed_file[i]),
            overwrite = TRUE
          )
        }
      }
      
      addLog(paste("Files saved to:", values$outputDirectory))
    } else {
      # For the download option, we'll create a ZIP file when the user clicks download
      addLog("Files prepared for download")
    }
    
    # Store the results
    values$processingResults <- results
    
    # Update the preview dropdown menus
    updateSelectInput(session, "previewOriginal", 
                      choices = results$original_file)
    updateSelectInput(session, "previewProcessed", 
                      choices = results$processed_file)
    
    # Enable the process button
    shinyjs::enable("processFiles")
    
    # Switch to results tab
    updateTabItems(session, "sidebarMenu", "results")
  })
  
  # Display results table
  output$resultsTable <- renderDT({
    req(values$processingResults)
    datatable(values$processingResults, options = list(pageLength = 10))
  })
  
  # File preview functionality (Original)
  observeEvent(input$previewOriginal, {
    req(input$previewOriginal, values$lastProcessedBatch)
    
    file_path <- file.path(values$lastProcessedBatch$input_dir, input$previewOriginal)
    
    if (file.exists(file_path)) {
      content <- readLines(file_path, warn = FALSE)
      content <- paste(content, collapse = "\n")
      updateAceEditor(session, "originalPreview", value = content)
      
      # Also update the processed file selection to match
      updateSelectInput(session, "previewProcessed", selected = input$previewOriginal)
    } else {
      updateAceEditor(session, "originalPreview", value = "File not found")
    }
  })
  
  # File preview functionality (Processed)
  observeEvent(input$previewProcessed, {
    req(input$previewProcessed, values$lastProcessedBatch)
    
    file_path <- file.path(values$lastProcessedBatch$output_dir, input$previewProcessed)
    
    if (file.exists(file_path)) {
      content <- readLines(file_path, warn = FALSE)
      content <- paste(content, collapse = "\n")
      updateAceEditor(session, "processedPreview", value = content)
      
      # Also update the original file selection to match
      updateSelectInput(session, "previewOriginal", selected = input$previewProcessed)
    } else {
      updateAceEditor(session, "processedPreview", value = "File not found")
    }
  })
  
  # Download all results as ZIP file
  output$downloadAllResults <- downloadHandler(
    filename = function() {
      paste0("processed_json_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(values$lastProcessedBatch, values$processingResults)
      
      # Get the path to the output directory
      output_dir <- values$lastProcessedBatch$output_dir
      
      # Create a temporary directory for zip creation
      # This is needed to handle Windows path issues
      tmp_zip_dir <- file.path(tempdir(), paste0("zip_temp_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(tmp_zip_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Use a mapping file to track original filenames
      files_to_zip <- list.files(output_dir, full.names = FALSE)
      
      # Copy files to temp directory with their original names
      for (filename in files_to_zip) {
        src_file <- file.path(output_dir, filename)
        # Create a safe filename by replacing problematic characters
        safe_filename <- gsub(":", "_", filename) # Replace colons with underscores
        dst_file <- file.path(tmp_zip_dir, safe_filename)
        file.copy(src_file, dst_file)
      }
      
      # Create the ZIP file with original filenames
      curr_dir <- getwd()
      setwd(tmp_zip_dir)
      
      tryCatch({
        # Use utils::zip for Windows compatibility
        utils::zip(file, list.files(tmp_zip_dir))
        addLog(paste("ZIP file created with original filenames:", file))
      }, error = function(e) {
        addLog(paste("Error creating ZIP file:", e$message))
        showNotification("Error creating ZIP file", type = "error")
      }, finally = {
        # Restore working directory and clean up
        setwd(curr_dir)
        unlink(tmp_zip_dir, recursive = TRUE)
      })
    }
  )
  
  # Open output directory button
  observeEvent(input$openOutputDir, {
    req(values$outputDirectory)
    
    # Try to open the directory with the appropriate command for the OS
    success <- FALSE
    
    if (.Platform$OS.type == "windows") {
      shell.exec(values$outputDirectory)
      success <- TRUE
    } else if (Sys.info()["sysname"] == "Darwin") {  # macOS
      system2("open", values$outputDirectory)
      success <- TRUE
    } else {  # Linux
      system2("xdg-open", values$outputDirectory)
      success <- TRUE
    }
    
    if (success) {
      addLog(paste("Output directory opened:", values$outputDirectory))
    } else {
      showNotification("Could not open directory", type = "error")
      addLog(paste("Error opening directory:", values$outputDirectory))
    }
  })
  
  # Validate default output directory
  observeEvent(input$validateDefaultPath, {
    req(input$defaultOutputDir)
    path_result <- is_valid_write_path(input$defaultOutputDir)
    
    if (path_result$valid) {
      showNotification(paste("Valid default path:", input$defaultOutputDir), type = "message")
      values$outputDirectory <- input$defaultOutputDir
      updateTextInput(session, "outputDir", value = input$defaultOutputDir)
    } else {
      # Try to create the directory if it doesn't exist
      if (!dir.exists(input$defaultOutputDir)) {
        dir_created <- dir.create(input$defaultOutputDir, recursive = TRUE, showWarnings = FALSE)
        if (dir_created) {
          showNotification(paste("Default directory created:", input$defaultOutputDir), type = "message")
          values$outputDirectory <- input$defaultOutputDir
          updateTextInput(session, "outputDir", value = input$defaultOutputDir)
        } else {
          showNotification(paste("Error:", path_result$message), type = "error")
        }
      } else {
        showNotification(paste("Error:", path_result$message), type = "error")
      }
    }
  })
  
  # Save default rules
  observeEvent(input$saveDefaultRules, {
    req(input$defaultRules)
    # Validate the rules before saving
    tryCatch({
      test_rules <- fromJSON(input$defaultRules, simplifyVector = FALSE)
      values$defaultRules <- input$defaultRules
      showNotification("Default rules saved", type = "message")
      addLog("Default transformation rules updated")
    }, error = function(e) {
      showNotification(paste("Invalid JSON rules:", e$message), type = "error")
      addLog(paste("Error saving default rules:", e$message))
    })
  })
  
  # Apply settings
  observeEvent(input$applySettings, {
    req(input$maxRequestSize)
    options(shiny.maxRequestSize = input$maxRequestSize * 1024^2)
    showNotification(paste("Maximum upload size set to", input$maxRequestSize, "MB"), type = "message")
    addLog(paste("Maximum upload size updated:", input$maxRequestSize, "MB"))
  })
  
  # ============================================
  # Rule management functions
  # ============================================
  
  # Display saved rules
  output$savedRulesList <- renderUI({
    rules <- values$savedRules
    
    if (is.null(rules) || length(rules) == 0) {
      return(p("No saved rules available."))
    }
    
    # Create a list of saved rules
    tagList(
      h4("Saved Rules:"),
      tags$ul(
        lapply(names(rules), function(rule_name) {
          tags$li(
            tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
              tags$span(rule_name),
              tags$div(
                actionButton(
                  inputId = paste0("load_", rule_name),
                  label = "Load",
                  onclick = sprintf("Shiny.setInputValue('loadRuleName', '%s');", rule_name)
                ),
                actionButton(
                  inputId = paste0("delete_", rule_name),
                  label = "Delete",
                  onclick = sprintf("Shiny.setInputValue('deleteRuleName', '%s');", rule_name)
                )
              )
            )
          )
        })
      )
    )
  })
  
  # Save the current rule
  observeEvent(input$saveRuleBtn, {
    req(input$ruleName, input$customRules)
    
    tryCatch({
      # Validate the rule
      parsed <- fromJSON(input$customRules, simplifyVector = FALSE)
      
      # Save the rule
      saveRule(input$ruleName, input$customRules, rule_storage_dir)
      
      # Update the list of saved rules
      values$savedRules <- loadRules(rule_storage_dir)
      
      # Show confirmation
      showNotification(paste("Rule", input$ruleName, "saved"), type = "message")
      addLog(paste("Rule saved:", input$ruleName))
      
    }, error = function(e) {
      showNotification(paste("Error saving rule:", e$message), type = "error")
      addLog(paste("Error saving rule:", e$message))
    })
  })
  
  # Load a saved rule
  observeEvent(input$loadRuleName, {
    rule_name <- input$loadRuleName
    rules <- values$savedRules
    
    if (!is.null(rules) && rule_name %in% names(rules)) {
      # Set the rule name
      updateTextInput(session, "ruleName", value = rule_name)
      
      # Set the rule content
      updateAceEditor(session, "customRules", value = rules[[rule_name]])
      
      # Select custom rules
      updateSelectInput(session, "ruleSet", selected = "custom")
      
      showNotification(paste("Rule", rule_name, "loaded"), type = "message")
      addLog(paste("Rule loaded:", rule_name))
    }
  })
  
  # Delete a saved rule
  observeEvent(input$deleteRuleName, {
    rule_name <- input$deleteRuleName
    
    # Show confirmation dialog
    showModal(modalDialog(
      title = "Delete Rule",
      paste("Are you sure you want to delete the rule", rule_name, "?"),
      footer = tagList(
        actionButton("confirmDeleteRule", "Yes, delete"),
        modalButton("Cancel")
      )
    ))
    
    # Store the rule name to delete
    values$ruleToDelete <- rule_name
  })
  
  # Confirm rule deletion
  observeEvent(input$confirmDeleteRule, {
    removeModal()
    
    rule_name <- values$ruleToDelete
    if (!is.null(rule_name)) {
      # Delete the rule
      success <- deleteRule(rule_name, rule_storage_dir)
      
      if (success) {
        # Update the list of saved rules
        values$savedRules <- loadRules(rule_storage_dir)
        
        showNotification(paste("Rule", rule_name, "deleted"), type = "message")
        addLog(paste("Rule deleted:", rule_name))
      } else {
        showNotification(paste("Error deleting rule", rule_name), type = "error")
      }
    }
  })
  
  # Load rule button
  observeEvent(input$loadRuleBtn, {
    # Show a modal with all saved rules
    rules <- values$savedRules
    
    if (is.null(rules) || length(rules) == 0) {
      showNotification("No saved rules available", type = "warning")
      return()
    }
    
    # Create a list of buttons to load rules
    rule_buttons <- lapply(names(rules), function(rule_name) {
      actionButton(
        inputId = paste0("modal_load_", rule_name),
        label = rule_name,
        onclick = sprintf("Shiny.setInputValue('loadRuleName', '%s'); $('#shiny-modal').modal('hide');", rule_name)
      )
    })
    
    showModal(modalDialog(
      title = "Load Rule",
      tagList(
        p("Select a rule to load:"),
        rule_buttons
      ),
      footer = modalButton("Close")
    ))
  })
  
  # Delete rule button
  observeEvent(input$deleteRuleBtn, {
    # Show a modal with all saved rules
    rules <- values$savedRules
    
    if (is.null(rules) || length(rules) == 0) {
      showNotification("No saved rules available", type = "warning")
      return()
    }
    
    # Create a list of buttons to delete rules
    rule_buttons <- lapply(names(rules), function(rule_name) {
      actionButton(
        inputId = paste0("modal_delete_", rule_name),
        label = rule_name,
        onclick = sprintf("Shiny.setInputValue('deleteRuleName', '%s'); $('#shiny-modal').modal('hide');", rule_name)
      )
    })
    
    showModal(modalDialog(
      title = "Delete Rule",
      tagList(
        p("Select a rule to delete:"),
        rule_buttons
      ),
      footer = modalButton("Cancel")
    ))
  })
}

# Start the Shiny app
shinyApp(ui = ui, server = server)
      