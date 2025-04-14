library(shiny)
library(shinydashboard)
library(jsonlite)
library(shinyAce)
library(DT)
library(jsonr)
library(shinyjs)

# ============================================
# Helper functions for rule storage
# ============================================

# Set up storage directory
setupRuleStorage <- function() {
  # Prüfe zuerst, ob eine Umgebungsvariable gesetzt ist
  env_path <- Sys.getenv("JSONR_STORAGE_PATH", unset = NA)
  
  if (!is.na(env_path)) {
    # Verwende den Pfad aus der Umgebungsvariable
    app_data_dir <- env_path
  } else {
    # Fallback auf OS-spezifische Standardpfade
    if (.Platform$OS.type == "windows") {
      base_dir <- Sys.getenv("APPDATA")
    } else {
      base_dir <- path.expand("~")
    }
    app_data_dir <- file.path(base_dir, "jsonr_rules")
  }
  
  # Normalisiere den Pfad für das aktuelle Betriebssystem
  app_data_dir <- normalizePath(app_data_dir, mustWork = FALSE)
  
  # Erstelle das Verzeichnis, falls es nicht existiert
  if (!dir.exists(app_data_dir)) {
    dir.create(app_data_dir, recursive = TRUE)
  }
  
  # Erstelle Indexdatei, falls sie nicht existiert
  index_file <- file.path(app_data_dir, "rule_index.json")
  if (!file.exists(index_file)) {
    writeLines('{"rules": []}', index_file)
  }
  
  return(app_data_dir)
}

# Save a rule to a file
saveRuleToFile <- function(rule, app_data_dir) {
  # Stellen Sie sicher, dass der Pfad korrekt formatiert ist
  app_data_dir <- normalizePath(app_data_dir, mustWork = TRUE)
  
  # Generiere einen Dateinamen basierend auf der Regel-ID
  rule_file <- file.path(app_data_dir, paste0(rule$id, ".json"))
  
  # Konvertiere die Regel zu JSON
  rule_json <- toJSON(rule, pretty = TRUE, auto_unbox = TRUE)
  
  # Speichere die Regel in eine Datei
  tryCatch({
    writeLines(rule_json, rule_file)
  }, error = function(e) {
    stop(paste("Fehler beim Speichern der Regel in Datei:", rule_file, "-", e$message))
  })
  
  # Aktualisiere den Index
  updateRuleIndex(rule, app_data_dir)
  
  return(rule_file)
}

# Update the index file with rule metadata
updateRuleIndex <- function(rule, app_data_dir) {
  index_file <- file.path(app_data_dir, "rule_index.json")
  
  # Read the current index
  index <- fromJSON(index_file, simplifyVector = FALSE)
  
  # Create rule metadata for the index
  rule_meta <- list(
    id = rule$id,
    name = rule$name,
    description = rule$description,
    created = rule$created,
    modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # Make sure index$rules exists and is a list
  if (!("rules" %in% names(index)) || !is.list(index$rules)) {
    index$rules <- list()
  }
  
  # Check if the rule already exists in the index
  if (length(index$rules) > 0) {
    # Safer version with explicit loop
    existing_index <- NULL
    for (i in seq_along(index$rules)) {
      if (is.list(index$rules[[i]]) && 
          "id" %in% names(index$rules[[i]]) && 
          index$rules[[i]]$id == rule$id) {
        existing_index <- i
        break
      }
    }
  } else {
    existing_index <- NULL
  }
  
  if (!is.null(existing_index)) {
    # Update existing entry
    index$rules[[existing_index]] <- rule_meta
  } else {
    # Add new entry
    index$rules <- c(index$rules, list(rule_meta))
  }
  
  # Write the updated index
  writeLines(toJSON(index, pretty = TRUE, auto_unbox = TRUE), index_file)
}

# Load all rules from the file system
loadRulesFromFiles <- function(app_data_dir) {
  index_file <- file.path(app_data_dir, "rule_index.json")
  
  # Check if the index exists
  if (!file.exists(index_file)) {
    return(list())
  }
  
  # Read the index
  index <- fromJSON(index_file, simplifyVector = FALSE)
  
  # Load all rules
  rules_list <- list()
  for (rule_meta in index$rules) {
    rule_file <- file.path(app_data_dir, paste0(rule_meta$id, ".json"))
    
    if (file.exists(rule_file)) {
      rule <- fromJSON(readLines(rule_file, warn = FALSE), simplifyVector = FALSE)
      rules_list <- c(rules_list, list(rule))
    }
  }
  
  return(rules_list)
}

# Load a specific rule by its ID
loadRuleById <- function(rule_id, app_data_dir) {
  rule_file <- file.path(app_data_dir, paste0(rule_id, ".json"))
  
  if (file.exists(rule_file)) {
    return(fromJSON(readLines(rule_file, warn = FALSE), simplifyVector = FALSE))
  } else {
    return(NULL)
  }
}

# Delete a rule by its ID
deleteRuleById <- function(rule_id, app_data_dir) {
  rule_file <- file.path(app_data_dir, paste0(rule_id, ".json"))
  
  # Remove the file if it exists
  if (file.exists(rule_file)) {
    file.remove(rule_file)
  }
  
  # Update the index
  index_file <- file.path(app_data_dir, "rule_index.json")
  index <- fromJSON(index_file, simplifyVector = FALSE)
  
  # Find the rule in the index
  rule_index <- which(sapply(index$rules, function(r) r$id == rule_id))
  
  if (length(rule_index) > 0) {
    # Remove the rule from the index
    index$rules <- index$rules[-rule_index]
    
    # Write the updated index
    writeLines(toJSON(index, pretty = TRUE, auto_unbox = TRUE), index_file)
    return(TRUE)
  }
  
  return(FALSE)
}

# Validate a rule before saving
validateRule <- function(rule_content) {
  # Basic validation - check if it's valid JSON
  tryCatch({
    parsed <- fromJSON(rule_content, simplifyVector = FALSE)
    
    # Check if it's an array of rule objects
    if (!is.list(parsed) || length(parsed) == 0) {
      return(list(valid = FALSE, message = "Rules must be a non-empty array"))
    }
    
    # Check if each rule has the required fields
    for (i in seq_along(parsed)) {
      rule <- parsed[[i]]
      
      if (!("operation" %in% names(rule))) {
        return(list(valid = FALSE, message = paste0("Rule #", i, " is missing the 'operation' field")))
      }
      
      # Specific validation for each operation type
      if (rule$operation == "add_element") {
        if (!all(c("path", "name") %in% names(rule))) {
          return(list(valid = FALSE, message = paste0("Rule #", i, " (add_element) is missing required fields")))
        }
      } else if (rule$operation == "insert_property") {
        if (!all(c("array_path", "property_name") %in% names(rule))) {
          return(list(valid = FALSE, message = paste0("Rule #", i, " (insert_property) is missing required fields")))
        }
      } else if (rule$operation == "modify_array") {
        if (!all(c("path", "action") %in% names(rule))) {
          return(list(valid = FALSE, message = paste0("Rule #", i, " (modify_array) is missing required fields")))
        }
      }
    }
    
    return(list(valid = TRUE, message = "Validation successful"))
  }, error = function(e) {
    return(list(valid = FALSE, message = paste0("Invalid JSON: ", e$message)))
  })
}

# Generate a unique ID for a rule
generateRuleId <- function() {
  # Simple ID generation with timestamp
  return(paste0("rule_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1)))
}

# ============================================
# UI Definition
# ============================================
ui <- dashboardPage(
  dashboardHeader(title = "JSON Schema Transformer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Transform", tabName = "transform", icon = icon("exchange-alt")),
      menuItem("Batch Processing", tabName = "batch", icon = icon("cubes")),
      menuItem("Rule Management", tabName = "rules", icon = icon("tasks")),
      menuItem("Logs", tabName = "logs", icon = icon("history"))
    )
  ),
  
  dashboardBody(
    # Javascript-Handler for custom messages to trigger events
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('jsCode', function(message) {
          eval(message);
        });
      "))
    ),
    
    tabItems(
      # Transform Tab
      tabItem(tabName = "transform",
              fluidRow(
                box(
                  title = "Input Data",
                  width = 4,
                  height = 450,
                  fileInput("jsonFile", "Upload JSON File", 
                            accept = c("application/json", ".json")),
                  actionButton("loadSampleData", "Load Sample Data"),
                  hr(),
                  h4("Transformation Rules"),
                  selectInput("savedRules", "Load Saved Rule:", choices = c("None" = "none")),
                  hr(),
                  checkboxInput("prettyOutput", "Formatted Output", TRUE),
                  actionButton("transformBtn", "Transform")
                ),
                
                box(
                  title = "Rule Editor",
                  width = 8,
                  height = 450,
                  aceEditor("rulesEditor", mode = "json", theme = "chrome", height = "380px",
                            value = '[\n  {\n    "operation": "add_element",\n    "path": "",\n    "name": "metadata",\n    "value": {"processed": true}\n  }\n]')
                )
              ),
              
              fluidRow(
                box(
                  title = "Input JSON",
                  width = 6,
                  height = 510,
                  aceEditor("inputJson", mode = "json", theme = "chrome", height = "400px",
                            value = '{\n  "Upload a JSON file or use the sample dataset"\n}')
                ),
                
                box(
                  title = "Output JSON",
                  width = 6,
                  height = 510,
                  aceEditor("outputJson", mode = "json", theme = "chrome", height = "400px",
                            readOnly = TRUE),
                  div(style = "margin-top: 10px",
                      downloadButton("downloadJson", "Download Transformed JSON"),
                      actionButton("copyBtn", "Copy to Clipboard"),
                      actionButton("clearOutputBtn", "Clear Output")
                  )
                )
              )
      ),
      
      # Batch Processing Tab
      tabItem(tabName = "batch",
              fluidRow(
                box(
                  title = "Batch Process Settings",
                  width = 4,
                  # Select a rule to apply
                  selectInput("batchRule", "Select Rule to Apply:", choices = c("None" = "none")),
                  
                  # Input directory
                  textInput("sourceDirPath", "Source Directory:", 
                            value = ifelse(.Platform$OS.type == "windows", "C:/path/to/json/files", "~/path/to/json/files")),
                  actionButton("browseSourceDir", "Browse..."),
                  
                  # Output directory
                  textInput("outputDirPath", "Output Directory:", 
                            value = ifelse(.Platform$OS.type == "windows", "C:/path/to/output", "~/path/to/output")),
                  actionButton("browseOutputDir", "Browse..."),
                  
                  # Options
                  checkboxInput("overwriteExisting", "Overwrite Existing Files", value = TRUE),
                  checkboxInput("preserveStructure", "Preserve Directory Structure", value = TRUE),
                  checkboxInput("recursiveSearch", "Include Subdirectories", value = TRUE),
                  
                  # File pattern (for filtering JSON files)
                  textInput("filePattern", "File Pattern:", value = "*.json"),
                  
                  # Execute button
                  br(),
                  actionButton("executeBatchBtn", "Execute Batch Processing", class = "btn-primary")
                ),
                
                # Preview box
                box(
                  title = "Files to Process",
                  width = 8,
                  DT::dataTableOutput("filesToProcess"),
                  textOutput("fileCount")
                )
              ),
              
              # Results and progress
              fluidRow(
                box(
                  title = "Processing Progress",
                  width = 12,
                  hidden(div(id = "progressSection",
                             uiOutput("progressText"),
                             div(
                               class = "progress",
                               div(
                                 id = "batchProgressBar",
                                 class = "progress-bar",
                                 role = "progressbar",
                                 style = "width: 0%;",
                                 "0%"
                               )
                             ),
                             br(),
                             verbatimTextOutput("currentFileProcessing")
                  )),
                  hidden(div(id = "resultsSection",
                             h4("Processing Results"),
                             DT::dataTableOutput("processingResults"),
                             downloadButton("downloadResultsBtn", "Download Results CSV")
                  ))
                )
              )
      ),
      
      # Rule Management Tab
      tabItem(tabName = "rules",
              fluidRow(
                box(
                  title = "Create/Edit Rule",
                  width = 4,
                  textInput("ruleName", "Rule Name:"),
                  textAreaInput("ruleDescription", "Description:", height = "100px"),
                  div(
                    style = "margin-top: 10px;",
                    actionButton("saveRule", "Save Rule"),
                    actionButton("newRuleBtn", "New Rule")
                  ),
                  hr(),
                  h4("Storage Settings"),
                  verbatimTextOutput("storageLocation"),
                  hr(),
                  h4("Backup & Restore"),
                  actionButton("backupRulesBtn", "Backup All Rules"),
                  actionButton("restoreBackupBtn", "Restore from Backup")
                ),
                
                tabBox(
                  title = "Rules",
                  width = 8,
                  id = "rulesTabBox",
                  tabPanel(
                    "Rule List",
                    DT::dataTableOutput("savedRulesTable"),
                    br(),
                    div(
                      style = "margin-bottom: 15px;",
                      actionButton("deleteRuleBtn", "Delete Selected"),
                      downloadButton("exportRuleBtn", "Export Selected"),
                      actionButton("duplicateRuleBtn", "Duplicate")
                    )
                  ),
                  tabPanel(
                    "Import/Statistics",
                    h4("Import Rules"),
                    actionButton("importRuleBtn", "Import from JSON File"),
                    # Hidden file upload field for import
                    div(style = "display: none;", 
                        fileInput("importRuleFile", "Choose File", 
                                  accept = c("application/json", ".json"))),
                    hr(),
                    h4("Rule Statistics"),
                    uiOutput("ruleStats")
                  )
                )
              )
      ),
      
      # Logs Tab
      tabItem(tabName = "logs",
              box(
                title = "Transformation Log",
                width = 12,
                verbatimTextOutput("transformLog")
              )
      )
    )
  )
)

# ============================================
# Server Logic
# ============================================
server <- function(input, output, session) {
  # Initialize reactive values
  logs <- reactiveVal(character())
  rules <- reactiveVal(list())
  backup_file_to_download <- reactiveVal(NULL)
  restore_source_dir <- reactiveVal(NULL)
  
  # Initialize file lists
  batch_files <- reactiveVal(NULL)
  processing_results <- reactiveVal(NULL)
  
  # Helper function to list JSON files in a directory
  listJsonFiles <- function(dir_path, pattern = "*.json", recursive = TRUE) {
    if (!dir.exists(dir_path)) {
      return(data.frame())
    }
    
    # List files matching the pattern
    files <- list.files(
      path = dir_path,
      pattern = pattern,
      recursive = recursive,
      full.names = TRUE
    )
    
    # Create a data frame with file information
    if (length(files) > 0) {
      file_info <- file.info(files)
      
      result <- data.frame(
        FilePath = files,
        FileName = basename(files),
        FileSize = file_info$size,
        LastModified = file_info$mtime,
        stringsAsFactors = FALSE
      )
      
      return(result)
    } else {
      return(data.frame())
    }
  }
  
  # Update the batch rule dropdown with saved rules
  observe({
    saved_rules <- rules()
    
    if (length(saved_rules) > 0) {
      rule_names <- sapply(saved_rules, function(r) r$name)
      rule_ids <- sapply(saved_rules, function(r) r$id)
      
      choices <- c("None" = "none")
      for (i in seq_along(rule_names)) {
        choices[rule_names[i]] <- rule_ids[i]
      }
      
      updateSelectInput(session, "batchRule", choices = choices)
    }
  })
  
  # Browse source directory button
  observeEvent(input$browseSourceDir, {
    # This would use shinyFiles in a real application
    # For the prototype, we'll just simulate directory selection
    showModal(modalDialog(
      title = "Select Source Directory",
      "In a production app, this would open a directory browser. For this prototype, please enter the directory path manually.",
      easyClose = TRUE
    ))
  })
  
  # Browse output directory button
  observeEvent(input$browseOutputDir, {
    # This would use shinyFiles in a real application
    showModal(modalDialog(
      title = "Select Output Directory",
      "In a production app, this would open a directory browser. For this prototype, please enter the directory path manually.",
      easyClose = TRUE
    ))
  })
  
  # Update file list when source directory or options change
  observeEvent(list(input$sourceDirPath, input$recursiveSearch, input$filePattern), {
    # Only proceed if the source directory is valid
    if (!is.null(input$sourceDirPath) && nzchar(input$sourceDirPath) && dir.exists(input$sourceDirPath)) {
      # List all JSON files in the source directory
      files_df <- listJsonFiles(
        dir_path = input$sourceDirPath,
        pattern = input$filePattern,
        recursive = input$recursiveSearch
      )
      
      # Update the reactive value
      batch_files(files_df)
      
      # Display a notification with the count
      if (nrow(files_df) > 0) {
        showNotification(paste("Found", nrow(files_df), "JSON files."), type = "message")
      } else {
        showNotification("No matching JSON files found in the directory.", type = "warning")
      }
    } else {
      batch_files(data.frame())
    }
  })
  
  # Display the list of files to be processed
  output$filesToProcess <- DT::renderDataTable({
    files_df <- batch_files()
    
    if (!is.null(files_df) && nrow(files_df) > 0) {
      # Format file size
      files_df$FileSize <- format(files_df$FileSize / 1024, digits = 2)
      
      # Return a datatable with selected columns
      DT::datatable(
        files_df[, c("FileName", "FileSize", "LastModified")],
        colnames = c("File Name", "Size (KB)", "Last Modified"),
        options = list(pageLength = 10)
      )
    } else {
      # Return an empty table if no files
      DT::datatable(
        data.frame(Message = "No JSON files found. Please select a valid source directory."),
        options = list(dom = 't')
      )
    }
  })
  
  # Display the count of files to be processed
  output$fileCount <- renderText({
    files_df <- batch_files()
    
    if (!is.null(files_df) && nrow(files_df) > 0) {
      paste(nrow(files_df), "files found in the source directory.")
    } else {
      "No files found. Please select a valid source directory."
    }
  })
  
  # Execute batch processing button
  observeEvent(input$executeBatchBtn, {
    # Validate inputs
    validate <- TRUE
    validate_message <- ""
    
    # Check if a rule is selected
    if (input$batchRule == "none") {
      validate <- FALSE
      validate_message <- "Please select a rule to apply."
    }
    
    # Check source directory
    if (!dir.exists(input$sourceDirPath)) {
      validate <- FALSE
      validate_message <- "Source directory does not exist."
    }
    
    # Check if there are files to process
    files_df <- batch_files()
    if (is.null(files_df) || nrow(files_df) == 0) {
      validate <- FALSE
      validate_message <- "No files to process."
    }
    
    # Check if output directory exists or can be created
    output_dir <- input$outputDirPath
    if (!dir.exists(output_dir)) {
      tryCatch({
        dir.create(output_dir, recursive = TRUE)
      }, error = function(e) {
        validate <- FALSE
        validate_message <- paste("Cannot create output directory:", e$message)
      })
    }
    
    if (!validate) {
      showNotification(validate_message, type = "error")
      return()
    }
    
    # Show progress UI
    shinyjs::show("progressSection")
    shinyjs::hide("resultsSection")
    
    # Get the selected rule
    rule_id <- input$batchRule
    selected_rule <- loadRuleById(rule_id, rule_storage_dir)
    
    if (is.null(selected_rule)) {
      showNotification("Error: Selected rule could not be loaded.", type = "error")
      return()
    }
    
    # Parse rule content
    rule_content <- selected_rule$content
    
    # Initialize results table
    results <- data.frame(
      FileName = character(),
      Status = character(),
      Message = character(),
      ProcessingTime = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Process files asynchronously (simulated here with a loop)
    withProgress(message = 'Processing files', value = 0, {
      total_files <- nrow(files_df)
      
      for (i in 1:total_files) {
        # Get current file
        file_path <- files_df$FilePath[i]
        file_name <- files_df$FileName[i]
        
        # Update progress
        incProgress(1/total_files, detail = paste("Processing", file_name))
        
        # Update progress UI
        progress_pct <- (i / total_files) * 100
        # Update the progress bar via JavaScript
        session$sendCustomMessage(type = "jsCode", 
                                  message = sprintf(
                                    "$('#batchProgressBar').css('width', '%s%%').attr('aria-valuenow', %s).text('%s%%');",
                                    progress_pct, progress_pct, round(progress_pct)
                                  ))
        
        output$progressText <- renderUI({
          h4(paste("Processing file", i, "of", total_files))
        })
        output$currentFileProcessing <- renderText({
          paste("Current file:", file_name)
        })
        
        # Determine output file path
        if (input$preserveStructure) {
          # Preserve directory structure relative to the source directory
          rel_path <- sub(normalizePath(input$sourceDirPath), "", normalizePath(dirname(file_path)))
          if (startsWith(rel_path, "/") || startsWith(rel_path, "\\")) {
            rel_path <- substring(rel_path, 2)
          }
          output_subdir <- file.path(output_dir, rel_path)
          
          # Create subdirectory if it doesn't exist
          if (!dir.exists(output_subdir)) {
            dir.create(output_subdir, recursive = TRUE)
          }
          
          output_file_path <- file.path(output_subdir, file_name)
        } else {
          # Just put all files in the output directory
          output_file_path <- file.path(output_dir, file_name)
        }
        
        # Check if output file exists and overwrite option
        if (file.exists(output_file_path) && !input$overwriteExisting) {
          # Skip this file
          results <- rbind(results, data.frame(
            FileName = file_name,
            Status = "Skipped",
            Message = "Output file already exists",
            ProcessingTime = 0,
            stringsAsFactors = FALSE
          ))
          next
        }
        
        # Process the file
        tryCatch({
          # Read the JSON file
          start_time <- Sys.time()
          
          # Read and parse input JSON
          json_content <- readLines(file_path, warn = FALSE)
          json_content <- paste(json_content, collapse = "\n")
          input_json <- fromJSON(json_content, simplifyVector = FALSE)
          
          # Parse transformation rules
          transform_rules <- fromJSON(rule_content, simplifyVector = FALSE)
          
          # Apply rules sequentially
          output_json <- input_json
          
          for (rule_index in seq_along(transform_rules)) {
            rule <- transform_rules[[rule_index]]
            
            # Call the appropriate function based on rule operation
            if (rule$operation == "add_element") {
              # Handle function value
              value <- rule$value
              if (is.list(value) && "_function_" %in% names(value)) {
                func_code <- value[["_function_"]]
                
                # Create a safe environment for function evaluation
                safe_env <- new.env(parent = baseenv())
                
                # Add necessary global functions
                safe_env$Sys.time <- Sys.time
                safe_env$paste <- paste
                safe_env$paste0 <- paste0
                safe_env$round <- round
                
                # Add context variables
                safe_env$root <- input_json
                
                # Evaluate the function
                tryCatch({
                  result <- eval(parse(text = func_code), envir = safe_env)
                  value <- result
                }, error = function(e) {
                  # Log error
                  value <- NULL
                })
              }
              
              # Add the element
              path <- rule$path
              name <- rule$name
              position <- if ("position" %in% names(rule)) rule$position else NULL
              
              output_json <- add_json_element(
                output_json, 
                path, 
                name, 
                value, 
                position = position,
                create_path = TRUE
              )
            } 
            else if (rule$operation == "insert_property") {
              # Extract parameters
              array_path_input <- rule$array_path
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
              # Case 3: JSON array was loaded as a list
              else if (is.list(array_path_input)) {
                array_path <- as.character(unlist(array_path_input))
              }
              
              property_name <- rule$property_name
              property_value <- rule$property_value
              position_type <- if("position_type" %in% names(rule)) rule$position_type else "last"
              position_ref <- if ("position_ref" %in% names(rule)) rule$position_ref else NULL
              
              # Handle function value
              if (is.list(property_value) && "_function_" %in% names(property_value)) {
                func_code <- property_value[["_function_"]]
                
                # Create a function for property_value
                property_value <- function(elem, index, parent, parent_index, root, ...) {
                  # Create a safe environment for evaluation
                  safe_env <- new.env(parent = baseenv())
                  
                  # Add necessary global functions
                  safe_env$paste <- paste
                  safe_env$paste0 <- paste0
                  safe_env$round <- round
                  safe_env$names <- names
                  safe_env$length <- length
                  
                  # Add context variables to the environment
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
                    result <- paste("Error:", e$message) # Fallback value
                  })
                  
                  return(result)
                }
              }
              
              # Apply the transformation
              result <- insert_json_property(
                json_data = output_json,
                array_path = array_path,
                property_name = property_name,
                property_value = property_value,
                position_type = position_type,
                position_ref = position_ref,
                filter_fn = NULL,
                verbose = FALSE
              )
              
              if (!is.null(result)) {
                output_json <- result
              }
            }
            # Add other rule operations as needed...
          }
          
          # Format and write the output
          output_json_str <- if (input$prettyOutput) {
            toJSON(output_json, pretty = TRUE, auto_unbox = TRUE)
          } else {
            toJSON(output_json, auto_unbox = TRUE)
          }
          
          writeLines(output_json_str, output_file_path)
          
          # Calculate processing time
          end_time <- Sys.time()
          proc_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
          
          # Add to results
          results <- rbind(results, data.frame(
            FileName = file_name,
            Status = "Success",
            Message = "Transformation completed",
            ProcessingTime = round(proc_time, 2),
            stringsAsFactors = FALSE
          ))
          
        }, error = function(e) {
          # Handle processing error
          results <- rbind(results, data.frame(
            FileName = file_name,
            Status = "Error",
            Message = e$message,
            ProcessingTime = 0,
            stringsAsFactors = FALSE
          ))
        })
        
        # Allow the UI to update
        Sys.sleep(0.1)
      }
    })
    
    # Update results table
    processing_results(results)
    
    # Hide progress and show results
    shinyjs::hide("progressSection")
    shinyjs::show("resultsSection")
    
    # Log completion
    addLog(paste("Batch processing completed:", nrow(results), "files processed."))
    
    # Show notification
    success_count <- sum(results$Status == "Success")
    error_count <- sum(results$Status == "Error")
    skipped_count <- sum(results$Status == "Skipped")
    
    showNotification(
      paste("Batch processing completed:", 
            success_count, "successful,", 
            error_count, "errors,", 
            skipped_count, "skipped."),
      type = "message"
    )
  })
  
  # Display processing results
  output$processingResults <- DT::renderDataTable({
    results <- processing_results()
    
    if (!is.null(results) && nrow(results) > 0) {
      # Color-code the Status column
      results$Status <- factor(results$Status, levels = c("Success", "Error", "Skipped"))
      
      DT::datatable(results, 
                    options = list(pageLength = 15),
                    rownames = FALSE) %>%
        DT::formatStyle(
          'Status',
          backgroundColor = DT::styleEqual(
            c('Success', 'Error', 'Skipped'), 
            c('#d4edda', '#f8d7da', '#fff3cd')
          ),
          color = DT::styleEqual(
            c('Success', 'Error', 'Skipped'), 
            c('#155724', '#721c24', '#856404')
          )
        )
    } else {
      DT::datatable(
        data.frame(Message = "No processing results available."),
        options = list(dom = 't')
      )
    }
  })
  
  # Download results as CSV
  output$downloadResultsBtn <- downloadHandler(
    filename = function() {
      paste0("batch_processing_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      results <- processing_results()
      write.csv(results, file, row.names = FALSE)
    }
  )
  # Initialize the rule storage directory
  rule_storage_dir <- setupRuleStorage()
  
  # JavaScript function for the integration of the import button
  runjs <- function(js_code) {
    session$sendCustomMessage(type = "jsCode", message = js_code)
  }
  
  # Add a log entry - show all logs by default
  addLog <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    current <- logs()
    logs(c(paste(timestamp, "-", message), current))
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
  
  # Load sample dataset
  observeEvent(input$loadSampleData, {
    json_content <- '{
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
    }'
    
    updateAceEditor(session, "inputJson", value = json_content)
    addLog("Sample dataset loaded")
  })
  
  # Load existing rules on startup
  observeEvent(1, {
    # Load rules from files
    saved_rules <- loadRulesFromFiles(rule_storage_dir)
    
    # Update the reactive value
    rules(saved_rules)
    
    # Update the dropdown
    updateRulesDropdown()
    
    addLog(paste(length(saved_rules), "rules loaded from storage"))
  }, once = TRUE)
  
  # Update the rules dropdown
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
  
  # New rule button
  observeEvent(input$newRuleBtn, {
    # Clear the editor
    updateAceEditor(session, "rulesEditor", value = "[\n  {\n    \"operation\": \"add_element\",\n    \"path\": \"\",\n    \"name\": \"metadata\",\n    \"value\": {\"processed\": true}\n  }\n]")
    
    # Clear the form
    updateTextInput(session, "ruleName", value = "")
    updateTextAreaInput(session, "ruleDescription", value = "")
    
    # Reset the selected rule
    updateSelectInput(session, "savedRules", selected = "none")
    
    addLog("New rule started")
  })
  
  # Save the current rule
  observeEvent(input$saveRule, {
    req(input$ruleName, input$rulesEditor)
    
    # Validate the rule content
    validation <- validateRule(input$rulesEditor)
    
    if (!validation$valid) {
      showNotification(validation$message, type = "error")
      addLog(paste("Rule validation failed:", validation$message))
      return()
    }
    
    tryCatch({
      # Check if we're updating an existing rule
      existing_rule_id <- NULL
      if (input$savedRules != "none") {
        existing_rule_id <- input$savedRules
      }
      
      # Create rule object
      new_rule <- list(
        id = if (is.null(existing_rule_id)) generateRuleId() else existing_rule_id,
        name = input$ruleName,
        description = input$ruleDescription,
        created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        content = input$rulesEditor
      )
      
      # Save to file
      rule_file <- saveRuleToFile(new_rule, rule_storage_dir)
      
      # Update the reactive value
      current_rules <- rules()
      
      # If this is an update to an existing rule, remove the old one
      if (!is.null(existing_rule_id)) {
        current_rules <- current_rules[sapply(current_rules, function(r) r$id != existing_rule_id)]
      }
      
      # Add the new/updated rule
      rules(c(current_rules, list(new_rule)))
      
      # Update the dropdown
      updateRulesDropdown()
      
      # Show confirmation
      showNotification(
        if (is.null(existing_rule_id)) "Rule successfully saved" else "Rule successfully updated", 
        type = "message"
      )
      addLog(paste(if (is.null(existing_rule_id)) "Saved" else "Updated", "rule:", input$ruleName))
      
      # Clear inputs only for new rules
      if (is.null(existing_rule_id)) {
        updateTextInput(session, "ruleName", value = "")
        updateTextAreaInput(session, "ruleDescription", value = "")
      }
      
    }, error = function(e) {
      showNotification(paste("Error saving rule:", e$message), type = "error")
      addLog(paste("Error saving rule:", e$message))
    })
  })
  
  # Load a saved rule
  observeEvent(input$savedRules, {
    rule_id <- input$savedRules
    
    if (rule_id != "none") {
      # Load the rule directly from the file
      selected_rule <- loadRuleById(rule_id, rule_storage_dir)
      
      if (!is.null(selected_rule)) {
        # Load the rule content into the editor
        updateAceEditor(session, "rulesEditor", value = selected_rule$content)
        
        # Also update name and description
        updateTextInput(session, "ruleName", value = selected_rule$name)
        updateTextAreaInput(session, "ruleDescription", value = selected_rule$description)
        
        addLog(paste("Rule loaded:", selected_rule$name))
      } else {
        showNotification("The selected rule could not be loaded", type = "error")
        addLog("Error loading rule: Rule not found")
      }
    } else {
      # Clear inputs
      updateTextInput(session, "ruleName", value = "")
      updateTextAreaInput(session, "ruleDescription", value = "")
    }
  })
  
  # Duplicate rule button
  observeEvent(input$duplicateRuleBtn, {
    selected <- input$savedRulesTable_rows_selected
    saved_rules <- rules()
    
    if (!is.null(selected) && length(saved_rules) >= selected) {
      # Get the rule to duplicate
      rule_to_duplicate <- saved_rules[[selected]]
      
      # Update editor with its content
      updateAceEditor(session, "rulesEditor", value = rule_to_duplicate$content)
      
      # Suggest a new name based on the original
      updateTextInput(session, "ruleName", value = paste0(rule_to_duplicate$name, " (Copy)"))
      updateTextAreaInput(session, "ruleDescription", value = rule_to_duplicate$description)
      
      # Reset the selected rule
      updateSelectInput(session, "savedRules", selected = "none")
      
      showNotification("Rule duplicated. Please save with a new name.", type = "message")
      addLog(paste("Rule duplicated:", rule_to_duplicate$name))
    } else {
      showNotification("Please select a rule to duplicate", type = "warning")
    }
  })
  
  # Delete selected rule
  observeEvent(input$deleteRuleBtn, {
    selected <- input$savedRulesTable_rows_selected
    saved_rules <- rules()
    
    if (!is.null(selected) && length(saved_rules) >= selected) {
      # Show a confirmation dialog
      showModal(modalDialog(
        title = "Confirm deletion",
        paste("Are you sure you want to delete the rule '", saved_rules[[selected]]$name, "'?"),
        footer = tagList(
          actionButton("confirmDeleteBtn", "Yes, delete"),
          modalButton("Cancel")
        )
      ))
    } else {
      showNotification("Please select a rule to delete", type = "warning")
    }
  })
  
  # Handle confirmed deletion
  observeEvent(input$confirmDeleteBtn, {
    # Close the dialog
    removeModal()
    
    # Get the selected rule
    selected <- input$savedRulesTable_rows_selected
    saved_rules <- rules()
    
    if (!is.null(selected) && length(saved_rules) >= selected) {
      # Get the rule to delete
      rule_to_delete <- saved_rules[[selected]]
      
      # Delete from the file system
      success <- deleteRuleById(rule_to_delete$id, rule_storage_dir)
      
      if (success) {
        # Remove it from the reactive list
        saved_rules <- saved_rules[-selected]
        rules(saved_rules)
        
        # Update the dropdown
        updateRulesDropdown()
        
        showNotification(paste("Rule deleted:", rule_to_delete$name), type = "message")
        addLog(paste("Rule deleted:", rule_to_delete$name))
      } else {
        showNotification("Error deleting rule", type = "error")
        addLog(paste("Error deleting rule:", rule_to_delete$name))
      }
    }
  })
  
  # Display saved rules in the table
  output$savedRulesTable <- renderDT({
    saved_rules <- rules()
    
    if (length(saved_rules) > 0) {
      # Create dataframe from saved rules
      df <- data.frame(
        Name = sapply(saved_rules, function(r) r$name),
        Description = sapply(saved_rules, function(r) r$description),
        Created = sapply(saved_rules, function(r) r$created),
        Modified = sapply(saved_rules, function(r) if("modified" %in% names(r)) r$modified else r$created),
        ID = sapply(saved_rules, function(r) r$id),
        stringsAsFactors = FALSE
      )
      
      datatable(df, selection = 'single', options = list(pageLength = 10))
    } else {
      datatable(data.frame(Message = "No saved rules"), options = list(dom = 't'))
    }
  })
  
  # Export selected rule
  output$exportRuleBtn <- downloadHandler(
    filename = function() {
      selected <- input$savedRulesTable_rows_selected
      saved_rules <- rules()
      
      if (!is.null(selected) && length(saved_rules) >= selected) {
        rule_name <- gsub("[^a-zA-Z0-9]", "_", saved_rules[[selected]]$name)
        return(paste0(rule_name, "_", format(Sys.time(), "%Y%m%d"), ".json"))
      } else {
        return(paste0("rule_export_", format(Sys.time(), "%Y%m%d"), ".json"))
      }
    },
    content = function(file) {
      selected <- input$savedRulesTable_rows_selected
      saved_rules <- rules()
      
      if (!is.null(selected) && length(saved_rules) >= selected) {
        # Export the selected rule
        rule_content <- saved_rules[[selected]]$content
        writeLines(rule_content, file)
        addLog(paste("Rule exported:", saved_rules[[selected]]$name))
      } else {
        # Export all rules
        rules_content <- lapply(saved_rules, function(r) fromJSON(r$content, simplifyVector = FALSE))
        rules_json <- toJSON(unlist(rules_content, recursive = FALSE), pretty = TRUE, auto_unbox = TRUE)
        writeLines(rules_json, file)
        addLog("All rules exported")
      }
    }
  )
  
  # Import button activates the hidden file upload field
  observeEvent(input$importRuleBtn, {
    # Simulate a click on the hidden file upload field
    runjs("$('#importRuleFile').click();")
  })
  
  # Import functionality
  observeEvent(input$importRuleFile, {
    req(input$importRuleFile)
    
    file <- input$importRuleFile
    
    # Check file extension
    if (tools::file_ext(file$datapath) != "json") {
      showNotification("Please upload a JSON file", type = "error")
      return()
    }
    
    # Try to read and parse the file
    tryCatch({
      rule_content <- readLines(file$datapath, warn = FALSE)
      rule_content <- paste(rule_content, collapse = "\n")
      
      # Validate the imported rule
      validation <- validateRule(rule_content)
      
      if (validation$valid) {
        # Fill the editor with the imported rules
        updateAceEditor(session, "rulesEditor", value = rule_content)
        
        # Suggest a name
        updateTextInput(session, "ruleName", value = paste0("Imported ", format(Sys.time(), "%Y-%m-%d")))
        
        showNotification("Rules successfully imported. Please review and save.", type = "message")
        addLog("Rules successfully imported")
      } else {
        showNotification(paste("Invalid rule file:", validation$message), type = "error")
        addLog(paste("Rule import validation failed:", validation$message))
      }
    }, error = function(e) {
      showNotification(paste("Error importing rules:", e$message), type = "error")
      addLog(paste("Error importing rules:", e$message))
    })
  })
  
  # Rule statistics
  output$ruleStats <- renderUI({
    saved_rules <- rules()
    
    # Count rules by type
    operations <- list()
    total_operations <- 0
    
    for (rule in saved_rules) {
      # Parse the rule content
      tryCatch({
        rule_content <- fromJSON(rule$content, simplifyVector = FALSE)
        
        # Count operations
        for (op in rule_content) {
          if ("operation" %in% names(op)) {
            op_type <- op$operation
            
            if (op_type %in% names(operations)) {
              operations[[op_type]] <- operations[[op_type]] + 1
            } else {
              operations[[op_type]] <- 1
            }
            
            total_operations <- total_operations + 1
          }
        }
      }, error = function(e) {
        # Skip invalid rules
      })
    }
    
    # Create the statistics UI
    tagList(
      p(paste("Number of rules:", length(saved_rules))),
      p(paste("Number of operations:", total_operations)),
      h4("Operations by type:"),
      tags$ul(
        lapply(names(operations), function(op) {
          tags$li(paste(op, ":", operations[[op]]))
        })
      )
    )
  })
  
  # Storage location
  output$storageLocation <- renderText({
    paste("Current storage location:", rule_storage_dir)
  })
  
  # Backup functionality
  observeEvent(input$backupRulesBtn, {
    saved_rules <- rules()
    
    if (length(saved_rules) > 0) {
      # Create a backup directory
      backup_dir <- file.path(tempdir(), "jsonr_backups")
      if (!dir.exists(backup_dir)) {
        dir.create(backup_dir, recursive = TRUE)
      }
      
      # Create a backup file with timestamp
      backup_file <- file.path(backup_dir, paste0("rules_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"))
      
      # Create a temporary directory for the files
      temp_backup_dir <- file.path(tempdir(), paste0("backup_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(temp_backup_dir, recursive = TRUE)
      
      # Copy all rule files to the temporary directory
      for (rule in saved_rules) {
        rule_file <- file.path(rule_storage_dir, paste0(rule$id, ".json"))
        if (file.exists(rule_file)) {
          file.copy(rule_file, file.path(temp_backup_dir, paste0(rule$id, ".json")))
        }
      }
      
      # Also copy the index file
      index_file <- file.path(rule_storage_dir, "rule_index.json")
      if (file.exists(index_file)) {
        file.copy(index_file, file.path(temp_backup_dir, "rule_index.json"))
      }
      
      # Create a ZIP file
      if (requireNamespace("utils", quietly = TRUE)) {
        # Change working directory to the temporary directory
        old_wd <- getwd()
        setwd(temp_backup_dir)
        
        # Create the ZIP file
        utils::zip(backup_file, files = list.files())
        
        # Restore the working directory
        setwd(old_wd)
        
        # Clean up the temporary directory
        unlink(temp_backup_dir, recursive = TRUE)
        
        showNotification(paste("Backup created:", backup_file), type = "message")
        addLog(paste("Backup created:", backup_file))
        
        # Offer to download the backup
        showModal(modalDialog(
          title = "Backup Created",
          paste("Backup created at:", backup_file),
          "Would you like to download this backup file?",
          footer = tagList(
            downloadButton("downloadBackupBtn", "Download Backup"),
            modalButton("Close")
          )
        ))
        
        # Save backup path for download
        backup_file_to_download(backup_file)
      }
    } else {
      showNotification("No rules to backup", type = "warning")
    }
  })
  
  # Backup download handler
  output$downloadBackupBtn <- downloadHandler(
    filename = function() {
      basename(backup_file_to_download())
    },
    content = function(file) {
      file.copy(backup_file_to_download(), file)
    }
  )
  
  # Restore from backup
  observeEvent(input$restoreBackupBtn, {
    showModal(modalDialog(
      title = "Restore Rules",
      fileInput("restoreBackupFile", "Upload Backup ZIP File", accept = ".zip"),
      p("Warning: This will overwrite your current rules. Make sure to create a backup first."),
      footer = tagList(
        actionButton("confirmRestoreBtn", "Restore"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Confirm restoration
  observeEvent(input$confirmRestoreBtn, {
    req(input$restoreBackupFile)
    
    backup_file <- input$restoreBackupFile$datapath
    
    # Create a temporary directory for extraction
    restore_dir <- file.path(tempdir(), paste0("restore_", format(Sys.time(), "%Y%m%d%H%M%S")))
    dir.create(restore_dir, recursive = TRUE)
    
    # Try to extract the backup file
    tryCatch({
      utils::unzip(backup_file, exdir = restore_dir)
      
      # Check if it has the expected structure
      index_file <- file.path(restore_dir, "rule_index.json")
      
      if (!file.exists(index_file)) {
        showNotification("Invalid backup file: No index found", type = "error")
        return()
      }
      
      # Read the index for verification
      index <- fromJSON(index_file, simplifyVector = FALSE)
      
      if (!("rules" %in% names(index))) {
        showNotification("Invalid backup file: Invalid index structure", type = "error")
        return()
      }
      
      # Count the rules in the backup
      rule_count <- length(index$rules)
      
      # Confirm restoration
      showModal(modalDialog(
        title = "Confirm Restoration",
        paste("Backup contains", rule_count, "rules. Continue with restoration?"),
        p("This will replace all your current rules."),
        footer = tagList(
          actionButton("executeRestoreBtn", "Yes, restore now"),
          modalButton("Cancel")
        )
      ))
      
      # Save the restoration directory for later
      restore_source_dir(restore_dir)
      
    }, error = function(e) {
      showNotification(paste("Error extracting backup:", e$message), type = "error")
      unlink(restore_dir, recursive = TRUE)
    })
  })
  
  # Execute the restoration
  observeEvent(input$executeRestoreBtn, {
    # Close all dialogs
    removeModal()
    
    # Copy all files from the restoration directory to the rule storage directory
    tryCatch({
      # Delete existing rules
      rule_files <- list.files(rule_storage_dir, pattern = "\\.json$", full.names = TRUE)
      sapply(rule_files, file.remove)
      
      # Copy files from the restoration directory
      restored_files <- list.files(restore_source_dir(), pattern = "\\.json$", full.names = TRUE)
      success <- sapply(restored_files, function(file) {
        file.copy(file, file.path(rule_storage_dir, basename(file)), overwrite = TRUE)
      })
      
      # Reload rules
      saved_rules <- loadRulesFromFiles(rule_storage_dir)
      rules(saved_rules)
      updateRulesDropdown()
      
      showNotification(paste("Successfully restored", sum(success), "rules"), type = "message")
      addLog(paste(sum(success), "rules restored from backup"))
      
      # Clean up
      unlink(restore_source_dir(), recursive = TRUE)
      
    }, error = function(e) {
      showNotification(paste("Error during restoration:", e$message), type = "error")
      addLog(paste("Error during restoration:", e$message))
    })
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
    
    # Read and validate the JSON
    tryCatch({
      json_content <- readLines(file$datapath)
      json_content <- paste(json_content, collapse = "\n")
      json_test <- fromJSON(json_content) # Just for validation
      
      updateAceEditor(session, "inputJson", value = json_content)
      addLog(paste("File loaded:", file$name))
    }, error = function(e) {
      showNotification(paste("Invalid JSON format:", e$message), type = "error")
      addLog(paste("Error loading file:", e$message))
    })
  })
  
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
      
      # Apply rules sequentially
      output_json <- input_json
      
      for (rule_index in seq_along(transform_rules)) {
        rule <- transform_rules[[rule_index]]
        
        addLog(paste("Processing rule", rule_index, ":", rule$operation))
        
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
            addLog(paste("Evaluating function:", func_code))
            
            # Create a safe environment for function evaluation
            safe_env <- new.env(parent = baseenv())
            
            # Add necessary global functions
            safe_env$Sys.time <- Sys.time
            safe_env$paste <- paste
            safe_env$paste0 <- paste0
            safe_env$round <- round
            
            # Add context variables
            safe_env$root <- input_json
            
            # Create the function with proper error handling
            tryCatch({
              # Evaluate the function
              result <- eval(parse(text = func_code), envir = safe_env)
              value <- result
            }, error = function(e) {
              addLog(paste("Error in function evaluation:", e$message))
              value <- NULL
            })
          }
          
          # Add the element with proper positioning
          path <- rule$path
          name <- rule$name
          position <- if ("position" %in% names(rule)) rule$position else NULL
          
          # Use add_json_element
          try({
            output_json <- add_json_element(
              output_json, 
              path, 
              name, 
              value, 
              position = position,
              create_path = TRUE
            )
          })
          
          addLog(paste("add_element applied:", name, "to path:", path, 
                       if(!is.null(position)) paste("at position", position) else ""))
        } 
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
          # Case 3: JSON array was loaded as a list
          else if (is.list(array_path_input)) {
            array_path <- as.character(unlist(array_path_input))
          }
          
          # Check if we have a valid character vector
          if (is.null(array_path) || length(array_path) == 0) {
            addLog("Error: array_path could not be converted to a valid character vector")
            next
          }
          
          property_name <- rule$property_name
          property_value <- rule$property_value
          position_type <- if("position_type" %in% names(rule)) rule$position_type else "last"
          position_ref <- if ("position_ref" %in% names(rule)) rule$position_ref else NULL
          
          # Debug logging
          addLog(paste("Original array_path:", 
                       if(is.character(array_path_input)) paste(array_path_input, collapse=", ") 
                       else "not a character vector"))
          addLog(paste("Converted array_path:", paste(array_path, collapse=", ")))
          addLog(paste("property_name:", property_name))
          addLog(paste("position_type:", position_type))
          if (!is.null(position_ref)) addLog(paste("position_ref:", position_ref))
          
          # Handle function value
          filter_fn <- NULL
          if (is.list(property_value) && "_function_" %in% names(property_value)) {
            func_code <- property_value[["_function_"]]
            addLog(paste("Function code is:", func_code))
            
            # Create an actual function to be passed to insert_json_property
            property_value <- function(elem, index, parent, parent_index, root, ...) {
              # Create a safe environment for evaluation
              safe_env <- new.env(parent = baseenv())
              
              # Add necessary global functions
              safe_env$paste <- paste
              safe_env$paste0 <- paste0
              safe_env$round <- round
              safe_env$names <- names
              safe_env$length <- length
              
              # Add context variables to the environment
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
            
            addLog("Dynamic function created for property_value")
          }
          
          # Apply transformation with careful error handling
          tryCatch({
            addLog("Attempting to call insert_json_property...")
            
            result <- insert_json_property(
              json_data = output_json,
              array_path = array_path,
              property_name = property_name,
              property_value = property_value,
              position_type = position_type,
              position_ref = position_ref,
              filter_fn = filter_fn,
              verbose = TRUE
            )
            
            # Only update if successful
            if (!is.null(result)) {
              output_json <- result
              addLog(paste("insert_property applied:", property_name, "to array path:", 
                           paste(array_path, collapse = " > ")))
            } else {
              addLog("Warning: insert_json_property returned NULL")
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
            
            # Create a safe environment for evaluation
            safe_env <- new.env(parent = baseenv())
            
            # Add necessary global functions
            safe_env$paste <- paste
            safe_env$paste0 <- paste0
            safe_env$round <- round
            
            # Add context variables to the environment
            safe_env$root <- input_json
            
            # Evaluate the function body
            tryCatch({
              value <- eval(parse(text = func_code), envir = safe_env)
            }, error = function(e) {
              addLog(paste("Error in function evaluation:", e$message))
              value <- NULL
            })
          }
          
          # Implement modify_array function
          modify_array <- function(json_data, path, action, value = NULL, position = NULL, filter = NULL) {
            # Get the array at the specified path
            path_parts <- unlist(strsplit(path, "\\."))
            current <- json_data
            
            for (part in path_parts) {
              if (!is.list(current) || !(part %in% names(current))) {
                return(json_data) # Path not found, return original
              }
              current <- current[[part]]
            }
            
            # Check if it's an array
            if (!is.list(current) || is.null(names(current)) && length(current) == 0) {
              return(json_data) # Not an array or empty array
            }
            
            # Perform the desired action
            if (action == "append") {
              # Add at the end
              current <- c(current, list(value))
            } 
            else if (action == "prepend") {
              # Add at the beginning
              current <- c(list(value), current)
            } 
            else if (action == "insert" && !is.null(position)) {
              # Insert at a specific position
              if (position < 1 || position > length(current) + 1) {
                addLog(paste("Warning: Invalid position:", position))
                return(json_data)
              }
              
              result <- list()
              for (i in 1:length(current)) {
                if (i == position) {
                  result <- c(result, list(value))
                }
                result <- c(result, list(current[[i]]))
              }
              
              if (position == length(current) + 1) {
                result <- c(result, list(value))
              }
              
              current <- result
            } 
            else if (action == "replace" && !is.null(position)) {
              # Replace at a specific position
              if (position < 1 || position > length(current)) {
                addLog(paste("Warning: Invalid position:", position))
                return(json_data)
              }
              
              current[[position]] <- value
            } 
            else if (action == "remove") {
              # Remove based on position or filter
              if (!is.null(position)) {
                if (position < 1 || position > length(current)) {
                  addLog(paste("Warning: Invalid position:", position))
                  return(json_data)
                }
                
                current <- current[-position]
              } 
              else if (!is.null(filter)) {
                # Apply filter
                filter_func <- NULL
                
                if (is.list(filter) && "_function_" %in% names(filter)) {
                  filter_code <- filter[["_function_"]]
                  
                  # Create filter function
                  filter_func <- function(elem) {
                    # Create a safe environment
                    safe_env <- new.env(parent = baseenv())
                    safe_env$elem <- elem
                    
                    result <- FALSE
                    tryCatch({
                      result <- eval(parse(text = filter_code), envir = safe_env)
                    }, error = function(e) {
                      addLog(paste("Error in filter:", e$message))
                    })
                    
                    return(result)
                  }
                  
                  # Find elements that match the filter
                  to_remove <- c()
                  for (i in 1:length(current)) {
                    if (filter_func(current[[i]])) {
                      to_remove <- c(to_remove, i)
                    }
                  }
                  
                  if (length(to_remove) > 0) {
                    current <- current[-to_remove]
                  }
                }
              }
            }
            
            # Update the original JSON with the modified array
            result <- json_data
            current_obj <- result
            
            for (i in 1:(length(path_parts) - 1)) {
              part <- path_parts[i]
              current_obj <- current_obj[[part]]
            }
            
            last_part <- path_parts[length(path_parts)]
            current_obj[[last_part]] <- current
            
            return(result)
          }
          
          # Execute the transformation
          output_json <- modify_array(
            output_json,
            path,
            action,
            value,
            position,
            filter
          )
          
          addLog(paste("modify_array applied:", action, "to path:", path))
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
      
      # Show error details in output
      error_detail <- paste0('{\n  "error": "', e$message, '",\n  "stacktrace": "', 
                             paste(capture.output(traceback()), collapse = "\\n"), '"\n}')
      updateAceEditor(session, "outputJson", value = error_detail)
    })
  })
  
  # Download transformed JSON
  output$downloadJson <- downloadHandler(
    filename = function() {
      paste0("transformed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      # Write the transformed JSON to the file
      writeLines(input$outputJson, file)
      addLog("Transformed JSON downloaded")
    }
  )
  
  # Copy to clipboard button
  observeEvent(input$copyBtn, {
    # In a real app we would use JavaScript to copy to clipboard
    showNotification("Copied to clipboard (simulated)", type = "message")
    addLog("Output copied to clipboard (simulated)")
  })
  
  # Initialize the app
  observeEvent(1, {
    addLog("Application initialized")
  }, once = TRUE)
}

# Start the application
shinyApp(ui = ui, server = server)