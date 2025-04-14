library(shiny)
library(jsonlite)
library(shinyAce)
library(DT)
library(shinyjs)
library(jsonr)

# Hilfsfunktionen für Regelspeicherung
# ====================================

# Speicherverzeichnis einrichten
setupRuleStorage <- function() {
  # Definiere, wo die Regeln gespeichert werden sollen
  app_data_dir <- file.path(tempdir(), "jsonr_rules")
  
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

# Speichere eine Regel in eine Datei
saveRuleToFile <- function(rule, app_data_dir) {
  # Generiere einen Dateinamen basierend auf der Regel-ID
  rule_file <- file.path(app_data_dir, paste0(rule$id, ".json"))
  
  # Speichere die Regel in eine Datei
  writeLines(toJSON(rule, pretty = TRUE, auto_unbox = TRUE), rule_file)
  
  # Aktualisiere den Index
  updateRuleIndex(rule, app_data_dir)
  
  return(rule_file)
}

# Aktualisiere die Indexdatei mit Regel-Metadaten
updateRuleIndex <- function(rule, app_data_dir) {
  index_file <- file.path(app_data_dir, "rule_index.json")
  
  # Lese den aktuellen Index
  index <- fromJSON(index_file, simplifyVector = FALSE)
  
  # Erstelle Regel-Metadaten für den Index
  rule_meta <- list(
    id = rule$id,
    name = rule$name,
    description = rule$description,
    created = rule$created,
    modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # Stellen Sie sicher, dass index$rules existiert und ist eine Liste
  if (!("rules" %in% names(index)) || !is.list(index$rules)) {
    index$rules <- list()
  }
  
  # Prüfe, ob die Regel bereits im Index existiert
  if (length(index$rules) > 0) {
    # Sicherere Version mit expliziter Schleife
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
    # Aktualisiere vorhandenen Eintrag
    index$rules[[existing_index]] <- rule_meta
  } else {
    # Füge neuen Eintrag hinzu
    index$rules <- c(index$rules, list(rule_meta))
  }
  
  # Schreibe den aktualisierten Index
  writeLines(toJSON(index, pretty = TRUE, auto_unbox = TRUE), index_file)
}

# Lade alle Regeln aus dem Dateisystem
loadRulesFromFiles <- function(app_data_dir) {
  index_file <- file.path(app_data_dir, "rule_index.json")
  
  # Prüfe, ob der Index existiert
  if (!file.exists(index_file)) {
    return(list())
  }
  
  # Lese den Index
  index <- fromJSON(index_file, simplifyVector = FALSE)
  
  # Lade alle Regeln
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

# Lade eine bestimmte Regel anhand ihrer ID
loadRuleById <- function(rule_id, app_data_dir) {
  rule_file <- file.path(app_data_dir, paste0(rule_id, ".json"))
  
  if (file.exists(rule_file)) {
    return(fromJSON(readLines(rule_file, warn = FALSE), simplifyVector = FALSE))
  } else {
    return(NULL)
  }
}

# Lösche eine Regel anhand ihrer ID
deleteRuleById <- function(rule_id, app_data_dir) {
  rule_file <- file.path(app_data_dir, paste0(rule_id, ".json"))
  
  # Entferne die Datei, falls sie existiert
  if (file.exists(rule_file)) {
    file.remove(rule_file)
  }
  
  # Aktualisiere den Index
  index_file <- file.path(app_data_dir, "rule_index.json")
  index <- fromJSON(index_file, simplifyVector = FALSE)
  
  # Finde die Regel im Index
  rule_index <- which(sapply(index$rules, function(r) r$id == rule_id))
  
  if (length(rule_index) > 0) {
    # Entferne die Regel aus dem Index
    index$rules <- index$rules[-rule_index]
    
    # Schreibe den aktualisierten Index
    writeLines(toJSON(index, pretty = TRUE, auto_unbox = TRUE), index_file)
    return(TRUE)
  }
  
  return(FALSE)
}

# Validiere eine Regel vor dem Speichern
validateRule <- function(rule_content) {
  # Grundlegende Validierung - prüfe, ob es gültiges JSON ist
  tryCatch({
    parsed <- fromJSON(rule_content, simplifyVector = FALSE)
    
    # Prüfe, ob es ein Array von Regelobjekten ist
    if (!is.list(parsed) || length(parsed) == 0) {
      return(list(valid = FALSE, message = "Regeln müssen ein nicht-leeres Array sein"))
    }
    
    # Prüfe, ob jede Regel die erforderlichen Felder hat
    for (i in seq_along(parsed)) {
      rule <- parsed[[i]]
      
      if (!("operation" %in% names(rule))) {
        return(list(valid = FALSE, message = paste0("Regel #", i, " fehlt das 'operation' Feld")))
      }
      
      # Füge spezifische Validierung für jeden Operationstyp hinzu
      if (rule$operation == "add_element") {
        if (!all(c("path", "name") %in% names(rule))) {
          return(list(valid = FALSE, message = paste0("Regel #", i, " (add_element) fehlen erforderliche Felder")))
        }
      } else if (rule$operation == "insert_property") {
        if (!all(c("array_path", "property_name") %in% names(rule))) {
          return(list(valid = FALSE, message = paste0("Regel #", i, " (insert_property) fehlen erforderliche Felder")))
        }
      } else if (rule$operation == "modify_array") {
        if (!all(c("path", "action") %in% names(rule))) {
          return(list(valid = FALSE, message = paste0("Regel #", i, " (modify_array) fehlen erforderliche Felder")))
        }
      }
    }
    
    return(list(valid = TRUE, message = "Validierung erfolgreich"))
  }, error = function(e) {
    return(list(valid = FALSE, message = paste0("Ungültiges JSON: ", e$message)))
  })
}

# Generiere eine eindeutige ID für eine Regel
generateRuleId <- function() {
  # Einfache ID-Generierung mit Zeitstempel
  return(paste0("rule_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1)))
}

# UI Definition
# =============
ui <- fluidPage(
  useShinyjs(),
  titlePanel("JSON Schema Transformer"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 # Input-Bereich
                 h4("Eingabedaten"),
                 fileInput("jsonFile", "JSON-Datei hochladen", 
                           accept = c("application/json", ".json")),
                 
                 # Oder Beispiel verwenden
                 selectInput("sampleData", "Oder Beispieldaten verwenden:",
                             c("Keine" = "none", 
                               "Produktkatalog" = "product", 
                               "Benutzerprofil" = "user", 
                               "API-Antwort" = "api")),
                 
                 # Transformationsregeln-Bereich
                 hr(),
                 h4("Transformationsregeln"),
                 
                 # Vordefinierte Transformationsvorlagen
                 selectInput("transformTemplate", "Vorlagen:",
                             c("Benutzerdefiniert" = "custom", 
                               "Felder umbenennen" = "rename", 
                               "Verschachtelte Felder abflachen" = "flatten",
                               "Metadaten hinzufügen" = "metadata",
                               "Array-Operationen" = "array_ops")),
                 
                 # Regelverwaltung
                 hr(),
                 h4("Regelverwaltung"),
                 
                 # Verbesserte Benutzeroberfläche für Regelspeicherung
                 div(
                   style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                   selectInput("savedRules", "Gespeicherte Regel laden:", choices = c("Keine" = "none")),
                   textInput("ruleName", "Regelname:"),
                   textAreaInput("ruleDescription", "Beschreibung:", height = "60px"),
                   div(
                     style = "display: flex; justify-content: space-between; margin-top: 10px;",
                     actionButton("saveRule", "Regel speichern", class = "btn-success"),
                     actionButton("newRuleBtn", "Neue Regel", class = "btn-primary")
                   )
                 ),
                 
                 # Transform-Button
                 hr(),
                 checkboxInput("prettyOutput", "Formatierte Ausgabe", TRUE),
                 actionButton("transformBtn", "Transformieren", class = "btn-primary btn-lg btn-block"),
                 
                 # Debug-Optionen
                 hr(),
                 checkboxInput("debugMode", "Debug-Modus", FALSE),
                 div(id = "debugOptions", style = "display: none;",
                     checkboxInput("verboseLogging", "Ausführliches Logging", FALSE),
                     checkboxInput("showErrorDetails", "Detaillierte Fehler anzeigen", TRUE),
                     actionButton("testBtn", "Test ausführen", class = "btn-info")
                 )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Editor",
                 fluidRow(
                   column(12,
                          h4("Regel-Editor"),
                          aceEditor("rulesEditor", mode = "json", theme = "chrome", height = "250px",
                                    value = '[\n  {\n    "operation": "add_element",\n    "path": "",\n    "name": "metadata",\n    "value": {"processed": true}\n  }\n]')
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Eingabe-JSON"),
                          aceEditor("inputJson", mode = "json", theme = "chrome", height = "400px",
                                    value = '{\n  "Lade eine JSON-Datei hoch oder wähle ein Beispiel"\n}')
                   ),
                   column(6,
                          div(style = "display: flex; justify-content: space-between; align-items: center;",
                              h4("Ausgabe-JSON"),
                              actionButton("clearOutputBtn", "Ausgabe löschen", class = "btn-warning btn-sm")
                          ),
                          aceEditor("outputJson", mode = "json", theme = "chrome", height = "400px",
                                    readOnly = TRUE),
                          div(style = "margin-top: 10px",
                              downloadButton("downloadJson", "Transformiertes JSON herunterladen"),
                              actionButton("copyBtn", "In Zwischenablage kopieren", icon = icon("clipboard"))
                          )
                   )
                 )
        ),
        
        # Verbesserte Regelverwaltungs-Tab
        tabPanel("Regelverwaltung",
                 fluidRow(
                   column(8,
                          h3("Gespeicherte Regeln"),
                          DT::dataTableOutput("savedRulesTable"),
                          br(),
                          div(
                            style = "display: flex; gap: 10px;",
                            actionButton("deleteRuleBtn", "Ausgewählte löschen", class = "btn-danger"),
                            downloadButton("exportRuleBtn", "Ausgewählte exportieren", class = "btn-info"),
                            actionButton("duplicateRuleBtn", "Duplizieren", class = "btn-default")
                          )
                   ),
                   column(4,
                          h3("Regeln importieren"),
                          fileInput("importRuleFile", "Aus JSON-Datei importieren",
                                    accept = c("application/json", ".json")),
                          hr(),
                          h3("Regelstatistik"),
                          uiOutput("ruleStats")
                   )
                 )
        ),
        
        # Einstellungs-Tab
        tabPanel("Einstellungen",
                 fluidRow(
                   column(6,
                          h3("Speichereinstellungen"),
                          verbatimTextOutput("storageLocation"),
                          br(),
                          h3("Backup & Wiederherstellung"),
                          actionButton("backupRulesBtn", "Alle Regeln sichern", class = "btn-info"),
                          actionButton("restoreBackupBtn", "Aus Backup wiederherstellen", class = "btn-warning")
                   ),
                   column(6,
                          h3("Anwendungseinstellungen"),
                          checkboxInput("confirmDeletes", "Löschen bestätigen", value = TRUE),
                          checkboxInput("autoSaveRules", "Regeln automatisch speichern", value = FALSE),
                          sliderInput("autoSaveInterval", "Auto-Speicher-Intervall (Minuten)", 
                                      min = 1, max = 30, value = 5, step = 1),
                          hr(),
                          h3("Design-Einstellungen"),
                          selectInput("editorTheme", "Editor-Theme:", 
                                      choices = c("chrome", "monokai", "github", "twilight", "xcode"))
                   )
                 )
        ),
        
        tabPanel("Log", 
                 verbatimTextOutput("transformLog"))
      )
    )
  )
)

# Server-Logik
# ============
server <- function(input, output, session) {
  # Initialisiere reaktive Werte
  logs <- reactiveVal(character())
  rules <- reactiveVal(list())
  backup_file_to_download <- reactiveVal(NULL)
  restore_source_dir <- reactiveVal(NULL)
  
  # Initialisiere das Regelspeicherverzeichnis
  rule_storage_dir <- setupRuleStorage()
  
  # Debug-Flag
  observe({
    if (input$debugMode) {
      shinyjs::show("debugOptions")
    } else {
      shinyjs::hide("debugOptions")
    }
  })
  
  # Füge einen Log-Eintrag hinzu
  addLog <- function(message, isDebug = FALSE) {
    if (isDebug && !input$verboseLogging) {
      return() # Überspringe Debug-Nachrichten, wenn ausführliches Logging nicht aktiviert ist
    }
    
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    prefix <- if (isDebug) "[DEBUG] " else ""
    current <- logs()
    logs(c(paste(timestamp, "-", prefix, message), current))
  }
  
  # Zeige Logs an
  output$transformLog <- renderText({
    paste(logs(), collapse = "\n")
  })
  
  # Ausgabe löschen Button
  observeEvent(input$clearOutputBtn, {
    updateAceEditor(session, "outputJson", value = "")
    addLog("Ausgabe gelöscht")
  })
  
  # Lade Beispieldaten basierend auf Auswahl
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
    addLog(paste("Beispieldaten geladen:", sample_type))
  })
  
  # Lade Vorlage basierend auf Auswahl
  observeEvent(input$transformTemplate, {
    template_type <- input$transformTemplate
    
    if (template_type == "custom") {
      return()  # Nicht überschreiben, wenn Benutzerdefiniert ausgewählt ist
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
  }
]'
    )
    
    updateAceEditor(session, "rulesEditor", value = template_content)
    addLog(paste("Vorlage geladen:", template_type))
  })
  
  # Lade vorhandene Regeln beim Start
  observeEvent(1, {
    # Lade Regeln aus Dateien
    saved_rules <- loadRulesFromFiles(rule_storage_dir)
    
    # Aktualisiere den reaktiven Wert
    rules(saved_rules)
    
    # Aktualisiere das Dropdown
    updateRulesDropdown()
    
    addLog(paste(length(saved_rules), "Regeln aus dem Speicher geladen"))
  }, once = TRUE)
  
  # Aktualisiere das Regeln-Dropdown
  updateRulesDropdown <- function() {
    saved_rules <- rules()
    
    if (length(saved_rules) > 0) {
      rule_names <- sapply(saved_rules, function(r) r$name)
      rule_ids <- sapply(saved_rules, function(r) r$id)
      
      choices <- c("Keine" = "none")
      for (i in seq_along(rule_names)) {
        choices[rule_names[i]] <- rule_ids[i]
      }
      
      updateSelectInput(session, "savedRules", choices = choices)
    }
  }
  
  # Neue Regel Button
  observeEvent(input$newRuleBtn, {
    # Leere den Editor
    updateAceEditor(session, "rulesEditor", value = "[\n  {\n    \"operation\": \"add_element\",\n    \"path\": \"\",\n    \"name\": \"metadata\",\n    \"value\": {\"processed\": true}\n  }\n]")
    
    # Leere das Formular
    updateTextInput(session, "ruleName", value = "")
    updateTextAreaInput(session, "ruleDescription", value = "")
    
    # Setze die ausgewählte Regel zurück
    updateSelectInput(session, "savedRules", selected = "none")
    
    addLog("Neue Regel begonnen")
  })
  
  # Speichere die aktuelle Regel
  observeEvent(input$saveRule, {
    req(input$ruleName, input$rulesEditor)
    
    # Validiere den Regelinhalt
    validation <- validateRule(input$rulesEditor)
    
    if (!validation$valid) {
      showNotification(validation$message, type = "error")
      addLog(paste("Regelvalidierung fehlgeschlagen:", validation$message))
      return()
    }
    
    tryCatch({
      # Prüfe, ob wir eine vorhandene Regel aktualisieren
      existing_rule_id <- NULL
      if (input$savedRules != "none") {
        existing_rule_id <- input$savedRules
      }
      
      # Erstelle Regelobjekt
      new_rule <- list(
        id = if (is.null(existing_rule_id)) generateRuleId() else existing_rule_id,
        name = input$ruleName,
        description = input$ruleDescription,
        created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        content = input$rulesEditor
      )
      
      # Speichere in Datei
      rule_file <- saveRuleToFile(new_rule, rule_storage_dir)
      
      # Aktualisiere den reaktiven Wert
      current_rules <- rules()
      
      # Wenn dies eine Aktualisierung einer vorhandenen Regel ist, entferne die alte
      if (!is.null(existing_rule_id)) {
        current_rules <- current_rules[sapply(current_rules, function(r) r$id != existing_rule_id)]
      }
      
      # Füge die neue/aktualisierte Regel hinzu
      rules(c(current_rules, list(new_rule)))
      
      # Aktualisiere das Dropdown
      updateRulesDropdown()
      
      # Zeige Bestätigung
      showNotification(
        if (is.null(existing_rule_id)) "Regel erfolgreich gespeichert" else "Regel erfolgreich aktualisiert", 
        type = "message"
      )
      addLog(paste(if (is.null(existing_rule_id)) "Gespeichert" else "Aktualisiert", "Regel:", input$ruleName))
      
      # Leere Eingaben nur für neue Regeln
      if (is.null(existing_rule_id)) {
        updateTextInput(session, "ruleName", value = "")
        updateTextAreaInput(session, "ruleDescription", value = "")
      }
      
    }, error = function(e) {
      showNotification(paste("Fehler beim Speichern der Regel:", e$message), type = "error")
      addLog(paste("Fehler beim Speichern der Regel:", e$message))
    })
  })
  
  # Lade eine gespeicherte Regel
  observeEvent(input$savedRules, {
    rule_id <- input$savedRules
    
    if (rule_id != "none") {
      # Lade die Regel direkt aus der Datei
      selected_rule <- loadRuleById(rule_id, rule_storage_dir)
      
      if (!is.null(selected_rule)) {
        # Lade den Regelinhalt in den Editor
        updateAceEditor(session, "rulesEditor", value = selected_rule$content)
        
        # Aktualisiere auch Name und Beschreibung
        updateTextInput(session, "ruleName", value = selected_rule$name)
        updateTextAreaInput(session, "ruleDescription", value = selected_rule$description)
        
        addLog(paste("Regel geladen:", selected_rule$name))
      } else {
        showNotification("Die ausgewählte Regel konnte nicht geladen werden", type = "error")
        addLog("Fehler beim Laden der Regel: Regel nicht gefunden")
      }
    } else {
      # Leere Eingaben
      updateTextInput(session, "ruleName", value = "")
      updateTextAreaInput(session, "ruleDescription", value = "")
    }
  })
  
  # Dupliziere Regel Button
  observeEvent(input$duplicateRuleBtn, {
    selected <- input$savedRulesTable_rows_selected
    saved_rules <- rules()
    
    if (!is.null(selected) && length(saved_rules) >= selected) {
      # Hole die zu duplizierende Regel
      rule_to_duplicate <- saved_rules[[selected]]
      
      # Aktualisiere Editor mit ihrem Inhalt
      updateAceEditor(session, "rulesEditor", value = rule_to_duplicate$content)
      
      # Schlage einen neuen Namen basierend auf dem Original vor
      updateTextInput(session, "ruleName", value = paste0(rule_to_duplicate$name, " (Kopie)"))
      updateTextAreaInput(session, "ruleDescription", value = rule_to_duplicate$description)
      
      # Setze die ausgewählte Regel zurück
      updateSelectInput(session, "savedRules", selected = "none")
      
      showNotification("Regel dupliziert. Bitte speichern Sie mit einem neuen Namen.", type = "message")
      addLog(paste("Regel dupliziert:", rule_to_duplicate$name))
    } else {
      showNotification("Bitte wählen Sie eine Regel zum Duplizieren aus", type = "warning")
    }
  })
  
  # Lösche ausgewählte Regel
  observeEvent(input$deleteRuleBtn, {
    if (input$confirmDeletes) {
      # Zeige Bestätigungsdialog, wenn aktiviert
      selected <- input$savedRulesTable_rows_selected
      saved_rules <- rules()
      
      if (!is.null(selected) && length(saved_rules) >= selected) {
        # Hole den Regelnamen
        rule_name <- saved_rules[[selected]]$name
        
        # Zeige einen Bestätigungsdialog
        showModal(modalDialog(
          title = "Löschen bestätigen",
          paste("Sind Sie sicher, dass Sie die Regel '", rule_name, "' löschen möchten?"),
          footer = tagList(
            actionButton("confirmDeleteBtn", "Ja, löschen", class = "btn-danger"),
            modalButton("Abbrechen")
          )
        ))
      } else {
        showNotification("Bitte wählen Sie eine Regel zum Löschen aus", type = "warning")
      }
    } else {
      # Direkt löschen ohne Bestätigung
      selected <- input$savedRulesTable_rows_selected
      saved_rules <- rules()
      
      if (!is.null(selected) && length(saved_rules) >= selected) {
        # Hole die zu löschende Regel
        rule_to_delete <- saved_rules[[selected]]
        
        # Lösche aus dem Dateisystem
        success <- deleteRuleById(rule_to_delete$id, rule_storage_dir)
        
        if (success) {
          # Entferne sie aus der reaktiven Liste
          saved_rules <- saved_rules[-selected]
          rules(saved_rules)
          
          # Aktualisiere das Dropdown
          updateRulesDropdown()
          
          showNotification(paste("Regel gelöscht:", rule_to_delete$name), type = "message")
          addLog(paste("Regel gelöscht:", rule_to_delete$name))
        } else {
          showNotification("Fehler beim Löschen der Regel", type = "error")
          addLog(paste("Fehler beim Löschen der Regel:", rule_to_delete$name))
        }
      } else {
        showNotification("Bitte wählen Sie eine Regel zum Löschen aus", type = "warning")
      }
    }
  })
  
  # Bestätigtes Löschen behandeln
  observeEvent(input$confirmDeleteBtn, {
    # Schließe den Dialog
    removeModal()
    
    # Hole die ausgewählte Regel
    selected <- input$savedRulesTable_rows_selected
    saved_rules <- rules()
    
    if (!is.null(selected) && length(saved_rules) >= selected) {
      # Hole die zu löschende Regel
      rule_to_delete <- saved_rules[[selected]]
      
      # Lösche aus dem Dateisystem
      success <- deleteRuleById(rule_to_delete$id, rule_storage_dir)
      
      if (success) {
        # Entferne sie aus der reaktiven Liste
        saved_rules <- saved_rules[-selected]
        rules(saved_rules)
        
        # Aktualisiere das Dropdown
        updateRulesDropdown()
        
        showNotification(paste("Regel gelöscht:", rule_to_delete$name), type = "message")
        addLog(paste("Regel gelöscht:", rule_to_delete$name))
      } else {
        showNotification("Fehler beim Löschen der Regel", type = "error")
        addLog(paste("Fehler beim Löschen der Regel:", rule_to_delete$name))
      }
    }
  })
  
  # Zeige gespeicherte Regeln in der Tabelle an
  output$savedRulesTable <- renderDT({
    saved_rules <- rules()
    
    if (length(saved_rules) > 0) {
      # Erstelle Datenrahmen aus gespeicherten Regeln
      df <- data.frame(
        Name = sapply(saved_rules, function(r) r$name),
        Beschreibung = sapply(saved_rules, function(r) r$description),
        Erstellt = sapply(saved_rules, function(r) r$created),
        Geändert = sapply(saved_rules, function(r) if("modified" %in% names(r)) r$modified else r$created),
        ID = sapply(saved_rules, function(r) r$id),
        stringsAsFactors = FALSE
      )
      
      datatable(df, selection = 'single', options = list(pageLength = 10))
    } else {
      datatable(data.frame(Nachricht = "Keine gespeicherten Regeln"), options = list(dom = 't'))
    }
  })
  
  # Exportiere ausgewählte Regel
  output$exportRuleBtn <- downloadHandler(
    filename = function() {
      selected <- input$savedRulesTable_rows_selected
      saved_rules <- rules()
      
      if (!is.null(selected) && length(saved_rules) >= selected) {
        rule_name <- gsub("[^a-zA-Z0-9]", "_", saved_rules[[selected]]$name)
        return(paste0(rule_name, "_", format(Sys.time(), "%Y%m%d"), ".json"))
      } else {
        return(paste0("regel_export_", format(Sys.time(), "%Y%m%d"), ".json"))
      }
    },
    content = function(file) {
      selected <- input$savedRulesTable_rows_selected
      saved_rules <- rules()
      
      if (!is.null(selected) && length(saved_rules) >= selected) {
        # Exportiere die ausgewählte Regel
        rule_content <- saved_rules[[selected]]$content
        writeLines(rule_content, file)
        addLog(paste("Regel exportiert:", saved_rules[[selected]]$name))
      } else {
        # Exportiere alle Regeln
        rules_content <- lapply(saved_rules, function(r) fromJSON(r$content, simplifyVector = FALSE))
        rules_json <- toJSON(unlist(rules_content, recursive = FALSE), pretty = TRUE, auto_unbox = TRUE)
        writeLines(rules_json, file)
        addLog("Alle Regeln exportiert")
      }
    }
  )
  
  # Import-Funktionalität
  observeEvent(input$importRuleFile, {
    req(input$importRuleFile)
    
    file <- input$importRuleFile
    
    # Prüfe Dateiendung
    if (tools::file_ext(file$datapath) != "json") {
      showNotification("Bitte laden Sie eine JSON-Datei hoch", type = "error")
      return()
    }
    
    # Versuche, die Datei zu lesen und zu parsen
    tryCatch({
      rule_content <- readLines(file$datapath, warn = FALSE)
      rule_content <- paste(rule_content, collapse = "\n")
      
      # Validiere die importierte Regel
      validation <- validateRule(rule_content)
      
      if (validation$valid) {
        # Fülle den Editor mit den importierten Regeln
        updateAceEditor(session, "rulesEditor", value = rule_content)
        
        # Schlage einen Namen vor
        updateTextInput(session, "ruleName", value = paste0("Importiert ", format(Sys.time(), "%Y-%m-%d")))
        
        showNotification("Regeln erfolgreich importiert. Bitte überprüfen und speichern.", type = "message")
        addLog("Regeln erfolgreich importiert")
      } else {
        showNotification(paste("Ungültige Regeldatei:", validation$message), type = "error")
        addLog(paste("Regelimport-Validierung fehlgeschlagen:", validation$message))
      }
    }, error = function(e) {
      showNotification(paste("Fehler beim Importieren der Regeln:", e$message), type = "error")
      addLog(paste("Fehler beim Importieren der Regeln:", e$message))
    })
  })
  
  # Aktualisiere Editor-Theme, wenn es geändert wird
  observeEvent(input$editorTheme, {
    updateAceEditor(session, "rulesEditor", theme = input$editorTheme)
    updateAceEditor(session, "inputJson", theme = input$editorTheme)
    updateAceEditor(session, "outputJson", theme = input$editorTheme)
  })
  
  # Auto-Save-Funktionalität
  observe({
    # Nur ausführen, wenn Auto-Save aktiviert ist
    req(input$autoSaveRules == TRUE)
    
    # Invalidiere alle X Minuten
    invalidateLater(input$autoSaveInterval * 60 * 1000)
    
    # Prüfe, ob es etwas zu speichern gibt
    if (input$ruleName != "" && input$rulesEditor != "") {
      # Validiere die Regel
      validation <- validateRule(input$rulesEditor)
      
      if (validation$valid) {
        # Löse den Speichern-Button aus
        click("saveRule")
        addLog("Regel automatisch gespeichert")
      }
    }
  })
  
  # Regelstatistik
  output$ruleStats <- renderUI({
    saved_rules <- rules()
    
    # Zähle Regeln nach Typ
    operations <- list()
    total_operations <- 0
    
    for (rule in saved_rules) {
      # Parse den Regelinhalt
      tryCatch({
        rule_content <- fromJSON(rule$content, simplifyVector = FALSE)
        
        # Zähle Operationen
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
        # Überspringe ungültige Regeln
      })
    }
    
    # Erstelle die Statistik-UI
    tagList(
      p(paste("Anzahl Regeln:", length(saved_rules))),
      p(paste("Anzahl Operationen:", total_operations)),
      h4("Operationen nach Typ:"),
      tags$ul(
        lapply(names(operations), function(op) {
          tags$li(paste(op, ":", operations[[op]]))
        })
      )
    )
  })
  
  # Speicherort
  output$storageLocation <- renderText({
    paste("Aktueller Speicherort:", rule_storage_dir)
  })
  
  # Backup-Funktionalität
  observeEvent(input$backupRulesBtn, {
    saved_rules <- rules()
    
    if (length(saved_rules) > 0) {
      # Erstelle ein Backup-Verzeichnis
      backup_dir <- file.path(tempdir(), "jsonr_backups")
      if (!dir.exists(backup_dir)) {
        dir.create(backup_dir, recursive = TRUE)
      }
      
      # Erstelle eine Backup-Datei mit Zeitstempel
      backup_file <- file.path(backup_dir, paste0("rules_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"))
      
      # Erstelle ein temporäres Verzeichnis für die Dateien
      temp_backup_dir <- file.path(tempdir(), paste0("backup_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(temp_backup_dir, recursive = TRUE)
      
      # Kopiere alle Regeldateien in das temporäre Verzeichnis
      for (rule in saved_rules) {
        rule_file <- file.path(rule_storage_dir, paste0(rule$id, ".json"))
        if (file.exists(rule_file)) {
          file.copy(rule_file, file.path(temp_backup_dir, paste0(rule$id, ".json")))
        }
      }
      
      # Kopiere auch die Indexdatei
      index_file <- file.path(rule_storage_dir, "rule_index.json")
      if (file.exists(index_file)) {
        file.copy(index_file, file.path(temp_backup_dir, "rule_index.json"))
      }
      
      # Erstelle eine ZIP-Datei
      if (requireNamespace("utils", quietly = TRUE)) {
        # Wechsle das Arbeitsverzeichnis zum temporären Verzeichnis
        old_wd <- getwd()
        setwd(temp_backup_dir)
        
        # Erstelle die ZIP-Datei
        utils::zip(backup_file, files = list.files())
        
        # Stelle das Arbeitsverzeichnis wieder her
        setwd(old_wd)
        
        # Räume das temporäre Verzeichnis auf
        unlink(temp_backup_dir, recursive = TRUE)
        
        showNotification(paste("Backup erstellt:", backup_file), type = "message")
        addLog(paste("Backup erstellt:", backup_file))
        
        # Biete an, das Backup herunterzuladen
        showModal(modalDialog(
          title = "Backup erstellt",
          paste("Backup erstellt unter:", backup_file),
          "Möchten Sie diese Backup-Datei herunterladen?",
          footer = tagList(
            downloadButton("downloadBackupBtn", "Backup herunterladen"),
            modalButton("Schließen")
          )
        ))
        
        # Speichere Backup-Pfad für Download
        backup_file_to_download(backup_file)
      }
    } else {
      showNotification("Keine Regeln zum Sichern vorhanden", type = "warning")
    }
  })
  
  # Backup herunterladen Handler
  output$downloadBackupBtn <- downloadHandler(
    filename = function() {
      basename(backup_file_to_download())
    },
    content = function(file) {
      file.copy(backup_file_to_download(), file)
    }
  )
  
  # Wiederherstellung aus Backup
  observeEvent(input$restoreBackupBtn, {
    showModal(modalDialog(
      title = "Regeln wiederherstellen",
      fileInput("restoreBackupFile", "Backup-ZIP-Datei hochladen", accept = ".zip"),
      p("Warnung: Dies überschreibt Ihre aktuellen Regeln. Stellen Sie sicher, dass Sie zuerst ein Backup erstellen."),
      footer = tagList(
        actionButton("confirmRestoreBtn", "Wiederherstellen", class = "btn-warning"),
        modalButton("Abbrechen")
      )
    ))
  })
  
  # Wiederherstellung bestätigen
  observeEvent(input$confirmRestoreBtn, {
    req(input$restoreBackupFile)
    
    backup_file <- input$restoreBackupFile$datapath
    
    # Erstelle ein temporäres Verzeichnis für die Extraktion
    restore_dir <- file.path(tempdir(), paste0("restore_", format(Sys.time(), "%Y%m%d%H%M%S")))
    dir.create(restore_dir, recursive = TRUE)
    
    # Versuche, die Backup-Datei zu entpacken
    tryCatch({
      utils::unzip(backup_file, exdir = restore_dir)
      
      # Prüfe, ob sie die erwartete Struktur hat
      index_file <- file.path(restore_dir, "rule_index.json")
      
      if (!file.exists(index_file)) {
        showNotification("Ungültige Backup-Datei: Kein Index gefunden", type = "error")
        return()
      }
      
      # Lese den Index zur Überprüfung
      index <- fromJSON(index_file, simplifyVector = FALSE)
      
      if (!("rules" %in% names(index))) {
        showNotification("Ungültige Backup-Datei: Ungültige Indexstruktur", type = "error")
        return()
      }
      
      # Zähle die Regeln im Backup
      rule_count <- length(index$rules)
      
      # Bestätige Wiederherstellung
      showModal(modalDialog(
        title = "Wiederherstellung bestätigen",
        paste("Backup enthält", rule_count, "Regeln. Mit der Wiederherstellung fortfahren?"),
        p("Dies ersetzt alle Ihre aktuellen Regeln."),
        footer = tagList(
          actionButton("executeRestoreBtn", "Ja, jetzt wiederherstellen", class = "btn-warning"),
          modalButton("Abbrechen")
        )
      ))
      
      # Speichere das Wiederherstellungsverzeichnis für später
      restore_source_dir(restore_dir)
      
    }, error = function(e) {
      showNotification(paste("Fehler beim Entpacken des Backups:", e$message), type = "error")
      unlink(restore_dir, recursive = TRUE)
    })
  })
  
  # Führe die Wiederherstellung aus
  observeEvent(input$executeRestoreBtn, {
    # Schließe alle Dialoge
    removeModal()
    
    # Kopiere alle Dateien aus dem Wiederherstellungsverzeichnis in das Regelspeicherverzeichnis
    tryCatch({
      # Lösche vorhandene Regeln
      rule_files <- list.files(rule_storage_dir, pattern = "\\.json$", full.names = TRUE)
      sapply(rule_files, file.remove)
      
      # Kopiere Dateien aus dem Wiederherstellungsverzeichnis
      restored_files <- list.files(restore_source_dir(), pattern = "\\.json$", full.names = TRUE)
      success <- sapply(restored_files, function(file) {
        file.copy(file, file.path(rule_storage_dir, basename(file)), overwrite = TRUE)
      })
      
      # Lade Regeln neu
      saved_rules <- loadRulesFromFiles(rule_storage_dir)
      rules(saved_rules)
      updateRulesDropdown()
      
      showNotification(paste("Erfolgreich", sum(success), "Regeln wiederhergestellt"), type = "message")
      addLog(paste(sum(success), "Regeln aus Backup wiederhergestellt"))
      
      # Aufräumen
      unlink(restore_source_dir(), recursive = TRUE)
      
    }, error = function(e) {
      showNotification(paste("Fehler bei der Wiederherstellung:", e$message), type = "error")
      addLog(paste("Fehler bei der Wiederherstellung:", e$message))
    })
  })
  
  # Handle JSON-Datei-Upload
  observeEvent(input$jsonFile, {
    file <- input$jsonFile
    
    if (is.null(file)) {
      return()
    }
    
    ext <- tools::file_ext(file$datapath)
    
    if (ext != "json") {
      showNotification("Bitte laden Sie eine JSON-Datei (.json) hoch", type = "error")
      return()
    }
    
    # Lesen und validieren des JSON
    tryCatch({
      json_content <- readLines(file$datapath)
      json_content <- paste(json_content, collapse = "\n")
      json_test <- fromJSON(json_content) # Nur zur Validierung
      
      updateAceEditor(session, "inputJson", value = json_content)
      addLog(paste("Datei geladen:", file$name))
    }, error = function(e) {
      showNotification(paste("Ungültiges JSON-Format:", e$message), type = "error")
      addLog(paste("Fehler beim Laden der Datei:", e$message))
    })
  })
  
  # Transformiere JSON basierend auf Regeln
  observeEvent(input$transformBtn, {
    req(input$inputJson)
    
    # Vorherige Ausgabe löschen
    updateAceEditor(session, "outputJson", value = "")
    
    tryCatch({
      # Parse Eingabe-JSON
      input_json <- fromJSON(input$inputJson, simplifyVector = FALSE)
      
      # Parse Transformationsregeln
      rules_json <- input$rulesEditor
      transform_rules <- fromJSON(rules_json, simplifyVector = FALSE)
      
      # Wende Regeln nacheinander an
      output_json <- input_json
      
      for (rule_index in seq_along(transform_rules)) {
        rule <- transform_rules[[rule_index]]
        
        addLog(paste("Verarbeite Regel", rule_index, ":", rule$operation), isDebug = TRUE)
        
        if (rule$operation == "add_element") {
          # Validiere erforderliche Felder
          if (!all(c("path", "name") %in% names(rule))) {
            addLog(paste("Warnung: Fehlende erforderliche Felder für add_element in Regel", rule_index))
            next
          }
          
          # Behandle Funktionswert
          value <- rule$value
          if (is.list(value) && "_function_" %in% names(value)) {
            func_code <- value[["_function_"]]
            addLog(paste("Evaluiere Funktion:", func_code), isDebug = TRUE)
            
            # Erstelle ein sicheres Umfeld für die Funktionsauswertung
            safe_env <- new.env(parent = baseenv())
            
            # Füge notwendige globale Funktionen hinzu
            safe_env$Sys.time <- Sys.time
            safe_env$paste <- paste
            safe_env$paste0 <- paste0
            safe_env$round <- round
            
            # Füge Kontextvariablen hinzu
            safe_env$root <- input_json
            
            # Erstelle die Funktion mit ordnungsgemäßer Fehlerbehandlung
            tryCatch({
              # Evaluiere die Funktion
              result <- eval(parse(text = func_code), envir = safe_env)
              value <- result
            }, error = function(e) {
              addLog(paste("Fehler bei der Funktionsauswertung:", e$message))
              value <- NULL
            })
          }
          
          # Füge das Element mit richtiger Positionierung hinzu
          path <- rule$path
          name <- rule$name
          position <- if ("position" %in% names(rule)) rule$position else NULL
          
          # Verwende add_json_element, wenn verfügbar
          if (exists("add_json_element")) {
            try({
              output_json <- add_json_element(
                output_json, 
                path, 
                name, 
                value, 
                position = position,
                create_path = TRUE
              )
            }, silent = TRUE)
          } else {
            # Fallback: Eigene Implementierung
            # (Falls jsonr nicht verfügbar ist)
            
            # Hier Code für eine einfache add_element Implementierung einfügen
          }
          
          addLog(paste("add_element angewendet:", name, "auf Pfad:", path, 
                       if(!is.null(position)) paste("an Position", position) else ""))
        } 
        else if (rule$operation == "insert_property") {
          # Validiere erforderliche Felder
          if (!all(c("array_path", "property_name") %in% names(rule))) {
            addLog(paste("Warnung: Fehlende erforderliche Felder für insert_property in Regel", rule_index))
            next
          }
          
          # Extrahiere Parameter
          array_path_input <- rule$array_path
          # Konvertiere verschiedene Formate zu Zeichenvektor
          array_path <- NULL
          
          if (is.character(array_path_input)) {
            # Fall 1: String mit Punkten
            if (length(array_path_input) == 1) {
              array_path <- unlist(strsplit(array_path_input, "\\."))
            } 
            # Fall 2: Bereits ein Zeichenvektor
            else {
              array_path <- array_path_input
            }
          } 
          # Fall 3: JSON-Array wurde als Liste geladen
          else if (is.list(array_path_input)) {
            array_path <- as.character(unlist(array_path_input))
          }
          
          # Prüfe, ob wir einen gültigen Zeichenvektor haben
          if (is.null(array_path) || length(array_path) == 0) {
            addLog("Fehler: array_path konnte nicht in einen gültigen Zeichenvektor konvertiert werden")
            next
          }
          
          property_name <- rule$property_name
          property_value <- rule$property_value
          position_type <- if("position_type" %in% names(rule)) rule$position_type else "last"
          position_ref <- if ("position_ref" %in% names(rule)) rule$position_ref else NULL
          
          # Debug-Logging
          if (input$debugMode) {
            addLog(paste("Debug - Original array_path:", 
                         if(is.character(array_path_input)) paste(array_path_input, collapse=", ") 
                         else "kein Zeichenvektor"), isDebug = TRUE)
            addLog(paste("Debug - Konvertierter array_path:", paste(array_path, collapse=", ")), isDebug = TRUE)
            addLog(paste("Debug - property_name:", property_name), isDebug = TRUE)
            addLog(paste("Debug - position_type:", position_type), isDebug = TRUE)
            if (!is.null(position_ref)) addLog(paste("Debug - position_ref:", position_ref), isDebug = TRUE)
          }
          
          # Handle Funktionswert
          filter_fn <- NULL
          if (is.list(property_value) && "_function_" %in% names(property_value)) {
            func_code <- property_value[["_function_"]]
            addLog(paste("Funktionscode ist:", func_code), isDebug = TRUE)
            
            # Erstelle eine tatsächliche Funktion, die an insert_json_property übergeben werden kann
            property_value <- function(elem, index, parent, parent_index, root, ...) {
              # Erstelle eine sichere Umgebung für die Auswertung
              safe_env <- new.env(parent = baseenv())
              
              # Füge notwendige globale Funktionen hinzu
              safe_env$paste <- paste
              safe_env$paste0 <- paste0
              safe_env$round <- round
              safe_env$names <- names
              safe_env$length <- length
              
              # Füge Kontextvariablen der Umgebung hinzu
              safe_env$elem <- elem
              safe_env$index <- index
              safe_env$parent <- parent
              safe_env$parent_index <- parent_index
              safe_env$root <- root
              
              # Evaluiere den Funktionskörper in dieser Umgebung
              result <- NULL
              tryCatch({
                result <- eval(parse(text = func_code), envir = safe_env)
              }, error = function(e) {
                addLog(paste("Fehler bei der Funktionsauswertung:", e$message))
                result <- paste("Fehler:", e$message) # Fallback-Wert
              })
              
              return(result)
            }
            
            addLog("Dynamische Funktion für property_value erstellt", isDebug = TRUE)
          }
          
          # Wende Transformation mit sorgfältiger Fehlerbehandlung an
          tryCatch({
            addLog("Versuche, insert_json_property aufzurufen...", isDebug = TRUE)
            
            # Verwende insert_json_property, wenn verfügbar
            if (exists("insert_json_property")) {
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
              
              # Nur aktualisieren, wenn erfolgreich
              if (!is.null(result)) {
                output_json <- result
                addLog(paste("insert_property angewendet:", property_name, "auf Array-Pfad:", 
                             paste(array_path, collapse = " > ")))
              } else {
                addLog("Warnung: insert_json_property gab NULL zurück", isDebug = TRUE)
              }
            } else {
              addLog("Warnung: insert_json_property Funktion nicht verfügbar", isDebug = TRUE)
            }
          }, error = function(e) {
            addLog(paste("Fehler in insert_json_property:", e$message))
          })
        }
        else if (rule$operation == "modify_array") {
          # Validiere erforderliche Felder
          if (!all(c("path", "action") %in% names(rule))) {
            addLog(paste("Warnung: Fehlende erforderliche Felder für modify_array in Regel", rule_index))
            next
          }
          
          # Extrahiere Parameter
          path <- rule$path
          action <- rule$action
          value <- if ("value" %in% names(rule)) rule$value else NULL
          position <- if ("position" %in% names(rule)) rule$position else NULL
          filter <- if ("filter" %in% names(rule)) rule$filter else NULL
          
          # Handle Funktionswert
          if (is.list(value) && "_function_" %in% names(value)) {
            func_code <- value[["_function_"]]
            
            # Erstelle eine sichere Umgebung für die Auswertung
            safe_env <- new.env(parent = baseenv())
            
            # Füge notwendige globale Funktionen hinzu
            safe_env$paste <- paste
            safe_env$paste0 <- paste0
            safe_env$round <- round
            
            # Füge Kontextvariablen der Umgebung hinzu
            safe_env$root <- input_json
            
            # Evaluiere den Funktionskörper
            tryCatch({
              value <- eval(parse(text = func_code), envir = safe_env)
            }, error = function(e) {
              addLog(paste("Fehler bei der Funktionsauswertung:", e$message))
              value <- NULL
            })
          }
          
          # Implementiere modify_array Funktion, falls nicht vorhanden
          if (!exists("modify_array")) {
            # Einfache Implementierung von modify_array
            modify_array <- function(json_data, path, action, value = NULL, position = NULL, filter = NULL) {
              # Hole das Array am angegebenen Pfad
              path_parts <- unlist(strsplit(path, "\\."))
              current <- json_data
              
              for (part in path_parts) {
                if (!is.list(current) || !(part %in% names(current))) {
                  return(json_data) # Pfad nicht gefunden, Original zurückgeben
                }
                current <- current[[part]]
              }
              
              # Prüfe, ob es ein Array ist
              if (!is.list(current) || is.null(names(current)) && length(current) == 0) {
                return(json_data) # Kein Array oder leeres Array
              }
              
              # Führe die gewünschte Aktion aus
              if (action == "append") {
                # Füge am Ende hinzu
                current <- c(current, list(value))
              } 
              else if (action == "prepend") {
                # Füge am Anfang hinzu
                current <- c(list(value), current)
              } 
              else if (action == "insert" && !is.null(position)) {
                # Füge an bestimmter Position ein
                if (position < 1 || position > length(current) + 1) {
                  addLog(paste("Warnung: Ungültige Position:", position))
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
                # Ersetze an bestimmter Position
                if (position < 1 || position > length(current)) {
                  addLog(paste("Warnung: Ungültige Position:", position))
                  return(json_data)
                }
                
                current[[position]] <- value
              } 
              else if (action == "remove") {
                # Entferne basierend auf Position oder Filter
                if (!is.null(position)) {
                  if (position < 1 || position > length(current)) {
                    addLog(paste("Warnung: Ungültige Position:", position))
                    return(json_data)
                  }
                  
                  current <- current[-position]
                } 
                else if (!is.null(filter)) {
                  # Wende Filter an
                  filter_func <- NULL
                  
                  if (is.list(filter) && "_function_" %in% names(filter)) {
                    filter_code <- filter[["_function_"]]
                    
                    # Erstelle Filter-Funktion
                    filter_func <- function(elem) {
                      # Erstelle eine sichere Umgebung
                      safe_env <- new.env(parent = baseenv())
                      safe_env$elem <- elem
                      
                      result <- FALSE
                      tryCatch({
                        result <- eval(parse(text = filter_code), envir = safe_env)
                      }, error = function(e) {
                        addLog(paste("Fehler im Filter:", e$message))
                      })
                      
                      return(result)
                    }
                    
                    # Finde Elemente, die dem Filter entsprechen
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
              
              # Aktualisiere das Original-JSON mit dem modifizierten Array
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
          }
          
          # Führe die Transformation aus
          output_json <- modify_array(
            output_json,
            path,
            action,
            value,
            position,
            filter
          )
          
          addLog(paste("modify_array angewendet:", action, "auf Pfad:", path))
        } 
        else {
          addLog(paste("Warnung: Unbekannte Operation:", rule$operation))
        }
      }
      
      # Formatiere und zeige die Ausgabe an
      output_json_str <- if (input$prettyOutput) {
        toJSON(output_json, pretty = TRUE, auto_unbox = TRUE)
      } else {
        toJSON(output_json, auto_unbox = TRUE)
      }
      
      updateAceEditor(session, "outputJson", value = output_json_str)
      addLog("Transformation erfolgreich abgeschlossen")
      
    }, error = function(e) {
      error_msg <- paste("Fehler während der Transformation:", e$message)
      showNotification(error_msg, type = "error")
      addLog(error_msg)
      
      if (input$showErrorDetails) {
        # Zeige Fehlerdetails in der Ausgabe
        error_detail <- paste0('{\n  "error": "', e$message, '",\n  "stacktrace": "', 
                               paste(capture.output(traceback()), collapse = "\\n"), '"\n}')
        updateAceEditor(session, "outputJson", value = error_detail)
      }
    })
  })
  
  # Test-Button für Debugging
  observeEvent(input$testBtn, {
    if (input$debugMode) {
      addLog("Führe Testfunktion aus", isDebug = TRUE)
      
      # Erstelle ein Test-JSON-Objekt
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
      
      # Teste insert_json_property mit Array primitiver Elemente
      addLog("Teste insert_json_property mit Array primitiver Elemente", isDebug = TRUE)
      
      # Testfall 1: Transformiere Array-Elemente zu Objekten
      if (exists("insert_json_property")) {
        test_result <- insert_json_property(
          test_json,
          array_path = c("user", "profile", "interests"),
          property_name = "display_name",
          property_value = function(elem, index, parent, parent_index, root) {
            return(paste0(toupper(substr(elem, 1, 1)), substr(elem, 2, nchar(elem))))
          }
        )
        
        result_json <- toJSON(test_result, pretty = TRUE, auto_unbox = TRUE)
        addLog("Testergebnis:", isDebug = TRUE)
        addLog(result_json, isDebug = TRUE)
        
        # Zeige Testergebnis in der Ausgabe
        updateAceEditor(session, "outputJson", value = result_json)
        showNotification("Test abgeschlossen, siehe Log für Details", type = "message")
      } else {
        addLog("insert_json_property Funktion nicht verfügbar", isDebug = TRUE)
      }
    }
  })
  
  # Transformiertes JSON herunterladen
  output$downloadJson <- downloadHandler(
    filename = function() {
      paste0("transformed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      # Schreibe das transformierte JSON in die Datei
      writeLines(input$outputJson, file)
      addLog("Transformiertes JSON heruntergeladen")
    }
  )
  
  # In Zwischenablage kopieren Button
  observeEvent(input$copyBtn, {
    # In einer echten App würden wir JavaScript verwenden, um in die Zwischenablage zu kopieren
    showNotification("In Zwischenablage kopiert (simuliert)", type = "message")
    addLog("Ausgabe in Zwischenablage kopiert (simuliert)")
  })
  
  # Initialisiere die App
  observeEvent(1, {
    addLog("Anwendung initialisiert")
  }, once = TRUE)
}

# Starte die Anwendung
shinyApp(ui = ui, server = server)