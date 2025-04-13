# Test data for JSON manipulation tests

# Simple test data
simple_json <- '{
  "person": {
    "name": "John Doe",
    "age": 30
  },
  "settings": {
    "darkMode": true,
    "notifications": false
  }
}'

# Convert to R list
simple_json_data <- jsonlite::fromJSON(simple_json, simplifyVector = FALSE)

# Complex test data with nested arrays
complex_json <- '{
  "person": {
    "name": "Max Mustermann",
    "age": 30,
    "email": "max@example.com",
    "address": {
      "street": "Hauptstraße 1",
      "city": "Berlin",
      "postalCode": "10115"
    }
  },
  "items": [
    {
      "id": 1,
      "name": "Laptop",
      "price": 999.99,
      "Test": [
        {
          "DisplayName": "Laptop Control Level 1",
          "Level": 1
        },
        {
          "DisplayName": "Control Level 2",
          "Level": 2
        }
      ]
    },
    {
      "id": 2,
      "name": "Smartphone",
      "price": 599.99,
      "Test": [
        {
          "DisplayName": "Smartphone Control Level 1",
          "Level": 1
        },
        {
          "DisplayName": "Control Level 2",
          "Level": 2
        }
      ]
    }
  ],
  "settings": {
    "darkMode": true,
    "notifications": false
  }
}'

# Convert to R list
complex_json_data <- jsonlite::fromJSON(complex_json, simplifyVector = FALSE)

# Incomplete test data (missing some fields)
incomplete_json <- '{
  "person": {
    "name": "Max Mustermann",
    "age": 30,
    "address": {
      "street": "Hauptstraße 1",
      "city": "Berlin",
      "postalCode": "10115"
    }
  },
  "items": [
    {
      "id": 1,
      "name": "Laptop"
    },
    {
      "id": 2,
      "name": "Smartphone"
    }
  ],
  "settings": {
    "darkMode": true,
    "notifications": false
  }
}'

# Convert to R list
incomplete_json_data <- jsonlite::fromJSON(incomplete_json, simplifyVector = FALSE)

# Create a temporary JSON file for testing
create_temp_json_file <- function(content, file_name = "temp_test.json") {
  temp_file <- file.path(tempdir(), file_name)
  writeLines(content, temp_file)
  return(temp_file)
}

# Create a test file paths
simple_json_file <- create_temp_json_file(simple_json, "simple_test.json")
complex_json_file <- create_temp_json_file(complex_json, "complex_test.json")
incomplete_json_file <- create_temp_json_file(incomplete_json, "incomplete_test.json")
