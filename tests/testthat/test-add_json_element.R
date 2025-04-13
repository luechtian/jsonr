# Test add_json_element function

test_that("add_json_element adds element to top level", {
  # Test adding to the top level of a JSON object (using empty path)
  result <- add_json_element(simple_json_data, "", "status", TRUE)
  
  # Check that the element was added
  expect_true("status" %in% names(result))
  expect_true(result$status)
})

test_that("add_json_element adds element to nested path", {
  # Test adding to a nested path
  result <- add_json_element(simple_json_data, "person", "email", "john@example.com")
  
  # Check that the element was added
  expect_true("email" %in% names(result$person))
  expect_equal(result$person$email, "john@example.com")
})

test_that("add_json_element adds element at specific position", {
  # Test adding at the beginning (position = 1)
  result <- add_json_element(simple_json_data, "person", "id", 123, position = 1)
  
  # Check that the element was added at position 1
  expect_equal(names(result$person)[1], "id")
  expect_equal(result$person$id, 123)
  
  # Test adding in the middle (position = 2)
  result <- add_json_element(simple_json_data, "person", "email", "john@example.com", position = 2)
  
  # Check that the element was added at position 2
  expect_equal(names(result$person)[2], "email")
  expect_equal(result$person$email, "john@example.com")
})

test_that("add_json_element works with deeply nested paths", {
  # Test adding to a deeply nested path
  result <- add_json_element(complex_json_data, "person.address", "country", "Germany")
  
  # Check that the element was added
  expect_true("country" %in% names(result$person$address))
  expect_equal(result$person$address$country, "Germany")
})

test_that("add_json_element creates missing path components when create_path=TRUE", {
  # Test creating a missing path
  result <- add_json_element(simple_json_data, "person.contact.phone", "mobile", 
                             "+1234567890", create_path = TRUE)
  
  # Check that the path was created
  expect_true("contact" %in% names(result$person))
  expect_true("phone" %in% names(result$person$contact))
  expect_equal(result$person$contact$phone$mobile, "+1234567890")
})

test_that("add_json_element throws error when path doesn't exist and create_path=FALSE", {
  # Test error when path doesn't exist
  expect_error(
    add_json_element(simple_json_data, "person.contact.phone", "mobile", "+1234567890"),
    "Path component 'contact' not found"
  )
})

test_that("add_json_element validates position", {
  # Test error when position is invalid
  expect_error(
    add_json_element(simple_json_data, "person", "email", "john@example.com", position = 0),
    "Invalid position: 0"
  )
  
  expect_error(
    add_json_element(simple_json_data, "person", "email", "john@example.com", position = 10),
    "Invalid position"
  )
})

test_that("add_json_element works with JSON files", {
  # Create a temporary file for testing
  temp_file <- simple_json_file
  
  # Test adding to the file - note using empty path "" and create_path=TRUE
  result <- add_json_element(temp_file, "", "status", TRUE, create_path = TRUE)
  
  # Check that the element was added
  expect_true("status" %in% names(result))
  expect_true(result$status)
  
  # Test adding to a nested path
  output_file <- file.path(tempdir(), "output_test.json")
  result <- add_json_element(temp_file, "person", "email", "john@example.com", 
                             output_file_path = output_file)
  
  # Check that the output file was created and contains the correct data
  expect_true(file.exists(output_file))
  output_data <- jsonlite::fromJSON(output_file, simplifyVector = FALSE)
  expect_true("email" %in% names(output_data$person))
  expect_equal(output_data$person$email, "john@example.com")
})

test_that("add_json_element handles various data types", {
  # Create independent tests for each data type
  
  # Number
  json1 <- list()
  json1 <- add_json_element(json1, "", "number_value", 42)
  expect_equal(json1$number_value, 42)
  
  # Boolean
  json2 <- list()
  json2 <- add_json_element(json2, "", "boolean_value", TRUE)
  expect_true(json2$boolean_value)
  
  # NULL
  json3 <- list()
  json3 <- add_json_element(json3, "", "null_value", NULL)
  expect_null(json3$null_value)
  
  # List
  json4 <- list()
  json4 <- add_json_element(json4, "", "list_value", list(width = 100, height = 200))
  expect_equal(json4$list_value$width, 100)
  expect_equal(json4$list_value$height, 200)
  
  # Array
  json5 <- list()
  json5 <- add_json_element(json5, "", "array_value", list(1, 2, 3))
  expect_equal(json5$array_value, list(1, 2, 3))
})

test_that("add_json_element validates input types", {
  # Test error when json_input is not a file path or a list
  expect_error(
    add_json_element(123, "person", "email", "john@example.com"),
    "Invalid input"
  )
  
  # Test error when file doesn't exist
  expect_error(
    add_json_element("non_existent_file.json", "person", "email", "john@example.com"),
    "File not found"
  )
})