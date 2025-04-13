# Test utility functions

test_that("is_valid_json correctly validates JSON strings", {
  # Test valid JSON
  expect_true(is_valid_json('{"name": "John"}'))
  expect_true(is_valid_json('[1, 2, 3]'))
  
  # Test invalid JSON
  expect_false(is_valid_json('{"name": "John"'))
  expect_false(is_valid_json('not a json'))
})

test_that("is_valid_json correctly validates JSON files", {
  # Test with valid JSON file
  expect_true(is_valid_json(simple_json_file, is_file = TRUE))
  
  # Test with non-existent file
  expect_false(is_valid_json("non_existent_file.json", is_file = TRUE))
  
  # Create an invalid JSON file
  invalid_file <- file.path(tempdir(), "invalid.json")
  writeLines('{"invalid": "json"', invalid_file)
  expect_false(is_valid_json(invalid_file, is_file = TRUE))
})

test_that("get_nested_value retrieves values from nested paths", {
  # Test simple path
  expect_equal(get_nested_value(complex_json_data, "person.name"), "Max Mustermann")
  
  # Test deeper path
  expect_equal(get_nested_value(complex_json_data, "person.address.city"), "Berlin")
  
  # Test path with array index
  expect_equal(get_nested_value(complex_json_data, "items.0.name"), NULL)  # This should fail as we don't support array indexing
  
  # Test non-existent path
  expect_null(get_nested_value(complex_json_data, "non.existent.path"))
  
  # Test default value
  expect_equal(get_nested_value(complex_json_data, "non.existent.path", default = "default"), "default")
})

test_that("pretty_json formats JSON objects and strings", {
  # Test with JSON object
  pretty_obj <- pretty_json(simple_json_data)
  expect_true(is.character(pretty_obj))
  expect_true(grepl("John Doe", pretty_obj))
  
  # Test with JSON string
  pretty_str <- pretty_json('{"name":"John","age":30}')
  expect_true(is.character(pretty_str))
  expect_true(grepl("John", pretty_str))
  
  # Test error with invalid input
  expect_error(pretty_json(123), "Invalid input")
  expect_error(pretty_json('{"invalid": "json"'), "Invalid JSON string")
})

test_that("compare_json identifies differences between JSON structures", {
  # Create two different JSON objects
  json1 <- jsonlite::fromJSON('{"name":"John","age":30,"city":"New York"}', simplifyVector = FALSE)
  json2 <- jsonlite::fromJSON('{"name":"John","age":35,"country":"USA"}', simplifyVector = FALSE)
  
  # Compare the structures
  diffs <- compare_json(json1, json2)
  
  # Check that differences were found
  expect_true(length(diffs) > 0)
  
  # Check specific differences
  age_diff <- NULL
  city_diff <- NULL
  country_diff <- NULL
  
  for (diff in diffs) {
    if (diff$path == "age") age_diff <- diff
    if (diff$path == "city") city_diff <- diff
    if (diff$path == "country") country_diff <- diff
  }
  
  # Verify age difference
  expect_false(is.null(age_diff))
  expect_equal(age_diff$value1, 30)
  expect_equal(age_diff$value2, 35)
  
  # Verify city difference (present in json1, missing in json2)
  expect_false(is.null(city_diff))
  expect_equal(city_diff$value1, "New York")
  expect_null(city_diff$value2)
  
  # Verify country difference (missing in json1, present in json2)
  expect_false(is.null(country_diff))
  expect_null(country_diff$value1)
  expect_equal(country_diff$value2, "USA")
  
  # Test with identical structures
  json3 <- jsonlite::fromJSON('{"name":"John","age":30}', simplifyVector = FALSE)
  json4 <- jsonlite::fromJSON('{"name":"John","age":30}', simplifyVector = FALSE)
  
  expect_equal(length(compare_json(json3, json4)), 0)
  
  # Test with nested structures
  json5 <- jsonlite::fromJSON('{"person":{"name":"John","address":{"city":"New York"}}}', simplifyVector = FALSE)
  json6 <- jsonlite::fromJSON('{"person":{"name":"John","address":{"city":"Boston"}}}', simplifyVector = FALSE)
  
  diffs2 <- compare_json(json5, json6)
  expect_true(length(diffs2) > 0)
  
  # Find the nested difference
  nested_diff <- NULL
  for (diff in diffs2) {
    if (diff$path == "person.address.city") nested_diff <- diff
  }
  
  expect_false(is.null(nested_diff))
  expect_equal(nested_diff$value1, "New York")
  expect_equal(nested_diff$value2, "Boston")
})
