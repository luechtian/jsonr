# Test insert_json_property function

test_that("insert_json_property adds property to all matching elements", {
  # Test inserting a property
  result <- insert_json_property(
    complex_json_data,
    array_path = c("items", "Test"),
    property_name = "TestId",
    property_value = 1,
    position_type = "last"  # Explicitly set position_type to avoid ambiguity
  )
  
  # Check that the property was added to all Test elements
  for (i in 1:length(result$items)) {
    for (j in 1:length(result$items[[i]]$Test)) {
      expect_true("TestId" %in% names(result$items[[i]]$Test[[j]]))
      expect_equal(result$items[[i]]$Test[[j]]$TestId, 1)
    }
  }
})

test_that("insert_json_property positions elements correctly with 'first'", {
  # Test inserting at the beginning
  result <- insert_json_property(
    complex_json_data,
    array_path = c("items", "Test"),
    property_name = "TestId",
    property_value = 1,
    position_type = "first"
  )
  
  # Check that the property was added at the first position
  for (i in 1:length(result$items)) {
    for (j in 1:length(result$items[[i]]$Test)) {
      expect_equal(names(result$items[[i]]$Test[[j]])[1], "TestId")
    }
  }
})

test_that("insert_json_property positions elements correctly with 'last'", {
  # Test inserting at the end
  result <- insert_json_property(
    complex_json_data,
    array_path = c("items", "Test"),
    property_name = "TestId",
    property_value = 1,
    position_type = "last"
  )
  
  # Check that the property was added at the last position
  for (i in 1:length(result$items)) {
    for (j in 1:length(result$items[[i]]$Test)) {
      last_pos <- length(names(result$items[[i]]$Test[[j]]))
      expect_equal(names(result$items[[i]]$Test[[j]])[last_pos], "TestId")
    }
  }
})

test_that("insert_json_property positions elements correctly with 'after'", {
  # Test inserting after a specific property
  result <- insert_json_property(
    complex_json_data,
    array_path = c("items", "Test"),
    property_name = "TestId",
    property_value = 1,
    position_type = "after",
    position_ref = "DisplayName"
  )
  
  # Check that the property was added after DisplayName
  for (i in 1:length(result$items)) {
    for (j in 1:length(result$items[[i]]$Test)) {
      # Find the index of DisplayName
      display_name_index <- which(names(result$items[[i]]$Test[[j]]) == "DisplayName")
      expect_equal(names(result$items[[i]]$Test[[j]])[display_name_index + 1], "TestId")
    }
  }
})

test_that("insert_json_property positions elements correctly with 'before'", {
  # Test inserting before a specific property
  result <- insert_json_property(
    complex_json_data,
    array_path = c("items", "Test"),
    property_name = "TestId",
    property_value = 1,
    position_type = "before",
    position_ref = "Level"
  )
  
  # Check that the property was added before Level
  for (i in 1:length(result$items)) {
    for (j in 1:length(result$items[[i]]$Test)) {
      # Find the index of Level
      level_index <- which(names(result$items[[i]]$Test[[j]]) == "Level")
      expect_equal(names(result$items[[i]]$Test[[j]])[level_index - 1], "TestId")
    }
  }
})

test_that("insert_json_property positions elements correctly with 'position'", {
  # Test inserting at a specific position
  result <- insert_json_property(
    complex_json_data,
    array_path = c("items", "Test"),
    property_name = "TestId",
    property_value = 1,
    position_type = "position",
    position_ref = 2
  )
  
  # Check that the property was added at position 2
  for (i in 1:length(result$items)) {
    for (j in 1:length(result$items[[i]]$Test)) {
      expect_equal(names(result$items[[i]]$Test[[j]])[2], "TestId")
    }
  }
})

test_that("insert_json_property applies filter_fn correctly", {
  # Test filtering specific elements
  result <- insert_json_property(
    complex_json_data,
    array_path = c("items", "Test"),
    property_name = "FilteredId",
    property_value = 1,
    position_type = "last",  # Explicitly set position_type to avoid ambiguity
    filter_fn = function(elem) {
      return(elem$Level == 1)
    }
  )
  
  # Check that the property was only added to elements with Level == 1
  for (i in 1:length(result$items)) {
    for (j in 1:length(result$items[[i]]$Test)) {
      if (result$items[[i]]$Test[[j]]$Level == 1) {
        expect_true("FilteredId" %in% names(result$items[[i]]$Test[[j]]))
      } else {
        expect_false("FilteredId" %in% names(result$items[[i]]$Test[[j]]))
      }
    }
  }
})

test_that("insert_json_property handles dynamic property values with functions", {
  # Test using a function for property values
  result <- insert_json_property(
    complex_json_data,
    array_path = c("items", "Test"),
    property_name = "ItemName",
    property_value = function(elem, index, parent, parent_index, root) {
      return(parent$name)
    },
    position_type = "last"  # Explicitly set position_type to avoid ambiguity
  )
  
  # Check that the property was added with the correct dynamic value
  for (i in 1:length(result$items)) {
    item_name <- result$items[[i]]$name
    for (j in 1:length(result$items[[i]]$Test)) {
      expect_equal(result$items[[i]]$Test[[j]]$ItemName, item_name)
    }
  }
})

test_that("insert_json_property validates inputs", {
  # Test error when json_data is not a list
  expect_error(
    insert_json_property(
      "not_a_list",
      array_path = c("items", "Test"),
      property_name = "TestId",
      property_value = 1,
      position_type = "last"
    ),
    "json_data must be a list"
  )
  
  # Test error when array_path is not a character vector
  expect_error(
    insert_json_property(
      complex_json_data,
      array_path = 123,
      property_name = "TestId",
      property_value = 1,
      position_type = "last"
    ),
    "array_path must be a non-empty character vector"
  )
  
  # Test error when property_name is not a string
  expect_error(
    insert_json_property(
      complex_json_data,
      array_path = c("items", "Test"),
      property_name = 123,
      property_value = 1,
      position_type = "last"
    ),
    "property_name must be a single character string"
  )
  
  # Test error when position_type is invalid
  expect_error(
    insert_json_property(
      complex_json_data,
      array_path = c("items", "Test"),
      property_name = "TestId",
      property_value = 1,
      position_type = "invalid"
    ),
    "position_type must be one of the following values"
  )
  
  # Test error when position_ref is missing for after/before
  expect_error(
    insert_json_property(
      complex_json_data,
      array_path = c("items", "Test"),
      property_name = "TestId",
      property_value = 1,
      position_type = "after"
    ),
    "position_ref must be a character string when position_type is 'after' or 'before'"
  )
  
  # Test error when position_ref is not numeric for position
  expect_error(
    insert_json_property(
      complex_json_data,
      array_path = c("items", "Test"),
      property_name = "TestId",
      property_value = 1,
      position_type = "position",
      position_ref = "invalid"
    ),
    "position_ref must be a number when position_type is 'position'"
  )
})

test_that("insert_json_property handles missing paths gracefully", {
  # Test with a path that doesn't exist
  result <- insert_json_property(
    simple_json_data,
    array_path = c("non_existent", "path"),
    property_name = "TestId",
    property_value = 1,
    position_type = "last"  # Explicitly set position_type to avoid ambiguity
  )
  
  # The function should return the original data unchanged
  expect_equal(result, simple_json_data)
})

test_that("insert_json_property handles multiple levels of nesting", {
  # Create a test JSON with multiple levels of nesting
  nested_json <- jsonlite::fromJSON('{
    "level1": [
      {
        "name": "First",
        "level2": [
          {
            "id": 1,
            "level3": [
              {"value": "A"},
              {"value": "B"}
            ]
          }
        ]
      }
    ]
  }', simplifyVector = FALSE)
  
  # Test with a deeply nested path
  result <- insert_json_property(
    nested_json,
    array_path = c("level1", "level2", "level3"),
    property_name = "added",
    property_value = TRUE,  # Changed true to TRUE
    position_type = "last"  # Explicitly set position_type to avoid ambiguity
  )
  
  # Check that the property was added to the deepest level
  expect_true(result$level1[[1]]$level2[[1]]$level3[[1]]$added)
  expect_true(result$level1[[1]]$level2[[1]]$level3[[2]]$added)
})