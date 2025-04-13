# jsonr: Advanced JSON Manipulation Tools for R

<!-- badges: start -->
[![R-CMD-check](https://github.com/luechtian/jsonr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/luechtian/jsonr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/luechtian/jsonr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/luechtian/jsonr?branch=main)
<!-- badges: end -->

## Overview

The `jsonr` package provides advanced tools for manipulating JSON data in R. It extends the functionality of the `jsonlite` package by offering specialized functions for adding elements to specific paths and inserting properties into nested array elements.

## Installation

You can install the development version of jsonr from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("luechtian/jsonr")
```

## Features

- Add elements to JSON structures at specific paths using dot notation
- Insert properties into elements of nested JSON arrays
- Finely control the position of new properties within objects
- Support for dynamic property values based on context

## Basic Usage


```r
library(jsonr)

# Add an element to the root level of a JSON structure
add_json_element(json_data, "", "status", "active") 

# Add an element to a nested JSON path
add_json_element("sample.json", "person", "email", "max@example.com")

# Add an element to a nested path with automatic path creation
add_json_element("sample.json", "person.address", "country", "Germany", create_path = TRUE)

# Insert properties into nested array elements
json_data <- jsonlite::fromJSON("sample.json", simplifyVector = FALSE)
result <- insert_json_property(
  json_data,
  array_path = c("items", "Test"),
  property_name = "TestId",
  property_value = 1,
  position_type = "after",
  position_ref = "DisplayName"
)
```

## Documentation

For more detailed examples and usage, please refer to the package vignettes:

```r
# View the introduction vignette
vignette("jsonr-introduction", package = "jsonr")

# View the advanced JSON manipulation vignette
vignette("advanced-json-manipulation", package = "jsonr")
```