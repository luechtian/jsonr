[
    {
      "operation": "add_element",
      "path": "MethodInformation",
      "name": "SamplesLayoutType",
      "value": "SAMPLES_LAYOUT_COMBINED"
    },
    {
      "operation": "insert_property",
      "array_path": "AssayInformation",
      "property_name": "StopPreparationWithFailedCalibrator",
      "property_value": false,
      "position_type": "position",
      "position_ref": 4
    },
    {
      "operation": "insert_property",
      "array_path": "AssayInformation",
      "property_name": "StopPreparationWithFailedControl",
      "property_value": false,
      "position_type": "position",
      "position_ref": 5
    },
    {
      "operation": "insert_property",
      "array_path": "AssayInformation",
      "property_name": "ValidityOfCalibrationInDays",
      "property_value": 5,
      "position_type": "position",
      "position_ref": 6
    },
    {
      "operation": "insert_property",
      "array_path": ["AssayInformation", "CalibratorLayoutRules"],
      "property_name": "DisplayName",
      "property_value": {
          "_function_": "if ('Level' %in% names(elem)) { return(paste(root$Type, 'Calibrator Level', index - 1)) } else { return(paste('Element', index)) }"
      },
      "position_type": "first"
    },
    {
      "operation": "insert_property",
      "array_path": ["AssayInformation", "ControlLayoutRules"],
      "property_name": "DisplayName",
      "property_value": {
          "_function_": "if ('Level' %in% names(elem)) { return(paste(root$Type, 'Control Level', index - 1)) } else { return(paste('Element', index)) }"
      },
      "position_type": "first"
    }
  ]