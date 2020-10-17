

test_encode <- function() {
  d = data.frame(x = 1:3,
                 y = c('a', 'b', 'c'),
                 stringsAsFactors = FALSE)

  d_simple <- cdata:::convert_data_frame_to_yaml(d)
  d_back1 <- cdata:::convert_yaml_to_data_frame(d_simple)
  expect_equal(d, d_back1)

  row_keys <- c('id', 'Species')

  block_record <- wrapr::qchar_frame(
    "Part" , "Measure", "Value"      |
    "Petal", "Length" , Petal.Length |
    "Petal", "Width"  , Petal.Width  |
    "Sepal", "Length" , Sepal.Length |
    "Sepal", "Width"  , Sepal.Width  )
  block_keys <- c('Part', 'Measure')

  brtrrs <- blocks_to_rowrecs_spec(controlTable = block_record,
                                  recordKeys = row_keys,
                                  controlTableKeys = block_keys)
  rrtbrs <- rowrecs_to_blocks_spec(controlTable = block_record,
                                  recordKeys = row_keys,
                                  controlTableKeys = block_keys)

  incoming_shape <- qchar_frame(
    "row",  "col1", "col2", "col3" |
      "row1",   v11,     v12,  v13   |
      "row2",   v21,     v22,  v23   |
      "row3",   v31,     v32,  v33   )


  outgoing_shape <- qchar_frame(
    "column", "row1", "row2", "row3" |
      "col1",      v11,  v21  ,  v31   |
      "col2",      v12,  v22  ,  v32   |
      "col3",      v13,  v23  ,  v33   )

  brtbrts <- layout_specification(
    incoming_shape = incoming_shape,
    outgoing_shape = outgoing_shape,
    recordKeys = 'record_id')

  brtrrs_simple <- convert_cdata_spec_to_yaml(brtrrs)
  brtrrs_back <- convert_yaml_to_cdata_spec(brtrrs_simple)
  expect_equal(format(brtrrs), format(brtrrs_back))

  rrtbrs_simple <- convert_cdata_spec_to_yaml(rrtbrs)
  rrtbrs_back <- convert_yaml_to_cdata_spec(rrtbrs_simple)
  expect_equal(format(rrtbrs), format(rrtbrs_back))

  brtbrts_simple <- convert_cdata_spec_to_yaml(brtbrts)
  brtbrts_back <- convert_yaml_to_cdata_spec(brtbrts_simple)
  expect_equal(format(brtbrts), format(brtbrts_back))

  if(requireNamespace('yaml', quietly = TRUE)) {
    brtrrs_yaml <- yaml::as.yaml(brtrrs_simple)
    brtrrs_back_y <- convert_yaml_to_cdata_spec(yaml::read_yaml(text = brtrrs_yaml))
    expect_equal(format(brtrrs), format(brtrrs_back_y))

    rrtbrs_yaml <- yaml::as.yaml(rrtbrs_simple)
    rrtbrs_back_y <- convert_yaml_to_cdata_spec(yaml::read_yaml(text = rrtbrs_yaml))
    expect_equal(format(rrtbrs), format(rrtbrs_back_y))

    brtbrts_yaml <- yaml::as.yaml(brtbrts_simple)
    brtbrts_back_y <- convert_yaml_to_cdata_spec(yaml::read_yaml(text = brtbrts_yaml))
    expect_equal(format(brtbrts), format(brtbrts_back_y))
  }

  invisible(NULL)
}

test_encode()


test_read_from_data_algebra <- function() {
  text = '
type: data_algebra.cdata_impl.RecordMap
blocks_out:
  type: data_algebra.cdata.RecordSpecification
  record_keys:
  - id
  - Species
  control_table_keys:
  - Part
  - Measure
  control_table:
    Part:
    - Petal
    - Petal
    - Sepal
    - Sepal
    Measure:
    - Length
    - Width
    - Length
    - Width
    Value:
    - Petal.Length
    - Petal.Width
    - Sepal.Length
    - Sepal.Width
'
  xform = convert_yaml_to_cdata_spec(yaml::read_yaml(text = text))
  expect_true(is(xform, "rowrecs_to_blocks_spec"))
}

test_read_from_data_algebra()


