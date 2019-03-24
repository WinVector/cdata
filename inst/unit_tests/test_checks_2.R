
test_checks_2 <- function() {

  # ragged designs allowed
  d <- wrapr::build_frame(
    "id", "meas", "value" |
    1   , "AUC" , 0.7     |
    1   , "R2"  , NA      |
    2   , "R2" , 0.3      )
  r <- cdata::pivot_to_rowrecs(d,
                               columnToTakeKeysFrom = 'meas',
                               columnToTakeValuesFrom = 'value',
                               rowKeyColumns = 'id')
  expect <- wrapr::build_frame(
    "id", "AUC"   , "R2"     |
    1   , 0.7     , NA_real_ |
    2   , NA_real_, 0.3      )
  RUnit::checkTrue(check_equiv_frames(expect, r))

  # NA in id column works as a values
  d1 <- wrapr::build_frame(
    "id", "meas", "value" |
    1   , "AUC" , 0.7     |
    1   , "R2"  , NA      |
    NA  , "AUC" , 0.62    |
    NA   , "R2"  , 0.3     )
  r <- cdata::pivot_to_rowrecs(d1,
                               columnToTakeKeysFrom = 'meas',
                               columnToTakeValuesFrom = 'value',
                               rowKeyColumns = 'id')
  expect <- wrapr::build_frame(
    "id"    , "AUC", "R2"     |
    1       , 0.7  , NA_real_ |
    NA_real_, 0.62 , 0.3      )
  RUnit::checkTrue(check_equiv_frames(expect, r))

  # NA in measure column not allowed
  d2 <- wrapr::build_frame(
    "id", "meas", "value" |
    1   , "AUC" , 0.7     |
    1   , "R2"  , 0.6     |
    2   , NA    , 0.62    |
    2   , "R2"  , NA      )
  RUnit::checkException({
  cdata::pivot_to_rowrecs(d2,
                          columnToTakeKeysFrom = 'meas',
                          columnToTakeValuesFrom = 'value',
                          rowKeyColumns = 'id')
  })

  RUnit::checkException({
  # duplicates not allowed
  d3 <- wrapr::build_frame(
    "id", "meas", "value" |
    1   , "AUC" , 0.7     |
    1   , "R2"  , 0.6     |
    2   , "AUC" , 0.62    |
    2   , "AUC" , 0.33    |
    2   , "R2"  , 0.25    )
  cdata::pivot_to_rowrecs(d3,
                          columnToTakeKeysFrom = 'meas',
                          columnToTakeValuesFrom = 'value',
                          rowKeyColumns = 'id')
  })

  # unpivot
  z <- wrapr::build_frame(
    "id", "AUC"   , "R2"     |
    1   , 0.7     , NA_real_ |
    2   , NA_real_, 0.3      )
  r <- cdata::unpivot_to_blocks(z,
                                nameForNewKeyColumn = "meas",
                                nameForNewValueColumn = "value",
                                columnsToTakeFrom = c("AUC", "R2"))
  expect <- wrapr::build_frame(
    "id", "meas", "value"  |
    1   , "AUC" , 0.7      |
    1   , "R2"  , NA_real_ |
    2   , "AUC" , NA_real_ |
    2   , "R2"  , 0.3      )
  RUnit::checkTrue(check_equiv_frames(expect, r))

  # don't allow duplicates
  z <- wrapr::build_frame(
    "id", "AUC"   , "R2"     |
    1   , 0.7     , NA_real_ |
    1   , 0.7     , NA_real_ |
    2   , NA_real_, 0.3      )
  RUnit::checkException({
  cdata::unpivot_to_blocks(z,
                           nameForNewKeyColumn = "meas",
                           nameForNewValueColumn = "value",
                           columnsToTakeFrom = c("AUC", "R2"),
                           checkKeys = TRUE)
  })

  # key collision
  d <- wrapr::build_frame(
    "id", "meas", "value" |
    1   , "AUC" , 0.7     |
    1   , "R2"  , NA      |
    2   , "id" , 0.3      )
  RUnit::checkException({
  cdata::pivot_to_rowrecs(d,
                          columnToTakeKeysFrom = 'meas',
                          columnToTakeValuesFrom = 'value',
                          rowKeyColumns = 'id')
  })

  # key collision
  z <- wrapr::build_frame(
    "meas", "AUC"   , "R2"     |
      1   , 0.7     , NA_real_ |
      2   , 0.5     , 0.3      )
  RUnit::checkException({
    cdata::unpivot_to_blocks(z,
                             nameForNewKeyColumn = "meas",
                             nameForNewValueColumn = "value",
                             columnsToTakeFrom = c("AUC", "R2"),
                             checkKeys = TRUE)
  })
  cT <- cdata::build_unpivot_control(nameForNewKeyColumn = "meas",
                                     nameForNewValueColumn = "value",
                                     columnsToTakeFrom = c("AUC", "R2"))
  RUnit::checkException({
  cdata::rowrecs_to_blocks(z,
                           controlTable = cT,
                           columnsToCopy = "meas",
                           checkKeys = TRUE)
  })

  invisible(NULL)
}
