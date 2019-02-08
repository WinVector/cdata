
test_composite_control_keys <- function() {
  # https://github.com/WinVector/cdata/issues/3

  d <- wrapr::build_frame(
    "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "id" |
    5.1           , 3.5          , 1.4           , 0.2          , "setosa" , 1L   |
    4.9           , 3            , 1.4           , 0.2          , "setosa" , 2L   )

  control_table <- wrapr::qchar_frame(
    Part,  Measure, Value        |
    Sepal, Length,  Sepal.Length |
    Sepal, Width,   Sepal.Width  |
    Petal, Length,  Petal.Length |
    Petal, Width,   Petal.Width  )

  res <- rowrecs_to_blocks(d,
                           control_table,
                           controlTableKeys = c("Part", "Measure"),
                           columnsToCopy = c("id", "Species"))

  expect <- wrapr::build_frame(
    "id", "Species", "Part" , "Measure", "Value" |
    1L  , "setosa" , "Sepal", "Length" , 5.1     |
    1L  , "setosa" , "Sepal", "Width"  , 3.5     |
    1L  , "setosa" , "Petal", "Length" , 1.4     |
    1L  , "setosa" , "Petal", "Width"  , 0.2     |
    2L  , "setosa" , "Sepal", "Length" , 4.9     |
    2L  , "setosa" , "Sepal", "Width"  , 3       |
    2L  , "setosa" , "Petal", "Length" , 1.4     |
    2L  , "setosa" , "Petal", "Width"  , 0.2     )

  RUnit::checkEquals(sort(colnames(expect)), sort(colnames(res)))
  res <- res[, colnames(expect), drop = FALSE]

  expect <- expect[wrapr::orderv(expect[, c("Part", "Measure")]), , drop = FALSE]
  res <- res[wrapr::orderv(res[, c("Part", "Measure")]), , drop = FALSE]
  RUnit::checkEquals(expect, res)

  back <- blocks_to_rowrecs(res,
                            keyColumns = c("id", "Species"),
                            control_table,
                            controlTableKeys = c("Part", "Measure"))

  RUnit::checkEquals(sort(colnames(d)), sort(colnames(back)))
  back <- back[, colnames(d), drop = FALSE]

  d <- d[wrapr::orderv(d), , drop = FALSE]
  back <- back[wrapr::orderv(back), , drop = FALSE]
  RUnit::checkEquals(d, back)
}
