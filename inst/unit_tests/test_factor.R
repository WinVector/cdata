
test_factor <- function() {

  df <- data.frame(ca = "a",
                   cb = "b",
                   stringsAsFactors = TRUE)
  ds <- data.frame(ca = "a",
                   cb = "b",
                   stringsAsFactors = FALSE)
  layout <- rowrecs_to_blocks_spec(
    wrapr::qchar_frame(
      "group" , "meas" |
        "cola"   ,  ca    |
        "colb"   , cb    ))

  rf <- df %.>% layout

  rs <- ds %.>% layout

  RUnit::checkTrue(wrapr::check_equiv_frames(rf, rs))

  expect <-  wrapr::build_frame(
    "group"  , "meas" |
      "cola" , "a"    |
      "colb" , "b"    )

  RUnit::checkTrue(wrapr::check_equiv_frames(expect, rs))
  RUnit::checkTrue(wrapr::check_equiv_frames(expect, rf))

  invisible(NULL)
}