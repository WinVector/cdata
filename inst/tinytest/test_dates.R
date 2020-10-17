
test_dates <- function() {

  # Note: can not mix time zones in combining columns!
  # Also can not mix lt and ct types in joined columns
  d <- data.frame(row_id = 1:4)
  d$d1 <- as.Date("2019-03-11")
  d$d2 <- as.Date("2019-03-21")
  d$t1 <- as.POSIXct(1472562988, origin = "2020-08-19", tz = "GMT")
  d$t2 <- as.POSIXct(1472562988, origin = "1960-01-01", tz = "GMT")
  d$t3 <- as.POSIXlt(1472562988, origin = "2020-08-19", tz = "America/Los_Angeles")
  d$t4 <- as.POSIXlt(1472562988, origin = "1960-01-01", tz = "America/Los_Angeles")

  layout <- rowrecs_to_blocks_spec(
    wrapr::qchar_frame(
        "group", "d", "t", "z" |
        "1"   ,   d1,  t1, t3  |
        "2"   ,   d2,  t2, t4  ),
    recordKeys = "row_id")

  r <- d %.>% layout

  expect_true("Date" %in% class(r$d))
  expect_true("POSIXct" %in% class(r$t))
  expect_true("POSIXlt" %in% class(r$z))

  inv <- t(layout)
  b <- r %.>% inv

  expect_true("Date" %in% class(b$d1))
  expect_true("Date" %in% class(b$d2))
  expect_true("POSIXct" %in% class(b$t1))
  expect_true("POSIXct" %in% class(b$t2))
  expect_true("POSIXlt" %in% class(b$t3))
  expect_true("POSIXlt" %in% class(b$t4))

  expect_true(wrapr::check_equiv_frames(d, b))

  invisible(NULL)
}

test_dates()

