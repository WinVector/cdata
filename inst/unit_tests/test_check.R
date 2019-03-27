

test_check <- function() {
  res <- wrapr::build_frame(
    "mean"  , "var", "sd"  , "naive_var", "naive_sd", "adj_sd" |
      1     , 0    , 0     , 0          , 0         , 0.09634  |
      1     , 0    , 0     , 0          , 0         , 0.09634  |
      1     , 0    , 0     , 0          , 0         , 0.09634  |
      1     , 0    , 0     , 0          , 0         , 0.09634  |
      0.8   , 0.2  , 0.4472, 0.16       , 0.4       , 0.6016   |
      0.8   , 0.2  , 0.4472, 0.16       , 0.4       , 0.6016   |
      1     , 0    , 0     , 0          , 0         , 0.09634  |
      0.8   , 0.2  , 0.4472, 0.16       , 0.4       , 0.6016   |
      0.6   , 0.3  , 0.5477, 0.24       , 0.4899    , 0.4818   |
      0.8   , 0.2  , 0.4472, 0.16       , 0.4       , 0.6016   )
  cT <- build_unpivot_control(nameForNewKeyColumn = "estimation_method",
                              nameForNewValueColumn = "sd_estimate",
                              columnsToTakeFrom = c("adj_sd", "naive_sd"))
  RUnit::checkException(rp <- rowrecs_to_blocks(res,
                                                controlTable = cT,
                                                checkKeys = TRUE))
  d1 <- rowrecs_to_blocks(res,
                          controlTable = cT)
  d2 <- unpivot_to_blocks(res,
                          nameForNewKeyColumn = "estimation_method",
                          nameForNewValueColumn = "sd_estimate",
                          columnsToTakeFrom = c("adj_sd", "naive_sd"))

  invisible(NULL)
}