
d <- wrapr::build_frame(
  'val_loss', 'val_acc', 'loss', 'acc'  |
  0.377     , 0.8722   , 0.5067, 0.7852 |
  0.2997    , 0.8895   , 0.3002, 0.904  |
  0.2964    , 0.8822   , 0.2166, 0.9303 |
  0.2779    , 0.8899   , 0.1739, 0.9428 |
  0.2843    , 0.8861   , 0.1411, 0.9545 |
  0.312     , 0.8817   , 0.1136, 0.9656 )
d$epoch <- seq_len(nrow(d))

controlTable <- wrapr::build_frame(
  'measure'                   , 'training', 'validation' |
  'minus binary cross entropy', 'loss'    , 'val_loss'   |
  'accuracy'                  , 'acc'     , 'val_acc'    )


# use control table to get into a triple-form (only one data column, all others keys).
cells <- as.character(unlist(unlist(controlTable[, -1])))
c2 <- controlTable
for(i in 1:nrow(controlTable)) {
  c2[i, ] <- c2[i, 1]
}
c2 <- as.character(unlist(c2[, -1]))
names(c2) <- cells
c3 <- controlTable
for(j in 2:ncol(controlTable)) {
  c3[, j] <- colnames(controlTable)[[j]]
}
c3 <- as.character(unlist(c3[, -1]))
names(c3) <- cells
dt <- data.table::melt(d, id.vars = "epoch", measure.vars = c('val_loss', 'val_acc', 'loss', 'acc'))
dt$row_label <- c2[dt$variable]
dt$col_label <- c3[dt$variable]

# cast to block form
data.table::dcast(dt, epoch + row_label ~ col_label )

# cast to rowrec form
data.table::dcast(dt, epoch  ~ variable )

