
# row rec form
d_rowrec_form <- wrapr::build_frame(
  'val_loss', 'val_acc', 'loss', 'acc' , 'epoch' |
  0.377     , 0.8722   , 0.5067, 0.7852, 1L      |
  0.2997    , 0.8895   , 0.3002, 0.904 , 2L      |
  0.2964    , 0.8822   , 0.2166, 0.9303, 3L      |
  0.2779    , 0.8899   , 0.1739, 0.9428, 4L      |
  0.2843    , 0.8861   , 0.1411, 0.9545, 5L      |
  0.312     , 0.8817   , 0.1136, 0.9656, 6L      )

# block form
d_block_form <- wrapr::build_frame(
  'epoch', 'row_label'                 , 'training', 'validation' |
  1L     , 'accuracy'                  , 0.8722    , 0.7852       |
  1L     , 'minus binary cross entropy', 0.377     , 0.5067       |
  2L     , 'accuracy'                  , 0.8895    , 0.904        |
  2L     , 'minus binary cross entropy', 0.2997    , 0.3002       |
  3L     , 'accuracy'                  , 0.8822    , 0.9303       |
  3L     , 'minus binary cross entropy', 0.2964    , 0.2166       |
  4L     , 'accuracy'                  , 0.8899    , 0.9428       |
  4L     , 'minus binary cross entropy', 0.2779    , 0.1739       |
  5L     , 'accuracy'                  , 0.8861    , 0.9545       |
  5L     , 'minus binary cross entropy', 0.2843    , 0.1411       |
  6L     , 'accuracy'                  , 0.8817    , 0.9656       |
  6L     , 'minus binary cross entropy', 0.312     , 0.1136       )

# conversion control table
controlTable <- wrapr::build_frame(
  'measure'                   , 'training', 'validation' |
  'minus binary cross entropy', 'loss'    , 'val_loss'   |
  'accuracy'                  , 'acc'     , 'val_acc'    )


# use control table to get into a triple-form (only one data column, all others keys).
cells <- as.character(unlist(unlist(controlTable[, -1])))
cells_to_row_labels <- controlTable
for(i in 1:nrow(controlTable)) {
  cells_to_row_labels[i, ] <- cells_to_row_labels[i, 1]
}
cells_to_row_labels <- as.character(unlist(cells_to_row_labels[, -1]))
names(cells_to_row_labels) <- cells
cells_to_col_labels <- controlTable
for(j in 2:ncol(controlTable)) {
  cells_to_col_labels[, j] <- colnames(controlTable)[[j]]
}
cells_to_col_labels <- as.character(unlist(cells_to_col_labels[, -1]))
names(cells_to_col_labels) <- cells
rows_cols_to_cells <- cells
names(rows_cols_to_cells) <- paste(cells_to_row_labels, ",", cells_to_col_labels)



# from rowrec to one value per row form (triple-like)
d_thin_r <- data.table::melt(d_rowrec_form,
                             id.vars = "epoch",
                             measure.vars = cells)
d_thin_r$cdata_cell_label <- d_thin_r$variable
d_thin_r$cdata_row_label <- cells_to_row_labels[d_thin_r$cdata_cell_label]
d_thin_r$cdata_col_label <- cells_to_col_labels[d_thin_r$cdata_cell_label]

# cast to block form
r <- data.table::dcast(d_thin_r, epoch + cdata_row_label ~ cdata_col_label )
r$row_label <- r$cdata_row_label
r$cdata_row_label <- NULL
r




# from block form to one value per row form (triple-like)
d_thin_b <- data.table::melt(d_block_form,
                             id.vars = c("epoch", "row_label"),
                             measure.vars = c("training", "validation"))
d_thin_b$cdata_row_label <- d_thin_b$row_label
d_thin_b$row_label <- NULL
d_thin_b$cdata_col_label <- d_thin_b$variable
d_thin_b$variable <- NULL
d_thin_b$cdata_cell_label <- rows_cols_to_cells[paste(d_thin_b$cdata_row_label, ",", d_thin_b$cdata_col_label)]

# cast to rowrec form
data.table::dcast(d_thin_b, epoch ~ cdata_cell_label )

