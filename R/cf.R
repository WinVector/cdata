
#' Build character data.frame
#'
#' A confineint way to build a character data.frame.  Position of
#' first "+" determines number of columns (all other "+"'s are aliases for ",").
#'
#' @param ... character values, + denotes end of row, first row is column names.
#' @return character data.frame
#'
#' @examples
#'
#' cf("measure",                    "training", "validation" +
#'    "minus binary cross entropy", "loss",     "val_loss"   +
#'    "accuracy",                   "acc",      "val_acc"    )
#'
#' @export
#'
cf <- function(...) {
  v <- as.list(substitute(list(...))[-1])
  lv <- length(v)
  # inspect input
  if(lv<2) {
    stop("wrapr::cf expect at least header, one column, and one row")
  }
  cls <- vapply(v, class, character(1))
  if(length(setdiff(c("character", "call"), cls))>0) {
    stop("wrapr::cf expect only strings, +, and commas")
  }
  if(sum(cls=="call") < 1) {
    stop("wrapr::cf expected at least 1 +")
  }
  ncol <- match("call", cls)
  # unpack
  vu <- lapply(v,
               function(vi) {
                 if(is.character(vi)) {
                   if(length(vi)!=1) {
                     stop("wrapr::cf expect only scalar character values")
                   }
                   return(vi)
                 } else {
                   if((vi[[1]]!="+") ||
                      (length(vi)!=3) ||
                      (!is.character(vi[[2]])) ||
                      (!is.character(vi[[3]]))) {
                     stop("wrapr::cf non-trivial expression")
                   }
                   return(c(vi[[2]], vi[[3]]))
                 }
               })
  vu <- unlist(vu)
  fr <- as.data.frame(matrix(data = vu[-seq_len(ncol)],
                             ncol=ncol,
                             byrow = TRUE),
                      stringsAsFactors = FALSE)
  colnames(fr) <- vu[seq_len(ncol)]
  fr
}

