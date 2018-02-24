
#' Build character data.frame.
#'
#' A confineint way to build a character data.frame.  Position of
#' first "+" (or other infix operator) determines number of columns
#' (all other infix operators are aliases for ",").
#' Names are de-referenced.
#'
#' @param ... cell names, first infix operator denotes end of header row of column names.
#' @param cf_eval_environment environment to evaluate names in.
#' @return character data.frame
#'
#' @seealso \link{qchar_frame}
#'
#' @examples
#'
#' tc_name <- "training"
#' char_frame(
#'    "measure",                   tc_name, "validation" +
#'    "minus binary cross entropy", "loss", "val_loss"   +
#'    "accuracy",                    "acc", "val_acc"    )
#'
#' @export
#'
char_frame <- function(..., cf_eval_environment = parent.frame()) {
  v <- as.list(substitute(list(...))[-1])
  lv <- length(v)
  # inspect input
  if(lv<2) {
    stop("wrapr::char_frame expect at least header, one column, and one row")
  }
  cls <- vapply(v, class, character(1))
  if(length(setdiff(cls, c("character", "call", "name")))>0) {
    stop("wrapr::char_frame expect only strings, names, +, and commas")
  }
  if(sum(cls=="call") < 1) {
    stop("wrapr::char_frame expected at least 1 +")
  }
  ncol <- match("call", cls)
  # unpack
  deal_with_name_or_char <- function(vi) {
    if(is.name(vi)) {
      vi <- cf_eval_environment[[as.character(vi)]]
    }
    if(length(vi)<=0) {
      stop("wrapr::char_frame unexpected NULL/empty")
    }
    if(!is.character(vi)) {
      stop("wrapr::char_frame expected a name or character")
    }
    as.character(vi) # strip attributes
  }
  vu <- lapply(v,
               function(vi) {
                 if(length(vi)<=0) {
                   stop("wrapr::char_frame unexpected NULL/empty element")
                 }
                 if(is.name(vi) || is.character(vi)) {
                   return(deal_with_name_or_char(vi))
                 }
                 if(is.call(vi)) {
                   if(length(vi)!=3) {
                     stop("wrapr::char_frame expected an infix operator")
                   }
                   return(c(deal_with_name_or_char(vi[[2]]),
                            deal_with_name_or_char(vi[[3]])))
                 }
                 stop(paste("wrapr::char_frame unexpected type ",
                            paste(class(vi), collapse = ", ")))
               })
  vu <- unlist(vu)
  fr <- as.data.frame(matrix(data = vu[-seq_len(ncol)],
                             ncol=ncol,
                             byrow = TRUE),
                      stringsAsFactors = FALSE)
  colnames(fr) <- vu[seq_len(ncol)]
  fr
}


#' Build a quoted data.frame.
#'
#' A confineint way to build a character data.frame.  Position of
#' first "+" (or other infix operator) determines number of columns
#' (all other infic operators are aliases for ",").
#' Names are treated as character types.
#'
#' @param ... cell names, first infix operator denotes end of header row of column names.
#' @param cf_eval_environment environment to evaluate names in.
#' @return character data.frame
#'
#' @seealso \link{char_frame}
#'
#' @examples
#'
#' qchar_frame(
#'    measure,                      training, validation /
#'    "minus binary cross entropy", loss,     val_loss   /
#'    accuracy,                     acc,      val_acc    )
#'
#' @export
#'
qchar_frame <- function(..., cf_eval_environment = parent.frame()) {
  v <- as.list(substitute(list(...))[-1])
  lv <- length(v)
  # inspect input
  if(lv<2) {
    stop("wrapr::qchar_frame expect at least header, one column, and one row")
  }
  cls <- vapply(v, class, character(1))
  if(length(setdiff(cls, c("character", "call", "name")))>0) {
    stop("wrapr::qchar_frame expect only strings, names, +, and commas")
  }
  if(sum(cls=="call") < 1) {
    stop("wrapr::qchar_frame expected at least 1 +")
  }
  ncol <- match("call", cls)
  # unpack
  deal_with_name_or_char <- function(vi) {
    if(is.name(vi)) {
      vi <- as.character(vi)
    }
    if(length(vi)<=0) {
      stop("wrapr::qchar_frame unexpected NULL/empty")
    }
    if(!is.character(vi)) {
      stop("wrapr::qchar_frame expected a name or character")
    }
    as.character(vi) # strip attributes
  }
  vu <- lapply(v,
               function(vi) {
                 if(length(vi)<=0) {
                   stop("wrapr::qchar_frame unexpected NULL/empty element")
                 }
                 if(is.name(vi) || is.character(vi)) {
                   return(deal_with_name_or_char(vi))
                 }
                 if(is.call(vi)) {
                   if(length(vi)!=3) {
                     stop("wrapr::qchar_frame expected an infix operator")
                   }
                   return(c(deal_with_name_or_char(vi[[2]]),
                            deal_with_name_or_char(vi[[3]])))
                 }
                 stop(paste("wrapr::qchar_frame unexpected type ",
                            paste(class(vi), collapse = ", ")))
               })
  vu <- unlist(vu)
  fr <- as.data.frame(matrix(data = vu[-seq_len(ncol)],
                             ncol=ncol,
                             byrow = TRUE),
                      stringsAsFactors = FALSE)
  colnames(fr) <- vu[seq_len(ncol)]
  fr
}

