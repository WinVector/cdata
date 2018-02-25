
#' Build a (non-empty) data.frame.
#'
#' A convenient way to build a data.frame in legible transposed form.  Position of
#' first "/" (or other infix operator) determines number of columns
#' (all other infix operators are aliases for ",").
#' Names are de-referenced.
#'
#' @param ... cell names, first infix operator denotes end of header row of column names.
#' @param cf_eval_environment environment to evaluate names in.
#' @return character data.frame
#'
#' @seealso \code{\link{draw_frame}}, \code{\link{qchar_frame}}
#'
#' @examples
#'
#' tc_name <- "training"
#' x <- build_frame(
#'    "measure",                   tc_name, "validation" /
#'    "minus binary cross entropy",      5, -7           /
#'    "accuracy",                      0.8, 0.6          )
#' print(x)
#' str(x)
#' cat(draw_frame(x))
#'
#' build_frame(
#'   "x" /
#'   -1  /
#'   2   )
#'
#' @export
#'
build_frame <- function(..., cf_eval_environment = parent.frame()) {
  v <- as.list(substitute(list(...))[-1])
  lv <- length(v)
  # inspect input
  if(lv<1) {
    stop("wrapr::build_frame expect at least a header, one column, and one row")
  }
  cls <- vapply(v, class, character(1))
  if(sum(cls=="call") < 1) {
    stop("wrapr::build_frame expected at least 1 infix operator")
  }
  ncol <- match("call", cls)
  # unpack
  unpack_val <- function(vi) {
    if(is.name(vi)) {
      vi <- cf_eval_environment[[as.character(vi)]]
    }
    if(length(vi)<=0) {
      stop("wrapr::build_frame unexpected NULL/empty element")
    }
    if(is.call(vi)) {
      if(length(vi)>2) {
        vi <- lapply(vi[-1], unpack_val)
      } else {
        vi <- eval(vi)
      }
    }
    Reduce(c, lapply(vi, as.list))
  }
  vu <- lapply(v, unpack_val)
  vu <- Reduce(c, lapply(vu, as.list))
  nrow <- (length(vu)/ncol) - 1
  seq <- seq_len(nrow)*ncol
  fr <- data.frame(x = unlist(vu[seq + 1],
                              recursive = FALSE,
                              use.names = FALSE),
                   stringsAsFactors = FALSE)
  colnames(fr) <- as.character(vu[[1]])
  if(ncol>1) {
    for(i in 2:ncol) {
      ci <- as.character(vu[[i]])
      fr[[ci]] <-  unlist(vu[seq + i],
                          recursive = FALSE,
                          use.names = FALSE)
    }
  }
  fr
}


#' Render a data.frame in draw_frame form.
#'
#' @param x data.frame (atomic types, with at least 1 row and 1 column).
#' @return chracter
#'
#' @seealso \code{\link{build_frame}},  \code{\link{qchar_frame}}
#'
#' @examples
#'
#' tc_name <- "training"
#' x <- build_frame(
#'    "measure",                   tc_name, "validation" /
#'    "minus binary cross entropy",      5, 7            /
#'    "accuracy",                      0.8, 0.6          )
#' print(x)
#' cat(draw_frame(x))
#'
#' @export
#'
draw_frame <- function(x) {
  nrow <- nrow(x)
  if(nrow<1) {
    stop("draw_frame need at least 1 row")
  }
  ncol <- ncol(x)
  if(ncol<1) {
    stop("draw_frame need at least 1 column")
  }
  # convert to character matrix
  xq <- x
  qts <- function(v) {
    # TODO: better function with escaping
    paste0('"', v, '"')
  }
  for(ci in colnames(x)) {
    if(is.character(x[[ci]])) {
      xq[[ci]] <- qts(x[[ci]])
    }
  }
  xm <- as.matrix(xq)
  xm <- matrix(data = as.character(xm),
               nrow = nrow, ncol = ncol)
  # convert header to values
  xm <- rbind(matrix(data = qts(colnames(x)),
                     nrow = 1, ncol = ncol),
              xm)
  # compute padding
  widths <- nchar(xm)
  colmaxes <- matrix(data = apply(widths, 2, max),
                     nrow = nrow+1, ncol = ncol,
                     byrow = TRUE)
  padlens <- colmaxes - widths
  pads <- matrix(data = vapply(padlens,
                               function(vi) {
                                 paste(rep(' ', vi), collapse = '')
                               }, character(1)),
                 nrow = nrow+1, ncol = ncol)
  # get intermediates
  seps <- matrix(data = ", ",
                 nrow = nrow+1, ncol = ncol)
  seps[, ncol] <- " /"
  seps[nrow+1, ncol] <- " )"
  # format
  fmt <- matrix(data = paste0(xm, pads, seps),
                nrow = nrow+1, ncol = ncol)
  rlist <- vapply(seq_len(nrow+1),
                  function(i) {
                    paste(fmt[i, , drop=TRUE], collapse = '')
                  }, character(1))
  rlist <- paste0("   ", rlist)
  res <- paste(rlist, collapse = "\n")
  res <- paste0("build_frame(\n", res, "\n")
  res
}



#' Build a (non-empty) quoted data.frame.
#'
#' A convenient way to build a character data.frame in legible transposed form.  Position of
#' first "/" (or other infix operator) determines number of columns
#' (all other infic operators are aliases for ",").
#' Names are treated as character types.
#'
#' @param ... cell names, first infix operator denotes end of header row of column names.
#' @return character data.frame
#'
#' @seealso \code{\link{draw_frame}}, \code{\link{build_frame}}
#'
#' @examples
#'
#' x <- qchar_frame(
#'    measure,                      training, validation /
#'    "minus binary cross entropy", loss,     val_loss   /
#'    accuracy,                     acc,      val_acc    )
#' print(x)
#' str(x)
#' cat(draw_frame(x))
#'
#' qchar_frame(
#'   x /
#'   1 /
#'   2 )
#'
#' @export
#'
qchar_frame <- function(...) {
  v <- as.list(substitute(list(...))[-1])
  lv <- length(v)
  # inspect input
  if(lv<1) {
    stop("wrapr::qchar_frame expect at least a header, one column, and one row")
  }
  cls <- vapply(v, class, character(1))
  if(length(setdiff(cls, c("character", "call", "name")))>0) {
    stop("wrapr::qchar_frame expect only strings, names, +, and commas")
  }
  if(sum(cls=="call") < 1) {
    stop("wrapr::qchar_frame expected at least 1 infix operator")
  }
  ncol <- match("call", cls)
  # unpack
  unpack_val <- function(vi) {
    if(length(vi)<=0) {
      stop("wrapr::qchar_frame unexpected NULL/empty element")
    }
    if(is.call(vi)) {
      if(length(vi)>2) {
        vi <- lapply(vi[-1], unpack_val)
      } else {
        vi <- eval(vi)
      }
    }
    as.character(unlist(vi))
  }
  vu <- lapply(v, unpack_val)
  vu <- unlist(vu)
  fr <- as.data.frame(matrix(data = vu[-seq_len(ncol)],
                             ncol=ncol,
                             byrow = TRUE),
                      stringsAsFactors = FALSE)
  colnames(fr) <- vu[seq_len(ncol)]
  fr
}

