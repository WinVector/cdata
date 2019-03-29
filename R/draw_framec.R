

#' Render a simple data.frame in qchar_frame format.
#'
#' @param x data.frame (with character types).
#' @param ... not used for values, forces later arguments to bind by name.
#' @param unquote_cols character, columns to elide quotes from.
#' @param adjust_for_auto_indent integer additional after first row padding.
#' @return character
#'
#' @seealso \code{\link{build_frame}},  \code{\link{qchar_frame}}
#'
#' @examples
#'
#' controlTable <- wrapr::qchar_frame(
#'   "flower_part", "Length"     , "Width"     |
#'     "Petal"    , Petal.Length , Petal.Width |
#'     "Sepal"    , Sepal.Length , Sepal.Width )
#' cat(draw_framec(controlTable, unquote_cols = qc(Length, Width)))
#'
#'
#' @noRd
#'
draw_framec <- function(x,
                        ...,
                        unquote_cols = character(0),
                        adjust_for_auto_indent = 2) {
  wrapr::stop_if_dot_args(substitute(list(...)), "wrapr::draw_framec")
  x_s <- substitute(x)
  if(!is.data.frame(x)) {
    stop("wrapr::draw_framec x needs to be a data.frame")
  }
  res <- "wrapr::qchar_frame()"
  nrow <- nrow(x)
  ncol <- ncol(x)
  if((nrow>=1) && (ncol<1)) {
    stop("wrapr::draw_framec bad input: no columns, but has rows")
  }
  qts <- function(v) {
    # wayts to quote: dput(), shQuote(), deparse()
    vapply(as.character(v),
           function(vi) {
             deparse(vi)
           },
           character(1))
  }
  if((nrow<1) || (ncol<1)) {
    if(ncol>=1) {
      res <- paste(qts(colnames(x)), collapse = ", ")
      res <- paste0("wrapr::qchar_frame(", res, ")")
    }
  } else {
    # convert to character matrix
    xq <- x
    for(ci in colnames(x)) {
      if(ci %in% unquote_cols) {
        xq[[ci]] <- as.character(x[[ci]])
      } else {
        xq[[ci]] <- qts(as.character(x[[ci]]))
      }
      xq[[ci]][is.na(x[[ci]])] <- "NA_character_"
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
    widths[is.na(as.numeric(widths))] <- 2
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
    seps[, ncol] <- " |"
    seps[nrow+1, ncol] <- " )"
    # format
    fmt <- matrix(data = paste0(xm, pads, seps),
                  nrow = nrow+1, ncol = ncol)
    if(adjust_for_auto_indent>0) {
      pad <- paste(rep(" ", adjust_for_auto_indent), collapse = "")
      fmt[1, 1] <- gsub(", $", paste0(pad, ", "), fmt[1, 1])
      for(i in wrapr::seqi(2, nrow(fmt))) {
        fmt[i, 1] <- paste0(pad, fmt[i, 1])
      }
    }
    rlist <- vapply(seq_len(nrow+1),
                    function(i) {
                      paste(fmt[i, , drop=TRUE], collapse = '')
                    }, character(1))
    rlist <- paste0("   ", rlist)
    res <- paste(rlist, collapse = "\n")
    res <- paste0("wrapr::qchar_frame(\n", res, "\n")
  }
  if(is.name(x_s)) {
    res <- paste0(as.character(x_s), " <- ", res)
  }
  res
}

