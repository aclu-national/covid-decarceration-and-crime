## Copied from internal package

#' Print clean cli information text
#'
#' @param ... Any params usable by \code{\link[glue]{glue}}
#' @param .envir Environment
#'
#' @return invisible
#' @export
#'
#' @examples
#' done_line("wooo....bullets")
done_line <- function (..., .envir = parent.frame())
{
  out <- glue::glue(..., .envir = .envir)
  cli::cat_line(check_ize(out))
}

#' @export
#' @rdname done_line
#' @examples
#' warning_line("wooo....bullets")
warning_line <- function(..., .envir = parent.frame()){
  out <- glue::glue(..., .envir = .envir)
  cli::cat_line(x_ize(out))
}

#' @export
#' @rdname done_line
#' @examples
#' info_line("wooo....bullets")
info_line <- function(..., .envir = parent.frame()){
  out <- glue::glue(..., .envir = .envir)
  cli::cat_line(info_ize(out))
}

bullet_ize <- function (line, bullet = "*")
{
  paste0(bullet, " ", line)
}

info_ize <- function(line){
  bullet_ize(line,bullet = crayon::blue(clisymbols::symbol$info))
}

check_ize <- function(line){
  bullet_ize(line,bullet = crayon::green(clisymbols::symbol$tick))
}

x_ize <- function(line){
  bullet_ize(line,bullet = crayon::red(clisymbols::symbol$cross))
}