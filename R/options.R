#' Options for the HexScape package
#'
#' @name hexscape_options
#' @aliases hexscape_options HexScape_options hexscape_getOption hexscape_getOption
#'
#' @description
#' Utility function to set/get options for the HexScape package, most notably the local folder used to store intermediate objects.
#'
#' @details
#' The following options can be specified:
#'
#' \itemize{
#' \item \bold{storage_folder} - the path to a folder to store intermediate objects created by HexScape.
#' }
#'
#' @param name the name of the option to get the current value of - for a  list of available options, see details below.
#' @param ... named option(s) to change - for a list of available options, see details below.
#'
#' @keywords methods
#' @return The current value of all available HexScaoe options (after applying any changes specified) is returned invisibly as a named list.
#' @seealso \code{\link{HexScape}}
#'
NULL

#' @rdname hexscape_options
#' @export
hexscape_options <- function(...){
  opts <- list(...)

  if(length(opts)>0){
    options <- hexscape_env$options
    recognised <- pmatch(names(opts), names(options))
    if(any(is.na(recognised))){
      warning(paste("Igoring unmatched or ambiguous option(s): ", paste(names(opts)[is.na(recognised)],collapse=", ")))
      opts <- opts[!is.na(recognised)]
    }
    optnames <- names(options)[recognised[!is.na(recognised)]]
    if(length(optnames)>0) for(i in 1:length(optnames)){
      options[optnames[i]] <- opts[[i]]
    }
    assign("options",options,envir=hexscape_env)
  }

  invisible(hexscape_env$options)
}

#' @rdname hexscape_options
#' @export
hexscape_getOption <- function(name){
  if(length(name)!=1) stop("Only 1 option can be retrieved at a time")
  opt <- pmatch(name,names(hexscape_env$options))
  if(is.na(opt)) stop(paste("Unmatched or ambiguous option '", name, "'", sep=""))
  # Use eval as some defaults may be put in using 'expression' to avoid evaluating at load time:
  return(eval(hexscape_env$options[[opt]]))
}


## Default options and package environment (not exported):
hexscape_env <- new.env()
default_options <- list(storage_folder = expression(stop("No storage_folder has been set!")))
assign("options", default_options, envir=hexscape_env)
