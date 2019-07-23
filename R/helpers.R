as.dsVectorArg <- function (x) {
   if (!is.null(x)) {
      x <- paste0(as.character(x), collapse = ",")
   }
   return(x)
}
