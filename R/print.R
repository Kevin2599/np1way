#' @title Print multiple comparisons
#'
#' @description
#' \code{print.npmc} prints pairwise group comparisons.
#'
#' @details
#' This function prints pairwise multiple comparisons created
#' by the \code{\link{npmc}} function.
#' 
#' @param x an object of class \code{npmc}.
#' @param ... additional arguments passed to the function.
#' @method print npmc
#' @export
#' @return the input object is returned silently.
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' states <- data.frame(state.region, state.x77)
#' results <- npmc(Illiteracy ~ state.region, states)
#' print(results)
print.npmc <- function(x, ...){
  if (!inherits(x, "npmc"))       
    stop("Object must be of class 'npmc'")
  
  cat("data:", x$vnames[1], "by", x$vnames[2], "\n\n")  
  cat("Multiple Comparisons (Wilcoxon Rank Sum Tests)\n")
  cat(paste("Probability Adjustment = ", x$method, "\n", sep=""))
  
  print(x$mc,  ...)                                                     #3
}
