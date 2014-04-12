#' @title Summarize multiple comparisons
#'
#' @description
#' \code{summary.npmc} summarizes pairwise group comparisons.
#'
#' @details
#' This function prints a summary of pairwise multiple
#' comparisons created by the 
#' \code{\link{npmc}} function.
#' 
#' @param object an object of class \code{npmc}.
#' @param ... additional parameters.
#' @method summary npmc
#' @export
#' @return the input object is returned silently.
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' states <- data.frame(state.region, state.x77)
#' results <- npmc(Illiteracy ~ state.region, states)
#' summary(results)
summary.npmc <- function(object, ...){
  if (!inherits(object, "npmc")) 
    stop("Object must be of class 'npmc'")
    
  if(!exists("digits")) digits <- 4L
  
  kw <- object$kw
  mc <- object$mc
  
  cat("data:", object$vnames[1], "on", object$vnames[2], "\n\n")
  
  cat("Omnibus Test\n")                        
  cat(paste("Kruskal-Wallis chi-squared = ", 
             round(kw$statistic,4), 
            ", df = ", round(kw$parameter, 3), 
            ", p-value = ", 
               format.pval(kw$p.value, digits = digits), 
            "\n\n", sep=""))
  
  cat("Descriptive Statistics\n")     
  print(object$sumstats, ...)
  
  
  mc$stars <- " "                  
  mc$stars[mc$p <   .1] <- "."
  mc$stars[mc$p <  .05] <- "*"
  mc$stars[mc$p <  .01] <- "**"
  mc$stars[mc$p < .001] <- "***"
  names(mc)[which(names(mc)=="stars")] <- " "                          
  
  cat("\nMultiple Comparisons (Wilcoxon Rank Sum Tests)\n")    
  cat(paste("Probability Adjustment = ", object$method, "\n", sep=""))
  print(mc, ...)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}
