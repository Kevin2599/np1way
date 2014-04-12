#' @title Plot nonparametric multiple comparisons
#'
#' @description
#' \code{plot.npmc} plots pairwise group comparisons.
#'
#' @details
#' This function plots pairwise multiple comparisons
#' created by the \code{\link{npmc}} function.
#' 
#' @param x an object of class \code{npmc}.
#' @param prob significance level.
#' @param ... additional arguments passed to the 
#' \code{\link{boxplot}} function.
#' @method plot npmc
#' @export
#' @return NULL
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' states <- data.frame(state.region, state.x77)
#' results <- npmc(Illiteracy ~ state.region, states)
#' plot(results, col="red", main="Multiple Comparisons",
#'      xlab="US Region", ylab="Illiteracy Rates")
plot.npmc <- function(x, prob=.05, ...){                   
  if (!inherits(x, "npmc")) 
    stop("Object must be of class 'npmc'")
  
  data <- x$data                                    
  y <- data[,1]
  g <- data[,2]
  ymin <- min(y) - 1
  ymax <- max(y) + 1
  boxplot(y~g, ylim=c(ymin, ymax), ...)
  
  groups <- levels(g)                                    
  k <- length(groups)
  if (k > 26)
    warning("Unable to  label more than 26 groups")
  else{mc <- x$mc
       labels <- letters[1:k]
       row <- 0
       for (i in 1:k)
         for(j in 1:k)
           if (j > i){
             row <- row + 1
             if(mc[row,4] > prob){
               labels[j] <- paste(labels[j], letters[i], sep="")
             } 
           } 
       
       for (i in 1:k)
         text(i, ymax, labels[i])
       
       caption <- paste("Groups without common letters differ statistically (p < ", 
                        deparse(substitute(prob)), ")", sep="")
       mtext(caption, side=3, cex=.8)
  }
}
