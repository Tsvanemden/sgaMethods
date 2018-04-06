#' @title Makes a correlation matrix of a data frame with screen results
#'
#' @description Analyzes SGAs
#'
#' @param x
#'
#' @return NULL
#'
#' @examples corMatrix(screenTable)
#'
#' @export

corMatrix <- function(x)
{
  # require(RColorBrewer)
  # require(ggplot2)
  # To create a correlation matrix of genelist you execute the code below
  y = as.matrix(x[,2:ncol(x)])
  rownames(y) = x[,1]
  co = cor(y, method="pearson", use="pairwise.complete.obs")
  pal = rev(brewer.pal(11,"RdYlBu"))
  # Play with margins, cexRow and cexCol to get a matrix you lke
  heatmap.2(co, trace="none", symbreaks = F, symkey=F, col=pal, margins = c(10,10), cexRow=0.8, cexCol=0.8 )
}
