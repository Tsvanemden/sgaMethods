#' @title Makes a simple PCA plot of dataframe containing screen results
#'
#' @description Analyzes SGAs
#'
#' @param x
#'
#' @return NULL
#'
#' @import ggfortify
#'
#' @examples quick.n.dirty(screenTable)
#'
#' @export

quick.n.dirtyPCA <- function(x)
{
  to.PCA <- x
  row.names(to.PCA) <- to.PCA[,1]
  to.PCA <- to.PCA[,-1]
  to.PCA <- log(to.PCA, base = 2)
  to.PCA <- do.call(data.frame,lapply(to.PCA, function(x) replace(x, is.infinite(x),NA)))
  to.PCA <- to.PCA[which(rowSums(is.na(to.PCA)) / ((ncol(to.PCA))) <= 0.2),]
  f1 <- function(vec) {
    m <- mean(vec, na.rm = TRUE)
    vec[is.na(vec)] <- m
    return(vec)
  }
  to.PCA = apply(to.PCA,2,f1)
  to.PCA <- as.data.frame(t(to.PCA))

  # Prepare the labels
  species <- to.PCA
  species$labels <- row.names(species)
  species$labels <- gsub("\\..*","",species$labels)

  # And plot the bad boy
  ir.pca <- prcomp(to.PCA)
  print(ggplot2::autoplot(ir.pca, data = species, colour = 'labels'))
}



