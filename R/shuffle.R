#' @title Reorders the dataframe containing your screen results
#'
#' @description Analyzes SGAs
#'
#' @param x
#'
#' @return NULL
#'
#' @examples shuffle(screen.df, order.df, to.order, nameOutput)
#'
#' @export

shuffle <- function(x, order.df, to.order, nameOutput){
  # prepare df for shuffling
  shuffled.screen.df <- x
  row.names(shuffled.screen.df) <- shuffled.screen.df$gene
  shuffled.screen.df <- shuffled.screen.df[,-1]
  # extract and convert colnames to seperate df
  q <- data.frame(matrix(unlist(strsplit(names(shuffled.screen.df), "_")),nrow=length(shuffled.screen.df),byrow=T))
  names(q) <- colnames(order.df)
  # start loop that orders the colnames
  for (i in to.order){
    target <- unlist(order.df[i][order.df[i] != ""])
    q[[i]] <- ordered(q[[i]], levels = target)
    q <- q[order(q[[i]]), ]
  }
  # paste colnames back together and order screen.df
  #q.order <- paste0(q$type,"_",q$media, "_", q$mutant,"_",q$assay_stress, "_", q$interest, "_", q$screenname )
  q.order <- apply(q, 1, paste, collapse = "_")
  to.print <- x[,c("gene", (q.order))]
  assign(nameOutput, to.print, envir=globalenv())
}
