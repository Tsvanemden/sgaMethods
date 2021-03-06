#' @title Combines results of multiple screens in single dataframe
#'
#' @return genelist
#'
#' @description Based on a sample table, extracts results of screens out of the
#'     .txt file generated by batchScreenAnalysis and combines them into a
#'     single datafame.
#'
#' @param x A dataframe containing info on each screen that needs to be added
#'     to the final dataframe.
#'
#' @param nameOutput Name of the output dataframe that will added to your global
#'     environment.
#'
#' @param interest Indicates in which batchScreenAnalysis column you are
#'     interested in. Chose between "ratio" and "MEDnorm".
#'
#' @param threshold Selects minimal pixels size for first media. Default is 100
#'
#' @param genelist List of all genes you want to have in you final data frame.
#'
#' @examples
#'     # Define sample table
#'     location_results <- c("/Desktop/screen1","/Desktop/screen2","/Desktop/screen3")
#'     screen_name <- c("screen1","screen2","screen3")
#'     type <- c("mat","mat","tel")
#'     mutant <- c("wt","abc","wt")
#'     assay_stress <- c("std","std","temp")
#'     media1 <- c("NS","NS","NS")
#'     media2 <- c("URA","URA","URA")
#'     sampleTable <- data.frame(location_results, screen_name, type, mutant, assay_stress, media1, media2)
#'     # Genes you are interested in
#'     genelist <- c("abc","def","ghi", "jkl","mno","oqr")
#'     # Call the function
#'     batchLookUp(sampleTable, "screen.df", "ratio", genelist, threshold = 150)
#'
#' @export

batchLookUp <- function(x, nameOutput, interest, genelist, threshold = 100)
{
  #Start a loop for every row in the sample table (after thesubset above)
  for (i in 1:nrow(x))
  {
    # Get the info needed from the sample table
    data_path <- sub("gitterData", "", paste0(as.character(x$dataPath[i]),as.character(x$screenName[i]), "_filtered_", threshold,".txt" ))
    screenName <- as.character(x$screenName[i])
    type <- as.character(x$type[i])
    mutant <- as.character(x$mutant[i])
    assay_stress <- as.character(x$assayStress[i])
    media1 <- as.character(x$media1[i])
    media2 <- as.character(x$media2[i])
    media3 <- as.character(x$media3[i])
    media4 <- as.character(x$media4[i])
    media5 <- as.character(x$media5[i])
    media <- as.numeric(x$numberMedia[i])

    # Open individual files
    filez <- read.csv(data_path, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
    filez <- subset(filez, filez$gene != "#N/A")

    for (j in 2:media){
      if (interest == "ratio"){
        coltomerge <- as.character(paste(type, get(paste0("media", j)), mutant, assay_stress, "ratio" ,screenName, sep = "_"))
        print(coltomerge)
        genelist <- merge(genelist, filez[ , c("gene", coltomerge )], all.x = TRUE)
      } else {
        coltomerge <- as.character(paste(type, get(paste0("media", j)), mutant, assay_stress, "MEDnorm" ,screenName, sep = "_"))
        print(coltomerge)
        genelist <- merge(genelist, filez[ , c("gene", coltomerge )], all.x = TRUE)
      }
    }
  }
  return(genelist)
}
