#' @title Analyzes a single screen
#'
#' @description Analyzes SGAs
#'
#' @param x
#'
#' @import RColorBrewer
#'
#' @import gplots
#'
#' @examples batch.screen.analysis(screenTable)
#'
#' @export

batchScreenAnalysis <- function(x, borderEffectCorrection = TRUE, threshold = 100, plateMED_med2_limit = 2, size.col = "size")
{
  datalist <- list()
  # Define the function
  f= function(y){
    # Extract necessary info from sample table
    screenName = as.character(y["screen_name"])
    message(screenName)
    libPlateNumb <- as.numeric(y["libPlateNumb"])
    media <- as.numeric(y["types_media"])
    workingDir <- as.character(y["location_results"])
    type <- as.character(y["type"])
    mutant <- as.character(y["mutant"])
    assay_stress <- as.character(y["assay_stress"])
    media1 <- as.character(y["media1"])
    media2 <- as.character(y["media2"])
    media3 <- as.character(y["media3"])
    media4 <- as.character(y["media4"])
    media5 <- as.character(y["media5"])
    info_path <- as.character(y["info_path"])

    setwd(workingDir)

    # Start analysing plates
    for (i in 1:libPlateNumb)
    {
      # Define plate number (incrementing by 1 by each round, total number of plates is 8)
      plate <- paste("plate", i, sep="")
      #open data info and read into dataframe
      plate_info <- read.delim(paste0(info_path, i, ".txt"), header=T)
      plate_comb <- plate_info
      for (j in 1:media)
      {
        plate_path <- paste(workingDir,"/", i, "med", j, ".txt", sep="")
        data <- read.table(plate_path, header=TRUE, fill = T, sep = ",") # note: header=F is different for HTgrid and spotsizer output
        data <- as.data.frame(data[[size.col]])
        colnames(data) <- paste0("med", j)
        data <- cbind(plate_info, data)
        column <- paste0("med", j)
        if (borderEffectCorrection == TRUE){
          # Divide in Rings, nE indicates no Empties
          data_nE <- subset(data, data$mutID != "#N/A")
          ssMiddle <- subset(data, data$Ring >= 3)
          ssR1 <- subset(data, data$Ring == 1)
          ssR2 <- subset(data, data$Ring == 2)
          ssMiddle_nE <- subset(data_nE, data_nE$Ring >= 3)
          ssR1_nE <- subset(data_nE, data_nE$Ring == 1)
          ssR2_NE <- subset(data_nE, data_nE$Ring == 2)
          ##
          R1CorFac <- median(ssMiddle_nE[[column]]) / median(ssR1_nE[[column]])
          R2CorFac <- median(ssMiddle_nE[[column]]) / median(ssR2_NE[[column]])
          # Apply correction factor
          ssR1[[column]] <- ssR1[[column]]*R1CorFac
          ssR2[[column]] <- ssR2[[column]]*R2CorFac
          data <- rbind(ssMiddle, ssR1, ssR2)
          data <- data[order(data$pos384),]
        }
        plate_comb <- cbind(plate_comb, data[column])
      }

      #add new variables: ratio med2/med1, plate-MEDIAN, med2/med1 MEDnormalized and log2 values of med2/med1 MEDnormalized
      plate_comb$ratio_med2_med1<-plate_comb$med2/plate_comb$med1 #ratio med2/med1 added
      median_plate_med2 <- median(plate_comb$ratio_med2_med1, na.rm = TRUE) #plate median calculated
      plate_comb$MED_med2 <- median_plate_med2 #col with plate median added
      plate_comb$MEDnorm_med2 <-plate_comb$ratio_med2_med1 / median_plate_med2 #med2/med1 data median-normalized
      plate_comb$LOG2_med2 <- log2(plate_comb$MEDnorm_med2) #log2 value of med2/med1 med-norm data

      if(media >= 3){
        #as above but for -med3
        plate_comb$ratio_med3_med1<-plate_comb$med3/plate_comb$med1 #ratio med3/med1 added
        median_plate_med3 <- median(plate_comb$ratio_med3_med1, na.rm = TRUE) #ratio med3/med1 added
        plate_comb$MED_med3 <- median_plate_med3 #col with plate median added
        plate_comb$MEDnorm_med3 <-plate_comb$ratio_med3_med1 / median_plate_med3 #med3/med1 data median-normalized
        plate_comb$LOG2_med3 <- log2(plate_comb$MEDnorm_med3) #log2 value of med3/med1 med-norm data
      }

      if(media >= 4){
        #as above but for med4
        plate_comb$ratio_med4_med1<-plate_comb$med4/plate_comb$med1 #ratio med4/med1 added
        median_plate_med4 <- median(plate_comb$ratio_med4_med1, na.rm = TRUE) #ratio med4/med1 added
        plate_comb$MED_med4 <- median_plate_med4 #col with plate median added
        plate_comb$MEDnorm_med4 <-plate_comb$ratio_med4_med1 / median_plate_med4 #med4/med1 data median-normalized
        plate_comb$LOG2_med4 <- log2(plate_comb$MEDnorm_med4) #log2 value of med4/med1 med-norm data
      }

      #combined table assigned with new variable=plate_number
      assign(plate, plate_comb)

      #QC for each plate by checking the ratio of med2:med1 col- and row-wise for each plate
      par(mfrow = c(3,2), mar=c(2,2,2,2), bg="white")
      boxplot (med1~col, data=plate_comb, outline=F, main=paste(screenName, plate, "med1 - col - BC "))
      boxplot (med1~row, data=plate_comb, outline=F, main=paste(screenName, plate, "med1 - row - BC "))
      boxplot (med2~col, data=plate_comb, outline=F, main=paste(screenName, plate, "med2 - col - BC "))
      boxplot (med2~row, data=plate_comb, outline=F, main=paste(screenName, plate, "med2 - row - BC "))
      boxplot (ratio_med2_med1~col, data=plate_comb, outline=F, main=paste(screenName, plate, "med2:med1 - col - BC "))
      boxplot (ratio_med2_med1~row, data=plate_comb, outline=F, main=paste(screenName, plate, "med2:med1 - row - BC "))

      #define colors used for heatmap "pal" from the 'brewer' packages:
      pal = rev(brewer.pal(11,"Spectral"))

      #this following section has commented out to reduce total number of plots (there is only a limited number that can be shown in RStudio)
      #platemed1 <- plate_comb$med1
      #dim(platemed1) <- c(24,16)
      #heatmap.2(t(platemed1), main=paste(plate, "med1"), trace="none", key=T, col=pal, Rowv=F, Colv=F, na.color="grey", dendrogram="none")

      platemed2 <- plate_comb$med2
      dim(platemed2) <- c(24,16)
      heatmap.2(t(platemed2),  main=paste(plate, "med2"), trace="none", key=T, col=pal, Rowv=F, Colv=F, na.color="grey", dendrogram="none")

      plateMED <- plate_comb$MEDnorm_med2
      plateMED[plateMED==Inf] <- 0
      plateMED[plateMED>plateMED_med2_limit] <- NA
      dim(plateMED) <- c(24,16)
      heatmap.2(t(plateMED), main=paste(plate, "med2:med1 MEDnorm"), na.rm=TRUE,  trace="none", key=T, col=pal, Rowv=F, Colv=F, na.color="grey", dendrogram="none")

      # printout - this has no real function but indicates whether the loop goes through all plate
      message(plate)

      # Make a list of all plates analyse
      datalist[[i]] <- plate_comb
    }

    # Rbind all plates
    plate_all <- do.call(rbind, datalist)

    #Removes all infinite values and replace it with "NA"
    is.na(plate_all) <- sapply(plate_all, is.infinite)
    #all plates (1-8) filtered for med1 (med4) defined by threshold)
    plate_all_filter <- subset (plate_all, plate_all$med1 >= threshold)

    #all plates (1-8) with selected data (see below)
    # if(media == 2){
    #   plate_all_log2 <- subset (plate_all, select=c("mutID", "gene", "med1", "med2", "LOG2_med2"), mutID != "#N/A")
    # }else if (media == 3){
    #   plate_all_log2 <- subset (plate_all, select=c("mutID", "gene", "med1", "med2", "med3", "LOG2_med2", "LOG2_med3"), mutID != "#N/A")
    # }else {
    #   plate_all_log2 <- subset (plate_all, select=c("mutID", "gene", "med1", "med2", "med3", "med4", "LOG2_med2", "LOG2_med3", "LOG2_med4"), mutID != "#N/A")
    # }

    par(mfrow = c(2,2), mar=c(2,2,2,2), bg="white", col.axis="blue")
    boxplot(med2~plate, data=plate_all_filter, outline=F, main="med2")
    boxplot(med1~plate, data=plate_all_filter, outline=F, main="med1")
    boxplot(ratio_med2_med1~plate, data=plate_all_filter,outline=F,  main="ratio med2/med1")
    boxplot(MEDnorm_med2~plate, data=plate_all_filter, outline=F, main="median-norm med2 values")

    if(media==3){
      #distribution of med2:med1 and med3:med1 filtered
      par(mfrow = c(3,2), mar=c(2,2,2,2), col.axis="grey")
      hist(plate_all_filter$med1, breaks=100, col="yellow", xlim=c(0,1500), main="med1")
      hist(plate_all_filter$med1, breaks=100, col="yellow", xlim=c(0,1500), main="med1")
      hist(plate_all_filter$med2, breaks=100, col="red",  xlim=c(0,1500), main="med2")
      hist(plate_all_filter$LOG2_med2, breaks=100, col="blue", xlim=c(-2,2), main="med2:med1")
      hist(plate_all_filter$med3, breaks=200, col="red",  xlim=c(0,1500), main="med3")
    }

    if(media==3){
      hist(plate_all_filter$LOG2_med2, breaks=100, col="darkgreen",  xlim=c(-2,2), main="-med2:med1")
    }

    colClean <- function(x)
    {
      for (k in 1:media)
      {

        colnames(x) <- gsub(paste0("ratio_med", k, "_", media1), paste(type, get(paste0("media", k)), mutant, assay_stress, "ratio" ,screenName, sep = "_"), colnames(x))
        colnames(x) <- gsub(paste0("MED_med", k), paste(type, get(paste0("media", k)), mutant, assay_stress, "MED" ,screenName, sep = "_"), colnames(x))
        colnames(x) <- gsub(paste0("MEDnorm_med", k), paste(type, get(paste0("media", k)), mutant, assay_stress, "MEDnorm" ,screenName, sep = "_"), colnames(x))
        colnames(x) <- gsub(paste0("LOG2_med", k), paste(type, get(paste0("media", k)), mutant, assay_stress, "LOG2" ,screenName, sep = "_"), colnames(x))
        colnames(x) <- gsub(paste0("med", k), get(paste0("media", k)), colnames(x))
      }
      ; x
    }
    #plate_all <- colClean(plate_all)
    plate_all_filter <- colClean(plate_all_filter)
    #plate_all_log2 <- colClean(plate_all_log2)

    # Remove excisting .txt result files
    setwd(sub("gitterData", "", workingDir))
    file.remove(list.files(pattern = "\\.txt"))

    #save tables (plate 1-8, non-filtered and filtered) into files in the same directory as original data files
    #write.table(plate_all, paste(screenName, "_non-filtered", ".txt", sep=""), sep="\t", row.names=F)
    write.table(plate_all_filter, paste(screenName, "_filtered_", threshold, ".txt", sep=""), sep="\t", row.names=F)
    #write.table(plate_all_log2, paste(screenName, ".txt", sep=""), sep="\t", row.names=F)
  }
  # Apply the function to all rows in x
  apply(x, 1,f)
}

