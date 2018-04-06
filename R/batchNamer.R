#' @title Renames files as they come from the colonie size analyzer
#'
#' @description Analyzes SGAs
#'
#' @param x
#'
#' @return NULL
#'
#' @examples batchNamer(screenTable, y)
#'
#' @export

batchNamer <- function(x, y)
{if(y == "libPlate")
  {apply(x,1,function(y)
    {
    # Extract necessary info from sample table
    screenName = as.character(y["screen_name"])
    message(screenName)
    libPlateNumb <- as.numeric(y["libPlateNumb"])
    media <- as.numeric(y["types_media"])
    workingDir <- as.character(y["location_results"])

    # Some settings we need for the upcoming loop
    filez <- list.files(path = workingDir) # This makes a list of all files in the gitData directory
    setwd(workingDir)
    counter <- 0
    filenumber <-1

    # Rename the files as they come from gitter
    if(media == 2){
      for (i in 1:libPlateNumb)
        {
        a <- filez[i+counter] # a will be 1,3,5, etc. b will be 2,4,6, etc.
        b <- filez[i+ 1 + counter]
        counter <- counter + 1
        file.rename(from=a, to = paste0(filenumber ,"med1", ".txt")) # Here the files will be renamed
        file.rename(from=b, paste0(filenumber ,"med2", ".txt"))
        filenumber <- filenumber + 1
        }
    } else if (media == 3 ){
      for (i in 1:libPlateNumb)
        {
        a <- filez[i+counter] # a will be 1,4,7 etc. b will be 2,5,8 etc. c will be 3,6,9 etc
        b <- filez[i+ 1 + counter]
        c <- filez[i+ 2 + counter]
        counter <- counter + 2
        file.rename(from=a, to = paste0(filenumber ,"med1", ".txt")) # Here the files will be renamed
        file.rename(from=b, paste0(filenumber ,"med2", ".txt"))
        file.rename(from=c, paste0(filenumber ,"med3", ".txt"))
        filenumber <- filenumber + 1
        }
    } else if (media == 4) {
      for (i in 1:libPlateNumb)
        {
        a <- filez[i+counter] # a will be 1,5,9, etc. b will be 2,6,10 etc. c will be 3,7,11, etc. d will be 4,8,12, etc.
        b <- filez[i+ 1 + counter]
        c <- filez[i+ 2 + counter]
        d <- filez[i+ 3 + counter]
        counter <- counter + 3
        file.rename(from=a, to = paste0(filenumber ,"med1", ".txt")) # Here the files will be renamed
        file.rename(from=b, paste0(filenumber ,"med2", ".txt"))
        file.rename(from=c, paste0(filenumber ,"med3", ".txt"))
        file.rename(from=d, paste0(filenumber ,"med4", ".txt"))
        filenumber <- filenumber + 1
        }
    } else {
        {
        a <- filez[i+counter] # a will be 1,5,9, etc. b will be 2,6,10 etc. c will be 3,7,11, etc. d will be 4,8,12, etc.
        b <- filez[i+ 1 + counter]
        c <- filez[i+ 2 + counter]
        d <- filez[i+ 3 + counter]
        e <- filez[i+ 4 + counter]
        counter <- counter + 4
        file.rename(from=a, to = paste0(filenumber ,"med1", ".txt")) # Here the files will be renamed
        file.rename(from=b, paste0(filenumber ,"med2", ".txt"))
        file.rename(from=c, paste0(filenumber ,"med3", ".txt"))
        file.rename(from=d, paste0(filenumber ,"med4", ".txt"))
        file.rename(from=e, paste0(filenumber ,"med5", ".txt"))
        filenumber <- filenumber + 1
        }
      }
    })
} else {
    print("yeah..... so this ain't ready yet.. sorry about that")
  }
}

