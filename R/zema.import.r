
#' @title Download Zema Profile Data
#'
#' @description Download data from zama profiles.
#'
#' @param profile.name Zema profile name to download
#' @param profileowner Owner of the zema profile
#' @param timestep c("Monthly", "Daily", NA). If "Monthly" data needs to be in a format of "yyyy mm"
#' and if "Daily," "mm/dd/yyyy." Default time formatting can be specified in Zema.
#' @param destfile A location and a file name where the data is temporarily stored.
#' Default is "temp.csv" at the local directory.
#' @param useCurl If OS is other than Windows, or if data set is large, use Curl. Otherwise set to FALSE.
#' @param username Your Zema accont username.
#' @param password Your Zema accont password.
#'
#' @import RCurl
#'
#' @export ImportFromZema
#' @export RegisterZemaAccount
#'
#' @examples
#' RegisterZemaAccount("eina.murphy", "m7g1z8f6")
#' ImportFromZema("HH Forward curves", "eina.murphy", timestep = "Monthly", useCurl=FALSE)


ImportFromZema <- function(profile.name, profile.owner, timestep = "", destfile = "temp.csv", useCurl=FALSE){

  # import user account
  zema.registry <- paste0(.libPaths()[1], "/zema.accounts.rds")

  if (file.exists(zema.registry)){
    zema.url <- readRDS(zema.registry)
  } else stop("Register your zema account before downloading data.")

  zema.url<-paste0(zema.url
                   , "&profilename=", profile.name
                   , "&profileowner=", profile.owner
                   , "&style=csv")

  # Import Data
  if (useCurl){
    RCurl::curlSetOpt(timeout = 200)
    data <- RCurl::getURL(zema.url, ssl.verifypeer=0L, followlocation=1L)
    writeLines(data,destfile)
    data.df <- read.csv(destfile, stringsAsFactors=FALSE)
  } else {
    setInternet2(use = NA)
    download.file(zema.url,destfile)
    data.df<-read.csv(destfile, stringsAsFactors=FALSE)
  }

  file.remove(destfile)

  # extract caption for column names
  for (i in 1:ncol(data.df)){
    temp <- strsplit(colnames(data.df)[i], ".", fixed = TRUE)[[1]]
    if (length(temp)>2)colnames(data.df)[i] <- temp[3]
  }

  # Format Date
  if (timestep == "Daily") data.df$Date <- as.Date(data.df$Date, "%m/%d/%Y")
  if (timestep == "Monthly") data.df$Date <- as.Date(paste(data.df$Date, "01"), "%Y %m %d")

  return(data.df)
}

#' @describeIn ImportFromZema Register Zema Account
RegisterZemaAccount <- function(username, password){
  zema.url <- paste0("https://analyze.zepower.com/datadirect/ZEData?command=LoadProfile"
                     , "&username=", username
                     , "&password=", password
                     , "&id=The%20Energy%20Authority")
  saveRDS(zema.url, paste0(.libPaths()[1], "/zema.accounts.rds"))
}








