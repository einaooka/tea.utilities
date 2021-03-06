% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zema.import.r
\name{ImportFromZema}
\alias{ImportFromZema}
\alias{RegisterZemaAccount}
\title{Download Zema Profile Data}
\usage{
ImportFromZema(profile.name, profile.owner, timestep = "",
  destfile = "temp.csv", useCurl = FALSE)

RegisterZemaAccount(username, password)
}
\arguments{
\item{profile.name}{Zema profile name to download}

\item{timestep}{c("Monthly", "Daily", NA). If "Monthly" data needs to be in a format of "yyyy mm"
and if "Daily," "mm/dd/yyyy." Default time formatting can be specified in Zema.}

\item{destfile}{A location and a file name where the data is temporarily stored.
Default is "temp.csv" at the local directory.}

\item{useCurl}{If OS is other than Windows, or if data set is large, use Curl. Otherwise set to FALSE.}

\item{username}{Your Zema accont username.}

\item{password}{Your Zema accont password.}

\item{profileowner}{Owner of the zema profile}
}
\description{
Download data from zama profiles.
}
\section{Functions}{
\itemize{
\item \code{RegisterZemaAccount}: Register Zema Account
}}
\examples{
RegisterZemaAccount("eina.murphy", "m7g1z8f6")
ImportFromZema("HH Forward curves", "eina.murphy", timestep = "Monthly", useCurl=FALSE)
}

