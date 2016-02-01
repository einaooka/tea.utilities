
#' @title Convert between Daily and Monthly Data Frames
#'
#' @description ConvertDailyToMonthly takes a daily table and convert it to a monthly table with values equal to the mean.
#' Conversely, ConvertMonthlyToDaily takes a monthly table and convert it to a daily table.
#'
#' @param df A data frame to be converted. The first column needs to be a date column.
#' @param type Smoothing type for monthly-to-daily conversion. c("step", "smooth", "smooth.naive). Default set at "step."
#' @param indicator.df An optional input for denton disaggregation. It should be a data frame with "Date" as the first column, 
#' and the same number of columns as df, in a daily granularity.    
#' 
#' @details When 'step' is chosen for disaggregation method, data is simply extrapolated as constants through the months.
#' For 'smooth' disaggregation, the default setting is by denton method. If naive smoothing is chosen, piece-wise deinfed 2nd degree 
#' polynomial method is used. 
#'
#' @import tea.datetime
#'
#' @export ConvertDailyToMonthly
#' @export ConvertMonthlyToDaily
#'
#' @examples
#' library(reshape)
#' library(tea.eo.plots)
#' niter <- 2
#' monthly.df <- data.frame("Date"=rep(seq(as.Date("2010-1-1"), by="month", length.out = 12),niter)
#' , "Iter" = rep(1:niter,each=12), "Y"=rnorm(12*niter))
#' monthly.df <- cast(monthly.df, Date ~ Iter, value="Y")
#' plot0(monthly.df$Date, monthly.df$'1', ylim=c(-3,3))
#' matlines(monthly.df$Date, monthly.df[,-1], lwd=2, lty=1)
#'
#' daily.df <- ConvertMonthlyToDaily(monthly.df)
#' matlines(daily.df$Date, daily.df[,-1], lty=2)
#'
#' daily.df <- ConvertMonthlyToDaily(monthly.df, type="smooth")
#' matlines(daily.df$Date, daily.df[,-1])
#' 
#' daily.df <- ConvertMonthlyToDaily(monthly.df, type="smooth"
#' , indicator.df = data.frame("Date"=seq(as.Date("2010-1-1"), by="day", length.out = 400), rnorm(400, sd=0.5), rnorm(400, sd=0.5)))
#' matlines(daily.df$Date, daily.df[,-1])
#'
#' verify.df <- ConvertDailyToMonthly(daily.df)
#' matlines(verify.df$Date, verify.df[,-1], lwd=10, lty=1, col=gray.f(0.3))
#'


ConvertDailyToMonthly <- function(df){
  temp.df <- aggregate(df[,-1], by=list(FirstDayOfMonth(df[,1])), mean, na.rm=TRUE)
  colnames(temp.df) <- colnames(df)
  return(temp.df)
}

#' @describeIn ConvertDailyToMonthly Take a monthly data frame and convert it to daily data frame.
ConvertMonthlyToDaily <- function(df, type="step", indicator.df = NULL){

  nvars <- ncol(df)
  if (ncol(df) ==2) df$temp <- 1

  # Dates
  endDate <- tail(df[,1],1) + tea.datetime::NumDaysInMonth(tail(df[,1],1))-1
  return.df <- data.frame("Date"=seq(df[1,1], endDate, 1))

  # Step
  idx <- match(tea.datetime::FirstDayOfMonth(return.df$Date), df[,1])
  return.df <- cbind(return.df, df[idx,-1])

  if (type == "smooth"){ # denton temporal disaggregation
    
    # number of steps
    n_l <- nrow(df)
    n <- nrow(return.df)
    
    # variance-covariance matrix for addtive Denton
    D <- diag(n)
    diag(D[2:n, 1:(n - 1)]) <- -1
    Q <- solve(t(D) %*% D)
    
    # Conversion matrix
    C <- matrix(0, nrow=n_l, ncol=n)
    idx <- 0
    for (i in 1:nrow(df)){
      num.days <- NumDaysInMonth(df$Date[i])
      C[i, idx+c(1:num.days)] <- 1/num.days
      idx <- idx + num.days
    }
    
    # Distribution matrix
    D <- Q %*% t(C) %*% solve(C %*% Q %*% t(C))
    
    # preliminary series
    if (is.null(indicator.df)) {p <- matrix(0, nrow=n, ncol=nvars-1)
    } else { p <- as.matrix(indicator.df[match(return.df$Date, indicator.df$Date),-1])}
    
    # Reconcile results
    return.df[,-1] <- p + D %*% (as.matrix(df[,-1])-C %*% p)
    
  } else if (type == "smooth.naive"){
    
    day.idx <- as.numeric(format(return.df$Date, "%d"))/tea.datetime::NumDaysInMonth(return.df$Date)
    
    temp <- glag(df[,-1],1)
    temp[1,] <- df[1,-1] # Fill the first row
    prev <- (return.df[,-1] + temp[idx,])/2
    
    temp <- glag(df[,-1],-1)
    temp[nrow(temp),] <- df[nrow(temp),-1] # Fill the end row
    after <- (return.df[,-1] + temp[idx,])/2
    
    # polynomial coefficients
    a <- prev
    b <- 6 * return.df[,-1] - 4 * prev - 2 * after
    c <- 3 * after + 3 * prev - 6 * return.df[,-1]
    
    return.df[,-1] <- a + apply(b, 2, "*", day.idx) + apply(c, 2, "*", (day.idx^2))
  }
  return(return.df[,1:nvars])
}
