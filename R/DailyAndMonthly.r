
#' @title Convert between Daily and Monthly Data Frames
#'
#' @description ConvertDailyToMonthly takes a daily table and convert it to a monthly table with values equal to the mean.
#' Conversely, ConvertMonthlyToDaily takes a monthly table and convert it to a daily table, either as a step function or
#' pricewise-defined 2nd-degree polynomials.
#'
#' @param df A data frame to be converted. The first column needs to be a date column.
#' @param type Smoothing type for monthly-to-daily conversion. c("step", "smooth"). Default set at "step."
#'
#' @import tea.datetime
#'
#' @export ConvertDailyToMonthly
#' @export ConvertMonthlyToDaily
#'
#' @examples
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
#' verify.df <- ConvertDailyToMonthly(daily.df)
#' matlines(verify.df$Date, verify.df[,-1], lwd=10, lty=1, col=gray.f(0.3))
#'


ConvertDailyToMonthly <- function(df){
  temp.df <- aggregate(df[,-1], by=list(FirstDayOfMonth(df[,1])), mean, na.rm=TRUE)
  colnames(temp.df) <- colnames(df)
  return(temp.df)
}

#' @describeIn ConvertDailyToMonthly Take a monthly data frame and convert it to daily data frame.
ConvertMonthlyToDaily <- function(df, type="step"){

  nvars <- ncol(df)
  if (ncol(df) ==2) df$temp <- 1

  # Dates
  endDate <- tail(df[,1],1) + tea.datetime::NumDaysInMonth(tail(df[,1],1))-1
  return.df <- data.frame("Date"=seq(df[1,1], endDate, 1))

  # Step
  idx <- match(tea.datetime::FirstDayOfMonth(return.df$Date), df[,1])
  return.df <- cbind(return.df, df[idx,-1])

  # Smooth -- 2nd degree polynomial
  if (type == "smooth"){

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
