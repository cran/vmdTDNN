\name{VMDARIMA}
\alias{VMDARIMA}
\title{
Variational Mode Decomposition Based Autoregressive Integrated Moving Average Model
}
\description{
The VMDARIMA function computes forecasted value with different forecasting evaluation criteria for Variational Mode Decomposition (VMD) Based Autoregressive Integrated Moving Average (ARIMA).
}
\usage{
VMDARIMA (data, stepahead=10, nIMF=4, alpha=2000, tau=0, D=FALSE)}
\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{stepahead}{
The forecast horizon.
}
  \item{nIMF}{
The number of IMFs.
}
  \item{alpha}{
The balancing parameter.
}
  \item{tau}{
Time-step of the dual ascent.
}
  \item{D}{
a boolean.
}
}
\details{
In this function, the variational mode decomposition (VMD) used for mining the trend features and detailed features contained in a time series. Moreover, the corresponding autoregressive integrated moving average (ARIMA) models were derived to reflect the different features of the IMFs. The final forecasted values obtained for a given time series.
}
\value{
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF}
  \item{FinalVMDARIMA_forecast }{Final forecasted value of the VMD based ARIMA model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_VMDARIMA }{Mean Absolute Error (MAE) for  VMDARIMA model.}
  \item{MAPE_VMDARIMA }{Mean Absolute Percentage Error (MAPE) for VMDARIMA model.}
  \item{rmse_VMDARIMA }{Root Mean Square Error (RMSE) for VMDARIMA model.}
}
\references{
Box, G. E., Jenkins, G. M., Reinsel, G. C. and Ljung, G. M. (2015). Time series analysis: forecasting and control. John Wiley and Sons.

Dragomiretskiy, K.and Zosso, D. (2014). Variational mode decomposition. IEEE transactions on signal processing, 62(3), 531--544.

Wang, H., Huang, J., Zhou, H., Zhao, L. and Yuan, Y. (2019). An integrated variational mode decomposition and arima model to forecast air temperature. Sustainability, 11(15), 4018.


}
\seealso{
VMDTDNN,VMDELM
}
\examples{
\donttest{
data("Data_Maize")
VMDARIMA(Data_Maize)
}
}
\keyword{VMD}
