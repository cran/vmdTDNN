\name{VMDELM}
\alias{VMDELM}
\title{
Variational Mode Decomposition Based Extreme Learning Machine Model
}
\description{
The VMDELM function computes forecasted value with different forecasting evaluation criteria for Variational Mode Decomposition (VMD) Based Extreme learning machine (ELM).
}
\usage{
VMDELM (data, stepahead=10, nIMF=4, alpha=2000, tau=0)}
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
}
\details{
This function decomposes a nonlinear, nonstationary time series into different IMFs using VMD (Qian et al., 2019). Extreme learning machine (ELM) is used to forecast decomposed IMFs individually. Finally, the prediction results of all three components are aggregated to formulate an ensemble output for the input time series.
}
\value{
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF}
  \item{FinalVMDELM_forecast }{Final forecasted value of the VMD based ELM model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_VMDELM }{Mean Absolute Error (MAE) for  VMDELM model.}
  \item{MAPE_VMDELM }{Mean Absolute Percentage Error (MAPE) for VMDELM model.}
  \item{rmse_VMDELM }{Root Mean Square Error (RMSE) for VMDELM model.}
}
\references{
Dragomiretskiy, K.and Zosso, D. (2014). Variational mode decomposition. IEEE transactions on signal processing, 62(3), 531--544.

Shao, Z., Chao, F., Yang, S. L., & Zhou, K. L. (2017). A review of the decomposition methodology for extracting and identifying the fluctuation characteristics in electricity demand forecasting. Renewable and Sustainable Energy Reviews, 75, 123--136.

Qian, Z., Pei, Y., Zareipour, H. andChen, N. (2019). A review and discussion of decomposition-based hybrid models for wind energy forecasting applications. Applied energy, 235, 939--953.


}
\seealso{
VMDTDNN,VMDARIMA
}
\examples{
\donttest{
data("Data_Maize")
VMDELM(Data_Maize)
}
}
\keyword{VMD}
