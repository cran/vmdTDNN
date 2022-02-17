\name{VMDTDNN}
\alias{VMDTDNN}
\title{
Variational Mode Decomposition Based Time Delay Neural Network Model
}
\description{
The VMDTDNN function computes forecasted value with different forecasting evaluation criteria for Variational Mode Decomposition (VMD) Based Time Delay Neural Network Model (TDNN).
}
\usage{
VMDTDNN (data, stepahead=10, nIMF=4, alpha=2000, tau=0)}
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
The Variational Mode Decomposition method is a novel adaptive, non-recursive signal decomposition technology, which was introduced by Dragomiretskiy and Zosso (2014). VMD method helps to solve current decomposition methods limitation such as lacking mathematical theory, recursive sifting process which not allows for backward error correction, hard-band limits, the requirement to predetermine filter bank boundaries, and sensitivity to noise. It decomposes a series into sets of IMFs. Time-delay neural networks are used to forecast decomposed components individually (Jha and Sinha, 2014). Finally, the prediction results of all components are aggregated to formulate an ensemble output for the input time series.
}
\value{
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF}
  \item{FinalVMDTDNN_forecast }{Final forecasted value of the VMD based TDNN model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_VMDTDNN }{Mean Absolute Error (MAE) for  VMDTDNN model.}
  \item{MAPE_VMDTDNN }{Mean Absolute Percentage Error (MAPE) for VMDTDNN model.}
  \item{rmse_VMDTDNN }{Root Mean Square Error (RMSE) for VMDTDNN model.}
}
\references{
Choudhury, K., Jha, G. K., Das, P. and Chaturvedi, K. K. (2019). Forecasting potato price using ensemble artificial neural networks. Indian Journal of Extension Education, 55(1), 73--77.

Choudhary, K., Jha, G. K., Kumar, R. R. and Mishra, D. C. (2019). Agricultural commodity price analysis using ensemble empirical mode decomposition: A case study of daily potato price series. Indian Journal of Agricultural Sciences, 89(5), 882--886.

Dragomiretskiy, K.and Zosso, D. (2014). Variational mode decomposition. IEEE transactions on signal processing, 62(3), 531--544.

Jha, G. K. and Sinha, K. (2014). Time-delay neural networks for time series prediction: An application to the monthly wholesale price of oilseeds in India. Neural Computing and Applications, 24(3--4), 563--571.

}
\seealso{
VMDARIMA,VMDELM
}
\examples{
\donttest{
data("Data_Maize")
VMDTDNN(Data_Maize)
}
}
\keyword{VMD}
