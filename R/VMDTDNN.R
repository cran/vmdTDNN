#' @importFrom VMDecomp vmd
#' @importFrom forecast nnetar forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'


VMDTDNN <- function(data, stepahead=10, nIMF=4, alpha=2000, tau=0, D=FALSE)
{
  data <- ts(data)
  data<- as.vector(data)
  v<- vmd(data , alpha=2000, tau=0, K=nIMF, DC=D, init=1, tol = 1e-6)
  AllIMF<-v$u
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data) - stepahead) + 1):length(data)),
  ]
  Fcast_AllIMF <- NULL
  for (AllIMF in 1:(ncol(IMF_trn))) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[, AllIMF]
    VMDTDNNFit <- forecast::nnetar(as.ts(IndIMF))
    VMDTDNN_fcast = forecast::forecast(VMDTDNNFit, h = stepahead)
    VMDTDNN_fcast_Mean = VMDTDNN_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(VMDTDNN_fcast_Mean))
  }
  FinalVMDTDNN_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_VMDTDNN = mean(abs(data_test - FinalVMDTDNN_fcast))
  MAPE_VMDTDNN = mean(abs(data_test - FinalVMDTDNN_fcast)/data_test)
  RMSE_VMDTDNN = sqrt(mean((data_test - FinalVMDTDNN_fcast)^2))
  return(list(AllIMF = AllIMF, data_test = data_test,
              AllIMF_forecast = Fcast_AllIMF, FinalVMDTDNN_forecast = FinalVMDTDNN_fcast,
              MAE_VMDTDNN = MAE_VMDTDNN, MAPE_VMDTDNN = MAPE_VMDTDNN,
              RMSE_VMDTDNN = RMSE_VMDTDNN ))
}
