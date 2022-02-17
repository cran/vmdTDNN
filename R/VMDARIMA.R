#' @importFrom vmd vmd
#' @importFrom forecast nnetar forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'
VMDARIMA <- function (data, stepahead=10, nIMF=4, alpha=2000, tau=0)
{
  v<- vmd(data , alpha=2000, tau=0, K=nIMF, orderModes=TRUE)
  AllIMF<-v$as.data.frame()
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data) - stepahead) + 1):length(data)),
  ]
  Fcast_AllIMF <- NULL
  for (AllIMF in 3:(ncol(IMF_trn)-1)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[, AllIMF]
    VMDARIMAFit <- forecast::auto.arima(as.ts(IndIMF))
    VMDARIMA_fcast = forecast::forecast(VMDARIMAFit, h = stepahead)
    VMDARIMA_fcast_Mean = VMDARIMA_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(VMDARIMA_fcast_Mean))
  }
  FinalVMDARIMA_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_VMDARIMA = mean(abs(data_test - FinalVMDARIMA_fcast))
  MAPE_VMDARIMA = mean(abs(data_test - FinalVMDARIMA_fcast)/data_test)
  RMSE_VMDARIMA = sqrt(mean((data_test - FinalVMDARIMA_fcast)^2))
  AllIMF_plots <- plot(v)
  return(list(AllIMF = AllIMF, AllIMF_plots = AllIMF_plots, data_test = data_test,
              AllIMF_forecast = Fcast_AllIMF, FinalVMDARIMA_forecast = FinalVMDARIMA_fcast,
              MAE_VMDARIMA = MAE_VMDARIMA, MAPE_VMDARIMA = MAPE_VMDARIMA,
              RMSE_VMDARIMA = RMSE_VMDARIMA ))
}
