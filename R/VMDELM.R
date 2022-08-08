#' @importFrom VMDecomp vmd
#' @importFrom forecast nnetar forecast
#' @importFrom utils head tail
#' @importFrom nnfor elm
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'

VMDELM <- function(data, stepahead=10, nIMF=4, alpha=2000, tau=0, D=FALSE)
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
    VMDELMFit <- nnfor::elm(as.ts(IndIMF))
    VMDELM_fcast = forecast::forecast(VMDELMFit, h = stepahead)
    VMDELM_fcast_Mean = VMDELM_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(VMDELM_fcast_Mean))
  }
  FinalVMDELM_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_VMDELM = mean(abs(data_test - FinalVMDELM_fcast))
  MAPE_VMDELM = mean(abs(data_test - FinalVMDELM_fcast)/data_test)
  RMSE_VMDELM = sqrt(mean((data_test - FinalVMDELM_fcast)^2))
  return(list(AllIMF = AllIMF, data_test = data_test,
              AllIMF_forecast = Fcast_AllIMF, FinalVMDELM_forecast = FinalVMDELM_fcast,
              MAE_VMDELM = MAE_VMDELM, MAPE_VMDELM = MAPE_VMDELM,
              RMSE_VMDELM = RMSE_VMDELM ))
}
