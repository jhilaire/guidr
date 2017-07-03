#' Perform regression diagnostics
#'
#' This function performs a set of diagnostics to assess the performance of a linear regression model
#' @param i_data,
#' @param i_dvar
#' @param i_idvar
#' @keywords regression, diagnostics
#' @export
#'
#'
#'
perform_regression_diagnostics <- function(i_data, i_dvar, i_idvar) {
  # Taken from: http://www.statmethods.net/stats/rdiagnostics.html

  #=== Perform LM =============
  # Compute (multi)linear regression
  fit <- lm(
    as.formula(paste(i_dvar, " ~ ", paste(idvar, collapse=" + "))),
    data=i_data)

  #=== Evaluate data =============
  #=== Assessing Outliers ========
  #--- Bonferroni Outlier Test -----
  # Reports the Bonferroni p-values for Studentized residuals in linear and generalized linear models,
  # based on a t-test for linear models and normal-distribution test for generalized linear models.
  out1 <- outlierTest(fit, labels = paste0(data$iso, "-", data$year)) # Bonferonni p-value for most extreme obs
  print(out1)
  paste0("Outliers (based on Bonf. p-value): ", paste0(names(out1$rstudent), collapse=", "))

  #--- QQ-plot -----------------
  # Plots empirical quantiles of a variable, or of studentized residuals from a linear model,
  # against theoretical quantiles of a comparison distribution.
  out2 <- qqPlot(fit, main="QQ Plot", id.n=20, labels = paste0(data$iso, "-", data$year)) #qq plot for studentized resid
  paste0("Outliers (based on studentized residuals): ", paste0(names(out2), collapse=", "))

  #=== Normality of Residuals ====================================
  #--- Distribution of studentized residuals ------
  sresid <- studres(fit)
  hist(sresid, freq=FALSE,
       main="Distribution of Studentized Residuals")
  xfit<-seq(min(sresid),max(sresid),length=40)
  yfit<-dnorm(xfit)
  lines(xfit, yfit, col="red")

  #=== Evaluate Collinearity ==========================================
  #--- Variance inflation factors  ------------
  # Calculates variance-inflation and generalized variance-inflation factors for
  # linear and generalized linear models.
  vif <- vif(fit)
  paste0("VIF results - Potential problems with: ", paste0(names(sqrt(vif) > 2)[which(sqrt(vif) > 2)], collapse=", "))


  #=== Test for Autocorrelated Errors =================================
  #--- Durbin-Watson Test for Autocorrelated Errors --------
  # Computes residual autocorrelations and generalized Durbin-Watson statistics and their bootstrapped p-values.
  # dwt is an abbreviation for durbinWatsonTest
  ace <- durbinWatsonTest(fit, max.lag=10)
  print(ace)

  #=== Global test of model assumptions ============================
  gvmodel <- gvlma(fit)
  tma     <- summary(gvmodel)
  print(tma)

  out <- list(
    out1 = out1,
    out2 = out2,
    ace  = ace,
    vif  = vif,
    tma  = tma
  )

  return(out)

}
