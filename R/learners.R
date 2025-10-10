### Learners: Contains wrapper functions for models ###

## Logistic ##
logistic_fit <- function(A, X, ...) {

  mod = glm(A ~ . , data=data.frame(A, X), family = "binomial")
  pi.hat = predict(mod, type="response")

  return(list(mod=mod, pi.hat=pi.hat))
}
# RFSRC (RandomForestSRC) #
rf_fit = function(A, X, Xtest=NULL, oob=TRUE, ntree=1000, ...){

  A_fac <- factor(A, levels=c("0", "1"))
  mod <- rfsrc(A_fac ~., data = data.frame(A_fac, X), na.action = "na.impute",
               ntree=ntree)
  if (oob) {
    pi.hat <- mod$predicted.oob[,2]
  }
  if (!oob) {
    pi.hat <- mod$predicted[,2]
  }
  if (is.null(Xtest)) {
    pi.hat_test <- NULL
  }
  if (!is.null(Xtest)) {
    pi.hat_test <- predict(mod, newdata = Xtest, na.action="na.impute")$predicted[,2]
  }

  return(list(mod=mod, pi.hat=pi.hat, pi.hat_test = pi.hat_test))
}

# RFSRC (RandomForestSRC; multivariate) #
rfmv_fit = function(Y, X, Xtest, pred.type = "response", oob=TRUE, importance=FALSE, ...) {

  # Fit randomforestSRC (with imputation) #
  form <- paste(colnames(Y), collapse = ",")
  form <- paste("cbind(", form, ")~.", sep="")
  form <- as.formula(form)
  rf.mod <- rfsrc(form, data = data.frame(Y, X), na.action = "na.impute",
                  importance=importance)
  # Training/Test Predictions #
  if (oob){ mu_train = get.mv.predicted(rf.mod) }
  if (!oob){ mu_train = get.mv.predicted(rf.mod, oob=FALSE) }
  out_test <- predict(rf.mod, newdata = Xtest, na.action="na.impute")
  mu_test <- get.mv.predicted(out_test)
  mu_train <- data.frame(mu_train)
  mu_test <- data.frame(mu_test)
  return(list(mu_train=mu_train, mu_test=mu_test, mod = rf.mod))
}
