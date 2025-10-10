# Process and eligibility data #
process_elig_data <- function(elig.dat) {

  elig_avail = rowMeans(elig.dat, na.rm=TRUE)
  elig_avail = ifelse(is.na(elig_avail), "NA", as.character(elig_avail))
  elig_nomiss = rowMeans(elig.dat)
  elig_nomiss = ifelse(is.na(elig_nomiss), "NA", as.character(elig_nomiss))
  # Set up elig outcome (0, NA, 1) #
  # If only using one eligibility criterion, elig_nomiss and elig_avail can
  # both be NA
  elig_fact <- case_when(
    elig_nomiss == "1" & elig_avail == "1" ~ "1",
    elig_nomiss == "NA" & elig_avail == "1" ~ "NA",
    elig_nomiss == "NA" & elig_avail == "NA" ~ "NA",
    elig_nomiss == "0" | elig_avail == "0" ~ "0",
    .default = "0"
  )
  
  elig_fact <- factor(elig_fact, levels = c("0", "NA", "1"))

  elig_clean <- data.frame(apply(elig.dat, 2, function(x) ifelse(is.na(x), 0, x)))

  return(list(elig_avail=elig_avail,
              elig_nomiss=elig_nomiss,
              elig_fact=elig_fact,
              elig_clean=elig_clean))
}
# Create eligibility data based on rules #
create_elig_data <- function(X_eval, elig.rules) {

  elig_dat <- NULL
  for (i in 1:dim(elig.rules)[1]) {

    var_i <- elig.rules$var[i]
    thres_i <- elig.rules$thres[i]
    ind_IE <- eval(parse(text=paste("ifelse(X_eval[,var_i]",thres_i, ", 1, 0)")))
    elig_dat <- cbind(elig_dat, ind_IE)

  }
  elig_dat <- data.frame(elig_dat)
  colnames(elig_dat) <- paste(elig.rules$var, "_IE", sep="")

  return(process_elig_data(elig_dat))

}

# Generate CV Folds #
CV_folds <- function(n, V, strata=NULL){
  if (is.null(strata)){
    folds <- sample(rep(1:V,ceiling(n/V))[1:n])
  }
  if (!is.null(strata)){
    folds = NULL
    for (s in unique(strata)){
      sub_i <- seq_len(n)[strata == s]
      ns = length(sub_i)
      folds.s <- sample(rep(1:V,ceiling(ns/V))[1:ns])
      folds[sub_i] = folds.s
    }
  }
  
  
  return(folds)
}

fractional_rep <- function(x, times) {
  full_reps <- floor(times)
  partial_len <- round((times - full_reps) * length(x))
  
  full_part <- rep(x, full_reps)
  partial_part <- x[seq_len(partial_len)]
  
  c(full_part, partial_part)
}

## Wrapper for Multivariate Eligibility Estimation ##
elig_mv <- function(elig_clean, X, Xtest, elig.mod, elig.hyper=NULL) {
  var_exclude <- sub("\\_IE*", "", colnames(elig_clean))
  X.star <- X[,!(colnames(X) %in% var_exclude), drop = FALSE]
  if (is.null(Xtest)) { Xtest.star <- NULL }
  if (!is.null(Xtest)) {
    Xtest.star <- Xtest[,!(colnames(Xtest) %in% var_exclude), drop = FALSE]
  }
  fit <- do.call(elig.mod, append(list(Y=elig_clean, X=X.star, Xtest=Xtest.star), elig.hyper))
  mu_train <- fit$mu_train
  mu_test <- fit$mu_test
  colnames(mu_train) <- paste("prob", colnames(mu_train), sep="_")
  colnames(mu_test) <- paste("prob", colnames(mu_test), sep="_")
  mu_train <- data.frame(mu_train, prob_IE = rowMeans(mu_train))
  mu_test <- data.frame(mu_test, prob_IE = rowMeans(mu_test))
  return(list(mod = fit$mod, mu_train=mu_train, mu_test=mu_test))
}
### Wrapper for univariate eligibility models (estimate each separately then combine) ###
elig_uni <- function(elig_clean, X, Xtest, elig.mod, elig.hyper=NULL) {

  uni_fit <- function(c) {
    Y_r <- elig_clean[,c]
    var_exclude <- sub("\\_IE*", "", c)
    X.star <- X[,!(colnames(X) %in% var_exclude), drop = FALSE]
    if (is.null(Xtest)) { Xtest.star <- NULL }
    if (!is.null(Xtest)) {
      Xtest.star <- Xtest[,!(colnames(Xtest) %in% var_exclude), drop = FALSE]
    }
    fit <- do.call(elig.mod, append(list(A=Y_r, X=X.star, Xtest = Xtest.star), elig.hyper))
    mu_train <- data.frame(fit$pi.hat)
    mu_test <- data.frame(fit$pi.hat_test)
    colnames(mu_train) <- paste("prob", c, sep="_")
    colnames(mu_test) <- paste("prob", c, sep="_")
    return(list(mod=fit$mod, mu_train=mu_train, mu_test=mu_test))
  }
  fits <- lapply(colnames(elig_clean), uni_fit)
  fits2 <- do.call(cbind, fits)
  mod <- fits2[1,]
  names(mod) <- paste("fit", colnames(elig_clean), sep="_")
  mu_train <- do.call(cbind, fits2[2,])
  mu_test <- do.call(cbind, fits2[3,])
  mu_train <- data.frame(mu_train, prob_IE = rowMeans(mu_train))
  mu_test <- data.frame(mu_test, prob_IE = rowMeans(mu_test))
  return(list(mu_train=mu_train, mu_test=mu_test, mod = mod))
}
CV.model_IE = function(elig_clean, X, elig.wrap, elig.mod, elig.hyper, V=5, stratify=TRUE){
  elig_ind <- ifelse(rowMeans(elig_clean)==1, 1, 0)
  # Generate CV Folds #
  n = dim(X)[1]
  if (stratify){
    strata <- elig_ind
  }
  if (!stratify){
    strata = NULL
  }
  folds <- CV_folds(n=n, V=V, strata=strata)
  id <- 1:nrow(X)

  # Calculate CV-AUC and select lambda #
  CV.looper = function(v){
    # Train/Test #
    X.train = X[folds!=v, ,drop = FALSE]
    X.test = X[folds==v, ,drop = FALSE]
    elig_clean_v <- elig_clean[folds!=v, ,drop = FALSE]
    fit <- do.call(elig.wrap, append(list(elig_clean=elig_clean_v, X=X.train,
                                          Xtest=X.test, elig.mod = elig.mod), elig.hyper))
    auc.indiv <- NULL
    for (c in 1:dim(elig_clean)[2]) {
      org_name <- colnames(elig_clean)[c]
      prob_name <- paste("prob", org_name, sep="_")
      auc0 <- tryCatch(as.numeric(suppressMessages(pROC::auc(elig_clean[folds==v, org_name],
                                                             fit$mu_test[,prob_name]))),
                       error = function(e) "error")
      if (is.character(auc0)) {
        auc0 <- NA
      }
      names(auc0) <- names(fit$mu_test)[c]
      auc.indiv <- c(auc.indiv, auc0)
    }
    auc.ovrl <- tryCatch(as.numeric(suppressMessages(pROC::auc(elig_ind[folds==v],
                                                               fit$mu_test$prob_IE))),
                         error = function(e) "error")
    if (is.character(auc.ovrl)) {
      auc.ovrl <- NA
    }
    names(auc.ovrl) <- "prob_IE"
    auc.dat <- t(data.frame(c(auc.indiv, auc.ovrl)))
    colnames(auc.dat) <- paste(colnames(auc.dat), "auc", sep=".")
    preds.CV <- fit$mu_test
    colnames(preds.CV) <- paste(colnames(preds.CV), "cv", sep=".")
    rownames(auc.dat) <- NULL
    pred.CV <- data.frame(id = id[folds==v], V=v, preds.CV)
    pred.CV <- data.frame(pred.CV, auc.dat)
    return(pred.CV)
  }
  fit.CV = lapply(1:V, CV.looper)
  CV.dat = do.call(rbind, fit.CV)
  CV.dat = CV.dat %>% arrange(id)
  return(CV.dat)
}
## Loss Function: RMST ###
loss_rmst = function(time, event, group, weights=NULL) {
  
  # Group=1 #
  dat.1 <- data.frame(time = time[group==1], event=event[group==1],
                      weights = weights[group==1])
  dat.1 = na.omit(dat.1)
  km.1 = survival::survfit(Surv(time, event) ~ 1, data = dat.1, weight=weights)
  surv.1 <- data.frame( km.1$time, km.1$surv )
  names(surv.1) <- c("Time","Surv")

  # Group=0 #
  dat.0 <- data.frame(time = time[group==0], event=event[group==0],
                      weights = weights[group==0])
  dat.0 = na.omit(dat.0)
  km.0 = survival::survfit(Surv(time, event) ~ 1, data = dat.0, weight=weights)
  surv.0 <- data.frame( km.0$time, km.0$surv )
  names(surv.0) <- c("Time","Surv")

  # Max time #
  tau  = min( max(dat.0$time), max(dat.1$time) )

  rmst_calc <- function(time, surv, tau){
    idx = time <= tau
    id.time = sort(c(time[idx], tau))
    id.surv = surv[idx]
    time.diff <- diff(c(0, id.time))
    areas <- time.diff * c(1, id.surv)
    rmst = sum(areas)
    return(rmst)
  }
  rmst_0 <- with(surv.0, rmst_calc(Time, Surv, tau) )
  rmst_1 <- with(surv.1, rmst_calc(Time, Surv, tau) )
  loss <- abs( rmst_1 - rmst_0 )
  return(loss)
}
# Adaptive Selection of cutoff #
adaptive_lambda = function(Y, prob.elig, res.CV, V=5, lambda.vec=NULL) {
  # Create data #
  if (survival::is.Surv(Y)) {
    hold = data.frame(time = Y[,1], event=Y[,2], res.CV)
  }
  if (!survival::is.Surv(Y)) {
    hold = data.frame(Y=Y, res.CV)
  }
  hold$elig <- with(hold, ifelse(elig=="1", 1, 0))

  if (is.null(lambda.vec)){
    lambda.vec = seq(min(prob.elig$prob_IE)+0.02, max(prob.elig$prob_IE)-0.02, by=0.02)
  }

  # Lambda Looper Function: Calculate Brier Score/Neff #
  lambda_loop = function(test, lambda){
    # Set up data #
    test.l <- test[(test$elig)==1 | (test$w_elig>lambda),]
    # Normalize Weights #
    test.l$w_elig[test.l$elig==1] = test.l$w_elig[test.l$elig==1] /
      mean(test.l$w_elig[test.l$elig==1])
    test.l$w_elig[test.l$elig==0] = test.l$w_elig[test.l$elig==0] /
      mean(test.l$w_elig[test.l$elig==0])
    # Calculate Loss #
    # with no missing values, loss_rmst gives an error
    est.loss <- suppressWarnings(tryCatch(loss_rmst(time=test.l$time,
                                                    event=test.l$event,
                                                    group=test.l$elig,
                                                    weights=test.l$w_elig),
                          error = function(e) return(as.character(e))))
    if (is.character(est.loss)) { est.loss <- NA }
    summ = data.frame(lambda=lambda, est.loss=est.loss)
    return(summ)
  }
  lambda.dat <- NULL
  for (v in 1:V){
    res <- lapply(lambda.vec, lambda_loop, test=hold[hold$V==v,])
    res <- do.call(rbind, res)
    res <- data.frame(V=v, res)
    lambda.dat <- rbind(lambda.dat, res)
  }
  # Calculate Optimal lambda (min and min-1*SE) #
  opt.lambda <- aggregate(est.loss ~ lambda, data=lambda.dat, FUN="mean")
  opt.min <- opt.lambda$lambda[which.min(opt.lambda$est.loss)]
  return(list(lambda.dat=lambda.dat, opt.min=opt.min))
}
## Trimming ##
# @Y: Outcome
# @prob.elig: Data-set with indicator for eligibility and probability estimates
# @res.CV: Eligibility model cross-validation results
trim_calculator <- function(Y, prob.elig, res.CV, V=5, lambda.vec=NULL) {
  ## Youden ##
  prob.elig$elig <- with(prob.elig, ifelse(elig=="1", 1, 0))
  roc.obj <- suppressMessages(with(prob.elig, pROC::roc(elig, prob_IE)))
  youden.thres <- suppressWarnings(pROC::coords(roc.obj, "best", ret="threshold"))
  youden.thres <- as.numeric(youden.thres)
  ## Outcome Based ##
  lambda.dat <- NULL
  outcome.thres <- NULL
  if (!is.null(res.CV) & survival::is.Surv(Y)) {
    res.out <- adaptive_lambda(Y=Y, prob.elig=prob.elig,
                               res.CV=res.CV, lambda.vec=lambda.vec)
    lambda.dat <- res.out$lambda.dat
    outcome.thres <- res.out$opt.min
  }
  # Return results #
  res <- list(youden.thres=youden.thres, outcome.thres=outcome.thres,
              lambda.dat=lambda.dat)
  return(res)
}

#### Functions for covariate balance ####
stand_cov_diff <- function(elig, weights, Xvar) {

  res <- NULL
  for (var in colnames(Xvar)) {

    X_use <- Xvar[,var]
    ind_miss <- !complete.cases(X_use)
    elig_use <- elig[!ind_miss]
    weights_use <- weights[!ind_miss]
    X_use <- X_use[!ind_miss]

    if (is.factor(X_use)) {

      res_f <- NULL
      for (f in levels(X_use)) {

        X_f = ifelse(X_use==f, 1, 0)

        res_w <- calc_std_diff_bin(elig_use=elig_use, X_use=X_f, weights_use=weights_use)
        res_0 <- calc_std_diff_bin(elig_use=elig_use, X_use=X_f, weights_use=rep(1, length(weights_use)))
        hold <- rbind(data.frame(type = "weighted", res_w),
                      data.frame(type = "unadjusted", res_0))
        hold <- data.frame(var=paste(var, f, sep=":"), hold)
        res_f <- rbind(res_f, hold)
      }
      res <- rbind(res, res_f)

    }
    if (!is.factor(X_use)) {

      res_w <- calc_std_diff(elig_use=elig_use, X_use=X_use, weights_use=weights_use)
      res_0 <- calc_std_diff(elig_use=elig_use, X_use=X_use, weights_use=rep(1, length(weights_use)))
      hold <- rbind(data.frame(type = "weighted", res_w),
                    data.frame(type = "unadjusted", res_0))
      hold <- data.frame(var=var, hold)
      res <- rbind(res, hold)

    }


  }
  return(res)
}

calc_std_diff_bin <- function(elig_use, X_use, weights_use) {

  dat_use <- NULL
  for (ee in unique(elig_use)) {
    mask_ee <- elig_use==ee
    hold_ee <- data.frame(elig = ee,
                          mu_w = weighted.mean(X_use[mask_ee], weights_use[mask_ee], na.rm=TRUE))
    dat_use <- rbind(dat_use, hold_ee)
  }

  std_pool <- sqrt( (dat_use$mu_w[dat_use$elig=="1"]*(1-dat_use$mu_w[dat_use$elig=="1"])+
                       dat_use$mu_w[dat_use$elig=="weighted"]*(1-dat_use$mu_w[dat_use$elig=="weighted"]))/2)

  mu_diff <- dat_use$mu_w[dat_use$elig=="1"] - dat_use$mu_w[dat_use$elig=="weighted"]
  d_est <- (mu_diff) / sqrt(std_pool)

  out <- data.frame(d_est = d_est, mu_diff = mu_diff, std_pool = std_pool)
  out <- data.frame(compare = c("1 vs weighted"),
                    d_est = c(d_est),
                    mu_est = c(mu_diff),
                    std_est = c(std_pool))
  return(out)
}

calc_std_diff <- function(elig_use, X_use, weights_use) {

  dat_use <- NULL
  for (ee in unique(elig_use)) {
    mask_ee <- elig_use==ee
    hold_ee <- data.frame(elig = ee,
                          mu_w = weighted.mean(X_use[mask_ee], weights_use[mask_ee], na.rm=TRUE),
                          std_w = std_calc(X_use[mask_ee], weights_use[mask_ee]))
    dat_use <- rbind(dat_use, hold_ee)
  }

  # 1 vs weighted #
  mu_1w <- dat_use$mu_w[dat_use$elig=="1"] - dat_use$mu_w[dat_use$elig=="weighted"]
  std_1w <- (dat_use$std_w[dat_use$elig=="1"] + dat_use$std_w[dat_use$elig=="weighted"])/2
  dest_1w <- (mu_1w) / sqrt(std_1w)

  out <- data.frame(compare = c("1 vs weighted"),
                    d_est = c(dest_1w),
                    mu_est = c(mu_1w),
                    std_est = c(std_1w))
  return(out)
}

std_calc <- function(x, w) {
  mu_w <- weighted.mean(x, w, na.rm = TRUE)
  std_w <- sum(w) / (sum(w)^2 - sum(w^2)) * sum(w*(x-mu_w)^2, na.rm = TRUE)
  return(std_w)
}
