#' elig_model: Estimate clinical eligibility probabilities
#'
#' For analyses including RWD and clinical trial data, one approach to make the populations
#' more similar is to apply the available clinical eligibility criteria on the RWD
#' patient. For example, only include RWD patients with ECOG<=1 and Age>18. Current options
#' include elig.mod="filter" (only include patients who satisfy all criteria),
#' elig.mod="simple" (for each patient, estimate the average number of criteria satisfied;
#' use this as a weight), and elig.mod="rfmv" (use multivariate random forest through
#' randomForestSRC to estimate the probability of satisfying all criteria).

#' @param Y Outcome. Must be numeric for continuous or binary outcomes. For survival outcomes, must be in Surv() form.
#' @param X Covariate space. Must be data.frame()
#' @param elig.mod Eligibility model. Default=NULL. Other options include "filter", which only include patients
#' who satisfy all eligibility criteria; or multivariate outcome random forest ("rfmv")
#' through randomForestSRC; or multiple imputation via chained equations ("mice_rf"). For all approaches,
#' elig.dat must  also be supplied. See below for details.
#' @param elig.hyper Hyper-parameters for eligibility model, must be list. Default=NULL.
#' @param elig.dat data.frame() with columns corresponding to individual eligibility criteria. Each column must be
#' binary (0, 1, or NA; which correspond to not-satisfying, satisfying, or uncertain about that specific criteria).
#' If not provided, must provide "elig.rules" (see below).
#' @param elig.rules Data-frame of eligibility rules. For example, data.frame(var = c("X1", "X2), thres=c(">0", ">0")),
#' which indicates that a patient is eligibile if X1>0 and X2>0. If not provided, must provide "elig.dat".
#' @param elig.CV Use cross-validation in eligibility model? Default=TRUE.
#' @param V Number of cross-validation folds. Default=5, only applicable if elig.CV=T.
#' @param stratify For CV, stratify by eligiblity status? Default=TRUE.
#' @param Xtest Test data-set
#' @param m_imp Number of imputed data-sets (only used for elig.mod="mice_rf", default=5)
#' @param ... Other arguments to pass through
#' @return
#' \itemize{
#' \item mod - Fitted eligibility model(s)
#' \item elig.dat - Eligibility output (elig indicator/probabilities/weights)
#' \item res.CV - cross-validation results (NULL if elig.CV=FALSE)
#' \item lambda.dat - Trimming threshold results
#' }
#' @export
#' @importFrom randomForestSRC rfsrc impute.rfsrc get.mv.predicted
#' @importFrom stats aggregate na.omit as.formula complete.cases confint
#' @importFrom stats glm lm predict rbinom rnorm weighted.mean weights
#' @importFrom survival survfit Surv
#' @importFrom pROC auc coords roc
#' @import dplyr
#'
#' @examples
#'
#' ## Testing ##
#' \donttest{
#' library(eligmodel)
#' library(dplyr)
#' library(survival)
#'
#' # Generate example data #
#' dat <- elig_data_gen(n=1000, dim_X=10, rho=0.3, miss_pct=0.1)
#' Y <- dat$Y
#' X <- dat$X
#' elig.dat <- dat$elig_dat
#'
#' # Test Different Approaches #
#'
#' # Filtering #
#' mod1 <- elig_model(Y=Y, X=X, elig.dat=elig.dat, elig.mod="filter")
#' summary(mod1$elig.dat)
#' aggregate(w_elig ~ elig, data=mod1$elig.dat, FUN="summary")
#' plot_elig(mod1)
#' plot_elig(mod1, type="outcome")
#' plot_elig(mod1, type="balance") # Differences should be zero, IE=1 is same as filter
#'
#' # Simple Weighting #
#' mod2 <- elig_model(Y=Y, X=X, elig.dat=elig.dat, elig.mod="simple")
#' summary(mod1$elig.dat)
#' aggregate(w_elig ~ elig, data=mod2$elig.dat, FUN="summary")
#' plot_elig(mod2)
#' plot_elig(mod2, type="outcome")
#' plot_elig(mod2, type="balance")
#'
#' # Model-based (multivariate RF) ##
#' mod3 <- elig_model(Y=Y, X=X, elig.dat=elig.dat, elig.mod="rfmv")
#' summary(mod3$elig.dat)
#' aggregate(w_elig ~ elig, data=mod3$elig.dat, FUN="summary")
#' plot_elig(mod3)
#' plot_elig(mod3, type="outcome")
#' plot_elig(mod3, type="balance")
#'
#' # MICE RF #
#' elig.rules <- data.frame(var = c("X1", "X2", "X3"),
#'                          thres = c(">= 0", ">0", ">0"))
#' mod4 <- elig_model(Y=Y, X=X, elig.mod="mice_rf", elig.rules = elig.rules)
#' summary(mod4$elig.dat)
#' aggregate(w_elig ~ elig, data=mod4$elig.dat, FUN="summary")
#' plot_elig(mod4)
#' plot_elig(mod4, type="outcome")
#' plot_elig(mod4, type="balance")
#'
#' }
elig_model <- function(Y,
                       X,
                       Xtest=NULL,
                       elig.dat=NULL,
                       elig.rules=NULL,
                       elig.mod="filter",
                       elig.hyper=NULL,
                       elig.CV=TRUE,
                       V=5,
                       stratify=TRUE,
                       m_imp=5, ...) {

  # Initialize #
  if (is.null(Xtest)) {
    Xtest <- X
  }
  if (elig.mod=="rfmv") {
    elig.mod <- "rfmv_fit"
  }
  if (elig.mod=="rf") {
    elig.mod <- "rf_fit"
  }
  # Process eligibility data #
  if (is.null(elig.dat) & is.null(elig.rules)) {

    stop("Must provided elig.dat or elig.rules argument")

  } else if (!is.null(elig.dat)) {
    init_elig <- process_elig_data(elig.dat=elig.dat)
    elig_avail <- init_elig$elig_avail
    elig_nomiss <- init_elig$elig_nomiss
    elig_fact <- init_elig$elig_fact
    elig_clean <- init_elig$elig_clean
  }
  else {
    init_elig <- create_elig_data(X_eval=X, elig.rules=elig.rules)
    elig_avail <- init_elig$elig_avail
    elig_nomiss <- init_elig$elig_nomiss
    elig_fact <- init_elig$elig_fact
    elig_clean <- init_elig$elig_clean
  }

  # Original Variable Names (for elig.dat) #
  org_vars_IE <- sub("\\_IE*", "", colnames(elig.dat))


  # Step 1: Estimate P(elig|X) #
  res.CV <- NULL
  mu_test <- NULL
  mod <- NULL
  if (elig.mod=="filter") {

    prob.elig <- data.frame(elig=elig_fact, prob_IE = ifelse(elig_fact=="1", 1, 0))

  } else if (elig.mod=="simple") {

    prob.elig <- data.frame(elig=elig_fact, prob_IE = rowMeans(elig_clean, na.rm=T))
    prob.elig$prob_IE <- with(prob.elig, ifelse(elig=="0", 0, prob_IE))

  } else if (elig.mod %in% c("rf_fit", "rfmv_fit")) {

    if (elig.mod %in% "rf_fit") {

      fit <- elig_uni(elig_clean=elig_clean, X=X, Xtest=Xtest, elig.mod = elig.mod, elig.hyper = elig.hyper)
      prob.elig <- data.frame(elig = elig_fact, fit$mu_train)
      mu_test <- fit$mu_test
      mod <- fit$mod
      elig.wrap <- "elig_uni"

    }
    if (elig.mod %in% "rfmv_fit") {

      fit <- elig_mv(elig_clean=elig_clean, X=X, Xtest=Xtest, elig.mod = elig.mod, elig.hyper = elig.hyper)
      prob.elig <- data.frame(elig = elig_fact, fit$mu_train)
      mu_test <- fit$mu_test
      mod <- fit$mod
      elig.wrap <- "elig_mv"

    }
    ## Cross-Validation? ##
    if (elig.CV) {
      res.CV <- CV.model_IE(elig_clean=elig_clean, X=X, elig.wrap=elig.wrap, elig.mod=elig.mod,
                            elig.hyper=elig.hyper, V=V, stratify=stratify)
      res.CV <- data.frame(elig=elig_fact, res.CV)
      res.CV$w_elig <- with(res.CV, ifelse(elig=="1", 1,
                                           ifelse(elig=="NA", prob_IE.cv, 0)))

    }
  } else if (elig.mod=="mice_rf") {

    if (!requireNamespace("mice", quietly = TRUE)) {
      stop("Package mice is needed for mice_rf imputation. Please install.")
    }
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Package ranger is needed for mice_rf imputation. Please install.")
    }
    prob.elig <- data.frame(elig=elig_fact)
    mod <- mice::mice(X, meth = "rf", m=m_imp)

    for (mm in 1:m_imp) {

      elig_mm <- create_elig_data(X_eval = mice::complete(mod, action=mm),
                                  elig.rules = elig.rules)
      prob_IE_m <- ifelse(elig_mm$elig_fact=="1", 1, 0)
      prob.elig <- cbind(prob.elig, prob_IE_m)

    }
    colnames(prob.elig)[c(2:(m_imp+1))] <- paste("prob_IE", seq(1, m_imp), sep="_")
    prob.elig$prob_IE <- rowMeans(prob.elig[c(2:(m_imp+1))])
  }
  # Generate Weights #
  prob.elig$w_elig <- with(prob.elig, ifelse(elig=="1", 1,
                                             ifelse(elig=="NA", prob_IE, 0)))
  # Calculate trimming thresholds #
  trim_res <- trim_calculator(Y=Y, prob.elig = prob.elig, res.CV=res.CV, V=V)
  outcome.thres <- ifelse(is.null(trim_res$outcome.thres), NA, trim_res$outcome.thres)
  elig.dat <- data.frame(prob.elig, youden.thres=trim_res$youden.thres,
                         outcome.thres=outcome.thres)
  res <- list(Y=Y, X=X, mod=mod, elig.dat=elig.dat, res.CV=res.CV,
              lambda.dat = trim_res$lambda.dat, mu_test=mu_test)
  class(res) <- "elig_model"
  return(res)
}

