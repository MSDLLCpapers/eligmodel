#' plot_elig: Plot diagnostic function for eligibility modeling.
#'
#' This function currently plots weights by eligibility status, estimates of Y by eligibility status,
#' and covariate balance across the X variables between IE=1 vs Weighted populations.
#'
#' @param fit A fitted \code{rwdcausal} or fitted \code{propensity_model} object.
#' @param type Type of plot. Options include "histo" (default: Histogram of eligibility weights by status), "outcome"
#' (Depending on outcome Y, mean estimates (continuous or binary) or Kaplan-Meier curves (survival) by eligibility status,
#' and "balance" (calculate standardized mean differences for each covariate between eligible and weighted groups).
#'
#' @return Eligibility diagnostic plot (ggplot2 object)
#' @importFrom ggplot2 ggplot ggtitle geom_histogram aes xlab ylab 
#' @export
#'
plot_elig <- function(fit,
                      type="histo") {
  
  # Register the custom font 
  regular_font_path <- system.file("www", "SourceSansPro-Regular.otf", package = "eligmodel")
  bold_font_path <- system.file("www", "SourceSansPro-Bold.otf", package = "eligmodel")
  sysfonts::font_add("Source Sans Pro", regular = regular_font_path, bold = bold_font_path)
  
  # Enable showtext rendering
  showtext::showtext_auto()

  if (!(class(fit) %in% c("elig_model"))) {
    stop("Fit must be fitted elig_model object")
  }
  if (!(type %in% c("histo", "outcome", "balance"))) {
    stop("type must be histo or outcome")
  }
  if (class(fit)=="elig_model") {
    pdat <- data.frame(Y=fit$Y, fit$elig.dat)
  }

  if (type=="histo") {
    out.plot <- ggplot2::ggplot(pdat, ggplot2::aes_string(x="w_elig", fill="elig")) +
      ggplot2::geom_histogram() + ggplot2::xlab("Eligibilty Weights") +
      ggplot2::ggtitle("Histogram: Eligiblity Weights by Eligbility Status") +
      scale_fill_discrete(labels = c("0" = "Ineligible", "NA"= "Unknown", "1" = "Eligible")) + 
      plot_theme()
  }

  if (type=="outcome") {

    if (!requireNamespace("survminer", quietly = TRUE) & survival::is.Surv(fit$Y)) {
      stop("Package survminer is needed for Kaplan-Meier based plots. Please install.")
    }

    # Create Weighted Data #
    pdat_w <- pdat[pdat$w_elig > 0,]
    pdat_w$w_elig <- pdat_w$w_elig / mean(pdat_w$w_elig)
    pdat_w$elig <- "weighted"
    pdat0 <- pdat
    pdat0$w_elig <- 1
    # rbind would remove the surv class. https://stackoverflow.com/questions/62854540/rbind-dataframes-with-survivalsurv-objects
    #pdat_all <- rbind(pdat0, pdat_w)
    pdat_all <- bind_rows(pdat0, pdat_w)
    pdat_all$Y <- pdat_all$Y
    # Survival Plots (KM) #
    if (survival::is.Surv(pdat_all$Y)) {

      # Fit Survival Plots by Eligibility Status #
      km.fit1 <- survival::survfit(Surv(Y[,1], Y[,2]) ~ elig, data = pdat_all, weights = pdat_all$w_elig)
      # Customized survival curves

      out.plot <- survminer::ggsurvplot(km.fit1, data = pdat_all,
                                          # surv.median.line = "hv", # Add medians survival
                                          legend.title = "Elig Status",
                                          tables.theme = survminer::theme_cleantable(),
                                          ggtheme = plot_theme()) # Change ggplot2 theme


    } else {
      # OLS estimates currently (simple code), change for binary Y? #
      ols_fit <- lm(Y ~ -1 + elig, data=pdat_all, weights = pdat_all$w_elig)
      ci_est <- confint(ols_fit)
      est_dat <- data.frame(elig = names(summary(ols_fit)$coefficients[,1]),
                            est = summary(ols_fit)$coefficients[,1],
                            LCL = ci_est[,1],
                            UCL = ci_est[,2])
      rownames(est_dat) <- NULL
      est_dat$elig <- factor(est_dat$elig, levels = c("elig0", "eligNA", "elig1", "eligweighted"))

      out.plot <- ggplot2::ggplot(est_dat, ggplot2::aes_string(x="elig", y="est", ymin="LCL", ymax="UCL", color="elig")) +
        ggplot2::geom_pointrange() +
        ggplot2::xlab("Eligibilty Status") +
        ggplot2::ylab("Mean Estimate [95% CI]") +
        ggplot2::ggtitle("Mean Estimates [95% CI] by Eligbility Status") +
        scale_color_discrete(labels = c("elig0" = "Ineligible", "eligNA"= "Unknown", "elig1" = "Eligible", "eligweighted" = "Augmented")) + 
        plot_theme()
    }
  }
  if (type=="balance") {


    # Create stacked data-set #
    pdat <- data.frame(Y=fit$Y, fit$elig.dat, fit$X)
    pdat_w <- pdat[pdat$w_elig > 0,]
    pdat_w$elig <- "weighted"
    pdat0 <- pdat[pdat$w_elig > 0,]
    pdat_s <- rbind(pdat0, pdat_w)
    X_cols <- colnames(fit$X)
    X_s <- pdat_s[,X_cols]
    pdat_s <- pdat_s[,!(colnames(pdat_s) %in% X_cols)]
    elig <- pdat_s$elig
    weights <- pdat_s$w_elig
    Xvar <- X_s

    # Calculate and plot balance #
    cb_out <- stand_cov_diff(elig=elig, weights=weights, Xvar=Xvar)

    out.plot <- ggplot2::ggplot(data=cb_out, ggplot2::aes_string(x="var", y="d_est",
                                                             color = "type", shape="type")) +
      ggplot2::geom_point(size=3) +
      ggplot2::geom_hline(yintercept=0.10, lty=2) +
      ggplot2::geom_hline(yintercept=-0.10, lty=2) +
      ggplot2::coord_flip() +
      ggplot2::ylab("Mean Differences (standardized)") +
      ggplot2::xlab("") +
      ggplot2::ggtitle("Covariate Balance Plot: Mean Differences (standardized), IE=1 vs Weighted") +
      scale_color_discrete(labels = c("unadjusted" = "Unadjusted", "weighted" = "Weighted")) +
      scale_shape_discrete(labels = c("unadjusted" = "Unadjusted", "weighted" = "Weighted")) +
      plot_theme() +
      ggplot2::theme(plot.title = ggplot2::element_text(size=14, face="bold"))
      # ggplot2::theme_bw() +
      # ggplot2::theme(axis.title.x = ggplot2::element_text(size=14, face="bold"),
      #       axis.text.x = ggplot2::element_text(size=14, face="bold"),
      #       axis.title.y = ggplot2::element_text(size=14, face="bold"),
      #       axis.text.y = ggplot2::element_text(size=12, face="bold"),
      #       plot.title = ggplot2::element_text(size=12, face="bold"),
      #       strip.text = ggplot2::element_text(size = 14, face="bold"),
      #       legend.text= ggplot2::element_text(size=14), legend.position="bottom")

  }

  return(out.plot)
}
