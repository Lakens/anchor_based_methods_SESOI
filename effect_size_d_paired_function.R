effect_size_d_paired <- function (x, y, conf.level = 0.95){ 
  
  #First load function from MBESS package so this function works without installing the entire MBESS package (which is often difficult and takes long)
  
  conf.limits.nct <- function (ncp, df, conf.level = 0.95, alpha.lower = NULL, alpha.upper = NULL, 
                               t.value, tol = 1e-09, sup.int.warns = TRUE, ...) 
  {
    if (missing(ncp)) {
      if (missing(t.value)) 
        stop("You need to specify either 'ncp' or its alias, 't.value,' you have not specified either")
      ncp <- t.value
    }
    if (df <= 0) 
      stop("The degrees of freedom must be some positive value.", 
           call. = FALSE)
    if (abs(ncp) > 37.62) 
      print("The observed noncentrality parameter of the noncentral t-distribution has exceeded 37.62 in magnitude (R's limitation for accurate probabilities from the noncentral t-distribution) in the function's iterative search for the appropriate value(s). The results may be fine, but they might be inaccurate; use caution.")
    if (sup.int.warns == TRUE) 
      Orig.warn <- options()$warn
    options(warn = -1)
    if (!is.null(conf.level) & is.null(alpha.lower) & !is.null(alpha.upper)) 
      stop("You must choose either to use 'conf.level' or define the 'lower.alpha' and 'upper.alpha' values; here, 'upper.alpha' is specified but 'lower.alpha' is not", 
           call. = FALSE)
    if (!is.null(conf.level) & !is.null(alpha.lower) & is.null(alpha.upper)) 
      stop("You must choose either to use 'conf.level' or define the 'lower.alpha' and 'upper.alpha' values; here, 'lower.alpha' is specified but 'upper.alpha' is not", 
           call. = FALSE)
    if (!is.null(conf.level) & is.null(alpha.lower) & is.null(alpha.upper)) {
      alpha.lower <- (1 - conf.level)/2
      alpha.upper <- (1 - conf.level)/2
    }
    .conf.limits.nct.M1 <- function(ncp, df, conf.level = NULL, 
                                    alpha.lower, alpha.upper, tol = 1e-09, sup.int.warns = TRUE, 
                                    ...) {
      if (sup.int.warns == TRUE) 
        Orig.warn <- options()$warn
      options(warn = -1)
      min.ncp = min(-150, -5 * ncp)
      max.ncp = max(150, 5 * ncp)
      .ci.nct.lower <- function(val.of.interest, ...) {
        (qt(p = alpha.lower, df = df, ncp = val.of.interest, 
            lower.tail = FALSE, log.p = FALSE) - ncp)^2
      }
      .ci.nct.upper <- function(val.of.interest, ...) {
        (qt(p = alpha.upper, df = df, ncp = val.of.interest, 
            lower.tail = TRUE, log.p = FALSE) - ncp)^2
      }
      if (alpha.lower != 0) {
        if (sup.int.warns == TRUE) 
          Low.Lim <- suppressWarnings(optimize(f = .ci.nct.lower, 
                                               interval = c(min.ncp, max.ncp), alpha.lower = alpha.lower, 
                                               df = df, ncp = ncp, maximize = FALSE, tol = tol))
        if (sup.int.warns == FALSE) 
          Low.Lim <- optimize(f = .ci.nct.lower, interval = c(min.ncp, 
                                                              max.ncp), alpha.lower = alpha.lower, df = df, 
                              ncp = ncp, maximize = FALSE, tol = tol)
      }
      if (alpha.upper != 0) {
        if (sup.int.warns == TRUE) 
          Up.Lim <- suppressWarnings(optimize(f = .ci.nct.upper, 
                                              interval = c(min.ncp, max.ncp), alpha.upper = alpha.upper, 
                                              df = df, ncp = ncp, maximize = FALSE, tol = tol))
        if (sup.int.warns == FALSE) 
          Up.Lim <- optimize(f = .ci.nct.upper, interval = c(min.ncp, 
                                                             max.ncp), alpha.upper = alpha.upper, df = df, 
                             ncp = ncp, maximize = FALSE, tol = tol)
      }
      if (alpha.lower == 0) 
        Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, 
                       Upper.Limit = Up.Lim$minimum, Prob.Greater.Upper = pt(q = ncp, 
                                                                             ncp = Up.Lim$minimum, df = df))
      if (alpha.upper == 0) 
        Result <- list(Lower.Limit = Low.Lim$minimum, Prob.Less.Lower = pt(q = ncp, 
                                                                           ncp = Low.Lim$minimum, df = df, lower.tail = FALSE), 
                       Upper.Limit = Inf, Prob.Greater.Upper = 0)
      if (alpha.lower != 0 & alpha.upper != 0) 
        Result <- list(Lower.Limit = Low.Lim$minimum, Prob.Less.Lower = pt(q = ncp, 
                                                                           ncp = Low.Lim$minimum, df = df, lower.tail = FALSE), 
                       Upper.Limit = Up.Lim$minimum, Prob.Greater.Upper = pt(q = ncp, 
                                                                             ncp = Up.Lim$minimum, df = df))
      if (sup.int.warns == TRUE) 
        options(warn = Orig.warn)
      return(Result)
    }
    .conf.limits.nct.M2 <- function(ncp, df, conf.level = NULL, 
                                    alpha.lower, alpha.upper, tol = 1e-09, sup.int.warns = TRUE, 
                                    ...) {
      .ci.nct.lower <- function(val.of.interest, ...) {
        (qt(p = alpha.lower, df = df, ncp = val.of.interest, 
            lower.tail = FALSE, log.p = FALSE) - ncp)^2
      }
      .ci.nct.upper <- function(val.of.interest, ...) {
        (qt(p = alpha.upper, df = df, ncp = val.of.interest, 
            lower.tail = TRUE, log.p = FALSE) - ncp)^2
      }
      if (sup.int.warns == TRUE) {
        Low.Lim <- suppressWarnings(nlm(f = .ci.nct.lower, 
                                        p = ncp, ...))
        Up.Lim <- suppressWarnings(nlm(f = .ci.nct.upper, 
                                       p = ncp, ...))
      }
      if (sup.int.warns == FALSE) {
        Low.Lim <- nlm(f = .ci.nct.lower, p = ncp, ...)
        Up.Lim <- nlm(f = .ci.nct.upper, p = ncp, ...)
      }
      if (alpha.lower == 0) 
        Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, 
                       Upper.Limit = Up.Lim$estimate, Prob.Greater.Upper = pt(q = ncp, 
                                                                              ncp = Up.Lim$estimate, df = df))
      if (alpha.upper == 0) 
        Result <- list(Lower.Limit = Low.Lim$estimate, Prob.Less.Lower = pt(q = ncp, 
                                                                            ncp = Low.Lim$estimate, df = df, lower.tail = FALSE), 
                       Upper.Limit = Inf, Prob.Greater.Upper = 0)
      if (alpha.lower != 0 & alpha.upper != 0) 
        Result <- list(Lower.Limit = Low.Lim$estimate, Prob.Less.Lower = pt(q = ncp, 
                                                                            ncp = Low.Lim$estimate, df = df, lower.tail = FALSE), 
                       Upper.Limit = Up.Lim$estimate, Prob.Greater.Upper = pt(q = ncp, 
                                                                              ncp = Up.Lim$estimate, df = df))
      return(Result)
    }
    Res.M1 <- Res.M2 <- NULL
    try(Res.M1 <- .conf.limits.nct.M1(ncp = ncp, df = df, conf.level = NULL, 
                                      alpha.lower = alpha.lower, alpha.upper = alpha.upper, 
                                      tol = tol, sup.int.warns = sup.int.warns), silent = TRUE)
    if (length(Res.M1) != 4) 
      Res.M1 <- NULL
    try(Res.M2 <- .conf.limits.nct.M2(ncp = ncp, df = df, conf.level = NULL, 
                                      alpha.lower = alpha.lower, alpha.upper = alpha.upper, 
                                      tol = tol, sup.int.warns = sup.int.warns), silent = TRUE)
    if (length(Res.M2) != 4) 
      Res.M2 <- NULL
    Low.M1 <- Res.M1$Lower.Limit
    Prob.Low.M1 <- Res.M1$Prob.Less.Lower
    Upper.M1 <- Res.M1$Upper.Limit
    Prob.Upper.M1 <- Res.M1$Prob.Greater.Upper
    Low.M2 <- Res.M2$Lower.Limit
    Prob.Low.M2 <- Res.M2$Prob.Less.Lower
    Upper.M2 <- Res.M2$Upper.Limit
    Prob.Upper.M2 <- Res.M2$Prob.Greater.Upper
    Min.for.Best.Low <- min((c(Prob.Low.M1, Prob.Low.M2) - alpha.lower)^2)
    if (!is.null(Res.M1)) {
      if (Min.for.Best.Low == (Prob.Low.M1 - alpha.lower)^2) 
        Best.Low <- 1
    }
    if (!is.null(Res.M2)) {
      if (Min.for.Best.Low == (Prob.Low.M2 - alpha.lower)^2) 
        Best.Low <- 2
    }
    Min.for.Best.Up <- min((c(Prob.Upper.M1, Prob.Upper.M2) - 
                              alpha.upper)^2)
    if (!is.null(Res.M1)) {
      if (Min.for.Best.Up == (Prob.Upper.M1 - alpha.upper)^2) 
        Best.Up <- 1
    }
    if (!is.null(Res.M2)) {
      if (Min.for.Best.Up == (Prob.Upper.M2 - alpha.upper)^2) 
        Best.Up <- 2
    }
    if (is.null(Res.M1)) {
      Low.M1 <- NA
      Prob.Low.M1 <- NA
      Upper.M1 <- NA
      Prob.Upper.M1 <- NA
    }
    if (is.null(Res.M2)) {
      Low.M2 <- NA
      Prob.Low.M2 <- NA
      Upper.M2 <- NA
      Prob.Upper.M2 <- NA
    }
    Result <- list(Lower.Limit = c(Low.M1, Low.M2)[Best.Low], 
                   Prob.Less.Lower = c(Prob.Low.M1, Prob.Low.M2)[Best.Low], 
                   Upper.Limit = c(Upper.M1, Upper.M2)[Best.Up], Prob.Greater.Upper = c(Prob.Upper.M1, 
                                                                                        Prob.Upper.M2)[Best.Up])
    return(Result)
  }
  
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(x-y) #standard deviation of the difference scores
  N <- length(x) #number of pairs
  s_av <- sqrt((sd1^2+sd2^2)/2) #averaged standard deviation of both measurements
  
  
  #Cohen's d_av, using s_av as standardizer
  m_diff <- mean(y-x)
  d_av <- m_diff/s_av
  d_av
  d_av_unb <- (1-(3/(4*(N-1)-1)))*d_av
  d_av_unb
  
  #get the t-value for the CI
  t_value <- m_diff/(s_diff/sqrt(N))
  
  nct_limits <- conf.limits.nct(t.value = t_value, df = N-1, conf.level = 0.95)
  ci_l_d_av <- nct_limits$Lower.Limit*s_diff/(s_av*sqrt(N))
  ci_u_d_av <- nct_limits$Upper.Limit*s_diff/(s_av*sqrt(N))
  ci_l_d_av
  ci_u_d_av

  #Cohen's d_z, using s_diff as standardizer
  d_z <- t_value/sqrt(N)
  d_z
  d_z_unb <- (1-(3/(4*(N-1)-1)))*d_z
  ci_l_d_z <- nct_limits$Lower.Limit/sqrt(N) #earlier version had /sqrt(df) - corrected by Dr. Erin Buchanan and Prof. Thom Baguley
  ci_u_d_z <- nct_limits$Upper.Limit/sqrt(N)
  ci_l_d_z
  ci_u_d_z

  #perform t-test for 95% CI around mean difference
  test_res <- t.test(y, x, paired = TRUE, conf.level = conf.level)
  
  cat("Mean Difference    : ", paste0(round(m_diff, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(test_res$conf.int[1], digits = 4)),";",paste0(round(test_res$conf.int[2], digits = 4)), "]", sep = "")
  cat("\n")
  cat("Cohen's d_z_unb    : ", paste0(round(d_z_unb, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(ci_l_d_z, digits = 4)),";",paste0(round(ci_u_d_z, digits = 4)), "]", sep = "")
  cat("\n")
  cat("Cohen's d_av_unb   : ", paste0(round(d_av_unb, digits = 4))," ", 100*conf.level, "% CI [", 
      paste0(round(ci_l_d_av, digits = 4)),";",paste0(round(ci_u_d_av, digits = 4)), "]",sep = "")
  cat("\n")
  cat("s_diff: ", paste0(round(s_diff, digits = 4)),
      ", s_av: ", paste0(round(s_av, digits = 4)),
      ", sd1: ", paste0(round(sd1, digits = 4)),
      ", sd2: ", paste0(round(sd2, digits = 4)),
      ", cor: ", paste0(round(cor(x,y), digits = 4)),
      sep = "")
  cat("\n")
  cat("N = ", paste0(N)," pairs.",
      sep = "")
  cat("\n")
  invisible(list(m_diff = m_diff, ci_l_m_diff = test_res$conf.int[1], ci_u_m_diff = test_res$conf.int[2], d_av = d_av, d_av_unb = d_av_unb, s_av = s_av, s_diff = s_diff, ci_l_d_av = ci_l_d_av, ci_u_d_av = ci_u_d_av, d_z = d_z, d_z_unb = d_z_unb, ci_l_d_z = ci_l_d_z, ci_u_d_z = ci_u_d_z, N = N, m1 = mean(x), m2 = mean(y), sd1 = sd(x), sd2 = sd(y), cor = cor(x,y) ))
}
