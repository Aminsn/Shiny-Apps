# NOTE 17 Nov 2020
# This is an adaptation of the noisy.optimizer from the DiceOptim package.
# It performs one iteration of the optimizer and returns the optimal design
# I am currently using only AKG and EQI from this function for a noisy advanced manufacturing experiment.

singlerun.noisy.optimizer = function (optim.crit, optim.param = NULL, model, n.ite, noise.var = NULL, 
         funnoise=1, lower, upper, parinit = NULL, control = NULL, CovReEstimate = TRUE, 
         NoiseReEstimate = FALSE, nugget.LB = 1e-05, estim.model = NULL, 
         type = "UK",maxgens=20,solntol=0.00001) {
  # TR ADDED maxgens and solntol for the optimisation routine as it wasn't always converging too easily! Gives it more time.
  message("Starting noisy optimization with the following criterion and parameters \n")
  message(optim.crit, "\n")
  if (!is.null(optim.param)) 
    message(optim.param, quote = FALSE)
  message("----------------- \n")
  optim.result <- list()
  if (NoiseReEstimate) {
    if (!CovReEstimate) {
      warning("Noise variance cannot be re-estimated without re-estimating the covariance parameters \n")
      warning("covReEstimate switched to TRUE \n")
      CovReEstimate = TRUE
    }
    obs.n.rep <- pmax(1, round(noise.var/as.numeric(model@noise.var)))
    if (!is.null(estim.model)) {
      noise.var <- estim.model@covariance@nugget
    }
    else {
      if (max(obs.n.rep) == 1) {
        estim.model <- km(formula = model@trend.formula, 
                          covtype = model@covariance@name, design = model@X, 
                          response = model@y, lower = model@lower, upper = model@upper, 
                          coef.cov = covparam2vect(model@covariance), 
                          coef.var = model@covariance@sd2, optim.method = model@optim.method, 
                          nugget = noise.var, control = model@control)
        if (type == "SK") {
          estim.model@trend.coef = model@trend.coef
        }
        estim.model@covariance@nugget.estim = TRUE
      }
      else {
        warning("Unless estim.model is provided, noisy.EGO cannot estimate the noise variance when the initial\n            model has heterogeneous noise variance. NoiseReEstimate switched to FALSE. \n")
        NoiseReEstimate <- FALSE
      }
    }
  }
  for (i.time.steps in 1:n.ite) {
    add.obs <- TRUE
    if (NoiseReEstimate) {
      obs.n.rep <- pmax(1, round(noise.var/as.numeric(model@noise.var)))
    }
    if (optim.crit == "reinterpolation") {
      pred <- predict(object = model, newdata = model@X, 
                      type = type, checkNames = FALSE)
      mk <- pred$mean
      optim.param$ymin <- min(mk)
      if (exists("optim.model")) 
        rm(optim.model)
      try(optim.model <- km(formula = model@trend.formula, 
                            design = model@X, response = mk, covtype = model@covariance@name, 
                            coef.trend = model@trend.coef, coef.cov = covparam2vect(model@covariance), 
                            coef.var = model@covariance@sd2, control = model@control))
      if (!exists("optim.model")) {
        message("Error occured during model update - small nugget added to the reinterpolating model")
        optim.model <- km(formula = model@trend.formula, 
                          design = model@X, response = mk, covtype = model@covariance@name, 
                          coef.trend = model@trend.coef, coef.cov = covparam2vect(model@covariance), 
                          coef.var = model@covariance@sd2, nugget = 1e-08, 
                          control = model@control)
      }
      oEGO <- max_EI(model = optim.model, plugin = min(optim.model@y), 
                     upper = upper, type = type, lower = lower, parinit = parinit, 
                     control = control)
      x.new <- oEGO$par
      i.best <- 0
    }
    else if (optim.crit == "EI.plugin") {
      if (is.null(optim.param$plugin.type)) {
        warning("Plugin type not provided: default value ytilde is used \n")
        optim.param$plugin.type <- "ytilde"
      }
      if (optim.param$plugin.type == "quantile") {
        pred <- predict(object = model, newdata = model@X, 
                        type = type, checkNames = FALSE)
        mk <- pred$mean
        sk <- pred$sd
        if (is.null(optim.param$quantile)) {
          warning("Quantile level not provided: default value 0.5 is used \n")
          optim.param$quantile <- 0.5
        }
        qk <- mk + qnorm(optim.param$quantile) * sk
        plugin <- min(qk)
      }
      else if (optim.param$plugin.type == "ytilde") {
        plugin <- min(model@y)
      }
      else if (optim.param$plugin.type == "fixed") {
        plugin <- optim.param$plugin
      }
      else {
        warning("Unknown plugin type: default value ytilde is used \n")
        optim.param$plugin.type = "ytilde"
        plugin <- min(model@y)
      }
      oEGO <- max_EI(model = model, plugin = plugin, type = type, 
                     lower = lower, upper = upper, parinit = parinit, 
                     control = control)
      x.new <- oEGO$par
      EI <- oEGO$val
      EI.doe <- rep(0, 1, model@n)
      for (i in 1:model@n) {
        EI.doe[i] <- EI(x = model@X[i, ], model = model, 
                        type = type, plugin = plugin)
      }
      i.best <- which.max(EI.doe)
      if (EI.doe[i.best] > EI) {
        x.new <- t(as.numeric(model@X[i.best, ]))
        add.obs <- FALSE
      }
    }
    else if (optim.crit == "EQI") {
      if (is.null(optim.param)) {
        beta <- 0.9
      }
      else {
        beta <- optim.param$quantile
      }
      new.noise.var <- noise.var/(n.ite + 1 - i.time.steps)
      pred <- predict(object = model, newdata = model@X, 
                      type = type, checkNames = FALSE)
      qk <- pred$mean + qnorm(beta) * pred$sd
      q.min <- min(qk)
      oEGO <- max_EQI(model = model, new.noise.var = new.noise.var, 
                      type = type, beta = beta, q.min = q.min, lower = lower, 
                      upper = upper, parinit = parinit, #control = control)
                      control = list( max.generations = maxgens, # TRI ADDED 11 NOV
                                      wait.generations = 5, BFGSburnin = 20,solution.tolerance=solntol)) 
      x.new <- oEGO$par
      EQI.global.search <- oEGO$val
      EQI.doe <- rep(0, 1, model@n)
      for (i in 1:model@n) {
        EQI.doe[i] <- EQI(x = model@X[i, ], model = model, 
                          new.noise.var = new.noise.var, q.min = q.min, 
                          beta = beta, type = type)
      }
      i.best <- which.max(EQI.doe)
      if (EQI.doe[i.best] > EQI.global.search) {
        x.new <- t(as.numeric(model@X[i.best, ]))
        add.obs <- FALSE
      }
    }
    else if (optim.crit == "min.quantile") {
      if (is.null(optim.param)) {
        beta <- 0.1
      }
      else {
        beta <- optim.param$quantile
      }
      oEGO <- min_quantile(model = model, beta = beta, 
                           type = type, lower = lower, upper = upper, parinit = parinit, 
                           control = control)
      x.new <- oEGO$par
      q <- oEGO$val
      pred <- predict(object = model, newdata = model@X, 
                      type = type, checkNames = FALSE)
      mk <- pred$mean
      sk <- pred$sd
      q.doe <- mk + qnorm(beta) * sk
      i.best <- which.min(q.doe)
      if (q.doe[i.best] < q) {
        x.new <- t(as.numeric(model@X[i.best, ]))
        add.obs <- FALSE
      }
    }
    else if (optim.crit == "AEI") {
      if (is.null(optim.param)) {
        beta <- 0.75
      }
      else {
        beta <- optim.param$quantile
      }
      pred <- predict(object = model, newdata = model@X, 
                      type = type, checkNames = FALSE)
      mk <- pred$mean
      sk <- pred$sd
      qk <- mk + qnorm(beta) * sk
      y.min <- mk[which.min(qk)]
      oEGO <- max_AEI(model = model, y.min = y.min, new.noise.var = noise.var, 
                      lower = lower, upper = upper, parinit = parinit, 
                      control = control, type = type)
      x.new <- oEGO$par
      AEI <- oEGO$val
      AEI.doe <- rep(0, 1, model@n)
      for (i in 1:model@n) {
        AEI.doe[i] <- AEI(x = model@X[i, ], model = model, 
                          y.min = y.min, type = type, new.noise.var = noise.var)
      }
      i.best <- which.max(AEI.doe)
      if (AEI.doe[i.best] > AEI) {
        x.new <- t(as.numeric(model@X[i.best, ]))
        add.obs <- FALSE
      }
    }
    else if (optim.crit == "AKG") {
      oEGO <- max_AKG(model = model, new.noise.var = noise.var, 
                      type = type, lower = lower, upper = upper, parinit = parinit, 
                      #control = control)
                      control = list( max.generations = maxgens, # TRI ADDED 11 NOV
                                      wait.generations = 5, BFGSburnin = 20,solution.tolerance = solntol))
      x.new <- oEGO$par
      AKG <- oEGO$val
      AKG.doe <- rep(0, 1, model@n)
      for (i in 1:model@n) {
        AKG.doe[i] <- AKG(x = model@X[i, ], model = model, 
                          type = type, new.noise.var = noise.var)
      }
      i.best <- which.max(AKG.doe)
      if (AKG.doe[i.best] > AKG) {
        x.new <- t(as.numeric(model@X[i.best, ]))
        add.obs <- FALSE
      }
    }
  }
  ## TR Other than the max and wait generations of the optimiser, this is all I have changed here in this function.
  ## For one iteration of the function, to return one optimal design, rather than genernation more data inside this function! 
    # #y.new <- funnoise(x.new)
    # if (add.obs) {
    #   message(c("Creating obs:", as.numeric(x.new), "\n"))
    # }
    # else {
    #   message(c("Repeating obs:", as.numeric(x.new), "\n"))
    # }
    # upmod <- update_km_noisyEGO(model = model, x.new = x.new, 
    #                             y.new = y.new, noise.var = noise.var, type = type, 
    #                             add.obs = add.obs, index.in.DOE = i.best, CovReEstimate = CovReEstimate, 
    #                             NoiseReEstimate = NoiseReEstimate, estim.model = estim.model, 
    #                             nugget.LB = nugget.LB)
    # model <- upmod$model
    # if (NoiseReEstimate) {
      # estim.model <- upmod$estim.model
      # noise.var <- upmod$noise.var
    # }
    # mu <- model@trend.coef
    # sd2 <- model@covariance@sd2
    # range <- model@covariance@range.val
    # theta <- c(mu, sd2, range)
   #   if (i.time.steps == 1) {
  #     all.X <- t(x.new)
  #     all.y <- t(y.new)
  #     all.thetas <- theta
  #     all.noise.var <- noise.var
  #   }
  #   else {
  #     all.X <- cbind(all.X, t(x.new))
  #     all.y <- c(all.y, t(y.new))
  #     all.thetas <- cbind(all.thetas, theta)
  #     all.noise.var <- c(all.noise.var, t(noise.var))
  #   }
  # }
  # if (!exists("optim.param$quantile")) {
  #   beta <- 0.5
  # }
  # else {
  #   beta <- optim.param$quantile
  # }
  ### TR THIS IS ONLY FOR EI PLUGINS
  # pred <- predict(object = model, newdata = model@X, type = type, 
  #                 checkNames = FALSE)
  # mk <- pred$mean
  # sk <- pred$sd
  # qk <- mk + qnorm(beta) * sk
  # i.best <- which.min(qk)
  # x.best <- model@X[i.best, ]
  # #y.best <- model@y[i.best]
  # if (optim.crit == "EI.plugin") {
  #   if (optim.param$plugin.type == "ytilde") {
  #     i.best <- which.min(model@y)
  #     x.best <- model@X[i.best, ]
  #     y.best <- model@y[i.best]
  #   }
  # }
  # optim.result$model <- model
  # optim.result$best.x <- x.best
  # optim.result$best.y <- y.best
  # optim.result$best.index <- i.best
  # optim.result$history.hyperparam <- all.thetas
  # optim.result$history.x <- all.X
  # optim.result$history.y <- all.y
  # if (NoiseReEstimate) {
  #   optim.result$estim.model <- estim.model
  #   optim.result$history.noise.var <- all.noise.var
  # }
  return(t(x.new))
}
