# Function to use 4 acquisition functions to find the best next design for an adaptive DoE for Sithara on week of 16 Nov 2020
# THIS IS LONGER optim_sithara <- function(acq,gp_model,prior_data,current_response,lower_x,upper_x,maxgens= 1000, solntol = 10^-21, n.grid=200){
# THIS IS FINE FOR MAKING THE APP!  
optim_sithara <- function(acq,gp_model,prior_data,current_response,lower_x,upper_x,maxgens= 1000, solntol = 10^-21, n.grid=200){
    
  # Random choice
  if (acq == "Random"){
    # print("finding random design")
    res <- numeric()
    for (i in 1:3){
      res[i]<- runif(1,min = lower_x[i], max = upper_x[i])
    }
    # print("printing random result")
    # print(res)
  } else if (acq == "Grid") {
    # Min predicted efficiency on a grid based on the current best GP model
    x1.grid <- seq(lower_x[1],upper_x[1],length = n.grid)
    x2.grid <- seq(lower_x[2],upper_x[2],length = n.grid)
    x3.grid <- seq(lower_x[3],upper_x[3],length = n.grid)
    design.grid = x1.grid# 1d case
    design.grid <- expand.grid(x1.grid, x2.grid)# 2d case
    design.grid <- expand.grid(x1.grid, x2.grid,x3.grid)
    names(design.grid) = names(prior_data)
    nt <- nrow(design.grid)
    print(paste("Calculating Gaussian process predictions for", nt, "designs"))
    pred.km <- predict(gp_model, newdata = design.grid, type = "UK", checkNames = FALSE)
    res = design.grid[which(pred.km$mean == min(pred.km$mean)),]
    # may need to pick if res is big. 
    # print(paste("Sampling from", dim(res)[1], "possible choices"))
    pick = sample(dim(res)[1],1)
    # print("printing grid result")
    # print(res)
    res = t(res[pick,])
  } else if(acq == "AKG"){  # Approximate Knowledge Gradient (AKG)
    # print("finding akg design")
    
      n.ite <- 1 # budget - number of iterations in optimiser    
      res <- singlerun.noisy.optimizer(optim.crit = acq, 
                                       optim.param = list(quantile = 0.9), # quantile level for eqi
                                       model = gp_model, 
                                       n.ite = 1, 
                                       noise.var = gp_model@noise.var[1], 
                                       funnoise = funnoise,
                                       lower = lower_x, 
                                       upper = upper_x,
                                       CovReEstimate = TRUE, maxgens = maxgens, solntol = solntol)
      # print("printing akg result")
      # print(res)
    }  else if(acq == "EQI"){ # Expected Quantile Improvement 
      # print("finding eqi design")
      
      res <- singlerun.noisy.optimizer(optim.crit = acq, 
                                         optim.param = list(quantile = 0.9),
                                         model = gp_model, 
                                         n.ite = 1, 
                                         noise.var = gp_model@noise.var[1], 
                                         funnoise = funnoise,
                                         lower = lower_x, 
                                         upper = upper_x,
                                         CovReEstimate = TRUE,maxgens = maxgens, solntol = solntol)
      # print("printing EQI result")
      # print(res)
    }

  
  # Print and plot results
  names(res) = names(prior_data)
  print(paste("Based on", acq, "acquisition function, please set up your next experiment at:"))
  print(round(t(res),1))
  # Plot the new suggested design as red lines on top of our current dataset
  #  par(mfrow = c(2,2))
   # plot(prior_data$rep_rate_kHz,prior_response,xlim = c(lower_x[1],upper_x[1]),ylim = c(min(prior_response),-0.1),main=acq)
    #abline(v = res[1],col = "red")
    #plot(prior_data$time_m,prior_response,xlim = c(lower_x[2],upper_x[2]),ylim = c(min(prior_response),-0.1),main=acq)
    #abline(v = res[2],col = "red")
    #plot(prior_data$scan_speed_mmpers,prior_response,xlim = c(lower_x[3],upper_x[3]),ylim = c(min(prior_response),-0.1),main=acq)
    #abline(v = res[3],col = "red")
  return(res)
}
 