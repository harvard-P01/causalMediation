causalMediation <-
function(data=df, 
                            outcome='death', 
                            treatment='smoking', 
                            mediator='lbw', 
                            covariates=c('drinking', 'agebelow20'), 
                            interaction=FALSE,
                            nbootstraps=0,
                            debug=FALSE){
  
  ### Perform regressions (http://www.statmethods.net/stats/regression.html)
  #   Build formulas for mediator and outcome regression
  mediator.basic <- paste(mediator, treatment, sep=' ~ ')
  outcome.basic  <- paste(paste(outcome, treatment, sep=' ~ '), mediator, sep=' + ')
  if(interaction==TRUE){
    outcome.basic <- paste(outcome.basic, paste(treatment, mediator, sep='*'), sep=' + ')
  }
  
  if(length(covariates)==0){
    mediator.formula <- mediator.basic
    outcome.formula  <- outcome.basic
  }else{
    mediator.formula <- paste(mediator.basic, paste(covariates, collapse=" + "), sep = ' + ')
    outcome.formula  <- paste(outcome.basic,  paste(covariates, collapse=" + "), sep = ' + ')
  }
  
  if(debug){
    print(paste("MEDIATION FORMULA", mediator.formula, sep=' : '))
    print(paste("  OUTCOME FORMULA", outcome.formula,  sep=' : '))
  }
  
  ### FIXME: hardcode to validate with SAS macro
  mediator.binary = all(unique(data[,mediator]) %in% 0:1)
  outcome.binary  = all(unique(data[,outcome])  %in% 0:1)
  
  if(debug){
    print(paste("MEDIATOR BINARY = ", mediator.binary, sep=""))
    print(paste(" OUTCOME BINARY = ", outcome.binary, sep=""))
  }
  
  ### Output vectors
  cdes      <- vector()
  ndes      <- vector()
  nies      <- vector()
  tdes      <- vector()
  betasVec  <- vector()
  thetasVec <- vector()
  
  for(i in seq(1:nbootstraps+1)){
    if(i%%100==0){
      print(paste("Running bootstrap sample", i, "out of", nbootstraps, sep=" "))
    }
      
    if(i==1){ 
      data  <- df
      debug <- TRUE
    }else{
      data  <- df[sample(nrow(df),replace=TRUE),]
      debug <- FALSE
    }
    
    ### Perform regression
    if(!mediator.binary){
      mediator.regression <<- lm(mediator.formula, data=data)
    }else{
      mediator.regression <<- glm(mediator.formula, family=binomial(), data=data)
    }
  
    if(!outcome.binary){
      outcome.regression  <<- lm(outcome.formula, data=data)
    }else{
      outcome.regression  <<- glm(outcome.formula, family=binomial(), data=data)
    }
  
    ### Store coefficients from regression
    betas  <- coefficients(mediator.regression)
    thetas <- coefficients(outcome.regression)
    
    if(debug){
      print("BETAS:")
      print(coefficients(mediator.regression))
      print("THETAS:")
      print(coefficients(outcome.regression))
    }
    
    ### Calculate cde, nde, nie and tde from regression coefficients
    if(mediator.binary | outcome.binary){
      cde <- CDE_bin(thetas, treatment, mediator)
    }
    
    variance <- (summary(mediator.regression)$sigma)^2
    
    if(mediator.binary & outcome.binary){
      cde <- CDE_bin(thetas, treatment, mediator)
      nde <- NDE_binbin(betas, thetas, treatment, mediator, covariates)
      nie <- NIE_binbin(betas, thetas, treatment, mediator, covariates)
  	  tde <- nde*nie
    }else if(mediator.binary & !outcome.binary){
      cde <- CDE_cont(thetas, treatment, mediator)
  	  nde <- NDE_bincont(betas, thetas, treatment, mediator, covariates)
      nie <- NIE_bincont(betas, thetas, treatment, mediator, covariates)
  	  tde <- nde+nie
    }else if(!mediator.binary & outcome.binary){
      cde <- CDE_bin(thetas, treatment, mediator)
  	  nde <- NDE_contbin(betas, thetas, treatment, mediator, covariates, variance)
      nie <- NIE_contbin(betas, thetas, treatment, mediator, covariates)
  	  tde <- nde*nie
    }else if(!mediator.binary & !outcome.binary){
      cde <- CDE_cont(thetas, treatment, mediator)
  	  nde <- NDE_contcont(betas, thetas, treatment, mediator, covariates)
      nie <- NIE_contcont(betas, thetas, treatment, mediator, covariates)
  	  tde <- nde+nie
    }
    cdes      <- c(cdes, cde)
    ndes      <- c(ndes, nde)
    nies      <- c(nies, nie)
    tdes      <- c(tdes, tde)
    betasVec  <- rbind(betasVec,  betas )
    thetasVec <- rbind(thetasVec, thetas)
  }
  
  ### Calculate mean and standard errors
  cde.mean <- cdes[1]
  cdes     <- sort(cdes[2:length(cdes)])
  cde.quant<- quantile(cdes, c(0.025, 0.975))
  cde.se   <- cde.quant[2]-cde.quant[1] 
  
  nde.mean <- ndes[1]
  ndes     <- sort(ndes[2:length(ndes)])
  nde.quant<- quantile(ndes, c(0.025, 0.975))
  nde.se   <- nde.quant[2]-nde.quant[1] 
  
  nie.mean <- nies[1]
  nies     <- sort(nies[2:length(nies)])
  nie.quant<- quantile(nies, c(0.025, 0.975))
  nie.se   <- nie.quant[2]-nie.quant[1] 
  
  tde.mean <- tdes[1]
  tdes     <- sort(tdes[2:length(tdes)])
  tde.quant<- quantile(tdes, c(0.025, 0.975))
  tde.se   <- tde.quant[2]-tde.quant[1] 
  
  t <- matrix(data=c(cde.mean, nde.mean, nie.mean, tde.mean,
                     cde.se,   nde.se,   nie.se,   tde.se, 
                     cde.mean-2*cde.se, nde.mean-2*nde.se, nie.mean-2*nie.se, tde.mean-2*tde.se,
                     cde.mean+2*cde.se, nde.mean+2*nde.se, nie.mean+2*nie.se, tde.mean+2*tde.se),
              ncol=4)
  rownames(t) <- c("CDE", "NDE", "NIE","TE")
  colnames(t) <- c("estimate","SE","CI 95% lower", "CI 95% upper")
  
  dt <- as.data.frame.matrix(t)
  
  #print(dt)
  dt #c(dt, cdes, ndes, nies, tdes)
}
