#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import deSolve
#' @noRd
SIS.model <- function(t, x, parameters) {
  S <- x[1]
  I <- x[2]

  with( as.list(parameters), {
    dS <- -beta*S*I + gamma*I
    dI <- beta*S*I - gamma*I
    res <- c(dS, dI)
    list(res)
  }
  )
}

SIR.model <- function(t, x, parameters) {
  S <- x[1]
  I <- x[2]
  R <- x[3]

  with( as.list(parameters), {
    dS <- -beta*S*I
    dI <- beta*S*I - gamma*I
    dR <- gamma*I
    res <- c(dS, dI, dR)
    list(res)
  }
  )
}


SIR_numbers.model <- function(t, x, parameters) {
  S <- x[1]
  I <- x[2]
  R <- x[3]

  N <- S + I + R

  with( as.list(parameters), {
    dS <- -beta*S*I/N
    dI <- beta*S*I/N - gamma*I
    dR <- gamma*I
    res <- c(dS, dI, dR)
    list(res)
  }
  )
}

SEIR.model <- function(t, x, parameters) {
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]

  with( as.list(parameters), {
    dS <- -beta*S*I
    dE <- beta*S*I - sigma*E
    dI <- sigma*E - gamma*I
    dR <- gamma*I
    res <- c(dS, dE, dI, dR)
    list(res)
  }
  )
}

SIRV.model <- function(t, x, parameters) {
  S <- x[1]
  I <- x[2]
  R <- x[3]
  V <- x[4]

  with( as.list(parameters), {
    dS <- -beta*S*I - epsilon*nu*S
    dI <- beta*(S+V)*I - gamma*I
    dR <- gamma*I + delta*V
    dV <- epsilon*nu*S - (beta*I+delta)*V
    res <- c(dS, dI, dR, dV)
    list(res)
  }
  )
}

runODEs <- function(beta,sigma,gamma,nu,delta,epsilon,length,cmodel) {
  # Define parameters
  pars <- list(beta = beta, sigma = sigma, gamma = gamma, nu = nu, delta = delta, epsilon = epsilon)

  # Define the time steps at which we would like to find a solution
  times <- seq(0, length, length/1000)

  if (cmodel==1){
    init <- c(S = 0.995, I = 0.005)
    out <- lsoda(func = SIS.model, y = init, parms = pars, times = times)
  } else if (cmodel==2){
    init <- c(S = 0.995, I = 0.005, R = 0.0)
    out <- lsoda(func = SIR.model, y = init, parms = pars, times = times)
  } else if (cmodel==3){
    init <- c(S = 0.995, E = 0.005, I = 0.0, R = 0.0)
    out <- lsoda(func = SEIR.model, y = init, parms = pars, times = times)
  } else {
    init <- c(S = 0.995, I = 0.005, R = 0.0, V = 0.0)
    out <- lsoda(func = SIRV.model, y = init, parms = pars, times = times)
  }

  # Turn the solution into a data.frame (so it's easier to access)
  out <- as.data.frame(out)

  return(out)
}

runEuler <- function(deltat){
  # Define the initial conditions
  x <- c(0)

  # Define time steps
  times <- seq(from=0,to=6+deltat,by=deltat)
  steps <- length(times)

  # Perform Euler's method
  for (k in 2:steps){
    # Calculate 'next estimate'
    x_next = x[k-1] + 2*exp(2*times[k-1])*deltat

    # Store next estimate in x vector
    x <- c(x,x_next)
  }

  return(x)

}


ncoupling.model <- function(t, x, parameters){

  n <- parameters$n

  S <- x[1:n] # Susceptibles in all patches
  I <- x[(n+1):(2*n)] # Same as above but for the infectios compartments
  R <- x[(2*n+1):(3*n)] # Same as above but for the removed compartments

  # Calculate the population size in patch
  N <- S + I + R

  dS <- vector(length=n)
  dI <- vector(length=n)
  dR <- vector(length=n)

  with( as.list(parameters), {

    for (i in 1:n){
      # System of ODEs
      dS[i] <- -sum((beta*S[i]*I/N[i])*r[i,])

      dI[i] <- sum((beta*S[i]*I/N[i])*r[i,]) - gamma*I[i]

      dR[i] <- gamma*I[i]
    }

    # Return the list of derivatives in the same order as the input state
    return(list(c(dS, dI, dR)))
  })
}


nmigration.model <- function(t, x, parameters){

  n <- parameters$n

  S <- x[1:n] # Susceptibles in all patches
  I <- x[(n+1):(2*n)] # Same as above but for the infectios compartments
  R <- x[(2*n+1):(3*n)] # Same as above but for the removed compartments

  # Calculate the population size in patch
  N <- S + I + R

  dS <- vector(length=n)
  dI <- vector(length=n)
  dR <- vector(length=n)

  # Migration rates:
  # m21 is the migration rate from patch 1 to patch 2
  # m12 is the migration rate from patch 2 to patch 1

  with( as.list(parameters), {

    # System of ODEs
    for (i in 1:n){
      dS[i] <- -beta*S[i]*I[i]/N[i] - sum(m[,i]*S[i]) + sum(m[i,]*S)

      dI[i] <- beta*S[i]*I[i]/N[i] - gamma*I[i] - sum(m[,i]*I[i]) + sum(m[i,]*I)

      dR[i] <- gamma*I[i] - sum(m[,i]*R[i]) + sum(m[i,]*R)
    }

    # Return the list of derivatives in the same order as the input state
    return(list(c(dS, dI, dR)))
  })
}


runCoupling <- function(beta,gamma,popsizes,length,n,initialI,couplingrates){

  pars <- list(beta=beta,gamma=gamma,n=n,r=couplingrates)

  times <- seq(0,length,length/1000)

  init <- rep(0,3*n)
  init[1:n] <- popsizes-initialI
  init[(n+1):(2*n)] <- initialI

  out <- lsoda(func = ncoupling.model, y = init, parms = pars, times = times)

  out <- as.data.frame(out)

  return(out)
}


runMigration <- function(beta,gamma,popsizes,length,n,initialI,migrationrates){

  pars <- list(beta=beta,gamma=gamma,n=n,m=migrationrates)

  times <- seq(0,length,length/1000)

  init <- rep(0,3*n)
  init[1:n] <- popsizes-initialI
  init[(n+1):(2*n)] <- initialI

  out <- lsoda(func = nmigration.model, y = init, parms = pars, times = times)

  out <- as.data.frame(out)

  return(out)
}


runObservationNoise <- function(beta,gamma,popsize,length,initialI,Pr,Pm){

  pars <- list(beta=beta,gamma=gamma)

  times <- seq(0,length,length/1000)

  init <- c(S = (popsize-initialI), I = initialI, R = 0)

  out <- lsoda(func = SIR_numbers.model, y = init, parms = pars, times = times)

  out <- as.data.frame(out)

  out$noisyI <- rbinom(rep(1,length(out$I)),round(out$I,0),Pr) + rbinom(rep(1,length(out$I)),round(popsize-out$I,0),Pm)

  return(out)

}


runProcessNoise <- function(beta,gamma,popsize,length,initialI,sigma_b,sigma_r){

  pars <- list(beta=beta,gamma=gamma,sigma_b=sigma_b,sigma_r=sigma_r)

  times <- seq(0,length,length/1000)

  init <- c(S = (popsize-initialI), I = initialI, R = 0)

  out <- SIR_discrete.model(times,init,pars)

  out <- as.data.frame(out)

  out$time <- times

  return(out)

}


SIR_discrete.model <- function(times,init,pars){

  S <- vector(length=length(times))
  I <- vector(length=length(times))
  R <- vector(length=length(times))

  S[1] <- init[1]
  I[1] <- init[2]
  R[1] <- init[3]

  N <- S[1]+I[1]+R[1]

  deltat <- times[2]-times[1]

  with( as.list(pars), {

    for (i in 2:length(times)){

      Brand <- rnorm(1,mean=0,sd=sigma_b)
      Grand <- rnorm(1,mean=0,sd=sigma_r)

      S[i] <- S[i-1] - (beta*(1+Brand)*S[i-1]*I[i-1]/N)*deltat
      I[i] <- I[i-1] + (beta*(1+Brand)*S[i-1]*I[i-1]/N)*deltat - gamma*I[i-1]*(1+Grand)*deltat
      R[i] <- R[i-1] + gamma*I[i-1]*(1+Grand)*deltat

    }
    out <- cbind(S=S,I=I,R=R)
  }
  )

}


SIR_rand.model <- function(t, x, parameters) {
  S <- x[1]
  I <- x[2]
  R <- x[3]

  N <- S + I + R

  with( as.list(parameters), {

    Brand <- rnorm(1,mean=0,sd=sigma_b)
    Grand <- rnorm(1,mean=0,sd=sigma_r)

    dS <- -beta*(1+Brand)*S*I/N
    dI <- beta*(1+Brand)*S*I/N - gamma*I*(1+Grand)
    dR <- gamma*I*(1+Grand)
    res <- c(dS, dI, dR)
    list(res)
  }
  )
}


runGillespie <- function(beta,gamma,popsize,maxt,initialI){

  t0 <- 0
  S0 <- popsize-initialI
  I0 <- initialI
  R0 <- 0
  N <- S0+I0+R0

  S <- S0
  I <- I0
  R <- R0

  SS <- c(S)
  II <- c(I)
  RR <- c(R)

  t <- t0
  times <- c(t)

  while ((t<=maxt)&&(I>0)&&(S>0)){

    r <- c(beta*S*I/N, gamma*I)
    rtotal <- sum(r)

    deltat <- (-1/rtotal)*log(runif(1))
    t <- t + deltat
    times <- c(times,t)

    if (runif(1)*rtotal<r[1]){
      if (S>=1){
        S <- S - 1
        I <- I + 1
      }
    } else{
      if (I>=1){
        I <- max(0,I-1)
        R <- R + 1
      }
    }
    SS <- c(SS,S)
    II <- c(II,I)
    RR <- c(RR,R)

  }

  out <- cbind(time=times,S=SS,I=II,R=RR)

  out <- as.data.frame(out)

}


