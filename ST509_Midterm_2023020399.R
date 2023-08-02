#ST509_Midterm_2023020399
# unpenalized poisson regression
UP_poisson <- function(X,y, init=NULL, max.iter = 100, eps = 1.0e-3)
{
    if(is.null(init)) init <- c(rep(0, ncol(X)))
    beta <- init
    for (iter in 1:max.iter){
        eta <- X %*% beta
        w <- c(p)
        z <- eta +(y-p)/w
        tilde.X <- as.matrix(X * sqrt(w)) 
        tilde.z <- as.matrix(z * sqrt(w))
        qr.obj <- qr(tilde.X)
        new.beta <- backsolve(qr.obj$qr, qr.qty(qr.obj, tilde.z))
        
        if (max(abs(new.beta-beta))/max(abs(beta)) < eps) break
        beta <- new.beta
    }
    if(iter == max.iter) warning("Algorithm may not be converged!")
    obj <- list(est = c(beta), iteration =iter)
}

# Ridge poisson regression
RIDG_poisson <- function(X,y, init=NULL, lambda = 0.5, max.iter = 200, eps = 1.0e-3)
{
    if(is.null(init)) init <- c(rep(0, ncol(X)))
    beta <- init
    for (iter in 1:max.iter){
        eta <- X %*% beta
        p <- exp(eta)
        w <- c(p)
        z <- eta +(y-p)/w
        
        LHS <- t(X) %*% diag(w) %*% X + c(lambda) * diag(1, ncol(X))
        RHS <- t(X) %*% diag(w) %*% z 
        #new.beta <- c(LU(LHS, RHS))
        new.beta <- solve(LHS, RHS)
        
        if (max(abs(new.beta-beta))/max(abs(beta)) < eps) break
        
        beta <- new.beta
        
    }
    if(iter == max.iter) warning("Algorithm may not be converged!")
    obj <- list(est = c(beta), iteration =iter)
}

# Lasso Poisson Regression
S <- function(z, lambda){
    (z-lambda) * (z> lambda) + (z + lambda) * (z < -lambda) + 0 * (abs(z) <= lambda)
}
LASS_poisson <- function(X, y, init=NULL, lambda=0.5, max.iter = 1000, eps = 1.0e-2)
{
    if(is.null(init)) init <- rep(0, ncol(X))
    beta <- init
    
    for(iter in 1:max.iter){
        new.beta <- beta
        temp<-beta
        for(j in 1: ncol(X)){
            w <- c(exp(X%*%temp))
            z <- X %*% temp + (y-w)/w
            tilde.X <- X * sqrt(w)
            tilde.y <- z * sqrt(w)
            denominator <- crossprod(tilde.X[,j], tilde.X[,j])
            Resi_j <- tilde.y - tilde.X %*% temp + temp[j] * tilde.X[,j]
            nominator <- crossprod(Resi_j, tilde.X[,j])
            new.beta[j] <- S(nominator, lambda)/denominator
            temp[j] <- new.beta[j] 
        }
        if (max(abs(new.beta - beta))< eps) break
        beta <- new.beta
    }
    if(iter == max.iter) warning("Algorithm may not be converged!")
    obj <- list(est = c(beta), iteration =iter)
}

# Elastic Poisson Regression
Elastic_poisson <- function(X, y, init=NULL, lambda=0.5, alpha = 0.5, max.iter = 1000, eps = 1.0e-2)
{
    if(is.null(init)) init <- rep(0, ncol(X))
    beta <- init
    
    for(iter in 1:max.iter){
        new.beta <- beta
        temp<-beta
        for(j in 1: ncol(X)){
            w <- c(exp(X%*%temp))
            z <- X %*% temp + (y-w)/w
            tilde.X <- X * sqrt(w)
            tilde.y <- z * sqrt(w)
            denominator <- lambda*(1-alpha) + crossprod(tilde.X[,j], tilde.X[,j])
            Resi_j <- tilde.y - tilde.X %*% temp + temp[j] * tilde.X[,j]
            nominator <- crossprod(Resi_j, tilde.X[,j])
            new.beta[j] <- S(nominator, lambda*alpha)/denominator
            temp[j] <- new.beta[j] 
        }
        if (max(abs(new.beta - beta))< eps) break
        beta <- new.beta
    }
    if(iter == max.iter) warning("Algorithm may not be converged!")
    obj <- list(est = c(beta), iteration =iter)
}

#SCAD penalized Poisson
SCAD_poisson <- function(X, y, init=NULL, lambda=0.5, a=3, max.iter = 1000, eps = 1.0e-2)
{
    if(is.null(init)) init <- rep(0, ncol(X))
    beta <- init
    
    for(iter in 1:max.iter){
        new.beta <- beta
        temp<-beta
        for(j in 1: ncol(X)){
            w <- c(exp(X%*%temp))
            z <- X %*% temp + (y-w)/w
            tilde.X <- X * sqrt(w)
            tilde.y <- z * sqrt(w)
            denominator <- crossprod(tilde.X[,j], tilde.X[,j])
            Resi_j <- tilde.y - tilde.X %*% temp + temp[j] * tilde.X[,j]
            
            penalty <- lambda*(abs(abs(beta[j])<=lambda)) + (a*lambda-abs(beta[j])) * (a*lambda-abs(beta[j]) > 0) * (abs(beta[j]) > lambda) 
            
            nominator <- crossprod(Resi_j, tilde.X[,j])
            new.beta[j] <- S(nominator, penalty)/denominator
            temp[j] <- new.beta[j] 
        }
        if (max(abs(new.beta - beta))< eps) break
        beta <- new.beta
    }
    if(iter == max.iter) warning("Algorithm may not be converged!")
    obj <- list(est = c(beta), iteration =iter)
}


#Generate data
library(MASS)
set.seed(2023020399)
Gen_data <- function(rho, p){
    beta <- c(1, rep(1,3), rep(0,p-3))
    
    meanvec <- c(rep(0,p)) 
    cov <- function(rho, p){
        Covmatrix <- matrix(0,p,p)
        for(i in 1:p) Covmatrix[i,] <- rho^abs(seq(1-i,p-i))
        return(Covmatrix)
    }
    covmat <- cov(rho,p)
    train_x <- matrix(mvrnorm(n=500, mu=meanvec, Sigma=covmat), ncol=p)
    train_X <- cbind(rep(1,500), train_x)
    train_y <- rpois(n=500, exp(train_X %*% beta))
    
    test_x <- matrix(mvrnorm(n=5000, mu=meanvec, Sigma=covmat), ncol=p)
    test_X <- cbind(rep(1,5000), test_x)
    test_y <- rpois(n, exp(test_X %*% beta))
    
    list('train_X' = train_X, 'train_y'= train_y, 'test_X' = test_X, 'test_y' = test_y)
} 


#Peformance Measure
Est_Accuracy <- function(all_est_beta,p){
    N<-100
    beta <- c(1, rep(1,3), rep(0,p-3))
    #MSE
    MC_MSE <- 0
    for(k in 1:100){
        MC_MSE <- MC_MSE + crossprod(c(all_est_beta[,k]) - beta, c(all_est_beta[,k]) - beta)
    }
    MC_MSE <- 1/N * MC_MSE
    #Var
    beta_bar <- c(rep(0,p+1))
    for(k in 1:100){
        beta_bar <- beta_bar+all_est_beta[,k]    
    }
    beta_bar <- 1/N * beta_bar
    MC_Var <- 1/N * sum(diag(tcrossprod(all_est_beta[,k]-beta_bar,all_est_beta[,k]-beta_bar)))
    #Bias
    onevec <- rep(1,p+1)
    MC_bias <- crossprod(onevec, abs(beta_bar-beta))
    
    list('MC MSE' = MC_MSE, 'MC Var'=MC_Var, 'MC Bias'=MC_bias)
}

VarSelect_Per <- function(all_est_beta,p, iter=100){
    CS <-0
    IS<-0
    AC<-0
    temp<-0
    for(k in 1:iter){
        for(i in 1: (p+1)){
            if(i<=4){
                if(all_est_beta[i,k] !=0) temp <- temp+1
                else IS <- IS+1
            }
            if(i>4){
                if(all_est_beta[i,k] < 0.001 ) temp <- temp+1
                else IS <- IS +1
            }
        }
        if(temp==p+1) AC<-AC+1
        CS <- CS + temp
        temp<-0
    }
    list('CS' = CS, 'IS'=IS, 'AC'=AC)
}

#parameters tuning function
lambdas <- seq(0.1, 2, 0.1)
lambdas


tuning <- function(all_est_beta,p, iter=100){
    CS <-0
    IS<-0
    AC<-0
    reposit <- c(rep(0, iter))
    for(k in 1:iter){
        for(i in 1: p+1){
            if(i<=4){
                if(all_est_beta[i,k] !=0) CS <- CS+1
                else IS <- IS+1
            }
            if(i>4){
                if(all_est_beta[i,k] < 0.001 ) CS <- CS+1
                else IS <- IS +1
            }
        }
        if(CS==p+1) AC<-AC+1
        reposit[k]<-CS
        CS<-0
    }
    print(reposit)
    a<- which.max(reposit)
    print(a)
    list('CS' = CS, 'IS'=IS, 'AC'=AC)
}

tune <- matrix(0, 51, 20)
for(k in 1:20){
    
    data<-Gen_data(0,50)
    X <- data$train_X
    y <- data$train_y
    obj2 <- SCAD_poisson(X,y,lambda=lambdas[k], max.iter=1000) #change model function
    tune[,k] <- obj2$est
}
tuning(tune,p=50,iter=20)
#tuning result -> LASSO : 1.2, ELastic : 1.4, SCAD : 1.6

#Simulation_Unpenalized, Ridge. change functions below 4 simulation.
#data1
start_time <- Sys.time()
n<-5000
est_betas <- matrix(0,4,100)
for(k in 1:100){
    
    data<-Gen_data(0,3)
    X <- data$test_X
    y <- data$test_y
    try(obj1 <- UP_poisson(X,y,max.iter=1000))
    est_betas[,k] <- obj1$est
}

Est_Accuracy(est_betas,3)

end_time <- Sys.time()
end_time - start_time

#data2
start_time <- Sys.time()
n<-5000
est_betas <- matrix(0,51,100)
for(k in 1:100){
    data<-Gen_data(0,50)
    X <- data$test_X
    y <- data$test_y
    try(obj1 <- UP_poisson(X,y,max.iter=1000))
    est_betas[,k] <- obj1$est
}
Est_Accuracy(est_betas, 50)

end_time <- Sys.time()
end_time - start_time

#data3
start_time <- Sys.time()
n<-5000
est_betas <- matrix(0,4,100)
for(k in 1:100){
    
    data<-Gen_data(0.7,3)
    X <- data$test_X
    y <- data$test_y
    try(obj1 <- UP_poisson(X,y,max.iter=1000))
    try(est_betas[,k] <- obj1$est)
}

Est_Accuracy(est_betas,3)

end_time <- Sys.time()
end_time - start_time

#data4
start_time <- Sys.time()
n<-5000
est_betas<- matrix(0,51,100)
for(k in 1:100){
    data<-Gen_data(0.7,50)
    X <- data$test_X
    y <- data$test_y
    try(obj1 <- UP_poisson(X,y,max.iter=1000))
    try(est_betas[,k] <- obj1$est)
}
Est_Accuracy(est_betas,50)

end_time <- Sys.time()
end_time - start_time

###########################
#Simulation_Lasso, Elastic, SCAD change functions below 4 simulation.
#data1
start_time <- Sys.time()
n<-5000
est_betas <- matrix(0,4,100)
for(k in 1:100){
    
    data<-Gen_data(0,3)
    X <- data$test_X
    y <- data$test_y
    try(obj1 <- UP_poisson(X,y,max.iter=1000))
    est_betas_UP[,k] <- obj1$est
}

Est_Accuracy(est_betas,3)
VarSelect_Per(est_betas,3)
end_time <- Sys.time()
end_time - start_time

#data2
start_time <- Sys.time()
n<-5000
est_betas<- matrix(0,51,100)
for(k in 1:100){
    
    data<-Gen_data(0,50)
    X <- data$test_X
    y <- data$test_y
    try(obj1 <- UP_poisson(X,y,max.iter=1000))
    try(est_betas[,k] <- obj1$est)
}
Est_Accuracy(est_betas,50)
VarSelect_Per(est_betas,50)
end_time <- Sys.time()
end_time - start_time

#data3
start_time <- Sys.time()
n<-5000
est_betas <- matrix(0,4,100)
for(k in 1:100){
    
    data<-Gen_data(0.7,3)
    X <- data$test_X
    y <- data$test_y
    try(obj1 <- UP_poisson(X,y,max.iter=1000))
    est_betas[,k] <- obj1$est
}

Est_Accuracy(est_betas,3)
VarSelect_Per(est_betas,3)
end_time <- Sys.time()
end_time - start_time

#data4
start_time <- Sys.time()
n<-5000
est_betas <- matrix(0,51,100)
for(k in 1:100){
    
    data<-Gen_data(0.7,50)
    X <- data$test_X
    y <- data$test_y
    try(obj1 <- UP_poisson(X,y,max.iter=1000))
    est_betas[,k] <- obj1$est
}
Est_Accuracy(est_betas,50)
VarSelect_Per(est_betas,50)
end_time <- Sys.time()
end_time - start_time