library(dplyr)
library(pda)
require(rootSolve)
require(rvalues)

expit = function(x){1/(1+exp(-x))}

###########Define log likelihood
f=function(bbar,gamma,x,Y){
  N = length(Y)
  site = ncol(bbar)
  den = rep(0,N)
  para = bbar + gamma
  for (i in 1:N){
    den[i] = (expit(x[i,] %*% para))^(Y[i]) * (1- expit(x[i,] %*% para))^(1 - Y[i])
  }
  den = prod(den)
  return(den)
}

##################first gradient

s1 = function(bbar,gamma,x,Y){
  ton = as.numeric(length(Y))
  t_s = length(bbar)
  outp = matrix(0,nrow = t_s, ncol = ton)
  para = bbar + gamma
  for (i in 1:ton){
    outp[,i] = x[i,] * as.numeric(Y[i] - expit(x[i,] %*% para))
  }
  outp = rowSums(outp)
  return(outp)
}

s1_b = function(bbar,s1){
  b = which(bbar[,1] != 0)
  out = s1[b]
  return(out)
}

s1_g = function(gamma,s1){
  g = which(gamma[,1] != 0)
  out = s1[g]
  return(out)
}


s2 = function(bbar,gamma,x,Y){
  N = length(Y)
  site = ncol(bbar)
  para = bbar + gamma
  grad2 = matrix(0,nrow = length(para),ncol = length(para))
  for (i in 1:N){
    grad2 = grad2 + as.numeric(expit(x[i,] %*% para) %*% (1 - expit(x[i,] %*% para))) * (x[i,] %*% t(x[i,]))
  }
  grad2 = -grad2
  return(grad2)
  
}

s2_bg = function(bbar,gamma,s2){
  b = which(bbar[,1] != 0)
  g = which(gamma[,1] != 0)
  h_bg = matrix(NA, nrow = length(b), ncol = length(g))
  for (i in 1:length(b)){
    for (j in 1:length(g)){
      tmp1 = b[i]
      tmp2 = g[j]
      h_bg[i,j] = s2[tmp1,tmp2]
    }
  }
  return(h_bg)
}

s2_gg = function(gamma,s2){
  g = which(gamma[,1] != 0)
  h_bg = matrix(NA, nrow = length(g), ncol = length(g))
  for (i in 1:length(g)){
    for (j in 1:length(g)){
      tmp1 = g[i]
      tmp2 = g[j]
      h_bg[j,i] = s2[tmp2,tmp1]
    }
  }
  return(h_bg)
}

g = function(bbar,gamma,dat){
  site = length(dat)
  g = list()
  y1 = dat[[1]]$y
  x1 = as.matrix(dat[[1]][,-1])
  for (i in 1:site){
    y = dat[[i]]$y
    x = as.matrix(dat[[i]][,-1])
    n = length(y)
    grad1 = s1(bbar[,i],gamma[,i],x,y)/n
    grad2 = s2(bbar[,i],gamma[,i],x,y)/n
    g[[i]] = f(bbar[,i],gamma[,i],x,y)/f(bbar[,1],gamma[,1],x1,y1)*
      (s1_b(bbar,grad1) - s2_bg(bbar,gamma,grad2) %*% solve(s2_gg(gamma,grad2)) %*% s1_g(gamma,grad1))
  }
  tot = g[[1]]
  for (i in 2:site){
    tot = tot + g[[i]]
  }
  latter = tot/site - g[[1]]
  return(latter)
}


g1 = function(bbar){
  tpt = rep(0,length(B[,1]))
  b = which(B[,1] != 0)
  tpt[b] = bbar
  y1 = datsplit[[1]]$y
  x1 = as.matrix(datsplit[[1]][,-1])
  n1 = length(y1)
  
  ton = as.numeric(length(y1))
  t_s = length(tpt)
  grad1 = matrix(0,nrow = t_s, ncol = ton)
  for (i in 1:ton){
    grad1[,i] = x[i,] * as.numeric(y1[i] - expit(x1[i,] %*% tpt))
  }
  grad1 = rowSums(grad1)
  
  
  grad2 = matrix(0,nrow = length(tpt),ncol = length(tpt))
  for (i in 1:N){
    grad2 = grad2 + as.numeric(expit(x[i,] %*% tpt) %*% (1 - expit(x[i,] %*% tpt))) * (x[i,] %*% t(x[i,]))
  }
  grad2 = -grad2
  
  (s1_b(B,grad1) - s2_bg(B,G,grad2) %*% solve(s2_gg(G,grad2)) %*% s1_g(G,grad1))/n1
}
U_til = function(beta){
  g1(beta) + latter
  
}

#first order gradient
Lgradient = function(beta,X,Y){
  design = X  # cbind(1,X)
  t(y-expit(design%*%bbar))%*%design/length(y)
}
#second-order gradient
Lgradient2 = function(beta,X){
  design = X # cbind(1,X)
  Z=expit(design%*%beta)
  t(c(-Z*(1-Z))*design)%*%design/nrow(X)
}
#likelihood function of logistic reg
Lik = function(beta,X,Y){
  design = X # cbind(1,X)
  sum(Y*(design%*%t(t(beta)))-log(1+exp(design%*%t(t(beta)))))/length(Y)
}
# target function
logL_tilde = function(beta){
  - (Lik(beta,X, Y) + (logL_all_D1/N - Lgradient(bbar, X, Y))%*%beta+
       t(beta-bbar)%*%(logL_all_D2/N - Lgradient2(bbar, X))%*%(beta-bbar) / 2)
}
# ODAL initialization
ODAL.int <- function(ipdata,site_num){
  init = list()
  for (i in 1:site_num){
    var = colnames(datsplit[[i]])
    var = var[-(1:2)]
    var = paste0(var, collapse="+")
    var = c("y",var)
    var = paste0(var,collapse = "~")
    fit_i <- glm(var, data=ipdata[[i]],family = "binomial"(link = "logit"))  
    init[[i]] <- list(site = i,site_size = nrow(ipdata[[i]]),
                      bhat_i = fit_i$coef,Vhat_i = diag(vcov(fit_i)))
  }
  return(init)
}
# ODAL likelihood derivative from each sites

ODAL.derive <- function(ipdata,site_num,int){
  derivatives = list()
  for (i in 1:site_num){
    px <- ncol(ipdata[[i]])   
    bhat <- rep(0, px-1)
    vbhat <- rep(0, px-1)    # cov matrix?
    
    #estimate from meta-analysis
    #betameta = apply(bhat/vbhat,2,function(x){sum(x, na.rm = T)})/apply(1/vbhat,2,function(x){sum(x, na.rm = T)})
    #vmeta = 1/apply(1/vbhat,2,function(x){sum(x, na.rm = T)})
    # b_meta <- betameta
    
    for(j in 1:site_num){
      bhat = rbind(bhat, int[[i]]$bhat_i)
      vbhat = rbind(vbhat, int[[i]]$Vhat_i)
    }
    bhat = bhat[-1,]
    vbhat = vbhat[-1,]
    
    #estimate from meta-analysis
    betameta = apply(bhat/vbhat,2,function(x){sum(x, na.rm = T)})/apply(1/vbhat,2,function(x){sum(x, na.rm = T)})
    vmeta = 1/apply(1/vbhat,2,function(x){sum(x, na.rm = T)})
    bbar <- int[[i]]$bhat_i #b_meta
    
    # 1st and 2nd derivatives
    y <- c(ipdata[[i]]$y)
    X <- as.matrix(ipdata[[i]][,-1])
    
    logL_D1 <- Lgradient(bbar,X,y)
    logL_D2 <- Lgradient2(bbar,X)
    
    derivatives[[i]] <- list(
      site=i, 
      site_size = nrow(ipdata[[i]]),
      logL_D1=logL_D1,
      logL_D2=logL_D2,
      betameta = betameta,
      vbhat = vbhat)
  }
  return(derivatives)
}

#deri = derivatives

#setwd("C:/Projects/FL")

N <- 2000
n_site = 5
datsplit = list()
d = 1.83
b1 <- c(-10,3,4.5,6,-5.9)+d
b2 <- c(-10,3,4.5,6,-5.8)+d
b3 <- c(-10,3,4.5,6,-5.7)+d
b4 <- c(-10,3,4.5,6,-6)+d
b5 <- c(-10,3,4.5,6,-3)+d
#b6 = c(-10,3,4.5,6,-3)+d
b <- cbind(b1,b2,b3,b4,b5)
sites = c('site1', 'site2', 'site3', 'site4', 'site5')
#par(mfrow=c(2,3))
n_para = ncol(b)

for (i in 1:n_site){
  set.seed(i+3)
  #x1 <- rnorm(N,mx1,sdx1)
  x0 = rep(1,N) # intercept
  
  x1 <- runif(N,0,2)
  #p2 <- 0.5
  x2 <- runif(N,0,2)
  #p3 <- 0.25
  x3 =  rbinom(N, size = 1, 0.5)
  #x4 <- rgamma(N, sx4, rate = rx4)
  x4 =  rbinom(N, size = 1, 0.5)
  
  
  p <- rnorm(N)  #vector used for latter
  f <- rnorm(N)
  y <- rep(0,N)
  n <- rnorm(N,mean = 0, sd = 0.05)
  
  #b7 <- c(1,3,2.0,1,2)
  #b8 <- c(1,3,2.0,1,2)
  #b9 <- c(1,3,2.0,1,2)
  #b10 <- c(1,3,2.0,1,2)
  
  x <- cbind(x0,x1,x2,x3,x4)
  
  #b6,b7,b8,b9,b10) 
  
  
  sim <- data.frame(y,x)
  # create sites
  #spl <- c(rep(1,N/n_site),rep(2,N/n_site),rep(3,N/n_site),rep(4,N/n_site),rep(5,N/n_site))
  #,
  #rep(6,500),rep(7,500),rep(8,500),rep(9,500),rep(10,500))
  tempt = i
  for (j in 1:N){
    f[j] <- x[j,] %*% b[,tempt] + n[j]
    p[j] <- 1/(1+exp(-f[j]))
    sim$y[j] <- rbinom(1,1,p[j])
  }
  datsplit[[i]] = sim
  #hist(p)
}
ci = list()
mod = list()
for (i in 1:n_site){
  set.seed(i)
  mod[[i]] = glm(y ~ x1 + x2 + x3 + x4 , family = 'binomial', data = datsplit[[i]])
  ci[[i]] = confint(mod[[i]])
}

#for (i in 1:5){
#  print(mod[[i]]$coefficients)
#}

new_ci = list()
vet = c(1:n_site)
for (i in 1:n_site){
  temp = vet[-i]
  new_ci[[i]] = cbind(ci[[temp[1]]],ci[[temp[2]]],ci[[temp[3]]],ci[[temp[4]]])
  #,ci[[temp[5]]])
  #ci[[temp[6]]],ci[[temp[7]]],ci[[temp[8]]],ci[[temp[9]]])
}

homo = matrix(0, nrow = 5, ncol = n_site-1)
blist = list()
for (i in 1:5){
  temp = new_ci[[i]]
  for (j in 1:5){
    for (k in 1:4){
      homo[j,k] = between(mod[[i]]$coefficients[j],temp[j,2*k-1],temp[j,2*k])
    }
  }
  blist[[i]] = homo
}

homo = rep(0,5)
for (i in 1:5){
  temp = rowSums(blist[[i]])
  homo = temp + homo
}

homo = homo/20
homo = as.integer(homo) # if beta equal to 1 it means it should fall into shared para beta

para_p = data.frame(mod[[1]]$coefficients)
for (i in 2:n_site){
  temp = mod[[i]]$coefficients
  para_p = data.frame(para_p,temp)
}
temp = c(1:n_site)
temp = paste0(temp,"_Site")
colnames(para_p) = temp

B = rowMeans(para_p)
B = matrix(B, nrow = length(B), ncol = n_site)
B = B * homo #beta[beta == 0] = NA;beta = na.omit(beta);beta = rowMeans(beta)
hete = homo;hete[which(homo == 0)] = 1;hete[which(homo == 1)] = 0
G = para_p * hete;#gamma[gamma == 0] = NA;gamma = na.omit(gamma)

latter = g(B,G,datsplit)

b_int = rowMeans(para_p)
b_int = b_int * homo
b_int = b_int[which(homo != 0)]

ss <- multiroot(f = U_til,start = b_int,rtol = 1e-6, atol = 1e-8, ctol = 1e-8)
h_global = ss$root

##################### ODAL ######################

################## Global para and site specific para ######################
ODAL_p = para_p;ODAL_p[which(homo != 0),] = h_global


#T = 100


############### Bootstrap ############
bot = 5000
p_ci = list()
for (j in 1:5){
  new.para <- ODAL_p[,j]
  HPD <- matrix(0,n_site,5000)
  for (i in 1:bot){
    new.x <- x                                      # create a copy of x
    lp <- new.x %*% new.para                        # predicted response
    mu <- exp(lp)/(1+exp(lp))                       
    #res.pear <- (sim$y - mu)/sqrt(mu*(1-mu))        
    
    
    #sig <- sum(sim$y - mu)/N                        
    V <- sqrt(mu*(1-mu))                            # V matrix in the paper
    delta <- exp(lp)/((1+exp(lp))^2)                # Delta matrix in the paper
    tp <- (t(V) %*% delta)                 
    tp <- as.numeric(tp)
    G <- tp*new.x                                   # G matrix
    hat <- diag(G %*% solve(t(G) %*% G) %*% t(G))
    res.pear <- (datsplit[[j]]$y - mu)/sqrt(V*(1-hat)) # pearson residuals
    new.res <- sample(res.pear,length(sim$y),replace = T)
    new.para <- new.para + solve(t(G) %*% G) %*% t(G) %*% new.res
    HPD[,i] <- solve(t(G) %*% G) %*% t(G) %*% new.res
  }
  
  lb = apply(HPD, 1, FUN = min)
  ub = apply(HPD, 1, FUN = max)
  lb = new.para+lb
  ub = new.para+ub
  p_ci[[j]] = data.frame(lb,ub)
}
tot = rbind(datsplit[[1]],datsplit[[2]],datsplit[[3]],
            datsplit[[4]],datsplit[[5]])

for (i in 1:bot){
  new.x <- b.odal                                      # create a copy of x
  lp <- new.x %*% new.para                        # predicted response
  mu <- exp(lp)/(1+exp(lp))                       # predicted odds
  #res.pear <- (sim$y - mu)/sqrt(mu*(1-mu))        
  
  
  
  #sig <- sum(sim$y - mu)/N                        
  V <- sqrt(mu*(1-mu))                            # V matrix in the paper
  delta <- exp(lp)/((1+exp(lp))^2)                # Delta matrix in the paper
  tp <- (t(V) %*% delta)                 
  tp <- as.numeric(tp)
  G <- tp*new.x                                   # G matrix
  hat <- diag(G %*% solve(t(G) %*% G) %*% t(G))
  res.pear <- (tot$y - mu)/sqrt(V*(1-hat)) # bootstrapped samples
  new.res <- sample(res.pear,bot,replace = T)
  new.para <- new.para + solve(t(G) %*% G) %*% t(G) %*% new.res
  HPD[,i] <- solve(t(G) %*% G) %*% t(G) %*% new.res
}

lb = apply(HPD, 1, FUN = min)
ub = apply(HPD, 1, FUN = max)
lb = new.para+lb
ub = new.para+ub
p_ci[[j+1]] = data.frame(lb,ub)
  