x3 <- runif(N, -3, -2)
#x4 <- rgamma(N, sx4, rate = rx4)
x4 <- runif(N, 5, 10)
p <- rnorm(N)
f <- rnorm(N)
y <- rnorm(N)
#n <- rnorm(N,mean = 0, sd = 0.01)
sites = c('site1', 'site2', 'site3','site4','site5','site6')
b1 <- c(0,-3,2,-1)
b2 <- c(0,-3,2,-1)
b3 <- c(0,-3,2,-1)
b4 <- c(0,-3,2,-1)
b5 <- c(0,-3,2,-1)
b6 <- c(0,3,-2,1)
x <- cbind(x1,x2,x3,x4)
b <- cbind(b1,b2,b3,b4,b5,b6)
sim <- data.frame(y,x)
# create sites
spl <- c(rep(1,500),rep(2,500),rep(3,500),rep(4,500),rep(5,500),rep(6,500))
for (i in 1:N){
  f[i] <- x[i,] %*% b[,spl[i]] #+ n[i]
  p[i] <- 1/(1+exp(-f[i]))
  sim$y[i] <- rbinom(1,1,p[i])
}
#datsplit <- split(sim,spl)
datsplit <- list(sim[1:500,],sim[501:1000,],sim[1001:1500,],sim[1501:2000,],sim[2001:2500,],sim[2501:3000,])
sim <- rbind(datsplit[[1]],datsplit[[2]],datsplit[[3]],datsplit[[4]],datsplit[[5]],datsplit[[6]])
fit.pool <- glm(y ~ x1 + x2 + x3 + x4 , family = 'binomial', data = sim)
control <- list(project_name = 'Sim study',
                step = 'initialize',
                sites = sites,
                heterogeneity = TRUE,
                model = 'ODAL',
                family = 'binomial',
                outcome = "y",
                variables = c('x1','x2','x3','x4'),
                optim_maxit = 100,
                lead_site = 'site1',
                upload_date = as.character(Sys.time()) )
pda(site_id = 'site1', control = control, dir = getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
1
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
1
config <- getCloudConfig(site_id = 'site1', dir=getwd())
fit.odal <- pdaGet(name = 'site1_estimate', config = config)
#control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coef,
            #b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
View(fit.pool)
#control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coefficients,
            #b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
fit.pool$coefficients
fit.odal$btilde
set.seed(123)
N <- 3000
#x1 <- rnorm(N,mx1,sdx1)
x1 <- runif(N,1,2)
#p2 <- 0.5
x2 <- runif(N,-5,-1)
#p3 <- 0.25
x3 <- runif(N, -3, -2)
#x4 <- rgamma(N, sx4, rate = rx4)
x4 <- runif(N, 5, 10)
p <- rnorm(N)
f <- rnorm(N)
y <- rnorm(N)
sites = c('site1', 'site2', 'site3','site4','site5','site6')
b1 <- c(0,-3,2,-1)
b2 <- c(0,-3,2,-1)
b3 <- c(0,-3,2,-1)
b4 <- c(0,-3,2,-1)
b5 <- c(0,-3,2,-1)
b6 <- c(0,3,-2,1)
x <- cbind(x1,x2,x3,x4)
b <- cbind(b1,b2,b3,b4,b5,b6)
sim <- data.frame(y,x)
# create sites
spl <- c(rep(1,500),rep(2,500),rep(3,500),rep(4,500),rep(5,500),rep(6,500))
for (i in 1:N){
  f[i] <- x[i,] %*% b[,spl[i]] #+ n[i]
  p[i] <- 1/(1+exp(-f[i]))
  sim$y[i] <- rbinom(1,1,p[i])
}
#datsplit <- split(sim,spl)
datsplit <- list(sim[1:500,],sim[501:1000,],sim[1001:1500,],sim[1501:2000,],sim[2001:2500,],sim[2501:3000,])
sim <- rbind(datsplit[[1]],datsplit[[2]],datsplit[[3]],datsplit[[4]],datsplit[[5]],datsplit[[6]])
fit.pool <- glm(y ~ x1 + x2 + x3 + x4 , family = 'binomial', data = sim)
control <- list(project_name = 'Sim study',
                step = 'initialize',
                sites = sites,
                heterogeneity = TRUE,
                model = 'ODAL',
                family = 'binomial',
                outcome = "y",
                variables = c('x1','x2','x3','x4'),
                optim_maxit = 100,
                lead_site = 'site1',
                upload_date = as.character(Sys.time()) )
pda(site_id = 'site1', control = control, dir = getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
1
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
1
config <- getCloudConfig(site_id = 'site1', dir=getwd())
fit.odal <- pdaGet(name = 'site1_estimate', config = config)
#control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coefficients,
            #b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
set.seed(123)
N <- 3000
#x1 <- rnorm(N,mx1,sdx1)
x1 <- runif(N,1,2)
#p2 <- 0.5
x2 <- runif(N,-5,-1)
#p3 <- 0.25
x3 <- runif(N, -3, -2)
#x4 <- rgamma(N, sx4, rate = rx4)
x4 <- runif(N, 5, 10)
p <- rnorm(N)
f <- rnorm(N)
y <- rnorm(N)
#n <- rnorm(N,mean = 0, sd = 0.01)
sites = c('site1', 'site2', 'site3','site4','site5','site6')
b1 <- c(0,-3,2,-1)
b2 <- c(0,-3,2,-1)
b3 <- c(0,-3,2,-1)
b4 <- c(0,-3,2,-1)
b5 <- c(0,-3,2,-1)
b6 <- c(0,3,-2,1)
x <- cbind(x1,x2,x3,x4)
b <- cbind(b1,b2,b3,b4,b5,b6)
sim <- data.frame(y,x)
# create sites
spl <- c(rep(1,500),rep(2,500),rep(3,500),rep(4,500),rep(5,500),rep(6,500))
for (i in 1:N){
  f[i] <- x[i,] %*% b[,spl[i]] #+ n[i]
  p[i] <- 1/(1+exp(-f[i]))
  sim$y[i] <- rbinom(1,1,p[i])
}
#datsplit <- split(sim,spl)
datsplit <- list(sim[1:500,],sim[501:1000,],sim[1001:1500,],sim[1501:2000,],sim[2001:2500,],sim[2501:3000,])
sim <- rbind(datsplit[[1]],datsplit[[2]],datsplit[[3]],datsplit[[4]],datsplit[[5]],datsplit[[6]])
fit.pool <- glm(y ~ x1 + x2 + x3 + x4 , family = 'binomial', data = sim)
control <- list(project_name = 'Sim study',
                step = 'initialize',
                sites = sites,
                heterogeneity = TRUE,
                model = 'ODAL',
                family = 'binomial',
                outcome = "y",
                variables = c('x1','x2','x3','x4'),
                optim_maxit = 100,
                lead_site = 'site2',
                upload_date = as.character(Sys.time()) )
pda(site_id = 'site2', control = control, dir = getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
1
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
1
config <- getCloudConfig(site_id = 'site2', dir=getwd())
fit.odal <- pdaGet(name = 'site2_estimate', config = config)
#control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coefficients,
            #b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
View(sim)
View(datsplit)
control <- list(project_name = 'Sim study',
                step = 'initialize',
                sites = sites,
                heterogeneity = TRUE,
                model = 'ODAL',
                family = 'binomial',
                outcome = "y",
                variables = c('x1','x2','x3','x4'),
                optim_maxit = 100,
                lead_site = 'site2',
                upload_date = as.character(Sys.time()) )
pda(site_id = 'site2', control = control, dir = getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
1
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
1
config <- getCloudConfig(site_id = 'site2', dir=getwd())
fit.odal <- pdaGet(name = 'site2_estimate', config = config)
#control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coefficients,
            #b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
set.seed(123)
N <- 3000
#x1 <- rnorm(N,mx1,sdx1)
x1 <- runif(N,1,2)
#p2 <- 0.5
x2 <- runif(N,-5,-1)
#p3 <- 0.25
x3 <- runif(N, -3, -2)
#x4 <- rgamma(N, sx4, rate = rx4)
x4 <- runif(N, 5, 10)
p <- rnorm(N)
f <- rnorm(N)
y <- rnorm(N)
#n <- rnorm(N,mean = 0, sd = 0.01)
sites = c('site1', 'site2', 'site3','site4','site5','site6')
b1 <- c(0,-3,2,-1)
b2 <- c(0,-3,2,-1)
b3 <- c(0,-3,2,-1)
b4 <- c(0,-3,2,-1)
b5 <- c(0,-3,2,-1)
b6 <- c(0,3,-2,1)
x <- cbind(x1,x2,x3,x4)
b <- cbind(b1,b2,b3,b4,b5,b6)
sim <- data.frame(y,x)
# create sites
spl <- c(rep(1,500),rep(2,500),rep(3,500),rep(4,500),rep(5,500),rep(6,500))
for (i in 1:N){
  f[i] <- x[i,] %*% b[,spl[i]] #+ n[i]
  p[i] <- 1/(1+exp(-f[i]))
  sim$y[i] <- rbinom(1,1,p[i])
}
#datsplit <- split(sim,spl)
datsplit <- list(sim[1:500,],sim[501:1000,],sim[1001:1500,],sim[1501:2000,],sim[2001:2500,],sim[2501:3000,])
sim <- rbind(datsplit[[1]],datsplit[[2]],datsplit[[3]],datsplit[[4]],datsplit[[5]],datsplit[[6]])
fit.pool <- glm(y ~ x1 + x2 + x3 + x4 , family = 'binomial', data = sim)
control <- list(project_name = 'Sim study',
                step = 'initialize',
                sites = sites,
                heterogeneity = TRUE,
                model = 'ODAL',
                family = 'binomial',
                outcome = "y",
                variables = c('x1','x2','x3','x4'),
                optim_maxit = 100,
                lead_site = 'site2',
                upload_date = as.character(Sys.time()) )
pda(site_id = 'site2', control = control, dir = getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
1
config <- getCloudConfig(site_id = 'site2', dir=getwd())
fit.odal <- pdaGet(name = 'site2_estimate', config = config)
#control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coefficients,
            #b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
standardized_resid <- residuals(fit.odal,type = "pearson")/sqrt(1-hatvalues(fit.pool))
standardized_resid <- residuals(fit.odal,type = "pearson")/sqrt(1-hatvalues(fit.odal))
fit.odal
hatvalues(fit.pool)
install.packages("boot")
library(voot)
library(boot)
#odal.error;glm.error
glm.diag(fit.pool)
#odal.error;glm.error
glm.diag(fit.pool)$rp
#odal.error;glm.error
new.x <- cbind(rep(1,3000),x)
View(sim)
View(new.x)
yhat <- new.x %*% fit.odal$btilde
View(yhat)
h <- new.x %*% solve(t(new.x) %*% new.x) %*% t(new.x)
View(h)
dim(h)
h2 <- hatvalues(fit.pool)
diag(h) - h2
rp <- (sim$y - yhat)/sqrt(yhat(1-yhat)(1-diag(h)))
rp <- (sim$y - yhat)/sqrt(yhat*(1-yhat)(1-diag(h)))
rp <- (sim$y - yhat)/sqrt(yhat*(1-yhat)*(1-diag(h)))
which(yhat*(1-yhat)*(1-diag(h) == 0)
)
which(yhat*(1-yhat)*(1-diag(h)) == 0)
div <- yhat*(1-yhat)*(1-diag(h))
View(div)
h <- new.x %*% solve(t(new.x) %*% new.x) %*% t(new.x)
diag(h)
(1-diag(h))
View(yhat)
fit.odal$Htilde
fit.odal$btilde
View(new.x)
View(yhat)
new.x[,1]
new.x[1,]
new.x[1,] %*% fit.odal$btilde
yhat <- exp(new.x %*% fit.odal$btilde)/(1-exp(new.x %*% fit.odal$btilde))
View(yhat)
yhat <- exp(new.x %*% fit.odal$btilde)/(1+exp(new.x %*% fit.odal$btilde))
View(yhat)
rp <- (sim$y - yhat)/sqrt(yhat*(1-yhat)*(1-diag(h)))
View(rp)
View(rp)
rp <- (p - yhat)/sqrt(yhat*(1-yhat)*(1-diag(h)))
View(rp)
library(pda)
require(survival)
require(data.table)
require(pda)
library(ROCR)
library(caret)
library(MLmetrics)
#standardized_resid <- residuals(fit.pool,type = "pearson")/sqrt(1-hatvalues(fit.pool))
set.seed(12345)
N <- 1500
sites <- 6
#x1 <- rnorm(N,mx1,sdx1)
x1 <- runif(N,-3,-1)
#p2 <- 0.5
x2 <- runif(N,-1,0)
#p3 <- 0.25
#x3 <- runif(N, 1, 2)
#x3 <- rbinom(N,1,0.5)
#x4 <- rgamma(N, sx4, rate = rx4)
x3 <- runif(N, 0, 1)
x4 <- runif(N, 2, 4)
p <- rnorm(N)
f <- rnorm(N)
y <- rnorm(N)
e <- rnorm(N,mean = 0, sd = 1)
#sites = c('site1',
#          'site2',
#'site3',
#          'site4'
#)
b1 <- c(-1.2,3,1.5,0.5)
b2 <- c(-1.2,3,1.5,0.5)
b3 <- c(-1.2,3,1.5,0.5)
b4 <- c(-1.2,3,1.5,0.5)
b5 <- c(-1.2,3,1.5,0.5)
b6 <- c(1.2,-3,-1.5,-0.5)
spl <- rep(0,N)
for (i in 1:sites){
  spl[((i-1)*(N/sites)+1):((i)*(N/sites))] <- rep(i,N/sites)
}
b <- cbind(b1,b2,b3,b4,b5,b6)
x <- cbind(x1,x2,x3,x4)
for (i in 1:sites){
  y[((i-1)*(N/sites)+1):((i)*(N/sites))] <- x[which(spl==i),] %*% b[,i] + e[which(spl==i)]
}
sim <- data.frame(y,x)
gen.mod <- lm(y~. ,data = sim)
s.mod <- list()
#for (i in 1:sites){
#  s.mod[[i]] <- lm(y~.,data = sim[which(spl==i),])
#}
## Initialization
s.mod[[1]] <- lm(y~.,data = sim[which(spl==1),])
new.x <- cbind(rep(1,N),x)
comp1 <- cbind((new.x^2)[,1] * s.mod[[1]]$coefficients[1],(new.x^2)[,2] * s.mod[[1]]$coefficients[2],
               (new.x^2)[,3] * s.mod[[1]]$coefficients[3],(new.x^2)[,4] * s.mod[[1]]$coefficients[4],
               (new.x^2)[,5] * s.mod[[1]]$coefficients[5])
comp2 <- y*new.x
grad <- colSums(comp1 - comp2)/N
sig <- sum(gen.mod$residuals^2)/N
res <- rep(0,N)
for (i in 1:sites){
  res[((i-1)*(N/sites)+1):((i)*(N/sites))] <- s.mod[[i]]$residuals
}
new.res <- sample(res,1500,replace = T)
new.y <- rep(0,1500)
for (i in 1:sites){
  new.y[((i-1)*(N/sites)+1):((i)*(N/sites))] <- s.mod[[i]]$fitted.values + new.res[((i-1)*(N/sites)+1):((i)*(N/sites))]
}
library(pda)
require(survival)
require(data.table)
require(pda)
library(ROCR)
library(caret)
library(boot)
set.seed(123)
N <- 3000
#x1 <- rnorm(N,mx1,sdx1)
x1 <- runif(N,1,2)
#p2 <- 0.5
x2 <- runif(N,-5,-1)
#p3 <- 0.25
x3 <- runif(N, -3, -2)
#x4 <- rgamma(N, sx4, rate = rx4)
x4 <- runif(N, 5, 10)
p <- rnorm(N)
f <- rnorm(N)
y <- rnorm(N)
#n <- rnorm(N,mean = 0, sd = 0.01)
sites = c('site1', 'site2', 'site3','site4','site5','site6')
b1 <- c(0,-3,2,-1)
b2 <- c(0,-3,2,-1)
b3 <- c(0,-3,2,-1)
b4 <- c(0,-3,2,-1)
b5 <- c(0,-3,2,-1)
b6 <- c(0,3,-2,1)
x <- cbind(x1,x2,x3,x4)
b <- cbind(b1,b2,b3,b4,b5,b6)
sim <- data.frame(y,x)
# create sites
spl <- c(rep(1,500),rep(2,500),rep(3,500),rep(4,500),rep(5,500),rep(6,500))
for (i in 1:N){
  f[i] <- x[i,] %*% b[,spl[i]] #+ n[i]
  p[i] <- 1/(1+exp(-f[i]))
  sim$y[i] <- rbinom(1,1,p[i])
}
#datsplit <- split(sim,spl)
datsplit <- list(sim[1:500,],sim[501:1000,],sim[1001:1500,],sim[1501:2000,],sim[2001:2500,],sim[2501:3000,])
sim <- rbind(datsplit[[1]],datsplit[[2]],datsplit[[3]],datsplit[[4]],datsplit[[5]],datsplit[[6]])
fit.pool <- glm(y ~ x1 + x2 + x3 + x4 , family = 'binomial', data = sim)
control <- list(project_name = 'Sim study',
                step = 'initialize',
                sites = sites,
                heterogeneity = TRUE,
                model = 'ODAL',
                family = 'binomial',
                outcome = "y",
                variables = c('x1','x2','x3','x4'),
                optim_maxit = 100,
                lead_site = 'site2',
                upload_date = as.character(Sys.time()) )
pda(site_id = 'site2', control = control, dir = getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site3', ipdata = datsplit[[3]], dir=getwd())
pda(site_id = 'site6', ipdata = datsplit[[6]], dir=getwd())
pda(site_id = 'site4', ipdata = datsplit[[4]], dir=getwd())
pda(site_id = 'site5', ipdata = datsplit[[5]], dir=getwd())
pda(site_id = 'site1', ipdata = datsplit[[1]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
pda(site_id = 'site2', ipdata = datsplit[[2]], dir=getwd())
1
config <- getCloudConfig(site_id = 'site2', dir=getwd())
fit.odal <- pdaGet(name = 'site2_estimate', config = config)
#control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coefficients,
            #b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
#control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coefficients,
            b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
control <- pdaGet(name = 'control', config)
round(cbind(b.pool=fit.pool$coefficients,
            b.meta=control$beta_init,
            b.odal=fit.odal$btilde ),4)
sum(sim$y[1:500,])
sum(sim$y[1:500])
log(0.2887)
exp(-1.4866)
exp(-1.4034)
