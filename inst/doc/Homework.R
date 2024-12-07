## -----------------------------------------------------------------------------
# 样本数据
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131) #身高，单位cm
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)           #体重，单位kg

#调用lm()函数
ans <- lm(y~x)

#调用summary()函数
summary(ans)$coef

## -----------------------------------------------------------------------------
plot(ans)

# 调用predict()判断身高为170cm的体重
a <- data.frame(x = 170)
result <-  predict(ans,a)
print(result)

## -----------------------------------------------------------------------------
plot(x,y,col = "blue",main = "Height & Weight Regression",abline(ans),cex = 1.3,pch = 16,ylab = "Weight in Kg",xlab = "Height in cm")

## -----------------------------------------------------------------------------
tab <- matrix(c(58,58,48,64,69,57,55,71,59,66,64,47,67,68,49),ncol=3,byrow=TRUE)
colnames(tab) <- c('A','B','C')
knitr::kable(tab)

## -----------------------------------------------------------------------------
library(tidyr)
library(dplyr)
x <- as.data.frame(tab)
long_table <- pivot_longer(x, cols = A:C, names_to = "grp", values_to = "value")
long_table <- arrange(long_table,grp)
print(long_table)

## -----------------------------------------------------------------------------
res <- aov(value ~ grp, data=long_table)
summary(res)

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(data = long_table, mapping = aes(x = grp, y = value)) + geom_boxplot()

## -----------------------------------------------------------------------------
bartlett.test(value ~ grp, data = long_table)

## -----------------------------------------------------------------------------
library(dplyr)
data(ToothGrowth, package="datasets")
d.tooth <- ToothGrowth
mutate(d.tooth, supp = factor(supp, levels=c("OJ", "VC")),dose = factor(dose, levels=c(0.5, 1.0, 2.0)))
x <- count(d.tooth, supp, dose)
knitr::kable(x)

## -----------------------------------------------------------------------------
aov.to1 <- aov(len ~ dose + supp + dose:supp, data = d.tooth)
summary(aov.to1)

## -----------------------------------------------------------------------------
set.seed(1)
u <- runif(1000)
s <- 0.5  # s表示参数sigma，这里取值0.5
x <- sqrt(-2*(s^2)*log(1-u))

## -----------------------------------------------------------------------------
hist(x,breaks = 50,freq = F,main = expression(f(x)==4*x*e^(-2*x^2)))
lines(density(x))

## -----------------------------------------------------------------------------
set.seed(2)
u <- runif(1000)
s <- 1  # s表示参数sigma，这里取值1
x <- sqrt(-2*(s^2)*log(1-u))
hist(x,breaks = 50,freq = F,main = expression(f(x)==x*e^(-x^2/2)))
lines(density(x))

## -----------------------------------------------------------------------------
set.seed(3)
u <- runif(1000)
s <- 2  # s表示参数sigma，这里取值2
x <- sqrt(-2*(s^2)*log(1-u))
hist(x,breaks = 50,freq = F,main = expression(f(x)==frac(x,4)*e^(-x^2/8)))
lines(density(x))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.75
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2

## -----------------------------------------------------------------------------
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.75,sqrt(2*pi))*e^(-x^2/2)+frac(0.25,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.1
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.1,sqrt(2*pi))*e^(-x^2/2)+frac(0.9,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.2
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.2,sqrt(2*pi))*e^(-x^2/2)+frac(0.8,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.3
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.3,sqrt(2*pi))*e^(-x^2/2)+frac(0.7,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.4
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.4,sqrt(2*pi))*e^(-x^2/2)+frac(0.6,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.5
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.5,sqrt(2*pi))*e^(-x^2/2)+frac(0.5,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.6
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.6,sqrt(2*pi))*e^(-x^2/2)+frac(0.4,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.7
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.7,sqrt(2*pi))*e^(-x^2/2)+frac(0.3,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.8
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.8,sqrt(2*pi))*e^(-x^2/2)+frac(0.2,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
N1 <- rnorm(1000,0,1)
N2 <- rnorm(1000,3,1)
p1 <- 0.9
p2 <- 1-p1
p <- sample(c(1,0),1000,replace=TRUE,prob = c(p1,p2))
X <- p*N1+(1-p)*N2
hist(X,breaks = 50,freq = F,main = expression(f(x)==frac(0.9,sqrt(2*pi))*e^(-x^2/2)+frac(0.1,sqrt(2*pi))*e^(-(x-3)^2/2)))
lines(density(X))

## -----------------------------------------------------------------------------
set.seed(1)
t <- 10
l <- 1            # λ=1
n <- rpois(1000,t*l)
x <- vector(mode = "numeric",length = 1000)
for (i in 1:1000) {
  y <- rgamma(n[i],shape = 1,rate = 1)
  x[i] <- sum(y)
}
x_mean_estimate <- mean(x)   # 均值的估计值
x_var_estimate <- var(x)     # 方差的估计值

x_mean_theoretical <- l*t*mean(y)    # 均值的理论值
x_var_theoretical <- l*t*mean(y^2)   # 方差的理论值

sprintf("x_mean_estimate: %f",x_mean_estimate)
sprintf("x_mean_theoretical: %f",x_mean_theoretical)
sprintf("x_var_estimate: %f",x_var_estimate)
sprintf("x_var_theoretical: %f",x_var_theoretical)

## -----------------------------------------------------------------------------
set.seed(1)
t <- 10
l <- 5            # λ=5
n <- rpois(1000,t*l)
x <- vector(mode = "numeric",length = 1000)
for (i in 1:1000) {
  y <- rgamma(n[i],shape = 1,rate = 1)
  x[i] <- sum(y)
}
x_mean_estimate <- mean(x)   # 均值的估计值
x_var_estimate <- var(x)     # 方差的估计值

x_mean_theoretical <- l*t*mean(y)    # 均值的理论值
x_var_theoretical <- l*t*mean(y^2)   # 方差的理论值

sprintf("x_mean_estimate: %f",x_mean_estimate)
sprintf("x_mean_theoretical: %f",x_mean_theoretical)
sprintf("x_var_estimate: %f",x_var_estimate)
sprintf("x_var_theoretical: %f",x_var_theoretical)

## -----------------------------------------------------------------------------
set.seed(1)
t <- 10
l <- 10            # λ=10
n <- rpois(1000,t*l)
x <- vector(mode = "numeric",length = 1000)
for (i in 1:1000) {
  y <- rgamma(n[i],shape = 1,rate = 1)
  x[i] <- sum(y)
}
x_mean_estimate <- mean(x)   # 均值的估计值
x_var_estimate <- var(x)     # 方差的估计值

x_mean_theoretical <- l*t*mean(y)    # 均值的理论值
x_var_theoretical <- l*t*mean(y^2)   # 方差的理论值

sprintf("x_mean_estimate: %f",x_mean_estimate)
sprintf("x_mean_theoretical: %f",x_mean_theoretical)
sprintf("x_var_estimate: %f",x_var_estimate)
sprintf("x_var_theoretical: %f",x_var_theoretical)

## -----------------------------------------------------------------------------
MyFun <- function(x)
{
  a <- 3
  b <- 3
  n <- 1000
  y <- runif(n,0,x)
  F_est <- x*mean(1/beta(a,b)*y^(a-1)*(1-y)^(b-1))
  return(F_est)
}

## -----------------------------------------------------------------------------
x <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
F_estimate <- numeric(length(x))
F_pbeta <- numeric(length(x))
for (i in 1:length(x)) {
  F_estimate[i] <- MyFun(x[i])  # 利用我们写的函数计算F(x)
  F_pbeta[i] <- pbeta(x[i],3,3) # 利用r语言自带的pbeta函数计算F(x)
}
print(round(rbind(x, F_estimate, F_pbeta), 3))

## -----------------------------------------------------------------------------
set.seed(1)
MyFunRay <- function(m=1000,antithetic=TRUE,sigma=1){  ##默认10000个抽样，使用对偶变数法，参数sigma=1
  a <- runif(m/2)
  if(antithetic)
    b <- 1-a
  else
    b <- runif(m/2)
  n <- c(a,b)
  Ray <- mean(sqrt(-2*sigma^2*log(1-n)))  ##逆变换法生成随机数，并求均值(Monte Carlo)
  return(Ray)
}

t <- 1000
Ray1 <- Ray2 <- numeric(t)
for (i in 1:t) {
  Ray1[i] <- MyFunRay(antithetic = FALSE)  ##不使用对偶变数法
  Ray2[i] <- MyFunRay()  ##使用对偶变数法
}
print(round(c(var(Ray1),var(Ray2),(var(Ray1) - var(Ray2))/var(Ray1)),9))

## -----------------------------------------------------------------------------
set.seed(1)
m <- 10000
theta.hat <- se <- numeric(3)
va <- numeric(2)
g <- function(x){
  x^2/sqrt(2*pi)*exp(-x^2/2) * (x>1)
}

x <- runif(m,1,5)
fg <- g(x)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

x <- runif(m,1,5)
f1 <- g(x)/exp(-x^2/2)  ##f1函数
theta.hat[2] <- mean(f1)
se[2] <- sd(f1)

x <- runif(m,1,5)
f2 <- g(x)/(x/sqrt(2*pi))  ##f2函数
theta.hat[3] <- mean(f2)
se[3] <- sd(f2)

va[1] <- var(abs(fg-f1))  ##f1产生的方差
va[2] <- var(abs(fg-f2))  ##f2产生的方差

rbind(theta.hat,se)
print(va)

## -----------------------------------------------------------------------------
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){
    t[j] <<- t[j]+1
    return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    t[j] <<- t[j]+1
    return(c(quick_sort(lower),a,quick_sort(upper)))}
}

m <- 100
n <- c(1e4,2*1e4,4*1e4,6*1e4,8*1e4)
y <- numeric(length(n))
for (i in 1:length(n)) {
  t <- numeric(m)
  for (j in 1:m) {
    x <- sample(1:n[i])
    quick_sort(x)  ## 快速排序算法
  }
  y[i] <- mean(t)  ## 100次模拟的平均时间
}
x <- n*log2(n)  ## 期望时间，O(nlog2(n))
ans <- lm(y~x)

plot(x,y,col = "blue",main = "Average Estimated Time & Expected Time Regression",abline(ans),cex = 1.3,pch = 16,ylab = "computation time averaged over 100 simulations",xlab = "expected time t = nlog(n)")

## -----------------------------------------------------------------------------
set.seed(1)
m <- 1e4
n <- 10
q.hat <- numeric(m)
for (i in 1:m) {
  x <- rnorm(m,0,6/n)
  q.hat[i] <- quantile(x,0.025)  ## Monte Carlo方法估算分位数
}
q <- qnorm(0.025,0,6/n)  ## 标准分位数
print(round(c(q,mean(q.hat),sd(q.hat),var(q.hat)),4))

## -----------------------------------------------------------------------------
set.seed(1)
m <- 1e4
n <- 10
q.hat <- numeric(m)
for (i in 1:m) {
  x <- rnorm(m,0,6/n)
  q.hat[i] <- quantile(x,0.05)  ## Monte Carlo方法估算分位数
}
q <- qnorm(0.05,0,6/n)  ## 标准分位数
print(round(c(q,mean(q.hat),sd(q.hat),var(q.hat)),4))

## -----------------------------------------------------------------------------
set.seed(1)
m <- 1e4
n <- 10
q.hat <- numeric(m)
for (i in 1:m) {
  x <- rnorm(m,0,6/n)
  q.hat[i] <- quantile(x,0.95)  ## Monte Carlo方法估算分位数
}
q <- qnorm(0.95,0,6/n)  ## 标准分位数
print(round(c(q,mean(q.hat),sd(q.hat),var(q.hat)),4))

## -----------------------------------------------------------------------------
set.seed(1)
m <- 1e4
n <- 10
q.hat <- numeric(m)
for (i in 1:m) {
  x <- rnorm(m,0,6/n)
  q.hat[i] <- quantile(x,0.975)  ## Monte Carlo方法估算分位数
}
q <- qnorm(0.975,0,6/n)  ## 标准分位数
print(round(c(q,mean(q.hat),sd(q.hat),var(q.hat)),4))

## -----------------------------------------------------------------------------
set.seed(1)
library(MASS)
options(digits = 3)
m <- 1000
nump <- numk <- nums <- numeric(m)
p1 <- p2 <- p3 <- numeric(m)
for (i in 1:m) {
  mean <- c(0, 1)
  sigma <- matrix(c(1, 0.15, 0.15, 1), ncol = 2, nrow = 2)
  data <- mvrnorm(n = 500, mean, sigma)
  x <- data[, 1]
  y <- data[, 2] 
  
  a1 <- cor.test(x, y, method =  "pearson")
  a2 <- cor.test(x, y, method =  "kendall")
  a3 <- cor.test(x, y, method =  "spearman")
  
  p1[i] <- a1$p.value
  p2[i] <- a2$p.value
  p3[i] <- a3$p.value
}

nump[p1 < 0.05] <- 1
numk[p2 < 0.05] <- 1
nums[p3 < 0.05] <- 1

ratio <- c(sum(nump), sum(numk), sum(nums)) / m
print(ratio)
barplot(ratio,col = c("red","green","blue"),names.arg = c("pearson","kendall","spearman"),main = "Power of a Test")

## -----------------------------------------------------------------------------
set.seed(1)
library(MASS)
m <- 1000
n <- 1000
nump <- numk <- nums <- numeric(m)
p1 <- p2 <- p3 <- numeric(m)
for (i in 1:m) {
  mean <- c(0, 1)
  sigma <- matrix(c(2, 0, 0, 3), ncol = 2, nrow = 2)
  data <- mvrnorm(n , mean, sigma)
  x <- data[, 1]
  y <- data[, 2] + runif(n, min = 0, max = 1)
  
  a1 <- cor.test(x, y, method =  "pearson")
  a2 <- cor.test(x, y, method =  "kendall")
  a3 <- cor.test(x, y, method =  "spearman")
  
  p1[i] <- a1$p.value
  p2[i] <- a2$p.value
  p3[i] <- a3$p.value
}

nump[p1 < 0.05] <- 1
numk[p2 < 0.05] <- 1
nums[p3 < 0.05] <- 1

ratio <- c(sum(nump), sum(numk), sum(nums)) / m
print(ratio)
barplot(ratio,col = c("red","green","blue"),names.arg = c("pearson","kendall","spearman"),main = "Power of a Test")

## -----------------------------------------------------------------------------
U <- (0.651-0.676)/sqrt(0.651*(1-0.651))*sqrt(10000)
U <- abs(U)
print(pnorm(U, mean = 0, sd = 1, lower.tail = TRUE))  ## 检验的p值

## ----eval=F-------------------------------------------------------------------
#  d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460,
#          -0.937, 0.779, -1.409, 0.027, -1.569);
#  d2  <- c(1.608, 1.009,  0.878,  1.600, -0.263,
#           0.680, 2.280,  2.390, 1.793, 1.468)

## -----------------------------------------------------------------------------
d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460, 
        -0.937, 0.779, -1.409, 0.027, -1.569);
d2  <- c(1.608, 1.009,  0.878,  1.600, -0.263,  
         0.680, 2.280,  2.390, 1.793, 1.468);
set.seed(12345); B <- 1e4; thetastar <- numeric(B)
theta <- mean(d1)-mean(d2);
for(b in 1:B){
  d1star <- sample(d1,replace=TRUE)
  d2star <- sample(d2,replace=TRUE)
  thetastar[b] <- mean(d1star)-mean(d2star)
}
round(c(original=theta,bias=mean(thetastar)-theta,se.boot=sd(thetastar),se.samp=sd(d1-d2)/sqrt(length(d2))),3)

## -----------------------------------------------------------------------------
set.seed(12345)
N <- 1e3
a <- 0.1
m <- 1e4
results <- matrix(0, nrow = 3, ncol = 2)
for (i in 1:m) {
  p <- runif(950)
  p <- c(p,rbeta(50,0.1,1))
  p.adj1 <- p.adjust(p,method = "bonferroni")  # Bonferroni adjusted p-values
  p.adj2 <- p.adjust(p,method = "BH")          # B-H adjusted p-values
  reject_null1 <- p.adj1 < a
  reject_null2 <- p.adj2 < a
  FWER1 <- mean(reject_null1[1:950])           # Bonferroni校正的FWER
  FWER2 <- mean(reject_null2[1:950])           # B-H校正的FWER
  FDR1 <- mean(reject_null1)                   # Bonferroni校正的FDR
  FDR2 <- mean(reject_null2)                   # B-H校正的FDR
  TPR1 <- mean(reject_null1[951:1000])         # Bonferroni校正的TPR
  TPR2 <- mean(reject_null2[951:1000])         # B-H校正的TPR
  results[,1] <- results[,1]+c(FWER1, FDR1, TPR1)
  results[,2] <- results[,2]+c(FWER2, FDR2, TPR2)
}
results <- results / m   # 取模拟的均值
colnames(results) <- c("Bonferroni correction", "B-H correction")
rownames(results) <- c("FWER", "FDR", "TPR")
print(results)

## -----------------------------------------------------------------------------
library(boot)
t(aircondit)
set.seed(12345)
B <- 1e4
x <- aircondit$hours
R <- numeric(B)
theta <- 1/mean(x)   # E(x)=1/λ
thetastar <- numeric(B)
for (b in 1:B) {
  hstar <- sample(x, replace = TRUE)
  thetastar[b] <- 1/mean(hstar)
}
round(c(MLE=theta),8)
round(c(bias=mean(thetastar)-theta,se.boot=sd(thetastar)),3)

## -----------------------------------------------------------------------------
library(boot)
t(aircondit)
set.seed(12345)
m <- 1e3;r <- 1e3;
x <- aircondit$hours
boot.mean <- function(x,i) mean(x[i])
de <- boot(data=x,statistic=boot.mean, R = r)
boot.ci(de,type=c("norm","basic","perc","bca"))

## -----------------------------------------------------------------------------
library(bootstrap)
sigma <- cov(scor)  ## 5X5的协方差矩阵
s <- eigen(sigma)  ## 求特征值
lambda <- sort(s$values, decreasing = TRUE)  ## 按特征值降序排列
theta <- lambda[1]/sum(lambda)  ## 求参数theta
## 利用jackknife方法求参数的估计值theta.jack
n <- 88
theta.jack<-numeric(n)
for(i in 1:n){
  sigma.jack <- cov(scor[-i,])
  s.jack <- eigen(sigma.jack)
  lambda.jack <- sort(s.jack$values, decreasing = TRUE)
  theta.jack[i] <- lambda.jack[1]/sum(lambda.jack)
}
bias.jack <- (n-1)*(mean(theta.jack)-theta)  ## jackknife estimates of bias
se.jack <- sqrt((n-1)*mean((theta.jack-theta)^2))  ## jackknife estimates of se
round(c(original=theta,bias.jack=bias.jack,se.jack=se.jack),3)

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic)
e1 <- e2 <- e3 <- e4 <- numeric(n)
ssr1 <- ssr2 <- ssr3 <- ssr4 <- numeric(n)
sst1 <- sst2 <- sst3 <- sst4 <- numeric(n)
ymean <- mean(magnetic)
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  # Linear model
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1
  ssr1[k] <- (magnetic[k] - yhat1)^2
  sst1[k] <- (magnetic[k] - ymean)^2
  # Quadratic model
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2
  ssr2[k] <- (magnetic[k] - yhat2)^2
  sst2[k] <- (magnetic[k] - ymean)^2
  # Exponential model
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3
  ssr3[k] <- (magnetic[k] - yhat3)^2
  sst3[k] <- (magnetic[k] - ymean)^2
  # Cubic polynomial model
  J4 <- lm(y ~ x + I(x^2) + I(x^3))
  yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] + J4$coef[3] * chemical[k]^2 + J4$coef[4] * chemical[k]^3
  e4[k] <- magnetic[k]- yhat4
  ssr4[k] <- (magnetic[k] - yhat4)^2
  sst4[k] <- (magnetic[k] - ymean)^2
}
r1 <- 1-(n-1)/(n-2)*sum(ssr1)/sum(sst1)
r2 <- 1-(n-1)/(n-2)*sum(ssr2)/sum(sst2)
r3 <- 1-(n-1)/(n-2)*sum(ssr3)/sum(sst3)
r4 <- 1-(n-1)/(n-2)*sum(ssr4)/sum(sst4)
round(c(Lin=mean(e1^2), Quad=mean(e2^2), Expo=mean(e3^2), Cubic=mean(e4^2)),3)  ## average squared prediction error
round(c(Lin_R2=r1, Quad_R2=r2, Expo_R2=r3, Cubic_R2=r4),3)  ## adjusted R^2

## -----------------------------------------------------------------------------
myFun <- function(x,y){
  n <- length(x)
  m <- length(y)
  f <- ecdf(x)
  g <- ecdf(y)
  w <- n*m/(n+m)^2*(sum((f(x)-g(x))^2) + sum((f(y)-g(y))^2))
}

attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
set.seed(12345)
R <- 999;z <- c(x, y);K <- 1:26;n<-length(x)
reps <- numeric(R);t0 <- myFun(x, y)
for (i in 1:R) {
  xy <- sample(z);
  x1 <- xy[1:n]; y1 <- xy[-(1:n)]
  reps[i] <- myFun(x1,y1)
}
p <- mean(c(t0, reps) >= t0)
print(p)
hist(reps, main = "the two-sample Cramer-von Mises test", freq = FALSE, xlab = "reps (p = 0.419)",breaks = "scott")
points(t0, 0, cex = 1, pch = 16)

## -----------------------------------------------------------------------------
x <- c(67, 70, 75, 78, 73, 89, 84, 99, 90, 91)
y <- c(22, 27, 30, 23, 25, 31, 38, 35, 34, 32)
cor0 <- cor(x, y, method = "spearman")
set.seed(12345)
R <- 1e3
n <- length(x)
z <- c(x, y)
cor1 <- numeric(R)
for (i in 1:R) {
  xy <- sample(z);
  x1 <- xy[1:n]; y1 <- xy[-(1:n)]
  cor1[i] <- cor(x1, y1, method = "spearman")
}
p <- mean(abs(c(cor0, cor1)) >= abs(cor0))
p_cor.test <- cor.test(x, y, method = "spearman")$p.value
round(c(correlation=cor0,p_per.test=p,p_cor.test=p_cor.test),3)

## -----------------------------------------------------------------------------
f <- function(x){  ## standard Cauchy density function
  return(dt(x, df = 1))
}

set.seed(12345)
m <- 1e4
x <- numeric(m)
x[1] <- rnorm(1,0,1)
k <- 0
u <- runif(m)

for (i in 2:m) {
  xt <- x[i-1]
  y <- rnorm(1,xt,1)  ## proposal distribution N(xt,1)
  num <- f(y) * dnorm(xt, mean = y, sd = 1)
  den <- f(xt) * dnorm(y, mean = xt, sd = 1)
  if(u[i] <= num/den)
    x[i] <- y      ## y被接受
  else{
    x[i] <- xt     ## y被拒绝
    k <- k+1       ## 被拒绝次数
  }
}
cat("reject probablity:",k/m)

b <- 1001          ## discard the burnin sample
y1 <- x[b:m]
a <- ppoints(100)
QR <- qcauchy(a)   ##quantiles of Cauchy
Q <- quantile(x, a)
qqplot(QR, Q, main="", xlab="Cauchy Quantiles", ylab="Sample Quantiles", xlim=c(-20,20), ylim=c(-20,20))
hist(y1, breaks="scott", main="", xlab="", freq=FALSE)
lines(QR, f(QR))

## -----------------------------------------------------------------------------
set.seed(12345)
N <- 1e4               #length of chain
burn <- 1000            #burn-in length
X <- matrix(0, N, 2)    #the chain, a bivariate sample
a <- 2
b <- 4
n <- 16

x0 <- sample(0:n,1)
y0 <- runif(1)
X[1,] <- c(x0,y0)
for (i in 2:N) {
  y <- X[i-1,2]
  X[i,1] <- rbinom(1,n,y)
  x <- X[i,1]
  X[i,2] <- rbeta(1,x+a,n-x+b)
}
b <- burn + 1
x1 <- X[b:N, ]
colMeans(x1)
cov(x1)
cor(x1)
plot(x1, main="", cex=.5, xlab=bquote(X[1]),ylab=bquote(X[2]), ylim=range(x1[,2]))

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)     #row means
  B <- n * var(psi.means)        #between variance est.
  psi.w <- apply(psi, 1, "var")  #within variances
  W <- mean(psi.w)               #within est.
  v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
  r.hat <- v.hat / W             #G-R statistic
  return(r.hat)
}

chain <- function(sigma, N, X1) {
  #generates a Metropolis chain for standard Cauchy
  #with Normal(X[t], sigma) proposal distribution
  #and starting value X1
  x <- rep(0, N)
  x[1] <- X1
  u <- runif(N)
  for (i in 2:N) {
    xt <- x[i-1]
    y <- rnorm(1, xt, sigma)    #candidate point
    r1 <- dt(y, df = 1) * dnorm(xt, y, sigma)
    r2 <- dt(xt, df = 1) * dnorm(y, xt, sigma)
    r <- r1/r2
    if (u[i] <= r) x[i] <- y else
      x[i] <- xt
  }
  return(x)
}

set.seed(12345)
sigma <- 1        #parameter of proposal distribution
k <- 4            #number of chains to generate
n <- 21000        #length of chains
b <- 12000        #burn-in length
x0 <- rnorm(k)

#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- chain(sigma, n, x0[i])

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
cat("R=",Gelman.Rubin(psi))

#plot psi for the four chains
for (i in 1:k)
  plot(psi[i, (b+1):n], type="l",xlab=i, ylab=bquote(psi))

#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)     #row means
  B <- n * var(psi.means)        #between variance est.
  psi.w <- apply(psi, 1, "var")  #within variances
  W <- mean(psi.w)               #within est.
  v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
  r.hat <- v.hat / W             #G-R statistic
  return(r.hat)
}

chain <- function(a, b, n, N, X1, Y1) {
  #generates a Metropolis chain for target distribution
  #with the conditional distributions are Binomial(n,y) and Beta(x+a,n−x+b)
  #and starting value X1,Y1
  x <- matrix(0, N, 2)
  x[1,] <- c(X1,Y1)
  for (i in 2:N) {
    Y <- x[i-1,2]
    x[i,1] <- rbinom(1,n,Y)
    X <- x[i,1]
    x[i,2] <- rbeta(1,X+a,n-X+b)
  }
  return(x)
}

set.seed(12345)
a <- 2
b <- 4
n <- 16
k <- 4            #number of chains to generate
N <- 10000        #length of chains
burn <- 2000      #burn-in length
x0 <- sample(0:n,k,replace = TRUE)
y0 <- runif(k)

#generate the chains
X <- array(0,dim = c(N,2,k))
for (i in 1:k)
  X[ , ,i] <- chain(a, b, n, N, x0[i], y0[i])

#compute diagnostic statistics
psi <- t(apply(X, 3, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))

#plot psi for the four chains
for (i in 1:k)
  plot(psi[i, (burn+1):(2*N)], type="l",xlab=i, ylab=bquote(psi))

#plot the sequence of R-hat statistics
rhat <- rep(0, (2*N))
for (j in (burn+1):(2*N))
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(burn+1):(2*N)], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
## function to compute the kth term
compute_kth_term <- function(k, d, a) {
  norm_a <- sqrt(sum(a^2))  # 计算向量a的欧几里得范数
  kth_term <- (-1)^k / exp(lgamma(k+1)) / (2^k) * (norm_a^(2*k+2)) / ((2*k+1)*(2*k+2)) * exp(lgamma((d+1)/2) + lgamma(k + 3/2) - lgamma(k + d/2 + 1))
  return(kth_term)
}

## function to compute and return the sum
compute_sum <- function(d, a, tol = 1e-10) {
  norm_a <- sqrt(sum(a^2))
  sum <- 0
  k <- 0
  while (TRUE) {
    term <- compute_kth_term(k, d, a)
    if (abs(term) < tol) {
      break    # 设置一个阈值来确定何时停止累加，当项绝对值小于1e-10时，认为其对求和的贡献可忽略不计。
    }
    sum <- sum + term
    k <- k + 1
  }
  return(sum)
}

## Evaluate the sum when a = (1,2)
a <- c(1,2)
d <- 1     # d ≥ 1 is an integer
print(compute_sum(d,a))

d <- 1e4
print(compute_sum(d,a))

## -----------------------------------------------------------------------------
k <- c(4:25, 100, 500, 1000)
n <- length(k)
A <- rep(0, n)
eps <- .Machine$double.eps ^ 0.25

for (i in 1:n) {
  fa <- function(a) {
    num1 <- sqrt(a ^ 2 * (k[i] - 1) / (k[i] - a ^ 2))
    num2 <- sqrt(a ^ 2 * k[i] / (k[i] + 1 - a ^ 2))
    pt(num2, k[i]) - pt(num1, k[i] - 1)
  }
  out <- uniroot(fa,lower = eps, upper = sqrt(k[i]-eps))
  A[i] <- out$root        # 11.4的根
}

B <- rep(0, n)            # 11.5的根
for (i in 1:n) {
  f <- function(x){
    return(exp(log(2) + lgamma(x/2) - (1/2)*(log(pi*(x-1))) - lgamma((x-1)/2)))
  }
  h <- function(a){
    num1 <- (f(k[i])*integrate(function(u)(1 + (u^2)/(k[i]-1))^(-k[i]/2), lower = 0, upper = sqrt((a^2)*(k[i]-1)/(k[i]-a^2)))$value)
    num2 <- (f(k[i]+1)*integrate(function(u)(1 + (u^2)/k[i])^(-(k[i]+1)/2), lower = 0, upper = sqrt((a^2)*k[i]/(k[i]+1-a^2)))$value)
    return(num1 - num2)
  }
  if(i<20)  # 为不同的k值寻找不同的上下界
    B[i] <- uniroot(h, lower = eps, upper = sqrt(k[i]-eps)-0.1)$root
  if(20<=i & i<=21)
    B[i] <- uniroot(h, lower = eps, upper = sqrt(k[i]-eps))$root
  if(22<=i & i<=23)
    B[i] <- uniroot(h, lower = 4, upper = sqrt(k[i]))$root
  if(i==24)
    B[i] <- uniroot(h, lower = sqrt(k[i])-0.03, upper = sqrt(k[i])-0.001)$root
  if(i==25)
    B[i] <- uniroot(h, lower = sqrt(k[i])-0.0037, upper = sqrt(k[i]))$root
}

knitr::kable(data.frame(k,A,B))

## -----------------------------------------------------------------------------
# 观察到的数据
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)

# 初始化参数
n <- length(Y) # 样本数量
tau <- 1
lambda <- 1 / mean(Y[Y <= tau]) # 初始估计λ_0

# EM算法
for (i in 1:1000) {
  # E-step: 计算每个样本的潜在分类概率（是否被删失）
  Z <- pexp(Y, rate = lambda)
  
  # M-step: 更新lambda参数
  lambda_new <- n / sum(Y * (1 - Z) + tau * Z)
  
  # 检查收敛性
  if (abs(lambda_new - lambda) < 1e-6) {
    break
  }
  
  # 更新参数和对数似然
  lambda <- lambda_new
}

# 输出结果
cat("Estimated lambda using EM algorithm:", lambda, "\n")

# 计算MLE
lambda_mle <- 1 / mean(Y[Y <= tau])
cat("Estimated lambda using MLE:", lambda_mle, "\n")

## -----------------------------------------------------------------------------
library(boot)
A1 <- rbind(c(2, 1, 1), c(1, -1, 3))
b1 <- c(2, 3)
a <- c(4, 2, 9)
simplex(a = a, A1 = A1, b1 = b1, maxi = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  formulas <- list(
#    mpg ~ disp,
#    mpg ~ I(1 / disp),
#    mpg ~ disp + wt,
#    mpg ~ I(1 / disp) + wt
#  )

## -----------------------------------------------------------------------------
library(datasets)
data(mtcars)

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

## use for loops to fit linear models to the mtcars
out1 <- vector("list", length(formulas))
for (i in seq_along(formulas)) {
  out1[[i]] <- lm(formulas[[i]], mtcars)
}

## use lapply() to fit linear models to the mtcars
out2 <- lapply(formulas, function(i) {lm(i, mtcars)})

out1
out2

## ----eval=FALSE---------------------------------------------------------------
#  bootstraps <- lapply(1:10, function(i) {
#    rows <- sample(1:nrow(mtcars), rep = TRUE)
#    mtcars[rows, ]
#  })

## -----------------------------------------------------------------------------
library(datasets)
data(mtcars)

set.seed(12345)
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

## use for loops
out1 <- vector("list", length(bootstraps))
for (i in seq_along(bootstraps)) {
  out1[[i]] <- lm(mpg ~ disp, bootstraps[[i]])
}

## use lapply() without an anonymous function
out2 <- lapply(bootstraps, lm, formula = mpg ~ disp)

out1
out2

## ----eval=FALSE---------------------------------------------------------------
#  rsq <- function(mod) summary(mod)$r.squared

## -----------------------------------------------------------------------------
library(datasets)
data(mtcars)

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

rsq <- function(mod) summary(mod)$r.squared

## use lapply() to fit linear models to the mtcars
out <- lapply(formulas, function(i) {rsq(lm(i, mtcars))})

out

## -----------------------------------------------------------------------------
library(datasets)
data(mtcars)

set.seed(12345)
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

rsq <- function(mod) summary(mod)$r.squared

## use lapply() without an anonymous function
out <- lapply(bootstraps, lm, formula = mpg ~ disp)

## extract R2
res <- numeric(length(out))
for (i in 1:length(out)) {
  res[i] <- rsq(out[[i]])
}

res

## ----eval=FALSE---------------------------------------------------------------
#  trials <- replicate(
#    100,
#    t.test(rpois(10, 10), rpois(7, 10)),
#    simplify = FALSE
#  )

## -----------------------------------------------------------------------------
set.seed(12345)
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
out <- sapply(trials, function(i) {i$p.value})
out

## -----------------------------------------------------------------------------
set.seed(12345)
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
out <- sapply(trials, '[[',3)
out

## -----------------------------------------------------------------------------
my_lapply <- function(f, df, fun_value) {
  # 检查输入列表长度是否一致
  list_lengths <- sapply(list(df), length)
  if (length(unique(list_lengths)) > 1) {
    stop("All input lists must have the same length.")
  }
  
  # 使用 Map() 并行应用函数f到所有输入
  mapped_results <- Map(f, df)
  
  # 使用 vapply() 将结果转换为向量或矩阵
  final_results <- vapply(mapped_results, function(x) x, FUN.VALUE=fun_value)
  return(final_results)
}

set.seed(12345)
df <- data.frame(replicate(6,sample(c(1:10),10,rep=T)))
fun_value <- c(min=0,Qu1=0,median=0,mean=0,Qu3=0,max=0)
my_lapply(summary, df, fun_value)

## -----------------------------------------------------------------------------
my_chisq_test <- function(x, y) {
  m <- rbind(x, y)
  # Calculate row totals
  row_total <- matrix(rowSums(m))
  
  # Calculate column totals
  col_total <- t(matrix(colSums(m)))
  
  # Calculate grand total
  grand_total <- sum(m)
  
  # Calculate expected frequencies
  m_expected <- (row_total %*% col_total) / grand_total
  
  # Calculate chi-square statistic
  chi_square <- sum((m - m_expected)^2 / m_expected)
  df <- (length(row_total) - 1) * (length(col_total) - 1)
  p <- pchisq(chi_square, df = df, lower.tail = FALSE)
  return(list(chi_square = chi_square, df = df, p_value = p))
}


x <- c(1,2,3,4,5)
y <- c(6,7,8,9,10)
my_chisq_test(x,y)

## -----------------------------------------------------------------------------
my_table <- function(x, y) {
  # Find the unique values in both vectors to determine the range for the table
  unique1 <- unique(x)
  unique2 <- unique(y)
  
  # Initialize the table
  # The table will have length(unique1) rows and length(unique2) columns
  table <- matrix(0, nrow = length(unique1), ncol = length(unique2),dimnames = list(unique1, unique2))
  
  # Fill in the table with counts
  for (i in seq_along(x)) {
    row_index <- match(x[i], unique1)
    col_index <- match(y[i], unique2)
    table[row_index, col_index] <- table[row_index, col_index] + 1
  }
  
  return(table)
}

x <- c(1,2,3,4,4)
y <- c('A','B','C','D','D')
my_table(x,y)

## ----eval=FALSE---------------------------------------------------------------
#  table <- my_table(x, y)
#  
#  # Calculate row and column totals
#  row_totals <- apply(table, 1, sum)
#  col_totals <- apply(table, 2, sum)
#  grand_total <- sum(table)

## ----eval=FALSE---------------------------------------------------------------
#  ## R function for Exercise 9.8
#  Gibbs_chain_R <- function(a, b, n, N, burn, x0, y0){
#    X <- matrix(0, N, 2)   #the chain, a bivariate sample
#    X[1,] <- c(x0,y0)
#    for (i in 2:N) {
#      y <- X[i-1,2]
#      X[i,1] <- rbinom(1,n,y)
#      x <- X[i,1]
#      X[i,2] <- rbeta(1,x+a,n-x+b)
#    }
#    b <- burn + 1
#    x1 <- X[b:N, ]
#    return(x1)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  #include <Rcpp.h>
#  using namespace Rcpp;
#  
#  // [[Rcpp::export]]
#  NumericMatrix Gibbs_chain_C(double a, double b, int n, int N, int burn, int x0, double y0) {
#    NumericMatrix X(N, 2);
#    X(0,0) = x0;
#    X(0,1) = y0;
#    int x;
#    double y;
#    for(int i = 1; i < N; i++){
#      y = X(i-1,1);
#      X(i,0) = rbinom(1,n,y)[0];
#      x = X(i,0);
#      X(i,1) = rbeta(1,x+a,n-x+b)[0];
#    }
#    NumericMatrix x1(N-burn, 2);
#    for(int i = burn; i < N; i++){
#      x1(i - burn,0) = X(i,0);
#      x1(i - burn,1) = X(i,1);
#    }
#    return x1;
#  }

## -----------------------------------------------------------------------------
Gibbs_chain_R <- function(a, b, n, N, burn, x0, y0){
  X <- matrix(0, N, 2)   #the chain, a bivariate sample
  X[1,] <- c(x0,y0)
  for (i in 2:N) {
    y <- X[i-1,2]
    X[i,1] <- rbinom(1,n,y)
    x <- X[i,1]
    X[i,2] <- rbeta(1,x+a,n-x+b)
  }
  b <- burn + 1
  x1 <- X[b:N, ]
  return(x1)
}

library(SA24168406)
library(Rcpp)
library(microbenchmark)
set.seed(12345)
N <- 1e4                #length of chain
burn <- 1000            #burn-in length
a <- 2
b <- 4
n <- 16
x0 <- sample(0:n,1)
y0 <- runif(1)

xR <- Gibbs_chain_R(a, b, n, N, burn, x0, y0)
xC <- Gibbs_chain_C(a, b, n, N, burn, x0, y0)
qqplot(xR, xC, main = "QQ Plot Comparison", xlab="R Quantiles", ylab="C Quantiles")

ts <- microbenchmark(gibbR=Gibbs_chain_R(a, b, n, N, burn, x0, y0),gibbC=Gibbs_chain_C(a, b, n, N, burn, x0, y0))
summary(ts)[,c(1,3,5,6)]

