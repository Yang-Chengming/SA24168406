## -----------------------------------------------------------------------------
library(SA24168406)
set.seed(123)
data <- matrix(rnorm(100 * 2), nrow = 100)
k <- 3
result <- My_kmeans_plusplus(data, k)
print(result$centers)
print(result$clusters)

## -----------------------------------------------------------------------------
library(SA24168406)
set.seed(123)
n <- 20
alpha <- 1
beta <- 2
rho <- 0.5
Q <- 100
AntsNum <- 10
max_iter <- 100
cities <- data.frame(x = runif(n, min = 0, max = 100),
                     y = runif(n, min = 0, max = 100))
result <- My_aco(cities, AntsNum, max_iter, alpha, beta, rho, Q)
print(result$min_bestPath)
print(result$min_bestLength)
plot(
  result$bestLength,
  type = "l",
  main = "蚁群算法用例",
  xlab = "迭代次数",
  ylab = "最佳路径长度"
)

## -----------------------------------------------------------------------------
library(Rcpp)
data <- data.frame(id = 1:9, category = rep(c("A", "B", "C"), each = 3), value = rnorm(9))
samples_per_category <- list(A = 1, B = 2, C = 1)  # 每个类别的样本数量
sampled_data <- My_stratified_samplingC(data, samples_per_category)
sampled_data

## -----------------------------------------------------------------------------
set.seed(12345)
N <- 1e4
burn <- 1000
a <- 2
b <- 4
n <- 16
x0 <- sample(0:n,1)
y0 <- runif(1)
xC <- Gibbs_chain_C(a, b, n, N, burn, x0, y0)

