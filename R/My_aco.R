# 计算两个城市之间的距离
distance <- function(city1, city2) {
  dist <- sqrt((city1$x - city2$x) ^ 2 + (city1$y - city2$y) ^ 2)
}

# 初始化信息素矩阵
initPheromones <- function(n) {
  matrix(1, nrow = n, ncol = n)
}

#' @title Ant Colony Optimization using R
#' @description Calculate the best path with ACO algorithm and show the iterative process by a figure using R
#' @importFrom stats runif
#' @useDynLib SA24168406
#' @param cities the coordinates of the cities
#' @param AntsNum the number of the ants
#' @param max_iter the maximum number of iterations for optimizing, the default value is 100.
#' @param alpha the pheromone factor
#' @param beta the heuristic factor
#' @param rho the pheromone volatile factor
#' @param Q the pheromone increment
#' @return a list of the optimal path in each iteration and its length and the best path with its length \code{n}
#' @examples
#' \dontrun{
#'     set.seed(123)
#'     n <- 20
#'     alpha <- 1
#'     beta <- 2
#'     rho <- 0.5
#'     Q <- 100
#'     AntsNum <- 10
#'     max_iter <- 100
#'     cities <- data.frame(x = runif(n, min = 0, max = 100),
#'                          y = runif(n, min = 0, max = 100))
#'     result <- My_aco(cities, AntsNum, max_iter, alpha, beta, rho, Q)
#'     print(result$min_bestPath)
#'     print(result$min_bestLength)
#'     plot(
#'       result$bestLength,
#'       type = "l",
#'       main = "蚁群算法用例",
#'       xlab = "迭代次数",
#'       ylab = "最佳路径长度"
#'     )
#' }
#' @export
# 蚁群算法
My_aco <- function(cities,
                   AntsNum,
                   max_iter = 100,
                   alpha,
                   beta,
                   rho,
                   Q) {
  n <- nrow(cities)
  pheromones <- initPheromones(n)
  bestPath <- NULL
  bestLength <- NULL

  for (iter in 1:max_iter) {
    # 将蚂蚁随机分配到各个城市，然后开始周游
    paths <- replicate(AntsNum, {
      path <- sample(1:n, 1)
      for (i in 1:(n - 1)) {
        # 计算下一个城市的概率
        currentCity <- path[i]
        probabilities <- (pheromones[currentCity, ] ^ alpha) *
          ((1 / distance(cities[currentCity, ], cities)) ^ beta)
        probabilities[path] <- 0  # 已访问城市的概率设为0
        # 轮盘赌算法选择下一个将要访问的城市
        probabilities <- probabilities / sum(probabilities)
        cumprobs <- cumsum(probabilities)
        rand_val <- runif(1)
        nextCity <- which(cumprobs >= rand_val)[1]
        path <- c(path, nextCity)
      }
      path
    })

    # 更新信息素
    pathLengths <- rep(0, AntsNum)
    pheromones_change <- matrix(0, nrow = n, ncol = n)
    for (i in 1:AntsNum) {
      pathLengths[i] <- sum(sapply(1:(n - 1), function(x)
        distance(cities[paths[x, i], ], cities[paths[x + 1, i], ])))
      for (j in 1:(n - 1)) {
        pheromones_change[paths[j, i], paths[j + 1, i]] <- pheromones_change[paths[j, i], paths[j +
                                                                                                  1, i]] + Q / pathLengths[i]
      }
    }
    pheromones <- (1 - rho) * pheromones + pheromones_change

    # 找到并记录每次迭代的最佳路径及最终的最优路径和长度
    bestLength_temp <- min(pathLengths)
    bestPath_temp <- paths[, which.min(pathLengths)]
    bestLength <- c(bestLength, bestLength_temp)
    bestPath <- rbind(bestPath, bestPath_temp)
  }
  min_bestLength <- min(bestLength)
  min_bestPath <- bestPath[which.min(bestLength), ]
  list(
    bestPath = bestPath,
    bestLength = bestLength,
    min_bestPath = min_bestPath,
    min_bestLength = min_bestLength
  )
}
