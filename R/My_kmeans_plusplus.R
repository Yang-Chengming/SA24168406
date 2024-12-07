#' @title Calculate the center of K-mean++ cluster using R
#' @description Calculate the centers of K-mean++ cluster and show the clustering results by a figure using R
#' @importFrom stats runif
#' @importFrom graphics points
#' @useDynLib SA24168406
#' @param data the data for clustering
#' @param k the number of the centers
#' @param max_iter the maximum number of iterations for clustering, the default value is 100.
#' @return a list of the centers and clusters \code{n}
#' @examples
#' \dontrun{
#'     set.seed(123)
#'     data <- matrix(rnorm(100 * 2), nrow = 100)
#'     k <- 3
#'     result <- My_kmeans_plusplus(data, k)
#'     print(result$centers)
#'     print(result$clusters)
#' }
#' @export
My_kmeans_plusplus <- function(data, k, max_iter = 100) {
  # 数据标准化
  data <- scale(data)

  # 随机选择第一个质心
  centers <- matrix(data[sample(nrow(data), 1), ], nrow = 1)

  # 初始化质心列表
  for (iter in 2:k) {
    # 计算每个点到最近质心的距离
    dist_to_nearest <- rep(-1, nrow(data))
    for (i in 1:nrow(data)) {
      dist_to_nearest[i] <- sqrt(sum((data[i, ] - centers[1, ]) ^ 2))
      for (j in 1:nrow(centers)) {
        temp_mindist <- sqrt(sum((data[i, ] - centers[j, ]) ^ 2))
        dist_to_nearest[i] <- min(dist_to_nearest[i], temp_mindist)
      }
    }

    # 选择下一个质心
    probs <- dist_to_nearest / sum(dist_to_nearest)
    cumprobs <- cumsum(probs)
    rand_val <- runif(1)
    select <- which(cumprobs >= rand_val)[1]
    centers <- rbind(centers, data[select, ])
  }

  # 初始化聚类分配
  clusters <- rep(1:k, length.out = nrow(data))
  dists <- rep(-1, nrow(centers))

  for (iter in 1:max_iter) {
    # 分配数据点到最近的质心
    for (i in 1:nrow(data)) {
      for (j in 1:nrow(centers)) {
        dists[j] <- sqrt(sum((data[i, ] - centers[j, ]) ^ 2))
      }
      clusters[i] <- which.min(dists)
    }

    # 更新质心
    new_centers <- matrix(nrow = k, ncol = ncol(data))
    for (j in 1:k) {
      points_in_cluster <- data[clusters == j, ]
      if (nrow(points_in_cluster) > 0) {
        new_centers[j, ] <- colMeans(points_in_cluster)
      }
    }

    # 检查质心是否变化
    if (all(new_centers == centers)) {
      break
    }
    centers <- new_centers
  }

  # 聚类结果的可视化展示
  plot(
    data,
    col = clusters,
    pch = 19,
    frame = FALSE,
    main = bquote("K-means++ with k =" ~ .(k)),
    xlab = "x",
    ylab = "y"
  )
  points(centers,
         col = 1:nrow(centers),
         pch = 8,
         cex = 3)

  list(centers = centers, clusters = clusters)
}
