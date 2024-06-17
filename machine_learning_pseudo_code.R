getwd()
setwd("D:/R/Class")

str(iris)

data <- iris
D <- data[, -5]  # iris 데이터의 마지막 열(Species) 제외

write.csv(D, "IRIS_sample_D.csv")


# 클러스터의 수 k를 설정
k <- 3

# 유클리디안 거리 계산 함수
euclidean_distance <- function(a, b) {
  return(sqrt(sum((a - b)^2)))
}

# K-Means 클러스터링 함수
kmeans_custom <- function(D, k) {
  m <- nrow(D)
  n <- ncol(D)
  
  set.seed(123)
  
  # 1. Randomly select k samples as the initial mean vectors (초기화)
  mu <- D[sample(1:m, k), ]
  
  # 2. Repeat (반복적인 최적화)
  repeat {
    
    # 3. C_i = Null (1<=i<=k)
    clusters <- vector("list", k)
    
    # 클러스터 분할
    
    # 4. for j=1,2,...,m do
    for (j in 1:m) {
      distances <- numeric(k)
      
      # 5. Compute the distance between sample x_j and each mean vector
      for (i in 1:k) {
        distances[i] <- euclidean_distance(D[j, ], mu[i, ])
      }
      
      # 6. According to the nearest mean vector, decide the cluster label
      cluster_idx <- which.min(distances)
      clusters[[cluster_idx]] <- rbind(clusters[[cluster_idx]], D[j, ])
    }
    
    # 7. Move x_j to the corresponding clusters
    old_mu <- mu
    
    # 8. end for
    
    # (평균 벡터)
    # 9. for i=1, 2, ..., k do
    for (i in 1:k) {
      
      # 10. Compute the updated mean vectors
      if (length(clusters[[i]]) > 0) {
        
        # 12. Update the current mean vector
        new_centroid <- colMeans(clusters[[i]], na.rm = TRUE)
        mu[i, ] <- new_centroid
      }
    }
    
    # 13. else
    # 14. Leave the current mean vector unchanged
    if (all(old_mu == mu)) {
      
      # 15. end if
      break
    }
  }
  
  # 16-17. end for; until All mean vectors remain unchanged
  
  # Return the output
  return(list(clusters = clusters, mu = mu))
}


# K-means 클러스터링 수행
result <- kmeans_custom(D, k)

# 결과 확인
print("클러스터 중심:")
print(result$mu)

print("클러스터 할당:")
for (i in 1:k) {
  cat(sprintf("클러스터 %d:\n", i))
  print(result$clusters[[i]])
}
