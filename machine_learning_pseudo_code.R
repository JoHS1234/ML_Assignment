getwd()
setwd("D:/R/Class")

str(iris)

data <- iris
D <- data[, -5]  # iris �������� ������ ��(Species) ����

write.csv(D, "IRIS_sample_D.csv")


# Ŭ�������� �� k�� ����
k <- 3

# ��Ŭ����� �Ÿ� ��� �Լ�
euclidean_distance <- function(a, b) {
  return(sqrt(sum((a - b)^2)))
}

# K-Means Ŭ�����͸� �Լ�
kmeans_custom <- function(D, k) {
  m <- nrow(D)
  n <- ncol(D)
  
  set.seed(123)
  
  # 1. Randomly select k samples as the initial mean vectors (�ʱ�ȭ)
  mu <- D[sample(1:m, k), ]
  
  # 2. Repeat (�ݺ����� ����ȭ)
  repeat {
    
    # 3. C_i = Null (1<=i<=k)
    clusters <- vector("list", k)
    
    # Ŭ������ ����
    
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
    
    # (��� ����)
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


# K-means Ŭ�����͸� ����
result <- kmeans_custom(D, k)

# ��� Ȯ��
print("Ŭ������ �߽�:")
print(result$mu)

print("Ŭ������ �Ҵ�:")
for (i in 1:k) {
  cat(sprintf("Ŭ������ %d:\n", i))
  print(result$clusters[[i]])
}