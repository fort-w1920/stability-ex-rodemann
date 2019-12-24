library(latex2exp)

plot_stability_paths <- function(stability_paths) {
  checkmate::assert_matrix(stability_paths)
  apply(stability_paths, c(1,2), FUN = checkmate::assert_number, lower = 0, upper = 1) 
  
  
  plot(y = stability_paths[,1], x = 0:8, ylim = c(0,1), type = "b", 
       xlab = " # Covariates", ylab = TeX('$\\Pi$'), main = "Stability Paths")
  #apply(stability_paths[,-1], 1, points)
  for (i in 2:nrow(matrix_selected)) {
    points(y = stability_paths[,i], x = 0:8, type = "b", col = sample(1:20,1))
  }
}
