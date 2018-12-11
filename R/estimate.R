#' Estiamte the growth rate of blockchain
#'
#'
#' @export
estimate_growth_rate <- function(block_rate, block_size, band_width = 512, min_term = Inf, log_eps = -1000, gamma_shape = 2)
{
  if (block_rate <= 0) return(0)
  
  delay <- block_size * 1024 * 8 / band_width
  scale <- delay/(gamma_shape-1)
  k <- 10
  p1 <- pgamma((0:(k-1))/block_rate, shape = gamma_shape, scale = scale, lower.tail = F, log.p = T)
  while (k < min_term && p1[length(p1)] > log_eps)
  {
    p1 <- c(p1, pgamma((k:(2*k-1))/block_rate, shape = gamma_shape, scale = scale, lower.tail = F, log.p = T))
    k <- k * 2
  }
  p0 <- pgamma((1:k)/block_rate, shape = gamma_shape, scale = scale, lower.tail = T, log.p = T)
  s <- (block_rate/(1:k)) * exp(p0 + cumsum(p1))
  sum(s)
}


#' Estimate block distance
#' 
#' 
#' @export
estimate_block_distance <- function(block_sequential_distance, block_rate, growth_rate)
{
  block_sequential_distance * growth_rate / block_rate
}

#' Estimate reachable probability
#' 
#' 
#' @export
estimate_reachable_probability <- function(N, block_rate, block_size, band_width = 512, gamma_shape = 2)
{
  p1 <- numeric(N)
  if (block_rate <= 0) return(p1)
  
  delay <- block_size * 1024 * 8 / band_width
  scale <- delay/(gamma_shape-1)
  p0 <- pgamma((1:N)/block_rate, shape = gamma_shape, scale = scale, lower.tail = T, log.p = F)
  
  p1[1] <- p0[1]
  if (N >= 2)
  {
    for (i in 2:N)
    {
      p1[i] <- p0[i] + (1 - p0[i]) * (1 - prod(1 - p1[1:(i-1)] * p1[(i-1):1]))
    }
  }
  p1
}
