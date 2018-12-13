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


#' Estimate observable probability
#' 
#' 
#' @export
estimate_observable_probability <- function(N, block_rate, block_size, band_width = 512, gamma_shape = 2)
{
  if (block_rate <= 0) return(numeric(N))
  
  delay <- block_size * 1024 * 8 / band_width
  scale <- delay/(gamma_shape-1)
  pgamma((1:N)/block_rate, shape = gamma_shape, scale = scale, lower.tail = T, log.p = F)
}


#' Estimate reachable probability
#' 
#' 
#' @export
estimate_reachable_probability <- function(N, block_rate, block_size, band_width = 512, gamma_shape = 2)
{
  p <- numeric(N)
  if (block_rate <= 0) return(p)
  
  p0 <- estimate_observable_probability(N, block_rate, block_size, band_width, gamma_shape)
  p[1] <- p0[1]
  if (N >= 2)
  {
    for (i in 2:N)
    {
      p[i] <- 1 - (1 - p0[i]) * prod(1 - p0[(i-1):1] * p[1:(i-1)])
    }
  }
  p
}


#' Estimate tip probability
#' 
#' 
#' @export
estimate_tip_probability <- function(N, block_rate, block_size, band_width = 512, gamma_shape = 2)
{
  p <- numeric(N)
  if (block_rate <= 0) return(p)
  
  p0 <- estimate_observable_probability(N, block_rate, block_size, band_width, gamma_shape)
  p1 <- estimate_reachable_probability(N - 1, block_rate, block_size, band_width, gamma_shape)
  p[1] <- 1
  if (N >= 2)
  {
    for (i in 2:N)
    {
      p[i] <- prod(1 - p0[(i-1):1] * p1[1:(i-1)])
    }
  }
  p
}


#' Estimate block distance
#' 
#' 
#' @export
estimate_block_distance <- function(N, block_rate, block_size, band_width = 512, gamma_shape = 2)
{
  d <- rep(Inf, N)
  if (block_rate <= 0) return(d)

  p0 <- estimate_observable_probability(N, block_rate, block_size, band_width, gamma_shape)
  p1 <- estimate_reachable_probability(N, block_rate, block_size, band_width, gamma_shape)
  p2 <- estimate_tip_probability(N, block_rate, block_size, band_width, gamma_shape)
  p0 <- p0 * p2

  d[1] <- 1
  if (N >= 2)
  {
    for (i in 2:N)
    {
      p <- p0[(i-1):1] * p1[1:(i-1)]
      p <- p / cumsum(p[(i-1):1])[(i-1):1]
      p <- c(1, cumprod(1 - p))[1:(i-1)] * p
      p <- c(p0[i], (1 - p2[i]) * p)
      d[i] <- sum(p * c(1, d[1:(i-1)] + 1)) / sum(p)
    }
  }
  d
}


#' Estimate maximum block discord
#' 
#' 
#' @export
estimate_max_block_discord <- function(block_rate, block_size, band_width = 512, gamma_shape = 2, p = 0.9999)
{
  if (block_rate <= 0) return(0)
  
  k <- 10
  repeat
  {
    reach_probs <- estimate_reachable_probability(k, block_rate, block_size, band_width, gamma_shape)
    if (reach_probs[k] >= p) break
    k <- k * 2
  }
  N <- min(which(reach_probs >= p))
  d <- estimate_block_distance(N, block_rate, block_size, band_width, gamma_shape)
  ceiling(d[N] * 2)
}
