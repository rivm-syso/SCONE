basic_bootstrap <- function(data, n = 1000, alpha = 0.95) {
  res <- replicate(n, mean(sample(data %>% pull(names(data)), replace = TRUE)))
  
  tibble(probs = c(0.5*(1-alpha), 0.5, 1-0.5*(1-alpha))) %>% 
    mutate(!!paste0(names(data), "_bs") := quantile(res, probs))
  
}
