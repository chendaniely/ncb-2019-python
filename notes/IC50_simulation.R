library(tidyverse)

fpl <- function(x, IC50, scal, sd = .1) {
  tibble(
    assay = (1/(1 + exp((IC50 - x)/scal))) + rnorm(length(x), sd = sd),
    conc = 10^x
  )
}

sim_IC50 <- function(n = 10, reps = 3) {
  doses <- -5:5 # on log-10 scale
  
  dat <- NULL
  
  for (i in 1:n) {
    IC50_param <- rnorm(1, mean = 1, sd = 2)
    scal_param <- exp(rnorm(1.5, sd = .5))
    for (j in 1:reps) {
      dat <- 
        fpl(doses, IC50 = IC50_param, scal = scal_param) %>% 
        mutate(compound = i, replicate = j) %>% 
        bind_rows(dat)
    }
  }
  dat
}

ggplot(sim_IC50()) +
  aes(x = conc, y = assay, group = replicate, col = factor(replicate)) +
  geom_point() + 
  scale_x_log10() + 
  facet_wrap(~compound)
