library(rstan)



stan.basketball =  list( 
  N = nrow(basketball),
  p = 5,
  y = basketball$y,
  Composite_Rating = as.numeric(basketball$Composite.Rating),
  Age = as.integer(basketball$Age),
  Is_Guard = as.integer(basketball$Is.Guard),
  Is_Big = as.integer(basketball$Is.Big)
)
str(stan.basketball)

rstan_options(javascript = FALSE, auto_write  = FALSE)
options(mc.cores = parallel::detectCores())

fileName = "~/Bayesian Statistics with RSCI Hoops/Code/stan_code.stan"
stan_code = readChar(fileName, file.info(fileName)$size)
cat(stan_code)
## Run Stan
resStan = stan(model_code = stan_code, data = stan.basketball)

# Show traceplot
#traceplot(resStan, pars = c("beta","sigma"), inc_warmup = TRUE)


print(resStan, pars = "beta", probs = c(0.025, 0.5, 0.975))

#pairs(resStan)

post_beta=As.mcmc.list(resStan, pars = c("beta"))
#plot(post_beta)

save.image(file = "~/Bayesian Statistics with RSCI Hoops/Env/MCMC_env.RData")