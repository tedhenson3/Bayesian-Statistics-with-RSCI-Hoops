library(rethinking)

flist <- alist(
  `y` ~ dnorm(mu, sigma),
  mu <- a+
  b.Comp*Composite.Rating+
  b.espn*ESPN+
  # b.vc*VC + 
  # b.247*`247Sports`+
  b.Rivals*Rivals +
  b.C*Is.C + 
  b.Big*Is.Big+
  b.Forward*Is.Forward+
  b.Wing*Is.Wing+
  b.Guard*Is.Guard+
  b.Age*Age,
  
  a ~ dnorm(4,1), 
  b.Comp ~ dnorm(1, 1),
  b.Rivals ~ dnorm(1,1),
  b.espn ~ dnorm(1,1), 
  # b.vc ~ dnorm(1/4,1/4),
  # b.247 ~ dnorm(1/4,1/4),
  
  b.C ~ dnorm(0, 1),
  b.Big ~ dnorm(0, 1),
  b.Forward ~ dnorm(0, 1),
  b.Wing ~ dnorm(0, 1),
  b.Guard ~ dnorm(0, 1),
  b.Age ~ dnorm(1,1),
  sigma ~ dexp(1)
)

fit <- quap(flist, 
            #start=list(a=0,b=-1/2,sigma=20) , 
            data=basketball)

prior.sims = extract.prior(fit)
# plot(prior.sims$a)
# plot(prior.sims$sigma)
# 
# plot(prior.sims$b.Comp)
# plot(prior.sims$b.C)


summary(fit)

mu = link(fit)

mu_mean = apply(mu, 2, mean)
mu_PI = apply(mu, 2, PI)

ws_sim = sim(fit)
ws_pi = apply(ws_sim, 2, PI)

# plot(mu_mean ~ basketball$y,
#      col = rangi2,
#      ylim = range(mu_PI),
#      xlab = 'Observed y',
#      ylab = 'Predicted y')
# abline(a = 0, b = 1, lty = 2)
# for(i in 1:nrow(basketball)) lines(rep(basketball$y[i],2),
#                                        mu_PI[,i],
#                                        col=rangi2)
# identify(x=basketball$y,
#          y=mu_mean,
#          labels=basketball$Player)