library(MASS)
library(pscl)
library(msm)
require(snowfall)
set.seed(666)

# initialize parallel cores.
sfInit( parallel=TRUE, cpus=12)


gen.sim <- function(df){
    z <- rnorm(df['nobs'],0,1)
    x <- rnorm(df['nobs'],0,1)
    u <- rnorm(df['nobs'],0,1)
 #generate count data
    log.mu <- 2*x + u
    y.count <- floor(exp(log.mu))

   # generate bernoulli data
    z1 <- 4*z + df['th']
    prob <- exp(z1)/(1+exp(z1))
    y.logit <- rbinom(df['nobs'],size=1,prob=prob)

    # zero-inflated poisson
    y <- ifelse(y.logit==1, y.count,y.logit)
    m1 <- zeroinfl(y ~ x | z)
    m1.x <- summary(m1)$coefficients$count['x','Estimate']-2
    # zero-inflated without a z
    m4 <- zeroinfl(y ~ x | x)
    m4.x <- summary(m4)$coefficients$count['x','Estimate']-2

    # poisson
    m2 <- glm(y ~ x, family = "poisson")
    m2.x <- summary(m2)$coefficients['x','Estimate']-2
    # log linear with plus 1
    y.plus1 <- y +1
    m3 <- lm(log(y.plus1) ~ x)
    m3.x <- exp(summary(m3)$coefficients['x','Estimate'])-2
    #
    #
    # negative binomial
#    m5.x <- tryCatch(nb1(y ~ x), error=function(e) NA)
    m5 <- glm(y ~ x, family=negative.binomial(2))
    m5.x <- summary(m5)$coefficients['x','Estimate']-2
    # hurdle model
    m6 <- hurdle(y ~ x)
#    m5 <- glm(y ~ x, family=negative.binomial(2))
    m6.x <- summary(m6)$coefficients$count['x','Estimate']-2


    return(c(zip1=m1.x, poisson=m2.x, log.linear=m3.x, zip2=m4.x, nb=m5.x, hurdle=m6.x))
}


# set parameter space
sim.grid = seq(1,100,1)
th.grid = seq(-4, 4, 2)
nobs.grid = ceiling(exp(seq(4,9,1))/100)*100

data.grid <- expand.grid(nobs.grid, sim.grid, th.grid)
names(data.grid) <- c('nobs', 'nsim','th')

# export functions to the slaves
# export data to the slaves if necessary
sfExport(list=list("gen.sim"))

# export function to the slaves
sfLibrary(msm)
sfLibrary(pscl)

results <- data.frame(t(sfApply(data.grid, 1, gen.sim)))

# stop the cluster
sfStop()

forshiny <- cbind(data.grid, results)
# write out for use in shiny.
write.csv(forshiny, 'results.csv')
