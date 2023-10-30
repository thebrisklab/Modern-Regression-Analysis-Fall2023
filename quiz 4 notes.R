a = rbinom(100,size=1,prob=0.6)

mean(a)

summary(glm(a~1,family=binomial))
exp(0.4055)/(1+exp(0.4055))

##
# Notes on multicollinearity and joint testing
nsample = 50
x1 = rnorm(nsample)
x2 = 0.1*rnorm(nsample)+x1
x3 = rnorm(nsample)
cor(x1,x2)
temp = x1+x2+x3 # log odds of a success, all equal to 1
proby = exp(temp)/(1+exp(temp)) # expit (inverse logistic) link function
y = rbinom(n=nsample,prob=proby,size=1)
model0 = glm(y~x1+x2+x3,family=binomial)

summary(model0)

car::vif(model0)
model.reduced = glm(y~x3,family=binomial)
anova(model.reduced,model0,test='LRT')

# you can calculate the variance of the joint effects
# to see how much it is inflated:
# grab var of x2 and x3 
selectx1x2 = c(0,1,1,0) # var(a'beta) = a'cov(beta)a
vcov(model0)
## this is the variance of the sum of the beta1 and beta2
t(selectx1x2)%*%vcov(model0)%*%selectx1x2

## look at the covariance between them -- covariance typically negative
vcov(model0)[2,3] # cov(\hat{\beta_1},\hat{\beta_2})

# wald test of the difference,
# similar to lrt, although can be differences with small samples
zstat = (coef(model0)%*%selectx1x2)/sqrt(t(selectx1x2)%*%vcov(model0)%*%selectx1x2)
2*(1-pnorm(abs(zstat)))

   