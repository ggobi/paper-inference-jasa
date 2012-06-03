# The complete R codes for JASA Paper
# Last modified by Mahbub on April 5, 2012.

library(ggplot2)
library(plyr)
library(reshape)


# =================== mathematical test t distribution  ===================

t <- seq(-4, 4, by = 0.1)
density_t <- dt(t, df = 15)
qplot(t, density_t, geom = "line") + geom_vline(xintercept = 2.5) + ylab("Density")
ggsave("../images/stat_mathematical_test.pdf",height=2,width=2.2)


# ===== computation of p-values =============

get_pval <- function(m,K,x){
	p <- pbinom(size=K,prob=1/m,q=x)
	px <- dbinom(size=K,prob=1/m,x=x)
	pval <- 1-p + px
	return(pval)
}

pvals <- NULL
for (K in 1:5){
	for (x in 0:K){
		pval <- get_pval(m=20,K=K,x=x)
		pvals <- rbind(pvals,c(m=20,K=K,x=x,pval=round(pval,4)))
	}
}




# === plotting test stat and lineup for categorical variable

# generating data
set.seed(29)

n <- 300
beta <- 10
sigma <- 12	
x1 <- rpois(n,lambda=20)
Xk <- factor(sample(c("A","B"), size=n, replace=T))
cnd <- sample(c("A","B"), size=1)
y <- round(5 + 15 * x1 + beta * (Xk==cnd) + rnorm(n=n, mean=0, sd=sigma))
#qplot(x1,y, colour=factor(Xk)) + geom_smooth(method="lm", se=F, size=1)


# plotting test stat for testing b2=0
fit <- lm(y~x1 )
fit.stat <- summary(fit)
sigma_hat <- fit.stat$sigma
obs.residuals <- as.vector(resid(fit)) 

qplot(Xk, obs.residuals,colour=Xk,geom="boxplot", ylab="Residual") + 
      xlab(expression(X[k])) + labs(colour=expression(X[k]))
ggsave("../images/stat_category.pdf",height=2,width=2.75)



# plotting lineup for testing b2=0
loc <- sample(1:20, size=1)  # location of observed plot
simdat <- matrix(rnorm(n=20*n,mean=0, sd=sigma_hat),ncol=20)
simdat[,loc] <- obs.residuals
pdat <- data.frame(simdat,Xk)
names(pdat) <- c(1:20,"Xk")
pdat.m <- melt(pdat,id="Xk")

qplot(Xk, value, data=pdat.m ,colour=Xk,,geom="boxplot", ylab="Residual") + 
     facet_wrap(~variable)+ xlab(expression(X[k])) + 
     labs(colour=expression(X[k]))
ggsave("../images/lineup_category.pdf",height=7,width=7.5)
ggsave("../images/lineup_category_small.pdf",height=4.5,width=5) # for comparizon table

# === plotting lineup for continuous variable variable (turk2)

generate_turk2_lineup <- function(n,beta,sigma){
	#n <- 100; a <- 6; b <- 2; sigma <- 12
	a <- 6
	b <- beta*sign(rnorm(1,0,1))
	x <- subset(read.csv("../data/Xdata.csv"),N==n)[,1]
	y <- a + b*x + rnorm(n=n, mean=0, sd=sigma)

	fit <- lm(y~1) # fitting model without slope
	fit.stat <- summary(fit)

	sim <- matrix(rnorm(n=n*20, mean = fit$coefficient[[1]], sd=fit.stat$sigma),ncol=20)
	loc <- sample(1:20, size=1)
	sim[,loc] <- y
	sim <- data.frame(x,sim)
	colnames(sim) <- c("X",1:20)
	m.sim <- melt(sim,id=c("X"))
	p <- qplot( X, value, data=m.sim, xlab=expression(X[k]),geom="point", ylab="Y", alpha=I(.2))
	p <- p + geom_smooth(method="lm", se=F, size=1) + facet_wrap(~variable)

	fit1 <- lm(y~x)
      fit1.stat <- summary(fit1)
      pval <-  fit1.stat$coefficients[2,4]
	res <- list(result = data.frame(n,b,sigma,pval,loc),p=p,dat_used=sim)
	return(res)
}

l2 <- generate_turk2_lineup(n=100,beta=1.25,sigma=5)
ggsave(plot=l2$p, file="../images/lineup_continuous.pdf",height=7,width=7.5)

# ==== plotting test statistics --------

generate_stat_slope <- function(n,beta,sigma){
	#n <- 100; a <- 6; b <- 2; sigma <- 12
	a <- 6
	x <- rnorm(n=n, mean=0, sd=1)
	y <- a + beta*x + rnorm(n=n, mean=0, sd=sigma)
	fit <- lm(y~1)
	p <- qplot(x, resid(fit), xlab=expression(X[k]),geom="point", ylab="Residual", alpha=I(.3))
	p <- p + geom_smooth(method="lm", se=F, size=1)
	return(p)
}
p <- generate_stat_slope(n=100,beta=2.5,sigma=5)
ggsave(plot=p, file="../images/stat_beta_k.pdf",height=2,width=2.05)




# =================== Turk1 data analysis  ================================

dat <- read.csv("../data/feedback_data_turk1_50p.txt")

# ------function to calculate UMP power -----------------------

calculate_ump_power1 <- function(beta, n, sigma) {
    # n <- 100
    # sigmasq <- 12^2
    alpha <- 0.05
    sigmasq <- sigma^2
    beta_not <- 0
    se_beta <- sqrt(sigmasq/(n * (0.5^2)))  # refer to docs derivation of power
    mu <- beta/se_beta
    alpha <- alpha/2
    t_n <- qt(p = 1 - alpha, df = n - 3)
    res <- pt(q = -t_n, df = n - 3, ncp = mu) - pt(q = t_n, df = n - 3, 
        ncp = mu) + 1
    return(res)
}
calculate_ump_power1(3, 100, 5)

# ---------------------- fitting loess smoother  ------------

get_smooth_power <- function(dat.n, test = "Empirical") {
    betas <- seq(0.01, 16, by = 0.2)
    dat_smooth <- NULL
    for (s in c(5, 12)) {
        for (n in c(100, 300)) {
            dats <- subset(dat.n, sigma == s & sample_size == n)
            dats$y <- as.numeric(dats$response)
            fit <- loess(y ~ beta, data = dats, span = 1 + (n == 100) * 
                0.2)
            dat_smooth <- rbind(dat_smooth, cbind(betas, predict(fit, 
                betas), n, s))
        }
    }
    colnames(dat_smooth) <- c("beta", "pow", "sample_size", "sigma")
    dat_empirical <- data.frame(rbind(dat_smooth, cbind(-dat_smooth[, 
        1], dat_smooth[, -1])))
    dat_empirical$test <- test
    return(dat_empirical)
}

get_ump_power1 <- function(dat, test = "UMP") {
    beta <- seq(0.01, 16, by = 0.2)
    dat_pow <- NULL
    for (n in c(100, 300)) {
        for (sg in c(5, 12)) {
            pow <- NULL
            for (i in beta) pow <- c(pow, calculate_ump_power1(beta = i, 
                n = n, sigma = sg))
            dat_ump <- data.frame(beta = c(-beta, beta), pow = c(pow, 
                pow))
            dat_ump$sample_size <- n
            dat_ump$sigma <- sg
            #head(dat_ump)
            dat_pow <- rbind(dat_pow, dat_ump)
        }
    }
    dat_pow$test <- test
    return(dat_pow)
}

dat_emp_pow <- get_smooth_power(dat)
dat_ump_pow <- get_ump_power1(dat)
p <- ggplot(dat_emp_pow, aes(beta, pow)) + geom_line(aes(colour = test))
p <- p + geom_line(aes(beta, pow, colour = test), data = dat_ump_pow)
p <- p + facet_grid(sample_size ~ sigma)
p + xlab(expression(beta)) + ylab("Power")

# ---------- bootstrap band for empirical power -----------------------

# dat_boot_pow <- NULL
# for (i in 1:1000){
# dat.b <- ddply(dat,.(beta,sample_size,sigma), summarize,
#            response = sample(response,replace=T)
#            )
# dat_boot_pow <-
#   rbind(dat_boot_pow,get_smooth_power(dat.b,test=paste('smooth',i,sep='')))
# }
#
#   write.csv(dat_boot_pow,file='../data/dat_bootstrap_power.txt',row.names=F)

dat_boot_pow <- read.csv("../data/dat_bootstrap_power.txt")

dat_boot_limit <- ddply(dat_boot_pow, .(beta, sample_size, 
    sigma), summarize, limit1 = quantile(pow, 0.025, na.rm = T), limit2 = quantile(pow, 
    0.975, na.rm = T))
dat_boot_pow_limit <- melt(dat_boot_limit, id = c("beta", "sample_size", 
    "sigma"))
colnames(dat_boot_pow_limit) <- c("beta", "sample_size", "sigma", 
    "test", "pow")
dat_obs_val <- ddply(dat, .(beta, sample_size, sigma, response), 
    summarize, responses = length(response))
dat_obs_val <- rbind(dat_obs_val, cbind(beta = -dat_obs_val[, 
    1], dat_obs_val[, -1]))
dat_obs_pow <- ddply(dat, .(beta, sample_size, sigma, replica), 
    summarize, pow = sum(response == "TRUE")/sum(response == response))
dat_obs_pow <- rbind(dat_obs_pow, cbind(beta = -dat_obs_pow[, 
    1], dat_obs_pow[, -1]))
dat_boot_ribbon <- ddply(dat_boot_pow, .(beta, sample_size, 
    sigma), summarize, limit1 = quantile(pow, 0.025, na.rm = T), limit2 = quantile(pow, 
    0.975, na.rm = T))
dat_boot_ribbon <- rbind(dat_boot_ribbon, cbind(beta = -dat_boot_ribbon[, 
    1], dat_boot_ribbon[, -1]))
p <- ggplot() + geom_point(aes(beta, as.numeric(response), 
    size = responses), data = dat_obs_val) + geom_ribbon(aes(x = beta, 
    ymin = limit1, ymax = limit2), data = dat_boot_ribbon) + geom_line(aes(beta, 
    pow, colour = test), data = dat_emp_pow) + geom_line(aes(beta, pow, 
    colour = test), data = dat_ump_pow) + facet_grid(sample_size ~ sigma) + 
    xlab(expression(beta)) + ylab("Power")
p
ggsave(p, filename = "../images/power_loess_exp1.pdf", height = 5.5, 
    width = 8.5)
# ------------------- Expected power calculations ------------------------
n <- 300
beta <- 3
sigma <- 12
ncp <- beta * sqrt(n)/(2 * sigma)  # refer to documentation for derivation of power (xbar=1/2 here)
#Density of the absolute value of a noncentral
#t random variable with df degrees of freedom
#and noncentrality parameter ncp.
#refer to Dr. nettleton power point slide in gmail
f.abs.t = function(t, df, ncp = 0) {
    (t > 0) * dt(t, df = df, ncp = ncp) + dt(-t, df = df, ncp = ncp)
}
#Density of the p-value from a t-test
#when the noncentrality parameter is ncp
#and the degrees of freedom are df.
f = function(p, df, ncp = 0) {
    tt = qt(1 - p/2, df = df)
    f.abs.t(tt, df = df, ncp = ncp)/f.abs.t(tt, df = df)
}
#Example: Density of p-value for t-test with
#df=8 and ncp=2.
df <- n - 3  # since 3 parameters in the regression model
p = seq(0.01, 1, by = 0.01)
y = f(p, df = df, ncp = ncp)
plot(p, y, type = "l", ylim = c(0, max(y)))
library(ggplot2)
qplot(p, y, geom = "line", ylim = c(0, max(y))) + xlab("p-value") + 
    ylab("density")
n <- 100
sigma <- 12
expected_power <- function(beta) {
    #beta <- 3
    m <- 19
    ncp <- beta * sqrt(n)/(2 * sigma)
    df <- n - 3
    p <- seq(1e-05, 1, by = 1e-04)
    N <- length(p)
    y <- f(p, df = df, ncp = ncp)
    x <- sample(p, prob = y, replace = T)
    z <- rbeta(n = N, shape1 = 1, shape2 = m - 1)
    return(sum(x < z)/N)
}
expected_power(6)
n <- 100
sigma <- 12
expected_power_thm <- function(beta) {
    #beta <- 3
    m <- 20
    ncp <- beta * sqrt(n)/(2 * sigma)
    df <- n - 3
    p <- runif(n = 1e+06)
    return(mean(((1 - p)^(m - 1)) * f(p, df = df, ncp = ncp)))
}
expected_power_thm(6)
mypow <- function(beta) {
    # n <- 100
    # sigmasq <- 12^2
    sigmasq <- sigma^2
    beta_not <- 0
    se_beta <- sqrt(sigmasq/(n * (0.5^2)))  # refer to Jon Hobbs document in gmail
    mu <- beta/se_beta
    alpha <- 0.05
    t_n <- qt(p = 0.975, df = n - 3)
    res <- pt(q = -t_n, df = n - 3, ncp = mu) - pt(q = t_n, df = n - 3, 
        ncp = mu) + 1
    return(res)
}
betas <- seq(0.01, 16, by = 0.3)
pow0 <- sapply(betas, expected_power_thm)
pow1 <- sapply(betas, expected_power_thm)
pow2 <- sapply(betas, expected_power_thm)
pow <- (pow0 + pow1 + pow2)/3
pw <- sapply(betas, mypow)
power <- c(rep(pow, 2), rep(pw, 2))
bt <- c(-betas, betas, -betas, betas)
test <- c(rep("Visual", length(betas) * 2), rep("UMP", length(betas) * 
    2))
qplot(bt, power, geom = "line", colour = test) + xlab(expression(beta))


# ====================== Turk2 data analysis ========================= 




# ======================= p_value vs %correct =====================

dat1 <- read.csv("../data/feedback_data_turk1_50p.txt")
dat2 <- read.csv("../data/feedback_data_turk2_30p.txt")
dat3 <- read.csv("../data/feedback_data_turk3_50p.txt")


pdat <- NULL
for (i in 1:3){
   dati <- eval(parse(text=paste("dat",i,sep="")))
   pdati <- ddply(dati, .(p_value, sample_size), summarize,
	  attempted = sum(response==response),
	  corrected = sum(response=="TRUE"),
	  percent_correct = sum(response=="TRUE")*100/sum(response==response)
	)
   pdati$experiment=paste("experiment",i)
   pdat <- rbind(pdat,pdati)
}

# pdat$percent_correct <- pdat$corrected*100/pdat$attempted

p <- ggplot() +
     geom_point(aes(p_value,percent_correct),data=pdat,size=2) + 
     facet_grid(.~experiment) +
     xlab(expression(paste("p-value(",p[B],")"))) +
     ylab("Percentage of correct responses") 
p 

ggsave(p,file="../images/p_val_percent_correct.pdf", height = 4.25, width = 10)



# ----- p-value vs plot signal strength --------------------

pdat <- NULL
for (i in 1:3){
   dati <- eval(parse(text=paste("dat",i,sep="")))
   pdati <- ddply(dati, .(p_value, sample_size), summarize,
	  attempted = sum(response==response),
	  corrected = sum(response=="TRUE"),
	  percent_correct = ifelse(sum(response=="TRUE")>0,sum(response=="TRUE")*100/sum(response==response),1*100/sum(response==response))
	)
   pdati$experiment=paste("experiment",i)
   pdat <- rbind(pdat,pdati)
}


pdat$strength <- 1- (pdat$percent_correct/100)^(1/19)


p <- ggplot(pdat) +
     geom_point(aes(p_value,strength),size=2) + 
     facet_grid(.~experiment) +
     xlab(expression(paste("p-value(",p[B],")"))) +
     ylab("plot signal strength") + 
     geom_abline(aes(intercept=0,slope=1))
p 

ggsave(p,file="../images/p_val_plot_signal.pdf", height = 4.25, width = 10)

# -------- Hieke's code for power vs m and p-value------------------

powerdf <- data.frame(expand.grid(m = 5:50,
pb=c(seq(0.05,0.5, by=0.05), seq(0.25, 0.5, by=0.05))))

powerdf$power <- with(powerdf, (1-pb)^(m-1))

p <- qplot(m, power, geom="line", group=pb, data=powerdf, colour=pb) + 
     geom_line(aes(m, 1/m), colour="grey30", lty=2) + 
     geom_text(aes(label=sprintf("%.2f",pb)), data=subset(powerdf, m==5), hjust=1.1) + 
     xlim(c(0,50)) + scale_colour_gradient(name=expression(p[B]))
p     
ggsave(p,file="../images/p_val_power_m.pdf", height = 4.25, width = 6)




