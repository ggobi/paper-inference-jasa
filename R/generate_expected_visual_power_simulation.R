# This file computes the expected power for visual test.
# Last modified Jan 19, 2012


library(ggplot2)

generate_visual_power <- function(n,beta,sigma){
	#This program generates expected visual power by simulation.
	# default m = 20
	m <- 20
#	M <- 5000
	age <- rpois(n,lambda=30)
	grp <- factor(sample(c("A","B"), size=n, replace=T))
#	cnd <- sample(c("A","B"), size=1)
#	gnd <- as.numeric(grp==cnd)

	X <- model.matrix(~age + factor(grp))

#	weights <- NULL
#	pvals <- NULL
#	for (i in 1:M){
#		weight <- round(5 + 15 * age + beta * gnd + rnorm(n=n, mean=0, sd=sigma))
#		weights <- cbind(weights,weight)
#		fit <- lm(weight~age + factor(grp)) # fitting full model
#		fit.stat <- summary(fit)
#		pval <- fit.stat$coefficients[3,4]
#		pvals <- c(pvals,pval)
#	}

	 sdbeta <- sigma *sqrt(ginv(t(X) %*% X)[2,2])
	ts <- abs(rt(5000, ncp=beta/sdbeta, df=n-1))
	pvals <- 2*pt(ts, n-1, lower.tail=FALSE)

	
#	y <- matrix(runif(n=M*(m-1)),nrow=(m-1),ncol=M)
#	b <- sum(pvals < apply(y,2,min))
#	pow <- b/M
# minimum of (m-1) uniform random variables has a Beta (1, m-1) distribution
	pow <- mean(qbeta(pvals, shape1=1, shape2=m-1, lower.tail=FALSE))
# equivalent:
#	pow <- mean(vpower(pvals, m=20))

	return(pow)
}



calculate_theoretical_power <- function (beta, n, sigma){
	# n <- 100
	# sigmasq <- 12^2
	alpha <- 0.05
	sigmasq <- sigma^2
	beta_not <- 0
	se_beta <- sqrt(sigmasq/(n*(.5^2)))  
	mu <- beta/se_beta
	alpha <- alpha/2
	t_n <- qt(p=1-alpha,df=n-3)
	res <- pt(q=-t_n, df=n-3, ncp=mu)-pt(q=t_n, df=n-3, ncp=mu)+1
	return(res)
}




calculate_power <- function(n,sigma){
  my_pow <- NULL
  ump_pow <- NULL
  my_beta <- seq(0,16,by=1)
  generate_visual_power(n=n,beta=my_beta,sigma=sigma)
  for (beta in my_beta){
	my_pow <- c(my_pow,generate_visual_power(n=n,beta=beta,sigma=sigma))
	ump_pow <- c(ump_pow,calculate_theoretical_power(beta=beta,n=n,sigma=sigma))
  }

  betas <- rep(c(-my_beta[-1],my_beta),2)
  powers <- c(my_pow[-1],my_pow,ump_pow[-1],ump_pow)
  Test <- rep(c("visual","UMP"),each=(2*length(my_beta)-1))
  pow_dat <- data.frame(betas,powers,Test)
  return(pow_dat)
}

# three sets of expected power is generated and averaged to get smooth curve 

pow_dat1 <- calculate_power(n=100,sigma=12)
pow_dat2 <- calculate_power(n=100,sigma=12)
pow_dat3 <- calculate_power(n=100,sigma=12)
#head(pow_dat1)

pow_dat <- data.frame(betas=pow_dat1[,1], powers=(pow_dat1[,2]+pow_dat2[,2]+pow_dat3[,2])/3,Test=pow_dat1[,3])
#head(pow_dat)

p <- qplot(betas,powers,geom="line", data=pow_dat, colour=Test)
p + xlab(expression(beta)) + ylab("Power")

#pow_dat_m10 <- pow_dat
#pow_dat_m12 <- pow_dat
#pow_dat_m20 <- pow_dat
#pow_dat_m16 <- pow_dat
#pow_dat_m80 <- pow_dat


pow_dat_m10$Test_m <- "UMP"
pow_dat_m10$Test_m[pow_dat_m10$Test=="visual"] <- "visual_m10"


pow_dat_m12$Test_m <- "UMP"
pow_dat_m12$Test_m[pow_dat_m12$Test=="visual"] <- "visual_m12"


pow_dat_m16$Test_m <- "UMP"
pow_dat_m16$Test_m[pow_dat_m16$Test=="visual"] <- "visual_m16"


pow_dat_m20$Test_m <- "UMP"
pow_dat_m20$Test_m[pow_dat_m20$Test=="visual"] <- "visual_m20"

pow_dat_m80$Test_m <- "UMP"
pow_dat_m80$Test_m[pow_dat_m80$Test=="visual"] <- "visual_m80"

dat <- rbind(pow_dat_m10,pow_dat_m12,pow_dat_m16,pow_dat_m20, pow_dat_m80)


p <- qplot(betas,powers,geom="line", data=dat, colour=Test_m, linetype=Test_m)
p + xlab(expression(beta)) + ylab("Power")








