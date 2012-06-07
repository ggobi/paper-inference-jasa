# This file computes the expected power for visual test.
# Last modified Jan 19, 2012


library(ggplot2)
library(plyr)
library(MASS)

generate_visual_power <- function(n,beta,sigma, m=20){
	#This program generates expected visual power by simulation.
	age <- rpois(n,lambda=30)
	grp <- factor(sample(c("A","B"), size=n, replace=T))

	X <- model.matrix(~age + factor(grp))


	sdbeta <- sigma *sqrt(ginv(t(X) %*% X)[3,3])
	ts <- abs(rt(5000, ncp=beta/sdbeta, df=n-3))
	pvals <- 2*pt(ts, n-3, lower.tail=FALSE)

	
# minimum of (m-1) uniform random variables has a Beta (1, m-1) distribution
	pow <- mean(pbeta(pvals, shape1=1, shape2=m-1, lower.tail=FALSE))
# equivalent:
#	pow <- mean(vpower(pvals, m=20))

	return(pow)
}



calculate_theoretical_power <- function (beta, n, sigma, alpha=0.05){
	# standard deviation of beta depends on modelling situation
	# only valid for experiment (1)
	se_beta <- sigma/sqrt(n*(.5^2))  
	mu <- beta/se_beta
	# get quantile for rejection at alpha for H0: beta=0
	t_n <- qt(p=1-alpha/2,df=n-3)
	
	# prob to reject for given beta
	res <- pt(q=-t_n, df=n-3, ncp=mu)-pt(q=t_n, df=n-3, ncp=mu)+1
	
	return(res)
}



powerdf <- data.frame(expand.grid(beta=seq(0, 16, by=0.1), sigma=12, n=100, m=seq(10, 30, by=5)))
powerdf$visual <- ldply(1:nrow(powerdf), function(x) {
	mean(replicate(10, with(powerdf, generate_visual_power(n[x], beta=beta[x], sigma=sigma[x], m=m[x]))))
})$V1
powerdf$theoretical <- ldply(1:nrow(powerdf), function(x) {
	with(powerdf, calculate_theoretical_power(n[x], beta=beta[x], sigma=sigma[x]))
})$V1

qplot(beta, theoretical, geom="line", linetype=I(2), size=I(1), data=subset(powerdf, m %in% c(10, 20, 30))) + ylab("Power") + geom_smooth(aes(y=visual, group=m, colour=m), alpha=0.5, fill=NA, method="loess", span=0.1) + geom_smooth(aes(y=visual, group=m, colour=m), subset(powerdf, m==20), size=1, fill=NA, method="loess", span=0.1, show.guide=FALSE)

ggsave("images/power_expected.pdf")
