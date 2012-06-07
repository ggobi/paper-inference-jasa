#Turk1 data analysis for jasa paper last modified on Mar 30, 2012 by Mahbub.

library(ggplot2)
library(plyr)
library(reshape)


raw.dat <- read.csv("../data/raw_data_turk1.csv")

# ========== policy for cleaning data =========
# Everyone is given at least one easy lineup. If a person can't correctly
# evaluate 50% of those extremely easy lineups, all his responses are discarded.
# A lineup with p_value < 0.0002 is considered easy

dp <- ddply(subset(raw.dat, p_value < 0.0002),.(id),summarise,
   easy_cnt = length(response),
   percent_correct = mean(response)*100
  )

included_id <- dp$id[ dp$percent_correct > 49]
dat <- subset(raw.dat, id %in% included_id)



# ------------ description of coded variables in the data -----------

## gender 1 = male 
#         2 = female
#
## age 1 = below 18 
#      2 = 18-25 
#      3 = 26-30 
#      4 = 31-35 
#      5 = 36-40
#      6 = 41-45
#      7 = 46-50
#      8 = 51-55
#      9 = 56-60
#     10 = above 60
#
## academic_study 1 = High school or less
#                 2 = Some under graduate courses 
#                 3 = Under graduate degree
#                 4 = Some graduate courses
#                 5 = Graduate degree
#
## conf_level 1 = most, 5 = least
#
## choice_reason 1 = Big vertical difference 
#                2 = Medians are different
#                3 = Outliers
#                4 = others
#
## unit(time_taken) = second 

      


# ------function to calculate UMP power -----------------------

calculate_ump_power <- function (beta, n, sigma){
	alpha <- 0.05
	se_beta <- sigma/(0.5 * sqrt(n))    # refer to docs for derivation of power
	mu <- beta/se_beta
	t_n <- qt(p=1-alpha/2,df=n-3)
	res <- pt(q=-t_n, df=n-3, ncp=mu)-pt(q=t_n, df=n-3, ncp=mu)+1
	return(res)
}

calculate_ump_power(3,100,5)
calculate_ump_power(3,300,5)

calculate_ump_power(3/5,300,1)


# ---- distribution of beta as the effect of algorithm for showing lineups ---

params <- ddply(dat, .(difficulty, sample_size, sigma), summarise,
   params_beta <- paste(unique(beta), collapse=",") )
   
sample_size <- function(g){
	res <- g*(1-g)/((.05/1.96)^2)
	return(round(res,0))
} 

params_comb <- ddply(dat, .(difficulty, sample_size, sigma), summarise,
      params_beta <- unique(beta),
      g <- calculate_ump_power(beta=unique(beta),n=sample_size[1],sigma=sigma[1]),
      s.size <- sample_size(calculate_ump_power(beta=unique(beta),n=sample_size[1],sigma=sigma[1])) ) 


# ---------------------- fitting loess smoother  ------------


fit <- loess(as.numeric(response) ~ beta,data=subset(dat,sigma==5 & sample_size==100),span=1.2)
betas <- seq(0,9,by=.1)
with(subset(dat,sigma==5 & sample_size==100),qplot(betas,predict(fit,data.frame(beta=betas))))


fit <- loess(as.numeric(response) ~ beta,data=subset(dat,sigma==5 & sample_size==300),span=.75)
betas <- seq(0,5,by=.1)
with(subset(dat,sigma==5 & sample_size==300),qplot(betas,predict(fit,data.frame(beta=betas))))


get_smooth_power <- function(dat.n,test="Empirical"){
 betas <- seq(0.01,16, by=.2)
 dat_smooth <- NULL
 for (s in c(5,12)){
	for (n in c(100,300)){
		dats <- subset(dat.n,sigma==s & sample_size==n)
		dats$y <- as.numeric(dats$response)
		fit <- loess(y ~ beta,data=dats,span=1+(n==100)*.2)
		dat_smooth <- rbind(dat_smooth,cbind(betas,predict(fit,betas),n,s))
		}
	}
 colnames(dat_smooth) <- c("beta","pow","sample_size","sigma")
 dat_empirical <- data.frame(rbind(dat_smooth,cbind(-dat_smooth[,1],dat_smooth[,-1])))
 dat_empirical$test <- test
 return(dat_empirical)
}


get_ump_power <- function(dat,test="UMP"){
 beta <- seq(0.01,16, by=.2)
 dat_pow <- NULL
 for(n in c(100,300)){
  for(sg in c(5,12)){
     pow <- NULL
     for (i in beta)pow <- c(pow,calculate_ump_power(beta=i,n=n,sigma=sg))
     dat_ump <- data.frame(beta=c(-beta,beta),pow=c(pow,pow))
     dat_ump$sample_size <- n
     dat_ump$sigma <- sg
     #head(dat_ump)
     dat_pow <- rbind(dat_pow,dat_ump)
  }
 }
 dat_pow$test <- test	
 return(dat_pow)
}

dat_emp_pow <- get_smooth_power(dat)
dat_ump_pow <- get_ump_power(dat)

p <- ggplot(dat_emp_pow,aes(beta,pow)) + geom_line(aes(colour=test)) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power")
p




# ---------- bootstrap band for empirical power -----------------------

# WARNIG: This code may take 30 min to run. please use the saved one.
# the link is at the end of this junk of codes.

dat_boot_pow <- NULL
for (i in 1:1000){
dat.b <- ddply(dat,.(beta,sample_size,sigma), summarize,
           response = sample(response,replace=T)
           )
dat_boot_pow <- rbind(dat_boot_pow,get_smooth_power(dat.b,test=paste("smooth",i,sep="")))
}

#write.csv(dat_boot_pow,file="../data/dat_bootstrap_power1.txt",row.names=F)
#dat_boot_pow <- read.csv("../data/dat_bootstrap_power1.txt")

# END WARNIG -----------

p <- ggplot() +
     geom_line(aes(beta,pow, group =test), data=dat_boot_pow,alpha=I(0.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 


dat_boot_limit <-  ddply(dat_boot_pow,.(beta,sample_size,sigma), summarize,
           limit1 = quantile(pow,.025, na.rm=T),
           limit2 = quantile(pow,.975, na.rm=T)
           )
           
dat_boot_pow_limit <- melt(dat_boot_limit, id=c("beta","sample_size","sigma"))
colnames(dat_boot_pow_limit) <- c("beta","sample_size","sigma","test","pow")          


p <- ggplot() +
     geom_line(aes(beta,pow, group =test,linetype=5), data=dat_boot_pow_limit,colour=alpha("red",.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 


p <- ggplot() +
     geom_line(aes(beta,pow, group =test,linetype=5), data=dat_boot_pow_limit,colour=alpha("red",.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     geom_point(aes(beta,as.numeric(response)), data=dat) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 


dat_obs_val <- ddply(dat,.(beta,sample_size,sigma,response), summarize,
           responses = length(response)
           )
dat_obs_val <- rbind(dat_obs_val,cbind(beta= -dat_obs_val[,1],dat_obs_val[,-1]))           

p <- ggplot() +
     geom_line(aes(beta,pow, group =test), data=dat_boot_pow,alpha=I(0.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 

# Empirical success ratio vs bootstrap band with ump power
dat_obs_pow <- ddply(dat,.(beta,sample_size,sigma,replica), summarize,
           pow = sum(response)/length(response)
           )
dat_obs_pow <- rbind(dat_obs_pow,cbind(beta= - dat_obs_pow[,1],dat_obs_pow[,-1]))           
           
           
p <- ggplot() +
     geom_line(aes(beta,pow, group =test,linetype=5), data=dat_boot_pow_limit,colour=alpha("red",.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     geom_point(aes(beta,pow), data=dat_obs_pow, colour=alpha("red",.3)) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 

# bootstrap ribon showing confidence band         
dat_boot_ribbon <-  ddply(dat_boot_pow,.(beta,sample_size,sigma), summarize,
           limit1 = quantile(pow,.025, na.rm=T),
           limit2 = quantile(pow,.975, na.rm=T)
           )
dat_boot_ribbon <- rbind(dat_boot_ribbon,cbind(beta= -dat_boot_ribbon[,1],dat_boot_ribbon[,-1]))

p <- ggplot() +
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val, colour=alpha("red",.3)) +
     geom_ribbon(aes(x=beta,ymin=limit1,ymax=limit2), data=dat_boot_ribbon, fill=alpha("red",.2)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 


p <- ggplot() +
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val, alpha=I(0.3)) +
     geom_ribbon(aes(x=beta,ymin=limit1,ymax=limit2), data=dat_boot_ribbon, fill=alpha("grey",.9)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 
p
         
ggsave(p, filename="../images/power_loess_exp1.pdf", height=5.5, width=8.5)



# --------------- p_value vs %correct ----------------------------------

pdat <- ddply(dat, .(pic_name), summarize,
   p_value = mean(p_value),
	attempted = length(response),
	corrected = sum(response),
	percent_correct = mean(response)*100
	)


p <- ggplot(pdat) +
     geom_point(aes(p_value,percent_correct)) + 
     xlab(expression(paste("p-value(",p[B],")"))) +
     ylab("Percentage of correct responses") 
p 

ggsave("../images/p_val_percent_correct.pdf")

pdat$lbl <-""
indx <- with(pdat,(percent_correct>30 & p_value>.05) | (percent_correct>5 & p_value>.5))
pdat$lbl[indx] <- as.character(pdat$pic_name[indx])
pdat$angle <- 0
pdat$angle[pdat$percent_correct>5 & pdat$p_value>.5] <- 90

p <- ggplot(pdat,aes(p_value,percent_correct,label=lbl)) +
     geom_point() + geom_text(hjust=-.05,vjust=0,aes(angle=angle)) +
     xlab(expression(paste("p-value(",p[B],")"))) +
     ylab("Percentage of correct responses") +
     geom_vline(xintercept=0.05) + geom_hline(yintercept=5) +geom_smooth()
p 


subset(dat,pic_name=="plot_turk1_100_1_5_1.png")
subset(dat,pic_name=="plot_turk1_100_0_5_1.png")
subset(dat,pic_name=="plot_turk1_300_1_12_1.png")

subset(dat,pic_name=="plot_turk1_100_1_5_3.png")
subset(dat,pic_name=="plot_turk1_100_1_12_3.png")

# ------------- observed and theorectical power curves ------------------------------

exact_ci <- function(yy, nn, alpha=0.05) {
	pL <- rep(0,length(yy))
	pU <- rep(1,length(yy))

	y <- yy[yy!=0]
	n <- nn[yy!=0]
	pL[yy!=0] <- 1/(1+ (n-y+1)/(y*qf(alpha/2, 2*y, 2*(n-y+1))))

	y <- yy[yy!=nn]
	n <- nn[yy!=nn]
	pU[yy!=nn] <- 1/(1+ (n-y)/((y+1)*qf(1-alpha/2, 2*(y+1), 2*(n-y))))
	pw <- yy/nn
	return(data.frame(pw,pL, pU))
}
exact_ci(yy=c(3,4),nn=c(5,7))


# plotting power for each replica

dat_obs <- ddply(dat,.(beta,sample_size,sigma,replica), summarize,
           empirical_power = exact_ci(yy=sum(response=="TRUE"),nn= sum(response==response))[,1]
           )
dat_obs1 <- dat_obs

power_obs_ump <- cbind(dat_obs[,1:4],ump=sapply(dat_obs[,1],calculate_ump_power ,n=100,sigma=5))


dat_obs1$beta <- -dat_obs1$beta
dat_obs <- rbind(dat_obs1,dat_obs)
dat_obs.m <- melt(dat_obs, id=c("beta","sample_size","sigma", "replica"))
colnames(dat_obs.m) <- c("beta","sample_size","sigma", "replica","test","pow")
head(dat_obs.m)

p <- qplot(beta,pow, geom=c("point","line"), data=dat_obs.m, colour=factor(replica)) 
p + facet_grid(sample_size~sigma)

p <- qplot(beta,pow, geom=c("smooth","point"), data=dat_obs.m) 
p + facet_grid(sample_size~sigma)

p <- ggplot(dat_obs.m, aes(beta,pow)) + geom_point()+geom_smooth()
p + facet_grid(sample_size~sigma)

# plotting average power on all replica

dat_obs <- ddply(dat,.(beta,sample_size,sigma), summarize,
           pow = exact_ci(yy=sum(response),nn= length(response))[,1]
           )
dat_obs$test <- "Visual"
dat_obs1 <- dat_obs
dat_obs1$beta <- -dat_obs1$beta
dat_obs <- rbind(dat_obs1,dat_obs)

beta <- seq(0.01,16, by=.2)
dat_pow <- NULL
for(n in c(100,300)){
  for(sg in c(5,12)){
     pow <- NULL
     for (i in beta)pow <- c(pow,calculate_ump_power(beta=i,n=n,sigma=sg))
     dat_ump <- data.frame(beta=c(-beta,beta),pow=c(pow,pow))
     dat_ump$sample_size <- n
     dat_ump$sigma <- sg
     #head(dat_ump)
     dat_pow <- rbind(dat_pow,dat_ump)
}}
dat_pow$test <- "UMP"
head(dat_pow)

plot_dat <- rbind(dat_obs,dat_pow[,c(1,3,4,2,5)])
head(plot_dat)
tail(plot_dat)

p <- qplot(beta,pow, geom="line", data=plot_dat, colour=test, linetype=test) + 
     facet_grid(sample_size~sigma) + 
     scale_linetype_manual(values=c(1,2,2,1)) + 
     xlab(expression(beta)) + ylab("Power")


p <- ggplot(dat_obs.m, aes(beta,pow, colour=test)) + geom_point(alpha=0.5)+geom_smooth() + 
     geom_line(aes(beta,pow, colour=test), data=dat_pow) + 
     facet_grid(sample_size~sigma) + 
     xlab(expression(beta)) + ylab("Power")

ggsave(p, filename="../images/emperical_ump_power_exp1.pdf", height=5.5, width=8.5)


# ----------- Estimating power curve ------------------
# This uses the restricted glm model which fixed b0 such that type-I error=0.05


my_glm_predict <- function(dat.new){
lb <- function(b){
   y <- as.numeric(dat$response)
   b0 <- log(0.05/(1-0.05))
   x <- with(dat,cbind(x1=beta,x2=beta*sample_size, x3=beta*sigma, x4=beta*sample_size*sigma))
   xb <- b0 + x%*%b
   p <- exp(xb)/(1+exp(xb))
   return(-sum(y*log(p)+(1-y)*log(1-p)))
}

#lb(c(.2,.002,.02,.0002))
bh <- optim(par=c(.2,.002,.02,.0002),lb)$par

xnew <- with(dat.new,cbind(x1=beta,x2=beta*sample_size, x3=beta*sigma, x4=beta*sample_size*sigma))
xbh <- b0 + xnew%*%bh
ph <- exp(xbh)/(1+exp(xbh)) 
return(ph)
}

beta <- rep(seq(.1,16, by=.25),4)
sample.size <- c(rep(100,length(beta)/2),rep(300,length(beta)/2))
sigma <- rep(c(12,5,12,5),each=length(beta)/4)
dat.new <- data.frame(beta, sample_size=sample.size,sigma)
power <- my_glm_predict(dat.new)
vdat <- data.frame(beta=c(beta,-beta),power=c(power,power),sample.size=rep(sample.size,2),sigma=c(sigma,sigma))
vdat$Test="Visual"
qplot(beta,power, geom="line", data=vdat)+facet_grid(sample.size~sigma)

qplot(beta,power, geom="line", data=subset(vdat,(sigma==12)&(sample.size==100)))

udat <- NULL
for(n in c(100,300)){
  for(sg in c(5,12)){
     pow <- NULL
     for (i in beta)pow <- c(pow,calculate_theoretical_power(beta=i,n=n,sigma=sg))
     dat_ump <- data.frame(beta=c(-beta,beta),power=c(pow,pow))
     dat_ump$sample.size <- n
     dat_ump$sigma <- sg
     #head(dat_ump)
     udat <- rbind(udat,dat_ump)
}}
udat$Test <- "UMP"

dat.pow <- rbind(vdat,udat)

qplot(beta,power, geom="line", data=dat.pow, colour=Test)+facet_grid(sample.size~sigma)



# ------------------ power curve using glm function -----------------------


fit.power <- glm(response ~ sample_size+beta+sigma
             , family=binomial,data=dat)
res <- summary(fit.power)
str(res)
res$coef

beta <- seq(.1,16, by=.25)
sample.size <- rep(100,length(beta))
sigma <- rep(12,length(beta))
newdat <- data.frame(sample_size=sample.size,sigma,beta)
power <- predict(fit.power, newdata = newdat, type="response", se.fit = TRUE)


pr <- predict(fit.power, newdata = newdat, se.fit = TRUE)
family <- family(fit.power)
lower <- family$linkinv(pr$fit - qnorm(0.95) * pr$se.fit)
upper <- family$linkinv(pr$fit + qnorm(0.95) * pr$se.fit)

ump_pow <- NULL
for (i in beta)ump_pow <- c(ump_pow,calculate_theoretical_power(beta=i,n=100,sigma=12))


pow.dat <- data.frame(betas = c(-beta, beta)
                    , empirical=rep(power$fit,2)
                    , lower_limit=rep(lower,2),upper_limit=rep(upper,2)
                    , UMP=rep(ump_pow,2))
pow.dat.m <- melt(pow.dat, id="betas")
head(pow.dat.m)
colnames(pow.dat.m) <- c("betas","Test","power")
p <- qplot(betas, power, geom="line", data=pow.dat.m, color=Test, linetype=Test)
p + xlab(expression(beta)) + ylab("Power") + scale_linetype_manual(values =c(1,2,2,1))


# ---------------- estimating power curves for each replicate of linup ----------------
# this is a restricted glm fixing parameter b0 such that type-I error=.05
# since there is no such package in R to fit I had to write my own function

my_glm_predict.rep <- function(dat.new){
  b0 <- log(0.05/(1-0.05))
  lb <- function(b){
     y <- as.numeric(dat$response)
     x <- with(dat,cbind(x1=beta,x2=beta*sample_size, x3=beta*sigma, 
             x4=replica*beta,x5=beta*sample_size*sigma*replica))
     xb <- b0 + x%*%b
     p <- exp(xb)/(1+exp(xb))
     return(-sum(y*log(p)+(1-y)*log(1-p)))
  }

  #lb(c(.2,.007,.002,-.0002,-.002))
  bh <- optim(par=c(.2,.007,.002,-.0002,-.002),lb)$par
  # 0.2162250624  0.0056327718 -0.0174778836  0.0773441682 -0.0001383814
  xnew <- with(dat.new,cbind(x1=beta,x2=beta*sample_size, x3=beta*sigma, 
             x4=replica*beta,x5=beta*sample_size*sigma*replica))

  xbh <- b0 + xnew%*%bh
  ph <- exp(xbh)/(1+exp(xbh)) 
  return(ph)
}


beta <- seq(.1,16, by=.25)
sample.size <- c(100,300)
sigma <- c(12,5)
replica <- 1:3
dat.new <- expand.grid(beta,sample.size,sigma,replica)
colnames(dat.new) <- c("beta","sample_size","sigma","replica")
dat.new$power <- my_glm_predict.rep(dat.new)
vdat <- with(dat.new,data.frame(beta=c(beta,-beta),power=c(power,power),
        sample.size=rep(sample_size,2),sigma=c(sigma,sigma), replica=c(replica,replica)))
vdat$Test=paste("Replicate",vdat$replica,sep="")
qplot(beta,power, geom="line", data=vdat, colour=Test)+facet_grid(sample.size~sigma)

qplot(beta,power, geom="line",colour=Test,data=subset(vdat,(sigma==5)&(sample.size==100)))


dat.X <- expand.grid(beta,sample.size,sigma)
colnames(dat.X) <- c("beta","sample.size","sigma")
upow <- apply(dat.X,1,function(x){return(calculate_theoretical_power(beta=x[1],n=x[2],sigma=x[3]))})
udat <- data.frame(beta=c(dat.X$beta, -dat.X$beta), 
                  power=c(upow,upow), rbind(dat.X[,-1],dat.X[,-1]),
                  replica=0, Test="UMP")


dat.pow <- rbind(vdat,udat)

p <- qplot(beta,power, geom="line", data=dat.pow, colour=Test, linetype=Test)
p <- p + facet_grid(sample.size~sigma)
p + xlab(expression(beta)) + ylab("Power") + scale_linetype_manual(values =c(2,3,4,1))


dat_obs <- ddply(dat,.(beta,sample_size,sigma,replica), summarize,
	power=sum(response=="TRUE")/length(response)
   )
dat_obs$sample.size=dat_obs$sample_size
dat_obs$Test <- paste("Replicate",dat_obs$replica, sep="")


ggplot()+
geom_line(data=dat.pow,aes(x=beta,y=power,colour=Test, linetype=Test)) +
geom_point(data=dat_obs,aes(x=beta,y=power,colour=Test)) +
facet_grid(sample.size~sigma) + 
xlab(expression(beta)) + ylab("Power") + scale_linetype_manual(values =c(2,3,4,1))


dat.pow.diff <- vdat
dat.pow.diff$power <- rep(udat$power,3)- vdat$power

p <- qplot(beta,power, geom="line", data=subset(dat.pow.diff,beta>=0), colour=Test, linetype=Test)
p <- p + facet_grid(sample.size~sigma)
p + xlab(expression(beta)) + ylab("Difference from UMP test Power") + scale_linetype_manual(values =c(2,3,4,1))




# ---------------- estimating power curves for each replicate of linup using glm ----------------

dat$bf <- dat$beta*dat$replica 
fit.power.rep <- glm(response ~ sample_size+sigma+bf
             , family=binomial,data=dat)
res <- summary(fit.power.rep)
str(res)
res$coef

dat.pw <- NULL
for ( nn in c(100,300)){
for (sg in c(5,12)){

beta <- seq(.1,16, by=.25)
nb <- length(beta)
sample.size <- rep(nn,nb)
sigma <- rep(sg,nb)
newdat <- data.frame(sample_size=sample.size,sigma)
newdat <- rbind(newdat,newdat,newdat)
newdat$bf  <- rep(beta,3)* rep(1:3, each=nb)
power.rep <- predict(fit.power.rep, newdata = newdat, type="response", se.fit = TRUE)


ump_pow <- NULL
for (i in beta)ump_pow <- c(ump_pow,calculate_theoretical_power(beta=i,n=nn,sigma=sg))


pow.dat <- data.frame(betas = c(-beta, beta)
                    , replicate1=rep(power.rep$fit[1:nb],2)
                    , replicate2=rep(power.rep$fit[(nb+1):(nb*2)],2)
                    , replicate3=rep(power.rep$fit[(2*nb+1):(nb*3)],2)
                    , UMP=rep(ump_pow,2))
pow.dat.m <- melt(pow.dat, id="betas")
head(pow.dat.m)
colnames(pow.dat.m) <- c("betas","Test","power")
#p <- qplot(betas, power, geom="line", data=pow.dat.m, color=Test, linetype=Test)
#p + xlab(expression(beta)) + ylab("Power") + scale_linetype_manual(values =c(2,3,4,1))

replicate1 <- ump_pow-power.rep$fit[1:nb]
replicate2 <- ump_pow-power.rep$fit[(nb+1):(nb*2)]
replicate3 <- ump_pow-power.rep$fit[(2*nb+1):(nb*3)]
overall <- ump_pow-power$fit
pow.diff <- data.frame(beta,replicate1,replicate2,replicate3,overall)
pow.diff.m <- melt(pow.diff,id="beta")
colnames(pow.diff.m) <- c("betas","Replicate","power")
#p <- qplot(betas, power, geom="line", data=pow.diff.m, color=Replicate, linetype=Replicate)
#p + xlab(expression(beta)) + ylab("Difference from UMP Power") + scale_linetype_manual(values =c(4,3,2,1))

dat.pw <- rbind(dat.pw,cbind(pow.diff.m,sample_size=nn,sigma=sg))

}}

p <- qplot(betas, power, geom="line", data=dat.pw, color=Replicate, linetype=Replicate)
p <- p + xlab(expression(beta)) + ylab("Difference from UMP Power") 
p + scale_linetype_manual(values =c(4,3,2,1)) + facet_grid(sample_size~sigma)



# ----------------- estimating subjectwise power curve -------------------------
library(lme4)
fit.mixed <- lmer(response ~ factor(sample_size)+beta+factor(sigma)
              + (1|id)
              , family="binomial"
              , data=dat)
res <- summary(fit.mixed)
B <- res@coefs[,1]
beta <- seq(0.01,16, by=.2)
#res@frame
sample.size <- rep(0,length(beta))
sigma.val <- rep(1,length(beta))
#conf_level <- rep(1,length(beta))
X <- cbind(rep(1,length(beta)),sample.size,beta,sigma.val)
dim(X)
delta <- as.numeric(res@REmat[4])

power=NULL
M <- 30
for (i in 1:M){
tau <- rnorm(mean=0,sd=delta, n=1)
xb <- X %*% B + tau
power <- cbind(power,exp(xb)/(1+exp(xb)))
}
colnames(power) <- 1:M

ump_pow <- NULL
for (i in beta)ump_pow <- c(ump_pow,calculate_theoretical_power(beta=i,n=100,sigma=12))


pow.dat <- data.frame(betas = c(-beta, beta)
                    , rbind(power,power)
                    , UMP=rep(ump_pow,2))
pow.dat.m <- melt(pow.dat, id="betas")
head(pow.dat.m)
tail(pow.dat.m)

p <- qplot(betas,value,group=variable,geom="line",colour=I(alpha("black", 0.1)), data=pow.dat.m)
p <- p + xlab(expression(beta)) + ylab("Power") 
p + geom_line(data=subset(pow.dat.m, variable=="UMP"),colour="hotpink", size=1)




# ----------------- subjectwise power curve with random slope ---------------
library(lme4)
fit.mixed <- lmer(response ~ factor(sample_size)+beta+factor(sigma)
              + (1+beta|id)
              , family="binomial"
              , data=dat)
res <- summary(fit.mixed)
B <- res@coefs[,1]
beta <- seq(0.01,16, by=.2)
#res@frame
sample.size <- rep(0,length(beta))
sigma.val <- rep(1,length(beta))
ones <- rep(1,length(beta))
#conf_level <- rep(1,length(beta))
X <- cbind(ones,sample.size,beta,sigma.val)
dim(X)

Z <- cbind(ones,beta)
cov <- as.numeric(res@REmat[2,5])*as.numeric(res@REmat[1,4])*as.numeric(res@REmat[2,4])
vv <- matrix(as.numeric(c(res@REmat[1,3],rep(cov,2),res@REmat[2,3])),ncol=2, byrow=F)


# library(MASS) is needed for mvrnorm

power=NULL
M <- 20
set.seed(21)
tau <- mvrnorm(n = M, mu=c(0,0),Sigma=vv)
for (i in 1:M){
xb <- X %*% B + Z %*%tau[i,]
power <- cbind(power,exp(xb)/(1+exp(xb)))
}
colnames(power) <- 1:M

ump_pow <- NULL
for (i in beta)ump_pow <- c(ump_pow,calculate_ump_power(beta=i,n=100,sigma=12))


pow.dat <- data.frame(betas = c(-beta, beta)
                    , rbind(power,power)
                    , UMP=rep(ump_pow,2))
pow.dat.m <- melt(pow.dat, id="betas")
head(pow.dat.m)
tail(pow.dat.m)

p <- qplot(betas,value,group=variable,geom="line",alpha=I(.2), data=pow.dat.m)
p <- p + xlab(expression(beta)) + ylab("Power") 
p + geom_line(data=subset(pow.dat.m, variable=="UMP"),colour="hotpink", size=1)



# ----------------- estimating subjectwise power curve by gender etc.------------------
library(lme4)
fit.mixed <- lmer(response ~ gender+factor(sample_size)+beta+factor(sigma)
              + (1|id)
              , family="binomial"
              , data=dat)
res <- summary(fit.mixed)
B <- res@coefs[,1]
beta <- seq(0.01,16, by=.2)
#head(res@frame)
sample.size <- rep(0,length(beta))
sigma.val <- rep(1,length(beta))
#conf_level <- rep(1,length(beta))


delta <- as.numeric(res@REmat[4])

power=NULL
gnd <- NULL
tau <- res@ranef    # estimates for existing subject
M <- length(tau)
for (i in 1:M){
  #tau <- rnorm(mean=0,sd=delta, n=1) # estimates for new subject
  gender.val <- rep(as.numeric(dat$gender[dat$id==i][1])-1,length(beta))
  X <- cbind(rep(1,length(beta)),gender.val,sample.size,beta,sigma.val)
  #dim(X)
  xb <- X %*% B + tau[i]
  power <- cbind(power,exp(xb)/(1+exp(xb)))
  gnd <- c(gnd,gender.val[1])
}
colnames(power) <- 1:M

ump_pow <- NULL
for (i in beta)ump_pow <- c(ump_pow,calculate_theoretical_power(beta=i,n=100,sigma=12))


pow.dat <- data.frame(betas = c(-beta, beta)
                    , rbind(power,power)
                    , UMP=rep(ump_pow,2))
head(pow.dat)
pow.dat.m <- melt(pow.dat, id="betas")
pow.dat.m$gender <- factor(c(gnd[rep(1:M, each=length(beta)*2)],rep(1,length(beta)*2)), labels=c("Male","Female"))
head(pow.dat.m)
tail(pow.dat.m)

p <- qplot(betas,value,group=variable,geom="line",colour=gender, data=pow.dat.m)
p <- p + xlab(expression(beta)) + ylab("Power") 
p + geom_line(data=subset(pow.dat.m, variable=="UMP"),colour="maroon4", size=1)




# ----------------- estimating subjectwise power curve by gender and random slope------------------
library(lme4)
fit.mixed <- glmer(response ~ gender+factor(sample_size)+beta+factor(sigma)
              + (1 + beta|id)
              , family=binomial
              , data=dat)
res <- summary(fit.mixed)
B <- fixef(fit.mixed)
u <- as.matrix(ranef(fit.mixed)$id)
head(u)
beta <- seq(0.01,16, by=.2)
nb <- length(beta)
sample.size <- rep(1,nb)
sigma.val <- rep(0,nb)
ones <- rep(1,nb)

#Z <- cbind(ones,beta,sample.size,sigma.val)
#Z <- cbind(ones,beta,sigma.val)
Z <- cbind(ones,beta)

power=NULL
gnd <- NULL
M <- length(u[,1])
s <- sample(1:M,size=20)
#s <- c(213,8)
for (i in s){
  #tau <- rnorm(mean=0,sd=delta, n=1) # estimates for new subject
  gender.val <- rep(as.numeric(dat$gender[dat$id==i][1])-1,nb)
  X <- cbind(ones,gender.val,sample.size,beta,sigma.val)
  #dim(X)
  xb <- X %*% B + Z %*% u[i,]
  power <- cbind(power,exp(xb)/(1+exp(xb)))
  gnd <- c(gnd,gender.val[1])
}
colnames(power) <- s

ump_pow <- NULL
for (i in beta)ump_pow <- c(ump_pow,calculate_theoretical_power(beta=i,n=100,sigma=12))


pow.dat <- data.frame(betas = c(-beta, beta)
                    , rbind(power,power)
                    , UMP=rep(ump_pow,2))
#head(pow.dat)
pow.dat.m <- melt(pow.dat, id="betas")
pow.dat.m$gender <- factor(c(gnd[rep(1:length(s), each=nb*2)],rep(1,nb*2)), labels=c("Male","Female"))
#head(pow.dat.m)
#tail(pow.dat.m)

p <- qplot(betas,value,group=variable,geom="line",colour=gender, data=pow.dat.m)
p <- p + xlab(expression(beta)) + ylab("Power") 
p + geom_line(data=subset(pow.dat.m, variable=="UMP"),colour="maroon4", size=1)
p + geom_line(data=subset(pow.dat.m, variable=="UMP"),colour="darkviolet", size=1)






# ----- individual observed and estimated power curve by gender and random slope----------

power=NULL
gnd <- NULL
M <- length(u[,1])
s <- sample(1:M,size=1)
for (i in s){
  #tau <- rnorm(mean=0,sd=delta, n=1) # estimates for new subject
  gender.val <- rep(as.numeric(dat$gender[dat$id==i][1])-1,nb)
  X <- cbind(ones,gender.val,sample.size,beta,sigma.val)
  #dim(X)
  xb <- X %*% B + Z %*% u[i,]
  power <- cbind(power,exp(xb)/(1+exp(xb)))
  gnd <- c(gnd,gender.val[1])
}
colnames(power) <- s

ump_pow <- NULL
for (i in beta)ump_pow <- c(ump_pow,calculate_theoretical_power(beta=i,n=100,sigma=12))


pow.dat <- data.frame(betas = c(-beta, beta)
                    , rbind(power,power)
                    , UMP=rep(ump_pow,2))
#head(pow.dat)
pow.dat.m <- melt(pow.dat, id="betas")
pow.dat.m$gender <- factor(c(gnd[rep(1, each=nb*2)],rep(1,nb*2)), labels=c("Male","Female"))
#head(pow.dat.m)
#tail(pow.dat.m)
dat_s=as.data.frame(subset(dat,id==s))
dat_s$variable=1
dat_pi <- merge(dat_s,ddply(dat_s,.(beta),summarize, obs_pi = mean(as.numeric(response)-1) ), by.beta=by.beta)

p <- qplot(betas,value,group=variable,geom="line",colour=gender, data=pow.dat.m)
p <- p + xlab(expression(beta)) + ylab("Power") 
p <- p + geom_line(data=subset(pow.dat.m, variable=="UMP"),colour="darkviolet", size=1)
p <- p + geom_point(aes(beta,as.numeric(response)-1), data=dat_s)
p + geom_point(aes(beta,obs_pi, colour="Observed"), data=dat_pi)


# ------------- observed and theorectical power curves ------------------------------

exact_ci <- function(yy, nn, alpha=0.05) {
	pL <- rep(0,length(yy))
	pU <- rep(1,length(yy))

	y <- yy[yy!=0]
	n <- nn[yy!=0]
	pL[yy!=0] <- 1/(1+ (n-y+1)/(y*qf(alpha/2, 2*y, 2*(n-y+1))))

	y <- yy[yy!=nn]
	n <- nn[yy!=nn]
	pU[yy!=nn] <- 1/(1+ (n-y)/((y+1)*qf(1-alpha/2, 2*(y+1), 2*(n-y))))
	pw <- yy/nn
	return(data.frame(pw,pL, pU))
}
exact_ci(yy=c(3,4),nn=c(5,7))

dat_obs <- ddply(dat,.(beta,sample_size,sigma), summarize,
               empirical = exact_ci(yy=sum(response=="TRUE"),nn= sum(response==response))[,1],
               lower_limit = exact_ci(yy=sum(response=="TRUE"),nn= sum(response==response))[,2],
               upper_limit = exact_ci(yy=sum(response=="TRUE"),nn= sum(response==response))[,3]
              )
dat_obs1 <- dat_obs

power_obs_ump <- cbind(dat_obs[,1:4],ump=sapply(dat_obs[,1],calculate_theoretical_power ,n=100,sigma=5))
#setwd("U:/phd research/simulation_study/turk")
#write.table(power_obs_ump,file="power_obs_ump_exp1.txt",row.names=F)

dat_obs1$beta <- -dat_obs1$beta
dat_obs <- rbind(dat_obs1,dat_obs)
dat_obs.m <- melt(dat_obs, id=c("beta","sample_size","sigma"))
colnames(dat_obs.m) <- c("beta","sample_size","sigma","test","pow")
head(dat_obs.m)

qplot(beta,pow, geom="line", data=dat_obs.m, colour=test) + facet_grid(sample_size~sigma)

beta <- seq(0.01,16, by=.2)
dat_pow <- NULL
for(n in c(100,300)){
  for(sg in c(5,12)){
     pow <- NULL
     for (i in beta)pow <- c(pow,calculate_theoretical_power(beta=i,n=n,sigma=sg))
     dat_ump <- data.frame(beta=c(-beta,beta),pow=c(pow,pow))
     dat_ump$sample_size <- n
     dat_ump$sigma <- sg
     #head(dat_ump)
     dat_pow <- rbind(dat_pow,dat_ump)
}}
dat_pow$test <- "UMP"
head(dat_pow)

plot_dat <- rbind(dat_obs.m,dat_pow[,c(1,3,4,5,2)])
head(plot_dat)
tail(plot_dat)

p <- qplot(beta,pow, geom="line", data=plot_dat, colour=test, linetype=test) 
p <- p + facet_grid(sample_size~sigma)
p <- p + scale_colour_manual(values=c("red","red","red","cyan4"))
p <- p + scale_linetype_manual(values=c(1,2,2,1))
p + xlab(expression(beta)) + ylab("Power")



# --------------- p_value vs time_taken ------------------------------

ggplot(dat,aes((p_value)^.2,(time_taken)^.1))+geom_point()+geom_smooth()

p <- ggplot(dat, aes(factor(p_value), time_taken))
p <- p + geom_boxplot() +ylim(0,450) + facet_grid(gender~.)
p 


pdat1 <- ddply(dat, .(p_value, sample_size), summarize,
	avg_time_taken = median(time_taken),
	num_responded = sum(p_value==p_value),
      nn = length(p_value)
    )

p <- ggplot(subset(pdat1, p_value<= 0.2), aes(p_value,avg_time_taken))
p <- p+ geom_point(aes(size=nn))
p <- p + stat_smooth(se=F) 
p + facet_grid(sample_size~.)

# --------------- Comparison of p_values from theory and actual ----------------------

pval <- function(x){
	N <- x[1]
	u <- x[2]
	k <- u:N
	binom <- choose(N,k)*((0.05)^k)*((0.95)^(N-k))
	return(sum(binom))
}

pval(c(13,3))
pdat <- ddply(dat, .(p_value, sample_size), summarize,
	attempted = sum(response==response),
	corrected = sum(response=="TRUE"),
	percent_correct = sum(response=="TRUE")*100/sum(response==response)
    )

pval_calculated <- apply(pdat[,3:4],1,pval)

qplot(pdat$p_value,pval_calculated, xlab="p_value of Mathematical Test", ylab="p_value of Visual Test")

ddply(dat, .(gender), summarize,
	attempted = sum(response==response),
	corrected = sum(response=="TRUE"),
	percent_correct = sum(response=="TRUE")*100/sum(response==response)
	)

ddply(subset(dat, pic_id==92), .(gender), summarize,
	attempted = sum(response==response),
	corrected = sum(response=="TRUE"),
	percent_correct = sum(response=="TRUE")*100/sum(response==response)
	)

ddply(subset(dat, substr(pic_name,1,12)=='plot_100_3_5'), .(gender), summarize,
	attempted = sum(response==response),
	corrected = sum(response=="TRUE"),
	percent_correct = sum(response=="TRUE")*100/sum(response==response)
	)


ddply(dat, .(pic), summarize,
	attempted = sum(response==response),
	corrected = sum(response=="TRUE"),
	percent_correct = sum(response=="TRUE")*100/sum(response==response)
	)


# --------------- p_value vs %correct ----------------------------------

pdat <- ddply(dat, .(pic), summarize,
      p_value = mean(p_value),
	attempted = sum(response==response),
	corrected = sum(response=="TRUE"),
	percent_correct = sum(response=="TRUE")*100/sum(response==response)
	)

# pdat$percent_correct <- pdat$corrected*100/pdat$attempted

p <- ggplot(pdat, aes(p_value,percent_correct))
p+ geom_point(size=2)

p <- ggplot(pdat, aes(p_value,percent_correct))
p <- p+ geom_point(size=3)
p <- p + stat_smooth(se=F) 
p + facet_grid(sample_size~.)


p <- ggplot(pdat, aes(p_value,percent_correct))
p <- p+ geom_point(size=3)
p <- p + stat_smooth( se=F) 
p + facet_grid(sample_size~.)


# --------------- distribution of time taken ----------------------------------


p <- ggplot(dat, aes(time_taken))+geom_histogram(binwidth=2)+xlim(0,350)
p <- p + facet_grid(gender~.)
p
# --------------- p_value vs %correct by male female ----------------------------------

pdat <- ddply(dat, .(p_value, gender, sample_size), summarize,
	attempted = sum(response==response),
	corrected = sum(response=="TRUE"),
	percent_correct = sum(response=="TRUE")*100/sum(response==response)
	)

# pdat$percent_correct <- pdat$corrected*100/pdat$attempted

p <- ggplot(pdat, aes(p_value,percent_correct, colour=gender, shape=gender))
p+ geom_point(size=3)

p <- ggplot(pdat, aes(factor(p_value),percent_correct, colour=gender, shape=gender))
p <- p+ geom_point(size=3)
p <- p + stat_smooth(aes(group=gender, linetype=gender), se=F) 
p + facet_grid(sample_size~.)



p <- ggplot(subset(pdat, p_value <= 0.099), aes(factor(p_value),percent_correct, colour=gender, shape=gender))
p <- p+ geom_point(size=3)
p <- p + stat_smooth(aes(group=gender, linetype=gender), se=F) 
p + facet_grid(sample_size~.)


# ------------- number of attempts and % correct

dat$attempt_no <- unlist(tapply(dat$start_time, dat$nick_name, 
      function(x){return(as.integer(factor(x)))}), use.names=F)

adat <- ddply(dat, .(attempt_no, gender), summarize,
	attempted = sum(response_no==response_no),
	corrected = sum(response_no==obs_no),
      percent_correct = sum(response_no==obs_no)*100/sum(response_no==response_no)
    )

p <- ggplot(subset(adat, attempt_no <= 12), aes(attempt_no,percent_correct, color=factor(gender), shape=gender))
p <- p + geom_point(size=3) 
p + stat_smooth(aes(group=gender, linetype=gender), se=F)



# --------------- number of attempts vs time_taken ------------------------

adat1 <- ddply(dat, .(attempt_no, gender), summarize,
	avg_time_taken = mean(time_taken),
	avg_p_value = mean(p_value)
    )


p <- ggplot(subset(adat1, attempt_no <= 12), aes(attempt_no,avg_time_taken, color=gender, shape=gender))
p <- p + geom_point(size=3) 
p + stat_smooth(aes(group=gender, linetype=gender), se=F)


p <- ggplot(subset(adat1, attempt_no <= 12), aes(attempt_no,avg_p_value, color=gender, shape=gender))
p <- p + geom_point(size=3) 
p + stat_smooth(aes(group=gender, linetype=gender), se=F)


# ----------- glmm with effect as covariate ---------------
#

dat$effect <-  with(dat,beta/sigma*sqrt(sample_size))

effect.d <- ddply(dat,.(effect, replica), summarize, 
   power = mean(response),
   ump_power = calculate_ump_power(beta[1],sample_size[1],sigma[1]))

ggplot(data=effect.d,aes(x=effect,y=power)) + 
   geom_point() + 
   geom_smooth(method="loess", span=0.8)

ggplot(data=effect.d,aes(x=effect,y=power)) + 
   geom_point(aes(colour=factor(replica))) + 
   geom_smooth(method="loess", span=0.8,aes(colour=factor(replica)))
   
ggplot(data=effect.d,aes(x=effect,y=power)) + 
   geom_point(aes(colour=factor(replica))) + 
   geom_smooth(method="lm", span=0.8,aes(colour=factor(replica)))
   
ggplot(data=effect.d,aes(x=effect,y=power)) + 
   geom_point() + 
   geom_smooth(method="loess", span=0.8,aes(colour=factor(replica))) +
   geom_line(aes(x=effect,y=ump_power)) +
   scale_colour_discrete(name = "Replication")+
   xlab(expression(Effect(E)))
   
ggsave(filename="../images/effect_power_exp1.pdf", height=7, width=7)   
   
ggplot(data=effect.d,aes(x=effect,y=power-ump_power)) + 
   geom_point() + 
   geom_smooth(method="loess", span=0.8,aes(colour=factor(replica))) +
   scale_colour_discrete(name = "Replication")  
   

# and then fit the following model:
# response ~ effect + (effect | id)
# response~ offset(alpha)+effect -1 + (effect|id), family=binomial())
# offset is set such that under H0, P(response=T|effect=0) = .05 (type I error)
# alpha = log(.05/0.95)

# ------- subjectwise power curve with random slope for covariate effect 
library(lme4)

dat$alpha <- log(.05/0.95)
fit.mixed <- lmer(response ~ offset(alpha) + effect -1 + (effect-1|id),
                  family="binomial",
                  data=dat)
qplot(group=effect, x=resid(fit.mixed), geom="histogram", data=dat, facets=~effect, binwidth=0.2)

res <- summary(fit.mixed)
B <- res@coefs[,1]
effect <- seq(0.01,16, by=.2)
#res@frame
ones <- rep(1,length(effect))
#conf_level <- rep(1,length(beta))
#X <- cbind(ones,effect)
#dim(X)

X <- effect
Z <- cbind(ones,effect)
# cov <- as.numeric(res@REmat[2,5])*as.numeric(res@REmat[1,4])*as.numeric(res@REmat[2,4])
# vv <- matrix(as.numeric(c(res@REmat[1,3],rep(cov,2),res@REmat[2,3])),ncol=2, byrow=F)
vv <- VarCorr(fit.mixed)$id


#################
pred.mixed <- function(X, intercept=0) {
  alpha <- log(.05/0.95)
  fit <- X * fixef(fit.mixed) + intercept + alpha
  return(exp(fit)/(1+exp(fit)))
}

qplot(effect, pred(X=effect)) + ylim(c(0,1))

newdata <- data.frame(expand.grid(effect=effect, subject=ranef(fit.mixed)[[1]][,1]))
newdata$pred <- pred(newdata$effect, intercept=newdata$subject)


qplot(effect, pred, group=subject, data=subset(newdata, subject %in% sample(size=2, x=unique(newdata$subject))), geom="line")

#################
# library(MASS) is needed for mvrnorm

power=NULL
M <- 20
set.seed(79345)
tau <- mvrnorm(n = M, mu=c(0,0),Sigma=vv)
for (i in 1:M){
xb <- X* B + Z %*%tau[i,]
power <- cbind(power,exp(xb)/(1+exp(xb)))
}
colnames(power) <- 1:M

ump_pow <- NULL
for (i in effect)ump_pow <- c(ump_pow,calculate_ump_power(beta=i/10,n=100,sigma=1))


pow.dat <- data.frame(effects = c(-effect, effect)
                    , rbind(power,power)
                    , UMP=rep(ump_pow,2))
pow.dat.m <- melt(pow.dat, id="effects")
head(pow.dat.m)
tail(pow.dat.m)

qplot(effects,value,group=variable,geom="line",alpha=I(.2), data=pow.dat.m) + 
 xlab(expression(beta)) + ylab("Power") + 
 geom_line(data=subset(pow.dat.m, variable=="UMP"),colour="hotpink", size=1) + 
 xlab("Effect(E)")
 
ggsave(filename="../images/effect_power_subject_exp1.pdf", height=5, width=7)   






