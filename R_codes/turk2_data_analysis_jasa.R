# This is a complete turk 2 data analysis for jasa paper
# R script file last modified on Mar 29, 2012 by Mahbub

library(ggplot2)
library(plyr)
library(reshape)


raw.dat <- read.csv("../data/raw_data_turk2.csv")


# ========== policy for excluding bad data =========
# Everyone is given at least one easy lineup. If a person can't correctly
# evaluate 50% of those extremely easy lineups, all his responses are discarded.
# A lineup with p_value < 0.0002 is considered easy


dp <- ddply(subset(raw.dat, p_value < 0.0002),.(id),summarise,
   cnt_easy = length(response),
   percent_correct = mean(response)*100
  )

included_id <- dp$id[ dp$percent_correct > 49]
dat <- subset(raw.dat, id %in% included_id)



# ------------- ump power function --------------------------- 

calculate_ump_power <- function (beta, n, sigma){
   alpha <- 0.05
   beta_not <- 0
   df <- n-2 # two parameters
   x <- subset(read.csv("../data/Xdata.csv"),N==n)[,1]
   ssx <- sum((x-mean(x))^2)
   se_beta <- sigma/sqrt(ssx) 
   mu <- beta/se_beta
   t_n <- qt(p=1-alpha/2,df=df)
   res <- pt(q=-t_n, df=df, ncp=mu)-pt(q=t_n, df=df, ncp=mu)+1
   return(res)
}

calculate_ump_power(beta = 5.15,n=100,sigma=12)

bet <- seq(-4,4, by=.1)
pw <- sapply(bet,calculate_ump_power,n=300,sigma=12)
qplot(bet,pw, geom="line") + 
   xlab(expression(beta))  +
   ylab("UMP Power")



# ---------------------- fitting loess smoother  ------------

fit <- loess(as.numeric(response) ~ abs(beta),data=subset(dat,sigma==12 & sample_size==100),span=1.2)
betas <- seq(0,6.2,by=.1)
with(subset(dat,sigma==12 & sample_size==100),qplot(betas,predict(fit,data.frame(beta=betas))))
with(subset(dat,sigma==12 & sample_size==100),qplot(beta,predict(fit)))


get_smooth_power <- function(dat.n,test="Empirical"){
 betas <- seq(0.01,6.5, by=.2)
 dat_smooth <- NULL
 for (s in c(5,12)){
	for (n in c(100,300)){
		dats <- subset(dat.n,sigma==s & sample_size==n)
		dats$y <- as.numeric(dats$response)
		fit <- loess(y ~ abs(beta),data=dats,span=1+(n==100)*.3)
		dat_smooth <- rbind(dat_smooth,cbind(betas,predict(fit,betas),n,s))
		}
	}
 colnames(dat_smooth) <- c("beta","pow","sample_size","sigma")
 dat_empirical <- data.frame(rbind(dat_smooth,cbind(-dat_smooth[,1],dat_smooth[,-1])))
 dat_empirical$test <- test
 return(dat_empirical)
}

get_ump_power <- function(dat,test="UMP"){
 beta <- seq(0.01,6.5, by=.2)
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
     scale_color_brewer(palette=6) +
     xlab(expression(beta))+ylab("Power")
p



# ---------- bootstrap band for empirical power -----------------------

# WARNING: following codes take 30 min to run

dat_boot_pow <- NULL
for (i in 1:1000){
dat.b <- ddply(dat,.(abs(beta),sample_size,sigma), summarize,
           response = sample(response,replace=T)
           )
colnames(dat.b) <- c("beta", colnames(dat.b)[-1])
dat_boot_pow <- rbind(dat_boot_pow,get_smooth_power(dat.b,test=paste("smooth",i,sep="")))
}

# END WARNING 

#write.csv(dat_boot_pow,file="../data/dat_bootstrap_power2.txt",row.names=F)
#dat_boot_pow <- read.csv("../data/dat_bootstrap_power2.txt")


ggplot(dat_boot_pow, aes(beta,pow,colour=grey)) + 
     geom_line(aes(group=test),alpha=I(0.1)) + 
     facet_grid(sample_size~sigma)


p <- ggplot() +
     geom_line(aes(beta,pow, group =test), data=dat_boot_pow,alpha=I(0.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 
p


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
p


p <- ggplot() +
     geom_line(aes(beta,pow, group =test,linetype=5), data=dat_boot_pow_limit,colour=alpha("red",.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     geom_point(aes(beta,as.numeric(response)), data=dat) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 
p


dat_obs_val <- ddply(dat,.(abs(beta),sample_size,sigma,response), summarize,
           responses = length(response)
           )
colnames(dat_obs_val) <- c("beta", colnames(dat_obs_val)[-1])           
dat_obs_val <- rbind(dat_obs_val,cbind(beta= -dat_obs_val[,1],dat_obs_val[,-1]))           

p <- ggplot() +
     geom_line(aes(beta,pow, group =test), data=dat_boot_pow,alpha=I(0.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 
p

         
dat_boot_ribbon <-  ddply(dat_boot_pow,.(abs(beta),sample_size,sigma), summarize,
           limit1 = quantile(pow,.025, na.rm=T),
           limit2 = quantile(pow,.975, na.rm=T)
           )
colnames(dat_boot_ribbon) <- c("beta",colnames(dat_boot_ribbon)[-1])           
dat_boot_ribbon <- rbind(dat_boot_ribbon,cbind(beta= -dat_boot_ribbon[,1],dat_boot_ribbon[,-1]))



p <- ggplot() +
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val) +
     geom_ribbon(aes(x=beta,ymin=limit1,ymax=limit2), data=dat_boot_ribbon) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) + scale_color_brewer(palette=6) +
     xlab(expression(beta))+ylab("Power") 
p

p <- ggplot() +
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val, colour=alpha("black",.3)) +
     geom_ribbon(aes(x=beta,ymin=limit1,ymax=limit2), data=dat_boot_ribbon, fill=alpha("black",.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) + scale_color_brewer(palette=6) +
     xlab(expression(beta))+ylab("Power") 
p
         
ggsave(p, filename="../images/power_loess_exp2.pdf", height=5.5, width=8.5)


# ------------ some exploratory data analysis ------------

pdat <- ddply(dat, .(sample_size,sigma,beta), summarize,
	attempted = length(response),
	corrected = sum(response),
	percent_correct = round(mean(response)*100,2)
    )

pdat <- ddply(dat, .(pic), summarize,
	attempted = length(response),
	corrected = sum(response),
	percent_correct = round(mean(response)*100,2)
    )
    
pdat <- ddply(dat, .(gender), summarize,
	avg.time = round(mean(time_taken),2),
	attempted = length(response),
	corrected = sum(response),
	percent_correct = round(mean(response)*100,2),
	participants = length(unique(id))
    )    
pdat

pdat <- ddply(dat, .(academic_study), summarize,
	avg.time = round(mean(time_taken),2),
	attempted = length(response),
	corrected = sum(response),
	percent_correct = round(mean(response)*100,2),
	participants = length(unique(id))
    )    
pdat   

pdat <- ddply(dat, .(conf_level), summarize,
	avg.time = round(mean(time_taken),2),
	attempted = length(response),
	corrected = sum(response),
	percent_correct = round(mean(response)*100,2),
	participants = length(unique(id))
    )    
pdat   

pdat <- ddply(dat, .(age), summarize,
	avg.time = round(mean(time_taken),2),
	attempted = length(response),
	corrected = sum(response),
	percent_correct = round(mean(response)*100,2),
	participants = length(unique(id))
    )    
pdat   


# --------------- p_value vs time_taken ------------------------------

ggplot(dat,aes((p_value)^.15,(time_taken)^.1))+geom_point()+geom_smooth()

p <- ggplot(dat, aes(factor(p_value), time_taken)) + 
  geom_boxplot() +ylim(0,450) + facet_grid(gender~.)
p 


pdat1 <- ddply(dat, .(p_value, sample_size,sigma), summarize,
	avg_time_taken = median(time_taken),
	num_responded = length(p_value)
    )

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
	corrected = sum(response),
	percent_correct = sum(response)*100/sum(response==response)
    )

pval_calculated <- apply(pdat[,3:4],1,pval)

qplot(pdat$p_value,pval_calculated, xlab="p_value of Mathematical Test", ylab="p_value of Visual Test")

ddply(subset(dat, pic_id==35), .(gender), summarize,
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = sum(response)*100/sum(response==response)
    )

ddply(subset(dat, substr(pic_name,1,12)=='plot_100_3_5'), .(gender), summarize,
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = sum(response)*100/sum(response==response)
    )


# --------------- p_value vs %correct ----------------------------------

pdat <- ddply(dat, .(p_value, sample_size,sigma), summarize,
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = mean(response)*100
    )

p <- ggplot(pdat, aes(p_value,percent_correct)) + 
     geom_point(size=3)

p <- ggplot(pdat, aes(p_value,percent_correct))+
     geom_point(size=3) + stat_smooth(se=F) +
     facet_grid(sample_size~sigma)


p <- ggplot(subset(pdat, p_value <= 0.099), aes(p_value,percent_correct))+
     geom_point(size=3) + stat_smooth( se=F) +
     facet_grid(sample_size~sigma)

# --------------- distribution of time taken ----------------------------------


ggplot(dat, aes(time_taken))+geom_histogram(binwidth=2)+xlim(0,500)

# --------------- p_value vs %correct by male female ----------------------------------

pdat <- ddply(dat, .(p_value, gender, sample_size,sigma), summarize,
	attempted = length(response),
	corrected = sum(response),
	percent_correct = mean(response)*100
   )

p <- ggplot(pdat, aes(p_value,percent_correct, colour=factor(gender), shape=factor(gender)))
p+ geom_point(size=3)

ggplot(pdat, aes(factor(p_value),percent_correct, colour=factor(gender), shape=factor(gender)))+
  geom_point(size=3)+
  stat_smooth(aes(group=factor(gender), linetype=factor(gender)), se=F) + 
  facet_grid(sample_size~.)


ggplot(subset(pdat, p_value <= 0.099), aes(factor(p_value),
  percent_correct, colour=factor(gender), shape=factor(gender)))+
  geom_point(size=3)+
  stat_smooth(aes(group=factor(gender), linetype=factor(gender)), se=F) + 
  facet_grid(sample_size~.)

# ------------- number of attempts and % correct ----------

dat$attempt_no <- unlist(tapply(dat$start_time, dat$id, 
      function(x){return(as.integer(factor(x)))}), use.names=F)

adat <- ddply(dat, .(attempt_no, gender), summarize,
	attempted = length(response),
	corrected = sum(response),
    percent_correct = mean(response)*100
    )

ggplot(subset(adat, attempt_no <= 12), aes(attempt_no,percent_correct, color=factor(gender), 
   shape=gender)) + geom_point(size=3) + 
   stat_smooth(aes(group=factor(gender), linetype=factor(gender)), se=F)

adat <- ddply(dat, .(attempt_no), summarize,
	attempted = length(response),
	corrected = sum(response),
    percent_correct = mean(response)*100
    )

ggplot(subset(adat, attempt_no <= 12), aes(attempt_no,percent_correct)) + 
   geom_point(size=3) + stat_smooth(se=F)


# --------------- number of attempts vs time_taken ------------------------

adat1 <- ddply(dat, .(attempt_no), summarize,
	avg_time_taken = mean(time_taken),
	avg_p_value = mean(p_value)
    )


ggplot(subset(adat1, attempt_no <= 12), aes(attempt_no,avg_time_taken))+ 
   geom_point(size=3) + 
   stat_smooth(se=F)


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

dat_obs <- ddply(dat,.(abs(beta),sample_size,sigma), summarize,
               empirical = exact_ci(yy=sum(response),nn= length(response))[,1],
               lower_limit = exact_ci(yy=sum(response),nn= length(response))[,2],
               upper_limit = exact_ci(yy=sum(response),nn= length(response))[,3]
              )
colnames(dat_obs) <- c("beta","sample_size", "sigma","empirical","lower_limit", "upper_limit")

power_obs_ump <- cbind(dat_obs[,1:4],ump=sapply(dat_obs[,1],calculate_ump_power,n=100,sigma=5))
#setwd("U:/phd research/simulation_study/turk")
#write.table(power_obs_ump,file="power_obs_ump_exp2.txt",row.names=F)

dat_obs1 <- dat_obs
dat_obs1$beta <- -dat_obs1$beta
dat_obs <- rbind(dat_obs1,dat_obs)
dat_obs.m <- melt(dat_obs, id=c("beta","sample_size","sigma"))
colnames(dat_obs.m) <- c("beta","sample_size","sigma","test","pow")
head(dat_obs.m)

qplot(beta,pow, geom="line", data=dat_obs.m, colour=test) + facet_grid(sample_size~sigma)

beta <- seq(0.01,6, by=.1)
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

plot_dat <- rbind(dat_obs.m,dat_pow[,c(1,3,4,5,2)])
head(plot_dat)
tail(plot_dat)

p <- qplot(beta,pow, geom="line", data=plot_dat, colour=test, linetype=test) + 
   facet_grid(sample_size~sigma) + 
   scale_colour_manual(values=c("red","red","red","cyan4")) + 
   scale_linetype_manual(values=c(1,2,2,1)) + 
   xlab(expression(beta)) + ylab("Power")


table(subset(dat_obs, sample_size==300 & sigma==12)$beta)
table(subset(dat, sample_size==300 & sigma==12)$beta)

qplot(beta, pow, geom="line",colour=test, data=subset(plot_dat, sample_size==300 & sigma==12))
qplot(beta, pow, geom="line",colour=test, data=subset(plot_dat, sample_size==100 & sigma==12))
qplot(beta, pow, geom="line",colour=test, data=subset(plot_dat, sample_size==300 & sigma==5))
qplot(beta, pow, geom="line",colour=test, data=subset(plot_dat, sample_size==100 & sigma==5))


# --------------- for symmetric power curves -----------------------------

dat_obs <- ddply(dat,.(abs(beta),sample_size,sigma), summarize,
               empirical = exact_ci(yy=sum(response),nn= sum(response==response))[,1],
               lower_limit = exact_ci(yy=sum(response),nn= sum(response==response))[,2],
               upper_limit = exact_ci(yy=sum(response),nn= sum(response==response))[,3]
              )
colnames(dat_obs)[1] <- "beta"
dat_obs1 <- dat_obs
dat_obs1$beta <- -dat_obs1$beta
dat_obs <- rbind(dat_obs1,dat_obs)
dat_obs.m <- melt(dat_obs, id=c("beta","sample_size","sigma"))
colnames(dat_obs.m) <- c("beta","sample_size","sigma","test","pow")
head(dat_obs.m)

qplot(beta,pow, geom="line", data=dat_obs.m, colour=test) + facet_grid(sample_size~sigma)

beta <- seq(0.01,6, by=.2)
dat_pow <- NULL
for(n in c(100,300)){
  for(sg in c(5,12)){
     pow <- NULL
     for (i in beta)pow <- c(pow,mypow(beta=i,n=n,sigma=sg))
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





# ----------- glm with effect as covariate ---------------

dat$effect <-  abs(with(dat,beta/sigma*sqrt(sample_size)))

effect.d <- ddply(dat,.(effect, replica), summarize, 
   power = mean(response),
   ump_power = calculate_ump_power(abs(beta[1]),sample_size[1],sigma[1]))

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
   
ggsave(filename="../images/effect_power_exp2.pdf", height=7, width=7)   
   
ggplot(data=effect.d,aes(x=effect,y=power-ump_power)) + 
   geom_point() + 
   geom_smooth(method="loess", span=0.8,aes(colour=factor(replica))) +
   scale_colour_discrete(name = "Replication")  
   
     



# and then fit the following model:
       response ~ effect + (effect | id)

# ------- subjectwise power curve with random slope  for covariate effect 
library(lme4)
fit.mixed <- lmer(response ~ effect + (1+effect|id)
              , family="binomial"
              , data=dat)
res <- summary(fit.mixed)
B <- res@coefs[,1]
effect <- seq(0.01,7, by=.2)
#res@frame
ones <- rep(1,length(effect))
#conf_level <- rep(1,length(beta))
X <- cbind(ones,effect)
dim(X)

Z <- cbind(ones,effect)
cov <- as.numeric(res@REmat[2,5])*as.numeric(res@REmat[1,4])*as.numeric(res@REmat[2,4])
vv <- matrix(as.numeric(c(res@REmat[1,3],rep(cov,2),res@REmat[2,3])),ncol=2, byrow=F)


# library(MASS) is needed for mvrnorm

power=NULL
M <- 20
set.seed(7945)
tau <- mvrnorm(n = M, mu=c(0,0),Sigma=vv)
for (i in 1:M){
xb <- X %*% B + Z %*%tau[i,]
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
 
ggsave(filename="../images/effect_power_subject_exp2.pdf", height=5, width=7)   











