# This is a complete turk 2 data analysis for jasa paper
# R script file last modified on Feb 10, 2012 by Mahbub

library(ggplot2)


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
	# n <- 100
	# sigmasq <- 12^2
	alpha <- 0.05
	sigmasq <- sigma^2
	beta_not <- 0
	df <- n-2 # two parameters
	x <- subset(read.csv("../data/Xdata.csv"),N==n)[,1]
   se_beta <- sqrt(sigmasq/sum((x-mean(x))^2)) 
	mu <- beta/se_beta
	alpha <- alpha/2
	t_n <- qt(p=1-alpha,df=df)
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


	#	get_smooth_power <- function(dat.n,test="Empirical"){
	#	 betas <- seq(0.01,6.5, by=.2)
	#	 betas <- c(-betas,betas)
	#	 dat_smooth <- NULL
	#	 for (s in c(5,12)){
	#	for (n in c(100,300)){
	#	dats <- subset(dat.n,sigma==s & sample_size==n)
	#	dats$y <- as.numeric(dats$response)
	#	fit <- loess(c(y,y) ~ c(-abs(beta),abs(beta)),data=dats,span=1+(n==100)*.3)
	#	dat_smooth <- rbind(dat_smooth,cbind(betas,predict(fit,betas),n,s))
	#	}
	#	}
	#	 colnames(dat_smooth) <- c("beta","pow","sample_size","sigma")
	#	 dat_empirical <- data.frame(rbind(dat_smooth,cbind(-dat_smooth[,1],dat_smooth[,-1])))
	#	 dat_empirical$test <- test
	#	 return(dat_empirical)
	#	}



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
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val, colour=alpha("black",.3)) +
     geom_ribbon(aes(x=beta,ymin=limit1,ymax=limit2), data=dat_boot_ribbon, fill=alpha("black",.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) + scale_color_brewer(palette=6) +
     xlab(expression(beta))+ylab("Power") 
p


p <- ggplot() +
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val) +
     geom_ribbon(aes(x=beta,ymin=limit1,ymax=limit2), data=dat_boot_ribbon) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) + scale_color_brewer(palette=6) +
     xlab(expression(beta))+ylab("Power") 
p
         
ggsave(p, filename="../images/power_loess_exp2.pdf", height=5.5, width=8.5)


# ggsave(p, filename="../images/power_loess_exp2.pdf")





# ----------------------- confidence interval for empirical power -------------------

exact <- function(yy, nn, alpha=0.05) {
	pL <- rep(0,length(yy))
	pU <- rep(1,length(yy))

	y <- yy[yy!=0]
	n <- nn[yy!=0]
	pL[yy!=0] <- 1/(1+ (n-y+1)/(y*qf(alpha/2, 2*y, 2*(n-y+1))))

	y <- yy[yy!=nn]
	n <- nn[yy!=nn]
	pU[yy!=nn] <- 1/(1+ (n-y)/((y+1)*qf(1-alpha/2, 2*(y+1), 2*(n-y))))
	return(data.frame(pL, pU))
}

param <- dat1$param
beta <- dat1$beta
#table(beta)

dat_pi <- ddply(dat1,.(beta), summarize, 
              yy=sum(response=="TRUE"),
              nn = length(response))


yy <- dat_pi$yy
nn <-dat_pi$nn

pw <- yy/nn
Power_graphical <- c(pw,pw)
beta_g <- c(dat_pi$beta,-dat_pi$beta)
#qplot(beta_g,Power_graphical, geom=c("point","line"), shape=3,xlab=expression(beta))  # symmetric power curve

cl <- exact(yy,nn, alpha=.05) # fisher's exact confidence interval
conf_limit <- c(cl$pL,cl$pL,cl$pU,cl$pU)


n <- 100
sigma <- 12
beta_t1 <- seq(-1.4,1.4, length=125)
Power_theoretical1 <- sapply(beta_t1, mypow, n=n, sigma=sigma)
qplot(beta_t1,Power_theoretical1, geom="line")

sigma <-5
beta_t2 <- seq(-.65,.65, length=125)
Power_theoretical2 <- sapply(beta_t2, mypow, n=n, sigma=sigma)
qplot(beta_t2,Power_theoretical2, geom="line")

xl <- expression(beta)

n <- 300
sigma <- 12
beta_t3 <- seq(-4,4, length=125)
#beta_t3 = c(-c(0, 0.8, 2, 3, 4),c(0, 0.8, 2, 3, 4))
Power_theoretical3 <- sapply(beta_t3, mypow, n=n, sigma=sigma)
Rsq_theoretical3 <- sapply(beta_t3, Rsqr, n=n, sigma=sigma)
qplot(beta_t3,Power_theoretical3, geom="line",xlab=xl, ylab="Power")
qplot(beta_t3,Rsq_theoretical3, geom="line",xlab=xl, ylab="Power")

sigma <-5
beta_t4 <- seq(-1.7,1.7, length=125)
Power_theoretical4 <- sapply(beta_t4, mypow, n=n, sigma=sigma)
qplot(beta_t4,Power_theoretical4, geom="line",xlab=xl, ylab="Power")

sample.size <- c(rep(100,250),rep(300,250))
sigma <- rep(c(rep(12,125),rep(5,125)),2)
betas <- c(beta_t1,beta_t2,beta_t3,beta_t4)
power_theo <- c(Power_theoretical1,Power_theoretical2,Power_theoretical3,Power_theoretical4)
pow.dat <- data.frame(beta=betas, power=power_theo,sample.size,sigma)
xl <- expression(beta)
qplot(betas,power, data=pow.dat, geom="line",xlab=xl)+ facet_grid(sample.size~sigma)

power_curve <- c(rep(" Theoretical test",length(Power_theoretical)),rep(" Visual test",length(Power_graphical)),rep("lower_CL",length(Power_graphical)),rep("upper_CL",length(Power_graphical)))
Power <- c(Power_theoretical,Power_graphical,conf_limit)
betas <- c(beta_t,rep(beta_g,3))
#qplot(betas,Power, geom=c("line"), linetype=power_curve,size=I(1),shape=8,colour=power_curve,xlab=expression(beta))  # symmetric power curve
plot_dat <- data.frame(betas,Power,power_curve)
return(plot_dat)
}


# ---------------------- some exploratory data analysis ------------

pdat <- ddply(dat, .(sample_size,sigma,beta), summarize,
	attempted = sum(response==response),
	corrected = sum(response==TRUE),
	percent_correct = round(sum(response==TRUE)*100/sum(response==response),2)
    )

pdat <- ddply(dat, .(pic), summarize,
	attempted = sum(response==response),
	corrected = sum(response==TRUE),
	percent_correct = round(sum(response==TRUE)*100/sum(response==response),2)
    )
    
pdat <- ddply(dat, .(gender), summarize,
	avg.time = round(mean(time_taken),2),
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = round(sum(response)*100/sum(response==response),2),
	participants = length(unique(id))
    )    
pdat

pdat <- ddply(dat, .(academic_study), summarize,
	avg.time = round(mean(time_taken),2),
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = round(sum(response)*100/sum(response==response),2),	participants = length(unique(id))
    )
pdat   

pdat <- ddply(dat, .(conf_level), summarize,
	avg.time = round(mean(time_taken),2),
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = round(sum(response)*100/sum(response==response),2),
	participants = length(unique(id))
    )
pdat   

pdat <- ddply(dat, .(age), summarize,
	avg.time = round(mean(time_taken),2),
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = round(sum(response)*100/sum(response==response),2),
	participants = length(unique(id))
    )
pdat   


# --------------- p_value vs time_taken ------------------------------

ggplot(dat,aes((p_value)^.15,(time_taken)^.1))+geom_point()+geom_smooth()

p <- ggplot(dat, aes(factor(p_value), time_taken))
p <- p + geom_boxplot() +ylim(0,450) + facet_grid(gender~.)
p 


pdat1 <- ddply(dat, .(p_value), summarize,
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

pdat <- ddply(dat, .(p_value, sample_size), summarize,
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = mean(response)*100
    )

# pdat$percent_correct <- pdat$corrected*100/pdat$attempted

p <- ggplot(pdat, aes(p_value,percent_correct))
p+ geom_point(size=3)

p <- ggplot(pdat, aes(p_value,percent_correct))
p <- p+ geom_point(size=3)
p <- p + stat_smooth(se=F) 
p + facet_grid(sample_size~.)


p <- ggplot(subset(pdat, p_value <= 0.099), aes(p_value,percent_correct))
p <- p+ geom_point(size=3)
p <- p + stat_smooth( se=F) 
p + facet_grid(sample_size~.)

# --------------- distribution of time taken ----------------------------------


ggplot(dat, aes(time_taken))+geom_histogram(binwidth=2)+xlim(0,500)

# --------------- p_value vs %correct by male female ----------------------------------

pdat <- ddply(dat, .(p_value, gender, sample_size), summarize,
	attempted = length(response),
	corrected = sum(response),
	percent_correct = mean(response)*100
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
	attempted = length(response),
	corrected = sum(response),
      percent_correct = mean(response)*100
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
               empirical = exact_ci(yy=sum(response==TRUE),nn= sum(response==response))[,1],
               lower_limit = exact_ci(yy=sum(response==TRUE),nn= sum(response==response))[,2],
               upper_limit = exact_ci(yy=sum(response==TRUE),nn= sum(response==response))[,3]
              )
colnames(dat_obs) <- c("beta","sample_size", "sigma","empirical","lower_limit", "upper_limit")

power_obs_ump <- cbind(dat_obs[,1:4],ump=sapply(dat_obs[,1],mypow,n=100,sigma=5))
#setwd("U:/phd research/simulation_study/turk")
#write.table(power_obs_ump,file="power_obs_ump_exp2.txt",row.names=F)

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



table(subset(dat_obs, sample_size==300 & sigma==12)$beta)
table(subset(dat, sample_size==300 & sigma==12)$beta)

dd <- subset(dat_obs, sample_size==300 & sigma==12)
dd.m <- melt(dd[,c(1,4:6)], id='beta')
beta <- seq(-5.5,5.5,by=.01)
ppw <- sapply(beta,mypow,n=300,sigma=12)
ppw <- data.frame(beta,variable="UMP",value=ppw)
dd.mm <- rbind(dd.m,ppw)
qplot(beta, value, geom="line",colour=variable, data=dd.mm)


dd <- subset(dat_obs, sample_size==300 & sigma==5)
dd.m <- melt(dd[,c(1,4:6)], id='beta')
beta <- seq(-5.5,5.5,by=.01)
ppw <- sapply(beta,mypow,n=300,sigma=5)
ppw <- data.frame(beta,variable="UMP",value=ppw)
dd.mm <- rbind(dd.m,ppw)
qplot(beta, value, geom="line",colour=variable, data=dd.mm)


dd <- subset(dat_obs, sample_size==100 & sigma==12)
dd.m <- melt(dd[,c(1,4:6)], id='beta')
beta <- seq(-5.5,5.5,by=.01)
ppw <- sapply(beta,mypow,n=100,sigma=12)
ppw <- data.frame(beta,variable="UMP",value=ppw)
dd.mm <- rbind(dd.m,ppw)
qplot(beta, value, geom="line",colour=variable, data=dd.mm)


dd <- subset(dat_obs, sample_size==100 & sigma==5)
dd.m <- melt(dd[,c(1,4:6)], id='beta')
beta <- seq(-5.5,5.5,by=.01)
ppw <- sapply(beta,mypow,n=100,sigma=5)
ppw <- data.frame(beta,variable="UMP",value=ppw)
dd.mm <- rbind(dd.m,ppw)
qplot(beta, value, geom="line",colour=variable, data=dd.mm)



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

# ----- power curves from loess fit ---------------

p <- qplot(c(-abs(beta),abs(beta)), as.numeric(rep(response,2)), geom="point", data=dat)
p <- p + facet_grid(sample_size~sigma)
p <- p + geom_smooth(method="loess") +xlab(expression(beta)) + ylab("Response")
p

beta <- seq(-6,6,by=.02)
ppw <- c(sapply(beta,mypow,n=100,sigma=5),
         sapply(beta,mypow,n=100,sigma=12),
         sapply(beta,mypow,n=300,sigma=5),
         sapply(beta,mypow,n=300,sigma=12))
sample_size <- rep(c(100,300), each=length(beta)*2)
sigma <- rep(c(5,12,5,12), each = length(beta))
power <- rep("Theoretical",length(beta))

theo_pow <- data.frame(beta=rep(beta,4),ppw,sample_size,sigma,power)
         
p <- qplot(beta,ppw,geom="line", data=theo_pow)
p + facet_grid(sample_size~sigma)

p + qplot(beta,ppw,geom="line", data=theo_pow)











