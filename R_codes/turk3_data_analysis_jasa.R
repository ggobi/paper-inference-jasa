# This is a complete turk3 experiment codes file 
# last updated my Mahbub on Feb 11, 2012

library(ggplot2)

dat <- read.csv("../data/feedback_data_turk3_50p.txt")

getx <- function(n){
	x1 <- rnorm(n,0,1)
	nc <- 15*n/100
	x2 <- rnorm(n=nc,mean=-1.75, sd=1/3)
	return(c(x2,x1))
}
getx(100)

gety <- function(x,beta,sigma){
	gamma <- 10
	alpha <- 0
	n <- length(x)
	nc <- n - n*100/115
	x1 <- x[(nc+1):n]
	y1 <- alpha + beta*x1 + rnorm(n=n-nc,0,sigma)
	y2 <- rnorm(nc,gamma,sigma/3)
	return(c(y2,y1))
}

x <- getx(n=100)
y <- gety(x,beta=.5,sigma=5)

qplot(x,y)

sg <- sapply(1:100, function(i){y <- gety(x,beta=.6,sigma=3.85);return(sd(y))})
qplot(sg,geom="density")

# ----------- examine beta sigma relation -----------------


est_sigma <- function(par){
	beta <- par[1]
	sigma <- par[2]
	y <- gety(x,beta=beta,sigma=sigma)
	return(sd(y))
}
bb <- seq(0,10,length=10)
ss <- seq(3,6,length=10)

ess <- apply(expand.grid(bb,ss),1,est_sigma)

contour(list(x=expand.grid(bb,ss)[,1],y=expand.grid(bb,ss)[,2],z=ess))



n <- 100
nc <- 15
beta <- 3
sigma <- 5
gamma <- 15
x1 <- rnorm(n,0,1)
y1 <- 5 + beta*x1 + rnorm(n,0,sigma)
qplot(x1,y1)

x2 <- rnorm(n=nc,mean=-1.75, sd=1/3)
y2 <- rnorm(nc,gamma,sigma/3)

x <- c(x2,x1)
y <- c(y2,y1)

qplot(x,y)+stat_smooth(method="lm")
ggsave("contaminated_data.pdf")


# ------------- ump power function --------------------------- 

calculate_ump_power <- function (beta, n, sigma,x){
       alpha <- 0.05
       sigmasq <- sigma^2
       beta_not <- 0
       df <- n-1 # one parameter(s)
       se_beta <- sqrt(sigmasq/sum((x-mean(x))^2))
       mu <- beta/se_beta
       alpha <- alpha/2
       t_n <- qt(p=1-alpha,df=df)
       res <- pt(q=-t_n, df=df, ncp=mu)-pt(q=t_n, df=df, ncp=mu)+1
       return(res)
}

calculate_ump_power(beta=.1,n=100,sigma=5,x=getx(100))

x <- getx(100)
betas <- seq(0,2.5,length=100)
pw <- sapply(betas,function(b){return(calculate_ump_power(beta=b,n=100,sigma=5,x=x))})
qplot(c(-betas,betas),c(pw,pw),geom="line")+xlab(expression(beta)) + ylab("power")


# ---------------------- fitting loess smoother  ------------

fit <- loess(as.numeric(response) ~ abs(beta),data=dat,span=1.5)
betas <- seq(0,2.5,by=.05)
with(dat,qplot(betas,predict(fit,data.frame(beta=betas))))
with(dat,qplot(beta,predict(fit)))


get_smooth_power <- function(dat.n,test="Empirical"){
 betas <- seq(0.01,2.5, by=0.05)
 dat_smooth <- NULL
 for (s in c(5)){
	for (n in c(100)){
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
 beta <- seq(0.01,2.5, by=0.05)
 dat_pow <- NULL
 for(n in c(100)){
  for(sg in c(5)){
     pow <- NULL
     x <- getx(100)
     for (i in beta)pow <- c(pow,calculate_ump_power(beta=i,n=n,sigma=sg,x=x))
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

p <- ggplot(dat_emp_pow,aes(beta,pow)) + geom_line(aes(colour=test))
p <- p + geom_line(aes(beta,pow, colour=test), data=dat_ump_pow)
p <- p + facet_grid(sample_size~sigma)
p + xlab(expression(beta))+ylab("Power")




# ---------- bootstrap band for empirical power -----------------------

dat_boot_pow <- NULL
for (i in 1:1000){
dat.b <- ddply(dat,.(abs(beta),sample_size,sigma), summarize,
           response = sample(response,replace=T)
           )
colnames(dat.b) <- c("beta", colnames(dat.b)[-1])
dat_boot_pow <- rbind(dat_boot_pow,get_smooth_power(dat.b,test=paste("smooth",i,sep="")))
}
# write.csv(dat_boot_pow,file="../data/dat_bootstrap_power3.txt",row.names=F)

dat_boot_pow <- read.csv("../data/dat_bootstrap_power3.txt")


p <- ggplot(dat_boot_pow, aes(beta,pow,colour=grey)) + geom_line(aes(group=test),alpha=I(0.1))
p + facet_grid(sample_size~sigma)


p <- ggplot() +
     geom_line(aes(beta,pow, group =test), data=dat_boot_pow,alpha=I(0.3)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 
p        


dat_obs_val <- ddply(dat,.(abs(beta),sample_size,sigma,response), summarize,
           responses = length(response)
           )
colnames(dat_obs_val) <- c("beta", colnames(dat_obs_val)[-1])           
dat_obs_val <- rbind(dat_obs_val,cbind(beta= -dat_obs_val[,1],dat_obs_val[,-1]))           

         
dat_boot_ribbon <-  ddply(dat_boot_pow,.(abs(beta),sample_size,sigma), summarize,
           limit1 = quantile(pow,.025, na.rm=T),
           limit2 = quantile(pow,.975, na.rm=T)
           )
colnames(dat_boot_ribbon) <- c("beta",colnames(dat_boot_ribbon)[-1])           
dat_boot_ribbon <- rbind(dat_boot_ribbon,cbind(beta= -dat_boot_ribbon[,1],dat_boot_ribbon[,-1])) 


p <- ggplot() +
     geom_point(aes(beta,as.numeric(response), size=responses), data=dat_obs_val, colour=alpha("black",.4)) +
     geom_ribbon(aes(x=beta,ymin=limit1,ymax=limit2), data=dat_boot_ribbon, fill=alpha("black",.4)) +
     geom_line(aes(beta,pow, colour=test), data=dat_emp_pow) +
     geom_line(aes(beta,pow, colour=test), data=dat_ump_pow) +
     facet_grid(sample_size~sigma) +
     xlab(expression(beta))+ylab("Power") 
p
         
ggsave(p, filename="../images/power_loess_exp3.pdf", height=5.5, width=8.5)

#ggsave(p, filename="../images/power_loess_exp3.pdf")







# ----- fitting the same model with beta=0 -------------

sim <- matrix(5 + rnorm(n=20*length(x),0,sigma),ncol=20)
sim[1:nc,] <- matrix(rep(y2,20),ncol=20) 
loc <- sample(1:20,size=1)
sim[,loc] <- y

dat <- data.frame(x,sim)
colnames(dat) <- c("X",1:20)

m.dat <- melt(dat, id="X")

qplot(X,value,data=m.dat,alpha=I(.2))+facet_wrap(~variable)

# --------- fitting linear model -------------------

fit <- lm(y~x)
sig_hat <- summary(fit)$sigma
beta_not <- fit$coefficients[[1]]

sim <- matrix(rnorm(n=20*length(x),mean=beta_not,sd=sig_hat),ncol=20)
loc <- sample(1:20,size=1)
sim[,loc] <- y

dat <- data.frame(x,sim)
colnames(dat) <- c("X",1:20)
m.dat <- melt(dat, id="X")

p <- qplot(X,value,data=m.dat,alpha=I(.2), ylab="Y")+facet_wrap(~variable)

#+ opts(aspect.ratio = 1)

# setwd("U:\\PhD Research\\feedback_turk3\\plots")
# ggsave("try5.png",plot=p, width=7.17, height=7.17, dpi=75)


# ---------- creating example plot ----------------------------

n <- 100
nc <- 15
beta <- -7
sigma <- 5
gamma <- 15
x1 <- rnorm(n,0,1)
y1 <- 5 + beta*x1 + rnorm(n,0,sigma)
#qplot(x1,y1)

x2 <- rnorm(n=nc,mean=-1.75*sign(beta), sd=1/3)
y2 <- rnorm(nc,gamma,sigma/3)

#qplot(x2,y2)

x <- c(x1,x2)
y <- c(y1,y2)

#qplot(x,y)

fit <- lm(y~x)
sig_hat <- summary(fit)$sigma
beta_not <- fit$coefficients[[1]]

sim <- matrix(rnorm(n=4*length(x),mean=beta_not,sd=sig_hat),ncol=4)
loc <- sample(1:4,size=1)
sim[,loc] <- y

dat <- data.frame(x,sim)
colnames(dat) <- c("X",1:4)
m.dat <- melt(dat, id="X")

p <- qplot(X,value,data=m.dat,alpha=I(.2), ylab="Y")+facet_wrap(~variable, ncol=4)

# setwd("U:\\PhD Research\\feedback_turk3\\plots")
# ggsave("example1.png",plot=p, width=5, height=2.5, dpi=75)



# ---------- creating a lineup plot ----------------------------


generate_plot <- function(n,sigma,beta){
	#n <- 100;nc <- 15; beta <- .1; sigma <- 4; 
	gamma <- 10 
	beta <- sign(rnorm(1))*beta
	nc <- n*15/100
	x1 <- rnorm(n,0,1)
	x2 <- rnorm(n=nc,mean=-1.75*sign(beta), sd=1/3)
	x <- c(x1,x2)
	sig_hat <- 0
	while(sig_hat<4.99 || sig_hat>5.01){
		y1 <- beta0 + beta*x1 + rnorm(n,0,sigma)
		y2 <- rnorm(nc,gamma,sigma/3)
		y <- c(y1,y2)
		fit <- lm(y~x-1)
		sig_hat <- summary(fit)$sigma
	}
	pval <- summary(fit)$coefficients[,4]

	sim <- matrix(rnorm(n=20*length(x),mean=0,sd=sig_hat),ncol=20)
	loc <- sample(1:20,size=1)
	sim[,loc] <- y

	dat <- data.frame(x,sim)
	colnames(dat) <- c("X",1:20)
	m.dat <- melt(dat, id="X")

	p <- qplot(X,value,data=m.dat,alpha=I(.2), ylab="Y")+facet_wrap(~variable)
	result = c(n,beta,5,pval,loc)
	return(list(result=result,p=p, dat_used=data.frame(x,y)))
}


sigma=3.5;beta=2.1
pl <- generate_plot(n=100,sigma=sigma,beta=beta)
pl$result

#file_dir <- "/Volumes/mahbub/PhD Research/feedback_turk3/plots/turk3"
#file_name <- paste(file_dir,"/100_",beta,"_",sigma,"_6.pdf",sep="")
#ggsave(file_name,plot=pl$p)

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
p


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

ddply(subset(dat, pic=='plot_turk3_100_10_5_1.png'), .(gender), summarize,
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = sum(response)*100/sum(response==response)
    )

# --------------- p_value vs %correct ----------------------------------

pdat <- ddply(dat, .(p_value, sample_size), summarize,
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = sum(response)*100/sum(response==response)
    )

# pdat$percent_correct <- pdat$corrected*100/pdat$attempted

p <- ggplot(pdat, aes(p_value,percent_correct))
p+ geom_point(size=3)

p <- ggplot(pdat, aes(p_value,percent_correct))
p <- p+ geom_point(size=3)
p <- p + stat_smooth(se=F) 
p 


p <- ggplot(subset(pdat, p_value <= 0.099), aes(p_value,percent_correct))
p <- p+ geom_point(size=3)
p <- p + stat_smooth( se=F) 
p 

# --------------- distribution of time taken ----------------------------------


ggplot(dat, aes(time_taken))+geom_histogram(binwidth=2)+xlim(0,500)

# --------------- p_value vs %correct by male female ----------------------------------

pdat <- ddply(dat, .(p_value, gender, sample_size), summarize,
	attempted = sum(response==response),
	corrected = sum(response),
	percent_correct = sum(response)*100/sum(response==response)
    )

# pdat$percent_correct <- pdat$corrected*100/pdat$attempted

p <- ggplot(pdat, aes(p_value,percent_correct, colour=gender, shape=gender))
p+ geom_point(size=3)

p <- ggplot(pdat, aes(factor(p_value),percent_correct, colour=gender, shape=gender))
p <- p+ geom_point(size=3)
p <- p + stat_smooth(aes(group=gender, linetype=gender), se=F) 
p 



p <- ggplot(subset(pdat, p_value <= 0.099), aes(factor(p_value),percent_correct, colour=gender, shape=gender))
p <- p+ geom_point(size=3)
p <- p + stat_smooth(aes(group=gender, linetype=gender), se=F) 
p 


# ------------- number of attempts and % correct

dat$attempt_no <- unlist(tapply(dat$start_time, dat$id, 
      function(x){return(as.integer(factor(x)))}), use.names=F)

adat <- ddply(dat, .(attempt_no, gender), summarize,
	attempted = sum(response==response),
	corrected = sum(response),
      percent_correct = sum(response)*100/sum(response==response)
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

power_obs_ump <- cbind(dat_obs[,1:4],ump=sapply(dat_obs[,1],mypow,n=100,sigma=5,x=getx(100)))
#setwd("U:/phd research/simulation_study/turk")
#write.table(power_obs_ump,file="power_obs_ump_exp3.txt",row.names=F)

dat_obs1 <- dat_obs
dat_obs1$beta <- -dat_obs1$beta
dat_obs <- rbind(dat_obs1,dat_obs)
dat_obs.m <- melt(dat_obs, id=c("beta","sample_size","sigma"))
colnames(dat_obs.m) <- c("beta","sample_size","sigma","test","pow")
head(dat_obs.m)

qplot(beta,pow, geom=c("point","line"), data=dat_obs.m, colour=test) + facet_grid(sample_size~sigma)

beta <- seq(0.01,3, by=.1)
x <- getx(100)
dat_pow <- NULL
for(n in c(100)){
  for(sg in c(5)){
     pow <- NULL
     for (i in beta)pow <- c(pow,mypow(beta=i,n=n,sigma=sg,x=x))
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

