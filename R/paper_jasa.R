# The complete R code for JASA Paper

library(ggplot2)
library(plyr)
library(reshape)
library(xtable)
library(lme4)
library(stringr)

# getting the data and some common functions

raw.dat1 <- read.csv("../data/raw_data_turk1.csv")
raw.dat2 <- read.csv("../data/raw_data_turk2.csv")
raw.dat3 <- read.csv("../data/raw_data_turk3.csv")

source("calculate_ump_power.R") # functions to compute power

# cleaning the data based on criteria 6 and removing the duplicated data
# (id,pic_id) should be unique since no person gets a lineup more than once.
clean_data <- function(raw_dat){
  easy_dat <- subset(raw_dat, p_value < 0.0002)
  if (raw_dat$experiment[1]=='turk3') easy_dat <- subset(raw_dat, difficulty==0)
  d <- ddply(subset(easy_dat),.(id),summarise,
             easy_cnt = length(response),
             percent_correct = mean(response)*100,
             include_id = response[1],
             excluded_lineup = paste(id[1],"_",pic_id[1],sep=""),
             excluded_lineup_p_value = round(p_value[1],4)
             )
  included_id <- d$id[d$include_id]
  indx <- raw_dat$id %in% included_id
  excluded_lineup <- paste(raw_dat$id,"_",raw_dat$pic_id,sep="") %in% d$excluded_lineup
  indx[excluded_lineup] <- FALSE
  cleaned_dat <- subset(raw_dat,indx)
  indx_dup <- with(cleaned_dat, !duplicated(data.frame(id,pic_id))) #duplication index
  # now returning cleaned data removing duplication
  return(subset(cleaned_dat,indx_dup))
}

indx1 <- c(3,6,7,8,9,17,18,20)
indx2 <- c(4,7,8,9,10,17,18,20)

dat1 <- clean_data(raw.dat1)[,-indx1]
dat2 <- clean_data(raw.dat2)[,-indx1]
dat3 <- clean_data(raw.dat3)[,-indx2]


# saving cleaned data as suplementary materials
write.csv(dat1, file='../submission_initial/suplementary/data/data_turk1.csv',row.names=F)
write.csv(dat2, file='../submission_initial/suplementary/data/data_turk2.csv',row.names=F)
write.csv(dat3, file='../submission_initial/suplementary/data/data_turk3.csv',row.names=F)




# =================== mathematical test t distribution  ===================

t <- seq(-4, 4, by = 0.1)
density_t <- dt(t, df = 15)
qplot(t, density_t, geom = "line") + geom_vline(xintercept = 2.5) + ylab("Density")
ggsave("../images/stat_mathematical_test.pdf",height=2,width=2.2)


# ===== computation of visual p-values for diffrent combination of K and x 

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

# plotting lineup (turk1) for testing b2=0
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

# plotting turk3 lineups --------------

generate_turk3_lineup <- function(n,sigma,beta){
	#n <- 100;nc <- 15; beta <- .1; sigma <- 4; 
	alpha <- 0
	lambda <- 10 
	beta <- sign(rnorm(1))*beta
	nc <- n*15/100
	x1 <- rnorm(n,0,1)
	x2 <- rnorm(n=nc,mean=-1.75*sign(beta), sd=1/3)
	x <- c(x1,x2)
	sig_hat <- 0
	while(sig_hat<4.99 || sig_hat>5.01){
		y1 <- alpha + beta*x1 + rnorm(n,0,sigma)
		y2 <- rnorm(nc,lambda,sigma/3)
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
	result = data.frame(n,beta,sigma=5,pval,loc)
	return(list(result=result,p=p, dat_used=data.frame(x,y)))
}

pl <- generate_turk3_lineup(n=100,sigma=3.5,beta=0.4)
pl$result
ggsave(plot=pl$p, file="../images/lineup_contaminated.pdf",height=7,width=7.5)

# == plotting contaminated data example
set.seed(59)
n <- 100 ; nc <- 15; beta <- 3; sigma <- 5; gamma <- 15
x1 <- rnorm(n,0,1)
y1 <- 5 + beta*x1 + rnorm(n,0,sigma)
x2 <- rnorm(n=nc,mean=-1.75, sd=1/3)
y2 <- rnorm(nc,gamma,sigma/3)
X <- c(x2,x1)
Y <- c(y2,y1)
summary(lm(Y~X)) # p-value for slope is large
qplot(X,Y)

ggsave("../images/contaminated_data.pdf", width=5,height=5)


# ==== plotting visual test statistics --------

set.seed(200)

generate_stat_slope <- function(n,beta,sigma){
	#n <- 100; a <- 6; b <- 2; sigma <- 12
	a <- 6
	x <- rnorm(n=n, mean=0, sd=1)
	y <- a + beta*x + rnorm(n=n, mean=0, sd=sigma)
	fit <- lm(y~1)
	p <- qplot(x, resid(fit), xlab=expression(X[k]),geom="point", ylab="Residual", alpha=I(.3))
	p <- p + geom_smooth(method="lm", se=F, size=1)
	return(list(dat=data.frame(x,y),plot=p))
}
rs <- generate_stat_slope(n=100,beta=2.5,sigma=5)
ggsave(plot=rs$plot, file="../images/stat_beta_k.pdf",height=2,width=2.05)

p <- qplot(x,y, geom="point", data=rs$dat, alpha=I(0.3)) + 
     xlab(expression(X[k]))+ ylab("Y") +
     geom_smooth(method="lm", se=F, size=1)
ggsave(plot=p, file="../images/stat_intercept.pdf",height=2,width=2.05)

p <- qplot(y, geom="histogram", binwidth=1, data=rs$dat)
ggsave(plot=p, file="../images/stat_goodness_simple.pdf",height=2,width=2.05)

p <- qplot(0,x, geom="boxplot", data=rs$dat, colour=factor(1),fill=factor(1), alpha=.3) +
     opts(legend.position="none",axis.text.x = theme_blank()) + 
     xlim(-1,1) + ylab("Residual")
ggsave(plot=p, file="../images/stat_sigma_box.pdf",height=2,width=2.05)

generate_stat_interection <- function(n,beta, sigma){  
  x1 <- rpois(n,lambda=20)
  x2 <- factor(sample(c("A","B"), size=n, replace=T))
  cnd <- sample(c("A","B"), size=1)
  y <- round(5+2*x1+3*(x2==cnd)+beta*x1*(x2==cnd)+rnorm(n=n, mean=0, sd=sigma))
  p <- qplot(x1,y/10, colour=factor(x2), alpha=I(0.3)) + xlab("X") +ylab("Y")+
       geom_smooth(method="lm", se=F, size=1)  +
       scale_colour_discrete(name=expression(X[k]))
  return(p)
}
set.seed(340)
p <- generate_stat_interection(n=100,beta=3.5,sigma=10)
ggsave(plot=p, file="../images/stat_interection.pdf",height=2,width=2.75)


X <- rnorm(n=100,mean=0,sd=3)
Y <- .5 - .5 * X + .3 * X^2  + rnorm(n=100,mean=0,sd=2)
fit <- lm(Y~X)
p <- qplot(X,resid(fit), ylab="Residual", alpha=I(0.3)) + 
     stat_smooth(method="loess", se=F)
ggsave(plot=p, file="../images/stat_nonlinear.pdf",height=2,width=2.05)


# ==== summary of the data applying different screening criteria to the raw data 

get_summary <- function(dat){
  g <- ddply(dat,.(gender),summarise, length(unique(id)))
  male <- g[1,2]
  female <- g[2,2]
  total <- length(unique(dat$id))
  feedbacks <- nrow(dat)
  lineups <- length(unique(dat$pic_id))
  return(data.frame(total,male,female, feedbacks,lineups))
}

get_screening_index <- function(dat){
  indx1 <- dat$id==dat$id
  indx2 <- !is.na(dat$gender)
  d34 <- ddply(dat,.(id), summarise, percent_correct=mean(response)*100)
  included_id3 <- d34$id[ d34$percent_correct > 0]
  indx3 <- dat$id %in% included_id3
  included_id4 <- d34$id[ d34$percent_correct >= 20 ]
  indx4 <- dat$id %in% included_id4
  easy_dat <- subset(dat, p_value < 0.0002)
  if (dat$experiment[1]=='turk3') easy_dat <- subset(dat, difficulty==0)
  d56 <- ddply(easy_dat,.(id),summarise,
              easy_cnt = length(response),
              percent_correct = mean(response)*100,
              include_criteria_6 = response[1],
              exclude_lineup_criteria_6 = paste(id[1],"_",pic_id[1],sep=""),
              exclude_lineup_p_value = p_value[1]
              )
  included_id5 <- d56$id[ d56$percent_correct > 49]
  indx5 <- dat$id %in% included_id5
  included_id6 <- d56$id[d56$include_criteria_6]
  indx6 <- dat$id %in% included_id6
  indx6[paste(dat$id,"_",dat$pic_id,sep="") %in% d56$exclude_lineup_criteria_6] <- FALSE
  return(data.frame(indx1,indx2,indx3,indx4,indx5,indx6))
}

get_screened_summary <- function(dat){
  indx <- get_screening_index(dat)
  s <- NULL
  for (i in 1:6) s <-  rbind(s,get_summary(subset(dat,indx[,i])))
  return(s)
}

sm <- cbind(get_screened_summary(raw.dat1),
      get_screened_summary(raw.dat2),
      get_screened_summary(raw.dat3))
#library(xtable)
xtable(sm)

# summary of the cleaned data

sm_clean <- rbind(get_summary(dat1),
                  get_summary(dat2),
                  get_summary(dat3))
xtable(sm_clean)

# --- fit mixed model with cleaned data
#library(lme4)

fit_model <- function(dat){
  dat$alpha <- log(.05/0.95)
  dat$effect <- with(dat,abs(beta)*sqrt(sample_size)/sigma)
  model <- as.formula(response ~ offset(alpha) + effect -1 + (effect -1|id))
  if (dat$experiment[1]=='turk3')model <- as.formula(response ~  effect + (effect |id))
  fit <- lmer(model,family="binomial",data=dat)
  return(summary(fit))
}

fit_model(dat1)
fit_model(dat2)
fit_model(dat3)


pred.mixed <- function(X, subject=0,fit) {
  alpha <- log(.05/0.95)
  if (length(fixef(fit))>1) { # true for experiment 3
    subject <- matrix(unlist(sapply(as.character(subject), strsplit, "_")), ncol=2, byrow=T)
    eta <- fixef(fit)[1] + X * fixef(fit)[2] + as.numeric(subject[,1]) + as.numeric(subject[,2])*X
  } else eta <- alpha + X * fixef(fit) + subject*X
  g.eta <- exp(eta)/(1+exp(eta))
  return(g.eta)
}

get_predict_mixed <- function(dat, newdat, intercept=F){
  fit.mixed <- fit_model(dat)
  X <- newdat$effect
  if(intercept){
    if (dat$experiment[1]=="turk3"){
      subject <- apply(ranef(fit.mixed)[[1]],1,paste,collapse="_")
      } else subject <- ranef(fit.mixed)[[1]][,1]
    d <- data.frame(expand.grid(effect=X, subject=subject))
    pred <- pred.mixed(X=d$effect, subject=d$subject, fit=fit.mixed)
    res <- data.frame(effect=d$effect, subject=d$subject, pred)
  } else res <- data.frame(effect=X,pred=pred.mixed(X, fit=fit.mixed))
  return(res)
}
effect <- seq(0.01,16, by=.2)
get_predict_mixed(dat3, newdat=data.frame(effect))
#get_predict_mixed(dat1, newdat=data.frame(effect), intercept=T)

pi_effect <- rbind(data.frame(experiment="Experiment 1",get_predict_mixed(dat1, newdat=data.frame(effect=seq(0,16, by=.2)))),
            data.frame(experiment="Experiment 2",get_predict_mixed(dat2, newdat=data.frame(effect=seq(0,5.5, by=.2)))),
            data.frame(experiment="Experiment 3",get_predict_mixed(dat3, newdat=data.frame(effect=seq(0,4.5, by=.2)))))

# source("calculate_ump_power.R")
ump <- get_ump_power_by_effect()

ggplot()+
  geom_line(aes(effect,pred, colour="Visual"), data=pi_effect, size=1.2) +
  geom_line(aes(effect,pow,colour="Conventional"), data=ump, size=1.2) +
  facet_grid(.~experiment, scales="free") +
  ylab("Power") + xlab(expression(Effect(E))) +
  scale_colour_discrete(name = "Test") +
  scale_colour_manual( values=c("Black","Blue"))

ggsave( file="../images/power_mixed.pdf",height=4,width=10)

# --- fit mixed model with each screening criteria and plot power

get_screened_predict <- function(dat, newdat,intercept=F){
  indx <- get_screening_index(dat)
  res <- NULL
  for (i in 1:6){
    pred <- get_predict_mixed(subset(dat,indx[,i]), newdat, intercept)
    res <- rbind(res, data.frame(screening=i,pred)) 
  }
  return(res)
}
get_screened_predict(raw.dat1, newdat=data.frame(effect))
pi <- rbind(data.frame(experiment="Experiment 1",get_screened_predict(raw.dat1, newdat=data.frame(effect=seq(0,16, by=.2)))),
            data.frame(experiment="Experiment 2",get_screened_predict(raw.dat2, newdat=data.frame(effect=seq(0,5.5, by=.2)))),
            data.frame(experiment="Experiment 3",get_screened_predict(raw.dat3, newdat=data.frame(effect=seq(0,4.5, by=.2)))))

# source("calculate_ump_power.R")
ump <- get_ump_power_by_effect()

ggplot()+
  geom_line(aes(effect,pred, colour=factor(screening)), data=pi) +
  geom_line(aes(effect,pow), data=ump) +
  facet_grid(.~experiment, scales="free") +
  ylab("Power") + xlab(expression(Effect(E))) +
  scale_colour_discrete(name = "Screening \ncriteria") 

ggsave( file="../images/power_screening.pdf",height=4,width=10)

# subject specific power from mixed model

pi_subject <- rbind(data.frame(experiment="Experiment 1",get_predict_mixed(dat1, newdat=data.frame(effect=seq(0,16, by=.2)), intercept=T)),
                   data.frame(experiment="Experiment 2",get_predict_mixed(dat2, newdat=data.frame(effect=seq(0,5.5, by=.2)), intercept=T)),
                   data.frame(experiment="Experiment 3",get_predict_mixed(dat3, newdat=data.frame(effect=seq(0,4.5, by=.2)), intercept=T)))

pi_effect$col <- 30
pi_effect$col[1] <- NA
ggplot()+
  geom_line(aes(effect,pred, group=subject), data=pi_subject,alpha=.08) +
  geom_line(aes(effect,pred, colour=col), data=pi_effect, size=1) +
  geom_line(aes(effect,pow), data=ump, size=1, linetype=2) +
  facet_grid(.~experiment, scales="free") +
  scale_colour_gradient("Subject",limits=c(10,30), guide="none") +
  ylab("Power") + xlab("Effect")

ggsave( file="../images/power_mixed_subject.pdf",height=4,width=10)

# subjects having more power than ump

subject_ump <- merge(pi_subject,ump, by=c("experiment","effect"))
dd <- ddply(subject_ump, .(experiment, effect), summarize,
      prop = mean(pred>pow))
qplot(effect,prop, data=subset(dd, prop>0), geom="line", colour=experiment) +
  ylab("Proportion of subject having  power \n above conventional test") +
  scale_colour_discrete(name="Data") +xlab("Effect")
ggsave( file="../images/subject_having_more_power.pdf",height=4,width=6)



# ----- subject specific power for each screening criteria from mixed model

pi <- rbind(data.frame(experiment="Experiment 1",get_screened_predict(raw.dat1, newdat=data.frame(effect=seq(0,16, by=.2)),intercept=T)),
            data.frame(experiment="Experiment 2",get_screened_predict(raw.dat2, newdat=data.frame(effect=seq(0,5.5, by=.2)),intercept=T)),
            data.frame(experiment="Experiment 3",get_screened_predict(raw.dat3, newdat=data.frame(effect=seq(0,4.5, by=.2)),intercept=T)))

ggplot()+
  geom_line(aes(effect,pred, group=subject), data=pi, alpha=.1) +
  geom_line(aes(effect,pow), data=ump, colour="hotpink", size=1) +
  facet_grid(screening~experiment, scales="free") +
  ylab("Power") + xlab(expression(Effect(E)))
ggsave( file="../images/power_screening_subject.pdf",height=8,width=10 )  

# ump1 <- calculate_ump_power1(3, 100, 5)
# qplot(effect, pred.mixed(X=effect, fit=fit.mixed)) + ylim(c(0,1))
# 
# newdata <- data.frame(expand.grid(effect=effect, subject=ranef(fit.mixed)[[1]][,1]))
# newdata$pred <- pred.mixed(newdata$effect, intercept=newdata$subject, fit=fit.mixed)
# 
# qplot(effect, pred, group=subject, data=subset(newdata, subject %in% sample(size=2, x=unique(newdata$subject))), geom="line")

# ======= checking minimum p-value assumption for expected power ======

pval1 <- read.csv("../data/pvalue_turk1.csv")
pval2 <- read.csv("../data/pvalue_turk2.csv")

get_response_count <- function(response_no){
  res <- rep(0,20)
  freq <- table(response_no)
  res[as.numeric(dimnames(freq)[[1]])] <- freq
  return(data.frame(counts=res))
}

get_merged_pvalue <- function(dat,pval){
  response_count <- ddply(dat, .(pic_name), summarise,
                          counts = get_response_count(response_no)$counts,
                          variable = paste("X",1:20,sep=""),
                          obs_pvalue = rep(p_value[1],20))
  p_values <- melt(pval, id="pic_name")
  pr <- merge(p_values,response_count, by=c("pic_name","variable"))
  prr <- ddply(pr, .(pic_name),summarise,
               counts=counts[order(value)],
               rfreq=counts[order(value)]/sum(counts[order(value)]),
               pvalue=round(value,4)[order(value)],
               rank_pval = 1:20,
               obs_pvalue = obs_pvalue)  
  return(prr)
}


prr <- get_merged_pvalue(dat1,pval1)
p <- qplot(factor(rank_pval), counts, geom="boxplot", data=prr) +
  xlab("rank of p-value") + ylab("Number of subjects")
ggsave(file="../images/p_val_rank_counts.pdf", height=4, width=7)

prr12 <- rbind(data.frame(Experiment="Experiment1",get_merged_pvalue(dat1,pval1)),
               data.frame(Experiment="Experiment2",get_merged_pvalue(dat2,pval2)))
p <- qplot(factor(rank_pval), counts, geom="boxplot", data=prr12) +
  facet_grid(.~Experiment) +
  xlab("rank of p-value") + ylab("Number of subjects")
ggsave(file="../images/p_val_rank_counts12.pdf", height=5, width=10)


prr$pic_name2 <- str_sub(prr$pic_name, 12, -5L)
x <- str_split(prr$pic_name2, "_")
prr$n <-  unlist(lapply(x, function(x) x[1]))
prr$beta <-  unlist(lapply(x, function(x) x[2]))
prr$beta <-  factor(prr$beta, levels=c("0", "1", "2", "3", "5", "7", "8", "10", "16"))
prr$sd <-  unlist(lapply(x, function(x) x[3]))
prr$rep <-  unlist(lapply(x, function(x) x[4]))
prr$pic_name3 <- factor(paste(prr$beta, prr$n, prr$sd, prr$rep, sep="."))
p <- qplot(pvalue, counts, geom="point", data=prr) +
  xlab("rank of p-value") + ylab("Number of subjects") +
  facet_wrap(~pic_name3, ncol=10, scales="free_y") + scale_x_log10()
prr$lpvalue<-log10(prr$pvalue+0.01)
prr$lobs_pvalue<-log10(prr$obs_pvalue+0.01)
p <- qplot(lpvalue, counts, geom="point", data=subset(prr,lpvalue != lobs_pvalue)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0)) +
  geom_segment(mapping=aes(xend=lobs_pvalue, yend=0, colour="hotpink"), data= subset(prr,lpvalue==lobs_pvalue))+ 
  geom_point(mapping=aes(xend=lobs_pvalue, yend=0, colour="hotpink"), data= subset(prr,lpvalue==lobs_pvalue))+ 
  xlab(expression(paste(log[10], " p-value"))) + ylab("Number of subjects") +
  facet_grid(beta~n+sd+rep, scales="free_y", labeller="label_both") +
  opts(legend.position="none")
p

prr.min<-ddply(prr, "pic_name", summarise,
                   lpvalue=min(lpvalue),
                   rfreq=rfreq[order(lpvalue)[1]],
                   beta=beta[1],
                   n=n[1],
                   sd=sd[1],
                   rep=rep[1])
prr.actual<-ddply(prr, "pic_name", summarise,
                   lpvalue=lpvalue[obs_pvalue==pvalue],
                   rfreq=rfreq[obs_pvalue==pvalue],
                   beta=beta[1],
                   n=n[1],
                   sd=sd[1],
                   rep=rep[1])
prr.actual <- subset(prr.actual, !(prr.actual$pic_name == prr.min$pic_name & prr.actual$lpvalue == prr.min$lpvalue))

p <- qplot(lpvalue, rfreq, geom="point", data=prr) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0, colour="hotpink"), data=prr.min) + 
  geom_point(mapping=aes(xend=lpvalue, yend=0, colour="hotpink"), data=prr.min) + 
  xlab(expression(paste("P-value on ",log[10]," scale"))) + ylab("Relative frequency of picks") +
  geom_segment(mapping=aes(xend=lpvalue, yend=0, colour="turquoise"), data=prr.actual) + 
  geom_point(mapping=aes(xend=lpvalue, yend=0, colour="turquoise"), data=prr.actual) + 
  facet_grid(beta~n+sd+rep, labeller="label_both") + scale_x_continuous(limits=c(-2.1,0.1), breaks=c(-2,-1,0), labels=c("0.01","0.1","1")) + 
  opts(legend.position="none")
p

ggsave(file="../images/p_val_log_counts.pdf", height=10, width=12)

# alternative ordering for the previous plot
prr$facets <- with(prr, paste("beta:", beta,"\n n:", n,"\n sd:",sd, paste=""))
prr$effect <- with(prr, sqrt(as.numeric(as.character(n)))*as.numeric(as.character(beta))/as.numeric(as.character(sd)))
prr$facets <- reorder(prr$facets, prr$effect, median)
prr$rep <- with(prr, paste("rep:", rep))


prr.actual <- subset(prr, pvalue==obs_pvalue)
prr.min<-ddply(prr, "pic_name", summarise,
               lpvalue=min(lpvalue),
               rfreq=rfreq[order(lpvalue)[1]],
               facets=facets[1],
               rep=rep[1])

p1 <- qplot(lpvalue, rfreq, geom="point", data=subset(prr, as.numeric(facets) <= 10)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0), colour="turquoise", data=subset(prr.actual, as.numeric(facets) <= 10)) +
  geom_point(mapping=aes(xend=lpvalue, yend=0), colour="turquoise", data=subset(prr.actual, as.numeric(facets) <= 10)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data=subset(prr.min, as.numeric(facets) <= 10)) + 
  geom_point(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data=subset(prr.min, as.numeric(facets) <= 10)) +
  xlab(" ") + ylab("Relative frequency of picks") +
  facet_grid(rep~facets) + scale_x_continuous(limits=c(-2.1,0.1), breaks=c(-2,-1,0), labels=c("0.01","0.1","1")) + 
  opts(legend.position="none") + scale_y_continuous(limits=c(0,1))
p1
ggsave(file="../images/p_val_log_counts-a.pdf", height=4, width=12)

p2 <- qplot(lpvalue, rfreq, geom="point", data=subset(prr, as.numeric(facets) > 10)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0), colour="turquoise", data=subset(prr.actual, as.numeric(facets) > 10)) +
  geom_point(mapping=aes(xend=lpvalue, yend=0), colour="turquoise", data=subset(prr.actual, as.numeric(facets) > 10)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data=subset(prr.min, as.numeric(facets) > 10)) + 
  geom_point(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data=subset(prr.min, as.numeric(facets) > 10)) +
  xlab(expression(paste("P-value on ",log[10]," scale"))) + ylab("Relative frequency of picks") +
  facet_grid(rep~facets) + scale_x_continuous(limits=c(-2.1,0.1), breaks=c(-2,-1,0), labels=c("0.01","0.1","1")) + 
  opts(legend.position="none") + scale_y_continuous(limits=c(0,1))
p2
ggsave(file="../images/p_val_log_counts-b.pdf", height=4, width=12)

 

# now same plot for turk2 data
prr2 <- get_merged_pvalue(dat2,pval2)
prr2$pic_name2 <- str_sub(prr2$pic_name, 12, -5L)
x <- str_split(prr2$pic_name2, "_")
prr2$n <-  unlist(lapply(x, function(x) x[1]))
prr2$b <-  unlist(lapply(x, function(x) as.numeric(x[2])/100))
#prr$b <-  factor(prr$b, levels=c("0", "1", "2", "3", "5", "7", "8", "10", "16"))
prr2$sd <-  unlist(lapply(x, function(x) x[3]))
prr2$rep <-  unlist(lapply(x, function(x) x[4]))
prr2$pic_name3 <- factor(paste(prr2$b, prr2$n, prr2$sd, prr2$rep, sep="."))
p <- qplot(pvalue, counts, geom="point", data=prr2) +
  xlab("rank of p-value") + ylab("Number of subjects") +
  facet_wrap(~pic_name3, ncol=10, scales="free_y") + scale_x_log10()
prr2$lpvalue<-log10(prr2$pvalue+0.01)
prr2$lobs_pvalue<-log10(prr2$obs_pvalue+0.01)
p <- qplot(lpvalue, counts, geom="point", data=subset(prr2,lpvalue != lobs_pvalue)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0)) +
  geom_segment(mapping=aes(xend=lobs_pvalue, yend=0, colour="hotpink"), data= subset(prr2,lpvalue==lobs_pvalue))+ 
  geom_point(mapping=aes(xend=lobs_pvalue, yend=0, colour="hotpink"), data= subset(prr2,lpvalue==lobs_pvalue))+ 
  xlab(expression(paste(log[10], " p-value"))) + ylab("Number of subjects") +
  facet_grid(b~n+sd+rep, scales="free_y", labeller="label_both") +
  opts(legend.position="none")
p


prr2.min<-ddply(prr2, "pic_name", summarise,
                   lpvalue=min(lpvalue),
                   rfreq=rfreq[order(lpvalue)[1]],
                   b=b[1],
                   n=n[1],
                   sd=sd[1],
                   rep=rep[1])
prr2.actual<-ddply(prr2, "pic_name", summarise,
                   lpvalue=lpvalue[obs_pvalue==pvalue],
                   rfreq=rfreq[obs_pvalue==pvalue],
                   b=b[1],
                   n=n[1],
                   sd=sd[1],
                   rep=rep[1])
prr2.actual <- subset(prr2.actual, !(prr2.actual$pic_name == prr2.min$pic_name & prr2.actual$lpvalue == prr2.min$lpvalue))
p <- qplot(lpvalue, rfreq, geom="point", data=prr2) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0), colour="turquoise", data=prr2.actual) + 
  geom_point(mapping=aes(xend=lpvalue, yend=0), colour="turquoise", data=prr2.actual) + 
  geom_segment(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data=prr2.min) + 
  geom_point(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data=prr2.min) + 
  xlab(expression(paste("p-value on ",log[10]," scale"))) + ylab("Relative frequency of picks") +
  facet_grid(b~n+sd+rep, labeller="label_both") + scale_x_continuous(limits=c(-2.1,0.1), breaks=c(-2,-1,0), labels=c("0.01","0.1","1")) + scale_y_continuous(limits=c(0,1.1), breaks=c(0,0.5,1), labels=c("","0.5","1")) +
  opts(legend.position="none")
p
ggsave(file="../images/p_val_log_counts2.pdf", height=10, width=14)



# alternative to previous plot with different ordering
prr2$facets <- with(prr2, paste("beta: ",b, "\nn:",n,"\nsd: ",sd))
prr2$effect <- with(prr2, sqrt(as.numeric(as.character(n)))*as.numeric(as.character(b))/as.numeric(as.character(sd)))
prr2$facets <- reorder(prr2$facets, prr2$effect, median)
prr2$rep <- with(prr2, paste("rep:", rep))


prr2$pic_name2 <- str_sub(prr2$pic_name, 12, -5L)
x <- str_split(prr2$pic_name2, "_")
#prr2$rep <-  unlist(lapply(x, function(x) x[4]))
prr2$pic_name3 <- factor(paste(prr2$b, prr2$n, prr2$sd, prr2$rep, sep="."))

prr2$lpvalue<-log10(prr2$pvalue+0.01)
prr2$lobs_pvalue<-log10(prr2$obs_pvalue+0.01)


prr2.actual <- subset(prr2,as.numeric(facets) <= 10 & (lpvalue == lobs_pvalue))
prr2.min <- ddply(prr2, "pic_name", summarise,
                            lpvalue=min(lpvalue),
                            rfreq=rfreq[order(lpvalue)[1]],
                            facets=facets[1],
                            rep=rep[1],
                            counts=counts[1])

prr2.actual <- subset(prr2,as.numeric(facets) <= 10 & (lpvalue == lobs_pvalue))
prr2.mina <- subset(prr2.min, as.numeric(facets) <= 10)
p1 <- qplot(lpvalue, counts, geom="point", data=subset(prr2,as.numeric(facets) <= 10)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0)) +
  geom_segment(mapping=aes(xend=lobs_pvalue, yend=0), colour="turquoise", data= prr2.actual)+ 
  geom_point(mapping=aes(xend=lobs_pvalue, yend=0), colour="turquoise", data= prr2.actual)+ 
  geom_segment(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data= prr2.mina)+ 
  geom_point(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data= prr2.mina)+ 
  xlab(" ") + ylab("Number of subjects") +
  facet_grid(rep~facets) +
  opts(legend.position="none") + 
  scale_x_continuous(limits=c(-2.1,0.1), breaks=c(-2,-1,0), labels=c("0.01","0.1","1")) 
p1

prr2.actual <- subset(prr2,as.numeric(facets) > 10 & (lpvalue == lobs_pvalue))
prr2.minb <- subset(prr2.min, as.numeric(facets) > 10)
p2 <- qplot(lpvalue, counts, geom="point", data=subset(prr2,as.numeric(facets) > 10)) +
  geom_segment(mapping=aes(xend=lpvalue, yend=0)) +
  geom_segment(mapping=aes(xend=lobs_pvalue, yend=0), colour="turquoise", data= prr2.actual)+ 
  geom_point(mapping=aes(xend=lobs_pvalue, yend=0), colour="turquoise", data= prr2.actual)+ 
  geom_segment(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data= prr2.minb)+ 
  geom_point(mapping=aes(xend=lpvalue, yend=0), colour="hotpink", data= prr2.minb)+ 
  xlab(expression(paste(log[10], " p-value"))) + ylab("Number of subjects") +
  facet_grid(rep~facets) +
  opts(legend.position="none") + 
  scale_x_continuous(limits=c(-2.1,0.1), breaks=c(-2,-1,0), labels=c("0.01","0.1","1")) 

p2

ggsave(p1, file="../images/p_val_log_counts2-a.pdf", height=6, width=12)
ggsave(p2, file="../images/p_val_log_counts2-b.pdf", height=6, width=12)


# --- summary of minimum p-value data -----

prr1 <- get_merged_pvalue(dat1,pval1)
min_pval1 <- ddply(prr1,.(pic_name), summarize,
                   most_pick_minimum_pval = max(counts)==counts[rank_pval==1])
prr2 <- get_merged_pvalue(dat2,pval2)
min_pval2 <- ddply(prr2,.(pic_name), summarize,
      most_pick_minimum_pval = max(counts)==counts[rank_pval==1])

pval_sm <- rbind(data.frame(Experiment=1,Total = nrow(min_pval1), Most_pick_minimum = sum(min_pval1$most_pick_minimum_pval)),
                 data.frame(Experiment=2,Total = nrow(min_pval2), Most_pick_minimum = sum(min_pval2$most_pick_minimum_pval))
                 )

# ---- getting minimum p-value summary for all screening criteria

get_all_pval_sam <- function(dat1,dat2,pval1,pval2){
  prr1 <- get_merged_pvalue(dat1,pval1)
  min_pval1 <- ddply(prr1,.(pic_name), summarize,
                     most_pick_minimum_pval = max(counts)==counts[rank_pval==1])
  prr2 <- get_merged_pvalue(dat2,pval2)
  min_pval2 <- ddply(prr2,.(pic_name), summarize,
                     most_pick_minimum_pval = max(counts)==counts[rank_pval==1])
  
  pval_sm <- cbind(data.frame(Experiment1=sum(min_pval1$most_pick_minimum_pval)),
                   data.frame(Experiment2=sum(min_pval2$most_pick_minimum_pval))
                   )
  return(pval_sm)
}


indx1 <- get_screening_index(raw.dat1)
indx2 <- get_screening_index(raw.dat2)
pval_sm <- NULL
for (i in 1:6) {
  d1 <- subset(raw.dat1,indx1[,i])
  d2 <- subset(raw.dat2,indx2[,i])
  pval_sm <- rbind(pval_sm,cbind(screening=i,get_all_pval_sam(d1,d2,pval1,pval2)))
}
pval_sm <- rbind(pval_sm,c("Total lineups",60,70))
print(xtable(pval_sm), include.rownames=FALSE)


# Difference between data plot p-value and minimum null plot p-value 

get_pval_success <- function (dat){
  dat <- subset(dat, abs(beta)>0)
  res <- ddply(dat, .(pic_name), summarize,      
        prop_correct=mean(response),
        p_value=p_value[1],
        actual_plot = plot_location[1])
  res$experiment=paste("Experiment", substr(dat$experiment,5,5)[1])
  return(res)
}
pval.success <- rbind(get_pval_success(dat1),get_pval_success(dat2))
pval.dat <- merge(pval.success,rbind(pval1,pval2), by="pic_name")
pval.dat.m <- melt(pval.dat, id=c("pic_name","prop_correct",
                                  "p_value","actual_plot","experiment"))
pval.diff <- ddply(pval.dat.m, .(pic_name), summarise,
                   experiment=experiment[1],
                   prop_correct=prop_correct[1],
                   pval_diff = p_value[1]- min(value[-actual_plot[1]]))

ggplot(pval.diff) +
  geom_point(aes(pval_diff, prop_correct)) +
  facet_grid(.~experiment) +
  xlab("Difference between p-value of actual data and minimum p-value of null data") +
  ylab("Proportion correct") +xlim(-.15,.15)
ggsave(file="../images/pval_difference.pdf", height=4, width=8)

# investigating large differences between p-values and minimum p-value of null data

dd <- subset(pval.dat, pic_name %in% subset(pval.diff,pval_diff >.8)[,1])



# Following code will produce figure power_expected.pdf by simulation
source("generate_expected_visual_power_simulation-hh.R")

# ====================== Turk2 conventional power calculations  

# source("calculate_ump_power.R")
calculate_ump_power2(beta=.1,n=100,sigma=5)

# ====================== turk3 conventional power calculations

# source("calculate_ump_power.R")
calculate_ump_power3(beta=.1,n=100,sigma=5,x=getx(100))

# experiment 3 reasoning (reasons for choice)

# 1. Most different plot
# 2. Visible trend
# 3. Clustering visible
# 4. Other

reason.exp3 <- c("Most different plot", "Visible trend","Clustering visible","Other")
reason.dat3 <- ddply(dat3,.(choice_reason), summarise,
                     prop_correct = mean(response),
                     counts = length(response))
reason.dat3$choice_reason <- with(reason.dat3, factor(choice_reason,
                                  levels=choice_reason[order(prop_correct,decreasing=T)]))
reason.dat3$n <- cut(reason.dat3$counts, breaks=c(0,50, 100, 150, 300, 500))
reason.dat3$n <- reorder(reason.dat3$n, reason.dat3$count, function(x) -max(x))
reason.dat3$choice_reason <- factor(reason.dat3$choice_reason, levels=rev(levels(reason.dat3$choice_reason)))
levels(reason.dat3$choice_reason)[c(7,4,5,1)] <- paste("(",1:4,") ",reason.exp3, sep="")
#levels(reason.dat3$choice_reason)[3] <- "Visible Gap"

qplot(prop_correct,choice_reason, geom="point", data=reason.dat3, size=I(3)) +
  ylab("Reasons for choice") + xlab("Proportion correct") + xlim(c(0,1)) +
  facet_grid(n~., scales="free", space="free_y") + opts(strip.text.y = theme_text())

ggsave("../images/choice_reason.pdf", height = 4, width = 6)

# Di's changes
reason.dat3.sub<-subset(reason.dat3, counts>10)
reason.dat3$choice_reason <- factor(reason.dat3$choice_reason)
ord<-order(reason.dat3.sub$prop_correct)
reason.dat3$choice_reason <- factor(reason.dat3$choice_reason, levels=levels(reason.dat3$choice_reason)[ord])
qplot(prop_correct, choice_reason, geom="point", data=reason.dat3.sub, size=counts) +
  ylab("Reasons for choice") + xlab("Proportion correct") + xlim(c(0,1)) + scale_size("Number", limits=c(0, 500))
ggsave("../images/choice_reason2.pdf", height = 4, width = 6)

# Now focus on "marginals"
dat3$choice_reason1 <- grepl("1", as.character(dat3$choice_reason))
dat3$choice_reason2 <- grepl("2", as.character(dat3$choice_reason))
dat3$choice_reason3 <- grepl("3", as.character(dat3$choice_reason))
dat3$choice_reason4 <- grepl("4", as.character(dat3$choice_reason))
reason.any1.dat3 <- ddply(dat3,.(choice_reason1), summarise,
                     prop_correct = mean(response),
                     counts = length(response))
reason.any2.dat3 <- ddply(dat3,.(choice_reason2), summarise,
                     prop_correct = mean(response),
                     counts = length(response))
reason.any3.dat3 <- ddply(dat3,.(choice_reason3), summarise,
                     prop_correct = mean(response),
                     counts = length(response))
reason.any4.dat3 <- ddply(dat3,.(choice_reason4), summarise,
                     prop_correct = mean(response),
                     counts = length(response))
reason.any.dat3 <- data.frame(choice_reason=c("Most different plot", "Visible trend","Clustering visible","Other"), prop_correct=c(reason.any1.dat3$prop_correct[2], reason.any2.dat3$prop_correct[2], reason.any3.dat3$prop_correct[2], reason.any4.dat3$prop_correct[2]), counts=c(reason.any1.dat3$counts[2], reason.any2.dat3$counts[2], reason.any3.dat3$counts[2], reason.any4.dat3$counts[2]))
ord<-order(reason.any.dat3$prop_correct)
reason.any.dat3$choice_reason <- factor(reason.any.dat3$choice_reason, levels=reason.any.dat3$choice_reason[ord])
qplot(prop_correct, choice_reason, geom="point", data=reason.any.dat3, size=counts) +
  ylab("Reasons for choice") + xlab("Proportion correct") + xlim(c(0,1)) + scale_size("Number", limits=c(0, 700))
ggsave("../images/choice_reason3.pdf", height = 4, width = 6)

reason.clustring <- ddply(dat3,.(response), summarize,
                          cnt = sum(choice_reason==3))
reason.clustring$response <- factor(as.character(reason.clustring$response), labels=c("Data plot not picked", "Data plot picked"))
qplot(response,cnt, data=reason.clustring, geom="bar", alpha=I(.7)) +
  xlab("Responses of those choosing Clustering visible") +
  ylab("Number of subjects") + opts(legend.position="none")
ggsave("../images/choice_reason_count.pdf", height = 4, width = 4.5)


# ======================= p_value vs proportion correct =====================

pdat <- NULL
for (i in 1:3){
   dati <- eval(parse(text=paste("dat",i,sep="")))
   pdati <- ddply(dati, .(pic_name), summarize,
	  attempted = length(response),
	  corrected = sum(response),
	  prop_correct = mean(response),
    strength = (1 - mean(response))/19,              
    p_value = p_value[1]
	)
   pdati$experiment=paste("Experiment",i)
   pdat <- rbind(pdat,pdati)
}

# uncontaminated p-value for experiment 3
pdati <- ddply(dat3, .(pic_name), summarize,
               attempted = length(response),
               corrected = sum(response),
               prop_correct = mean(response),
               strength = (1 - mean(response))/19,              
               p_value = uncontaminated_pvalue[1]
               )
pdati$experiment= "Experiment 3 uncontaminated"
pdat <- rbind(pdat,pdati)


p <- ggplot(subset(pdat, substr(experiment,1,12) !="Experiment 3")) +
     geom_point(aes(p_value,prop_correct),size=2) + 
     facet_grid(.~experiment) +
     xlab(expression(paste("p-value(",p[D],") on square root scale"))) +
     ylab("Proportion correct on square root scale") +
     scale_x_sqrt()+ scale_y_sqrt()
p 
ggsave(p,file="../images/p_val_prop_correct.pdf", height = 4, width = 7.5)


qplot(uncontaminated_pvalue,p_value, data=dat3) +
  scale_x_sqrt()+scale_y_sqrt()+
  ylab("Contaminated p-value") +
  xlab("Uncontaminated p-value")

ggplot(subset(pdat, substr(experiment,1,12) =="Experiment 3")) + 
  geom_point(aes(p_value,strength),size=2) +
  facet_grid(.~experiment) + xlab("p-value") +ylab("Plot signal strength")+
  scale_x_sqrt()+ scale_y_sqrt()

ggsave(file="../images/p_val_plot_signal_exp3.pdf", height = 4, width = 8)

# ----- p-value vs plot signal strength (visual p-value) 


p <- ggplot(pdat) +
     geom_point(aes(p_value,strength),size=2) + 
     facet_grid(.~experiment) +
     xlab(expression(paste("Conventional test p-value (",p[D],") on ",log[10]," scale"))) +
     ylab(expression(paste("Estimate of visual p-value (", hat(p)[D],") on ",log[10]," scale"))) + 
     geom_abline(aes(intercept=0,slope=1)) +
     scale_x_log10() + scale_y_log10()
p 


p <- ggplot(pdat) +
  geom_abline(aes(intercept=0,slope=1), color="grey") +
  geom_point(aes(p_value,strength),size=2) + 
  stat_smooth(aes(p_value,strength),method="loess", se=F, span=1, degree=1)+
  facet_grid(.~experiment) +
  xlab(expression(paste("Conventional test p-value (",p[D],") on square root scale"))) +
  ylab(expression(paste("Plot signal strength (", hat(p)[D],") on square root scale"))) + 
  scale_x_sqrt() +scale_y_sqrt()
p

ggsave(p,file="../images/p_val_plot_signal.pdf", height = 4, width = 13)


# estimate visual p-value from definition

pdat$visual_pval <- apply(pdat[,2:3],1,function(x)return(get_pval(m=20,K=x[1],x=x[2])))
qplot(p_value,visual_pval, data=subset(pdat,nchar(experiment) < 13)) +
  geom_abline(colour="grey") +
  stat_smooth(method="loess", se=F, span=.45, degree=1)+
  xlab(expression(paste("Conventional test p-value (",p[D],") on square root scale"))) +
  ylab(expression(paste("Visual p-value on square root scale"))) +
  facet_grid(.~experiment)+
  scale_x_sqrt() +scale_y_sqrt()

ggsave(file="../images/p_val_definition.pdf", height = 4, width = 10)

qplot(p_value,visual_pval, data=pdat) +
  geom_abline(colour="grey") +
  stat_smooth(method="loess", se=F, span=.45, degree=1)+
  xlab(expression(paste("Conventional test p-value (",p[D],") on square root scale"))) +
  ylab(expression(paste("Visual p-value on square root scale"))) +
  facet_grid(.~experiment)+
  scale_x_sqrt() +scale_y_sqrt()




qplot(strength,visual_pval, data=pdat) +
  xlab(expression(paste("Plot signal strength on square root scale"))) +
  ylab(expression(paste("Visual p-value on square root scale"))) +
  facet_grid(.~experiment)+
  scale_x_sqrt() +scale_y_sqrt()


# correlation between visual and conventional p-value

get_corr <- function(dat){
  d <- subset(dat,p_value < .2)
  dd <- ddply(d, .(pic_id), summarize,
              conventional_p = p_value[1],
              visual_p = (1 - mean(response))/19
              )
  dd <- subset(dd,visual_p < 0.05)
  n <- nrow(dd)
  corr_p <- cor(dd$conventional_p,dd$visual_p)
  return(data.frame(n,correlation=corr_p))
}

get_corr(dat1)
get_corr(dat2)
get_corr(dat3)


# -------- power vs m and p-value------------------

powerdf <- data.frame(expand.grid(pd = seq(0.0001, 0.20, by=0.001), m = seq(10, 30, by=5)))
powerdf$power <- with(powerdf, pbeta(pd, 1, m-1, lower.tail=FALSE))
powerdf$size <- with(powerdf, I(.75+(m==20)))
powerdf$labels <- paste("m =",powerdf$m)
powerdf$hjust <- c(-0.15,1.15)[(powerdf$m > 20) + 1]
powerdf$vjust <- c(.5,.5,.8,.5,1.5)[(powerdf$m/5)- 1]
powerdf$power <- with(powerdf, pmax(power, 1/m))

qplot(pd, power, data=powerdf, geom="line", group=m, size=size) + 
  ylab("Probability that data plot has lowest p-value") + 
  xlab(expression(p[D])) + scale_size_identity() +
  geom_text(aes(x=pd, y=power, label=labels, hjust=hjust, vjust=vjust), size=3, 
            data=subset(powerdf, abs(4*pd - power) < 0.005 )) + 
  geom_hline(y=1/seq(15,30, by=5), linetype=2, alpha=0.3) 

ggsave(file="../images/powerplot.pdf", height = 5, width = 5)


# empirical power by effect with bootstrap confidence band

get_response_by_effect <- function(dat){
  dat$effect <- with(dat,sqrt(sample_size)*abs(beta)/sigma)
  effet.pow <- ddply(dat,.(effect,response), summarize,
                     responses = length(response))
  return(data.frame(effet.pow))
}

get_power_loess <- function(dat) {
  if (is.null(dat$effect)) dat$effect <- with(dat,sqrt(sample_size)*abs(beta)/sigma)
  effects <- seq(0.01, max(dat$effect), by = 0.2)
  dat$y <- as.numeric(dat$response)
  experiment <- as.numeric(substr(dat$experiment,5,5)[1])
  spn <- c(1,.9,1.4)[experiment]  #different span for 3 experiments
  fit <- loess(y ~ effect, data = dat, span = spn)
  pow_smooth <- data.frame(effect=effects, pow=predict(fit, effects))
  return(pow_smooth)
}

get_bootstrap_limit_loess <- function(dat){
  set.seed(56)
  dat$effect <- with(dat,sqrt(sample_size)*abs(beta)/sigma)
  dat_boot_pow <- NULL
  for (i in 1:1000){
    dat.b <- ddply(dat,.(effect), summarize,
                   response = sample(response,replace=T)
                   )
    dat.b$experiment <- dat$experiment
    dat_boot_pow <- rbind(dat_boot_pow,get_power_loess(dat.b))
  }
  limits <- ddply(dat_boot_pow, .(effect), summarize, 
                  limit1 = quantile(pow, 0.025, na.rm = T), 
                  limit2 = quantile(pow, 0.975, na.rm = T))
  return(limits)
}

effect.dat <- rbind(data.frame(experiment="Experiment 1", get_response_by_effect(dat1)),
                    data.frame(experiment="Experiment 2", get_response_by_effect(dat2)),
                    data.frame(experiment="Experiment 3", get_response_by_effect(dat3)))
dat3$effect_hat <- with(dat3,sqrt(sample_size)*abs(beta_hat)/sigma)
dat3$effect <- with(dat3,sqrt(sample_size)*abs(beta)/sigma)
effect_hat.dat <- ddply(dat3,.(effect_hat), summarize,
                        experiment = "Experiment 3",
                        effect = effect[1],
                        pow = calculate_ump_power3(beta=effect_hat[1]/sqrt(115),n=115,sigma=1,x=getx(100))  
                        )

loess.power <- rbind(data.frame(experiment="Experiment 1", get_power_loess(dat1)),
                    data.frame(experiment="Experiment 2", get_power_loess(dat2)),
                    data.frame(experiment="Experiment 3", get_power_loess(dat3)))

#the following codes generates bootstrap limits and takes 10 minutes to run
#The limits are saved so without running this we can get the saved limit data
# loess.limts <- rbind(data.frame(experiment="Experiment 1", get_bootstrap_limit_loess(dat1)),
#                      data.frame(experiment="Experiment 2", get_bootstrap_limit_loess(dat2)),
#                      data.frame(experiment="Experiment 3", get_bootstrap_limit_loess(dat3)))
# write.csv(loess.limts,file='../data/loess_bootstrap_limits.txt',row.names=F)

loess.limts <- read.csv('../data/loess_bootstrap_limits.txt')

source("calculate_ump_power.R")
ump.power <- get_ump_power_by_effect()
power.dat <- rbind(data.frame(Test="Visual",loess.power),
                   data.frame(Test="Conventional", ump.power)) 

power.dat$m <- NA
power.dat$m[power.dat$Test=="Visual"] <- 20
ggplot()+
  geom_ribbon(aes(x = effect, ymin = limit1, ymax = limit2), 
              data = loess.limts, alpha=.3) +
  geom_point(aes(effect,as.numeric(response), size=responses), data=effect.dat, alpha=.3) +
  geom_line(aes(effect,pow,linetype=Test, colour=m), data=power.dat, size=1.2) +
  geom_point(aes(effect,pow), data=effect_hat.dat, shape=4) +
  facet_grid(.~experiment, scales="free") + 
  scale_colour_gradient("Test",limits=c(10,30), guide="none") + 
  ylab("Power") + xlab("Effect") + scale_size_continuous("# Responses") + 
  opts(asp.ratio=1)
ggsave(filename = "../images/power_loess_effect.pdf", height = 4.5,width = 12)

# examining dat3
dat3$effect= with(dat3, sqrt(sample_size)*abs(beta)/sigma)
d3 <- ddply(dat3, .(pic_name), summarize,
            prop_correct=mean(response),
            effect=effect[1],
            num_responses = length(effect))
qplot(effect, prop_correct, data=d3, size=num_responses) + geom_smooth(method="loess")

qplot(effect,as.numeric(response), data=dat3)+ geom_smooth(method="loess")



# checking duplicated data; (id,pic_id) should be unique.

dd <- ddply(dat3,.(id), summarise,
            total_reponses = length(id),
            cnt_duplication = sum(duplicated(data.frame(id,pic_id))),
            correct_duplication = sum(response[duplicated(data.frame(id,pic_id))])
            )
dd.m <- melt(dd[,c(1,3,4)], id="id")
qplot(id, value, geom="line", data=dd.m, colour=variable)

tt =data.frame(subset(dat3, id==119)[,1:8],duplicated=duplicated(subset(dat3, id==119)$pic_id))




