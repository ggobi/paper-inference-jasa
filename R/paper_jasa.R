# The complete R codes for JASA Paper
# Last modified by Di on Jun 7, 2012.

library(ggplot2)
library(plyr)
library(reshape)
library(xtable)
library(lme4)
library(stringr)


dat1 <- read.csv("../data/raw_data_turk1.csv")
dat2 <- read.csv("../data/raw_data_turk2.csv")
dat3 <- read.csv("../data/raw_data_turk3.csv")



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

# ==== plotting visual test statistics --------

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


# ==== summary of the data applying different screening criteria =====

get_summary <- function(dat){
  g <- ddply(dat,.(gender),summarise, length(unique(id)))
  male <- g[1,2]
  female <- g[2,2]
  total <- length(unique(dat$id))
  feedbacks <- nrow(dat)
  return(data.frame(total,male,female, feedbacks))
}

get_sceering_index <- function(dat){
  indx1 <- dat$id==dat$id
  indx2 <- !is.na(dat$gender)
  d34 <- ddply(dat,.(id), summarise, percent_correct=mean(response)*100)
  included_id3 <- d34$id[ d34$percent_correct > 0]
  indx3 <- dat$id %in% included_id3
  included_id4 <- d34$id[ d34$percent_correct >= 20 ]
  indx4 <- dat$id %in% included_id4
  d56 <- ddply(subset(dat, p_value < 0.0002),.(id),summarise,
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
  indx <- get_sceering_index(dat)
  s <- NULL
  for (i in 1:6) s <-  rbind(s,get_summary(subset(dat,indx[,i])))
  return(s)
}

sm <- cbind(get_screened_summary(dat1),
      get_screened_summary(dat2),
      get_screened_summary(dat3))
#library(xtable)
xtable(sm)

# --- fit mixed model with each screening criteria and plot power
#library(lme4)

pred.mixed <- function(X, subject=0,fit) {
  alpha <- log(.05/0.95)
  eta <- alpha + X * fixef(fit) + subject
  g.eta <- exp(eta)/(1+exp(eta))
  return(g.eta)
}

get_predict_mixed <- function(dat, newdat, intercept=F){
  dat$effect <-  with(dat,abs(beta)/sigma*sqrt(sample_size))
  dat$alpha <- log(.05/0.95)
  fit.mixed <- lmer(response ~ offset(alpha) + effect -1 + (effect-1|id),
                    family="binomial",
                    data=dat) 
  X <- newdat$effect
  if(intercept){
    subject <- ranef(fit.mixed)[[1]][,1]
    d <- data.frame(expand.grid(effect=X, subject=subject))
    pred <- pred.mixed(d$effect, subject=d$subject, fit=fit.mixed)
    res <- data.frame(effect=d$effect, subject=d$subject, pred)
  } else res <- data.frame(effect=X,pred=pred.mixed(X, fit=fit.mixed))
  return(res)
}
effect <- seq(0.01,16, by=.2)
get_predict_mixed(dat1, newdat=data.frame(effect))
#get_predict_mixed(dat1, newdat=data.frame(effect), intercept=T)

get_screened_predict <- function(dat, newdat,intercept=F){
  indx <- get_sceering_index(dat)
  res <- NULL
  for (i in 1:6){
    pred <- get_predict_mixed(subset(dat,indx[,i]), newdat, intercept)
    res <- rbind(res, data.frame(screening=i,pred)) 
  }
  return(res)
}
get_screened_predict(dat1, newdat=data.frame(effect))
pi <- rbind(data.frame(experiment="Experiment 1",get_screened_predict(dat1, newdat=data.frame(effect=seq(0,18, by=.2)))),
            data.frame(experiment="Experiment 2",get_screened_predict(dat2, newdat=data.frame(effect=seq(0,7, by=.2)))),
            data.frame(experiment="Experiment 3",get_screened_predict(dat3, newdat=data.frame(effect=seq(0,6, by=.2)))))
set.seed(2035)
ump <- rbind(data.frame(experiment="Experiment 1",effect=seq(0,18, by=.2),
                        ump= calculate_ump_power1(beta=seq(0,18, by=.2)/10, n=100, sigma=1)),
             data.frame(experiment="Experiment 2",effect=seq(0,7, by=.2),
                        ump= calculate_ump_power2(beta=seq(0,7, by=.2)/10, n=100, sigma=1)),
             data.frame(experiment="Experiment 3",effect=seq(0,6, by=.2),
                        ump= calculate_ump_power3(beta=seq(0,6, by=.2)/sqrt(115), n=115, sigma=1, x=getx(100))))

ggplot()+
  geom_line(aes(effect,pred, colour=factor(screening)), data=pi) +
  geom_line(aes(effect,ump), data=ump) +
  facet_grid(.~experiment, scales="free") +
  ylab("power") + xlab(expression(Effect(E))) +
  scale_colour_discrete(name = "Screening \ncriteria") 

ggsave( file="../images/power_screening.pdf",height=4.25,width=10)


# ----- subject specific power for each screening criteria from mixed model

pi <- rbind(data.frame(experiment="Experiment 1",get_screened_predict(dat1, newdat=data.frame(effect=seq(0,18, by=.2)),intercept=T)),
            data.frame(experiment="Experiment 2",get_screened_predict(dat2, newdat=data.frame(effect=seq(0,7, by=.2)),intercept=T)),
            data.frame(experiment="Experiment 3",get_screened_predict(dat3, newdat=data.frame(effect=seq(0,6, by=.2)),intercept=T)))

ggplot()+
  geom_line(aes(effect,pred, group=subject), data=pi, alpha=.1) +
  geom_line(aes(effect,ump), data=ump, colour="hotpink", size=1) +
  facet_grid(screening~experiment, scales="free") +
  ylab("power") + xlab(expression(Effect(E)))
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
ggsave(file="../images/p_val_log_counts.pdf", height=10, width=12)

# now same plot for turk2 data
prr2 <- get_merged_pvalue(dat2,pval2)
prr2$pic_name2 <- str_sub(prr2$pic_name, 12, -5L)
x <- str_split(prr2$pic_name2, "_")
prr2$n <-  unlist(lapply(x, function(x) x[1]))
prr2$B <-  unlist(lapply(x, function(x) as.numeric(x[2])/100))
#prr$beta <-  factor(prr$beta, levels=c("0", "1", "2", "3", "5", "7", "8", "10", "16"))
prr2$sd <-  unlist(lapply(x, function(x) x[3]))
prr2$rep <-  unlist(lapply(x, function(x) x[4]))
prr2$pic_name3 <- factor(paste(prr2$beta, prr2$n, prr2$sd, prr2$rep, sep="."))
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
  facet_grid(B~n+sd+rep, scales="free_y", labeller="label_both") +
  opts(legend.position="none")
p
ggsave(file="../images/p_val_log_counts2.pdf", height=10, width=14)


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
  
  pval_sm <- rbind(data.frame(Experiment="1",Total = nrow(min_pval1), Most_pick_minimum = sum(min_pval1$most_pick_minimum_pval)),
                   data.frame(Experiment="2",Total = nrow(min_pval2), Most_pick_minimum = sum(min_pval2$most_pick_minimum_pval))
                   )
  return(pval_sm)
}


indx1 <- get_sceering_index(dat1)
indx2 <- get_sceering_index(dat2)
pval_sm <- NULL
for (i in 1:6) {
  d1 <- subset(dat1,indx1[,i])
  d2 <- subset(dat2,indx2[,i])
  pval_sm <- rbind(pval_sm,cbind(screening=i,get_all_pval_sam(d1,d2,pval1,pval2)))
}

print(xtable(pval_sm), include.rownames=FALSE)



# =================== Turk1 data analysis  ================================

dat <- read.csv("../data/raw_data_turk1.csv")

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

calculate_ump_power2 <- function (beta, n, sigma){
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


# ==== turk3 data anaysis ===========


getx <- function(n){
  x1 <- rnorm(n,0,1)
  nc <- 15*n/100
  x2 <- rnorm(n=nc,mean=-1.75, sd=1/3)
  return(c(x2,x1))
}
getx(100)

calculate_ump_power3 <- function (beta, n, sigma,x){
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

calculate_ump_power3(beta=.1,n=100,sigma=5,x=getx(100))



# ======================= p_value vs %correct =====================

pdat <- NULL
for (i in 1:3){
   dati <- eval(parse(text=paste("dat",i,sep="")))
   pdati <- ddply(dati, .(p_value, sample_size), summarize,
	  attempted = length(response),
	  corrected = sum(response),
	  percent_correct = mean(response)*100
	)
   pdati$experiment=paste("experiment",i)
   pdat <- rbind(pdat,pdati)
}

# pdat$percent_correct <- pdat$corrected*100/pdat$attempted

p <- ggplot() +
     geom_point(aes(p_value,percent_correct),data=pdat,size=2) + 
     facet_grid(.~experiment) +
     xlab(expression(paste("p-value(",p[D],")"))) +
     ylab("Percentage of correct responses") 
p 

ggsave(p,file="../images/p_val_percent_correct.pdf", height = 4, width = 10)

# ----- p-value vs plot signal strength --------------------

pdat <- NULL
for (i in 1:3){
   dati <- eval(parse(text=paste("dat",i,sep="")))
   pdati <- ddply(dati, .(p_value), summarize,
	  attempted = length(response),
	  corrected = sum(response),
	  percent_correct = ifelse(sum(response)>0,mean(response),1/length(response))
	)
   pdati$experiment=paste("experiment",i)
   pdat <- rbind(pdat,pdati)
}

#pdat$strength <- 1 - (pdat$percent_correct/100)^(1/19) #previous estimation
pdat$strength <- (1 - pdat$percent_correct)/19
p <- ggplot(pdat) +
     geom_point(aes(p_value,strength),size=2) + 
     facet_grid(.~experiment) +
     xlab(expression(paste("Classical test p-value (",p[D],")"))) +
     ylab(expression(paste("plot signal strength (", hat(p)[D],")"))) + 
     geom_abline(aes(intercept=0,slope=1))
p 

ggsave(p,file="../images/p_val_plot_signal.pdf", height = 4, width = 10)

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




