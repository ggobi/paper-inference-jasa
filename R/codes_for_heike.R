lineups <- ddply(raw.dat, .(pic_name, academic_study), summarise,
	ni = sum(response),
	K = length(response),
	sample_size=sample_size[1],
	p_value=p_value[1],
	beta=beta[1],
	sigma=sigma[1],
	age=mean(age)
)
lineups$effect <- with(lineups, beta/sigma)


lineups.glm <- glm(cbind(ni,K-ni)~effect+ sample_size, data=lineups, family=binomial(link="logit"))
preds <- predict(lineups.glm, type="response", se.fit=TRUE)

lineups$preds<- preds$fit
lineups$se <- preds$se.fit


calculate_ump_power <- function (beta, n, sigma){
	alpha <- 0.05/2
	se_beta <- sigma/(0.5 * sqrt(n))    # refer to docs for derivation of power
	mu <- beta/se_beta
	t_n <- qt(p=1-alpha,df=n-3)
	res <- pt(q=-t_n, df=n-3, ncp=mu)-pt(q=t_n, df=n-3, ncp=mu)+1
	return(res)
}

ump100 <- data.frame(effect=seq(0,1.6,by=.01),
          power = sapply(seq(0,1.6,by=.01),calculate_ump_power,n=100,sigma=1),
          sample_size=100)
ump300 <- data.frame(effect=seq(0,1.6,by=.01),
          power = sapply(seq(0,1.6,by=.01),calculate_ump_power,n=300,sigma=1),
          sample_size=300)
ump <- rbind(ump100,ump300)   
 
ggplot() + 
geom_point(aes(effect, preds,colour=factor(sample_size)), data=lineups) +
geom_errorbar(aes(effect,preds, colour=factor(sample_size),ymax = preds + 1.95*se, ymin=preds - 1.95*se), data=lineups)+ 
geom_line(data=ump,aes(effect,power, colour=factor(sample_size)))   # how do I get that line here?

