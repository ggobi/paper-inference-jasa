
library(ggplot2)

powerdf <- data.frame(expand.grid(pd = seq(0.0001, 0.20, by=0.001), m = seq(10, 30, by=5)))
powerdf$power <- with(powerdf, pbeta(pd, 1, m-1, lower.tail=FALSE))
powerdf$size <- with(powerdf, I(.75+(m==20)))
powerdf$labels <- paste("m =",powerdf$m)
powerdf$hjust <- c(-0.15,1.15)[(powerdf$m > 20) + 1]
powerdf$power <- with(powerdf, pmax(power, 1/m))

qplot(pd, power, data=powerdf, geom="line", group=m, size=size) + ylab("Prob that data plot has lowest p-value") + xlab(expression(p[D])) + scale_size_identity() +geom_text(aes(x=pd, y=power, label=labels, hjust=hjust), size=3, data=subset(powerdf, abs(4*pd - power) < 0.005 )) + geom_hline(y=1/seq(15,30, by=5), linetype=2, alpha=0.3) 

ggsave(file="powerplot.pdf")





powerdf$EX <- with(powerdf, (m-1)*(1-pb))


ggplot(aes(x=pb, m,  group = power, colour=power), data=powerdf) + geom_point()



qplot(m, EX, geom="line", group=pb, data=powerdf, colour=pb) + geom_line(aes(m, 1/m), colour="grey30", lty=2)  + geom_text(aes(label=sprintf("%.2f",pb)), data=subset(powerdf, m==5), hjust=1.1) + xlim(c(0,50)) + scale_colour_gradient(name=expression(p[B]))



library(nullabor)
n <- 100
x <- rnorm(n)
y <- rnorm(n) + 0.15*x
summary(lm(y~x))
df <- data.frame(x=x, y=y)
qplot(x, y, data = df)  %+%  lineup(null_permute("y")) + facet_wrap(~ .sample) + geom_smooth(method="lm")
