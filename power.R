
library(ggplot2)

powerdf <- data.frame(expand.grid(m = 5:50,
pb=c(seq(0.05,0.5, by=0.05), seq(0.25, 0.5, by=0.05))))

powerdf$power <- with(powerdf, (1-pb)^(m-1))

K <- 10
ggplot(aes(x=m, power,  group=pb, colour=pb), data=powerdf) + geom_line(aes(m, 1/m), colour="grey30", lty=2) + geom_text(aes(label=sprintf("%.2f", pb), show_guide=FALSE), data=subset(powerdf, m==5), hjust=1.1) + xlim(c(0,50)) + scale_colour_gradient(name=expression(p[B])) + geom_ribbon(aes(x=m, ymin=1/m-1.96*sqrt(1/m*(m-1)/m)/K, ymax=1/m+1.96*sqrt(1/m*(m-1)/m)/K), fill="grey80", lty=0, alpha=0.1) +
geom_line()
ggsave(file="powerplot.pdf")


powerdf$EX <- with(powerdf, (m-1)*(1-pb))


qplot(m, EX, geom="line", group=pb, data=powerdf, colour=pb) + geom_line(aes(m, 1/m), colour="grey30", lty=2)  + geom_text(aes(label=sprintf("%.2f",pb)), data=subset(powerdf, m==5), hjust=1.1) + xlim(c(0,50)) + scale_colour_gradient(name=expression(p[B]))



library(nullabor)
n <- 100
x <- rnorm(n)
y <- rnorm(n) + 0.15*x
summary(lm(y~x))
df <- data.frame(x=x, y=y)
qplot(x, y, data = df)  %+%  lineup(null_permute("y")) + facet_wrap(~ .sample) + geom_smooth(method="lm")
