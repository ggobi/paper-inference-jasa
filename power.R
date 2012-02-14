
library(ggplot2)

powerdf <- data.frame(expand.grid(m = 5:50,
pb=c(seq(0.05,0.5, by=0.05), seq(0.25, 0.5, by=0.05))))

powerdf$power <- with(powerdf, (1-pb)^(m-1))

qplot(m, power, geom="line", group=pb, data=powerdf, colour=pb) + geom_line(aes(m, 1/m), colour="grey30", lty=2) + geom_text(aes(label=sprintf("%.2f",pb)), data=subset(powerdf, m==5), hjust=1.1) + xlim(c(0,50)) + scale_colour_gradient(name=expression(p[B]))
ggsave(file="powerplot.pdf")


powerdf$EX <- with(powerdf, (m-1)*(1-pb))


qplot(m, EX, geom="line", group=pb, data=powerdf, colour=pb) + geom_line(aes(m, 1/m), colour="grey30", lty=2) + geom_text(aes(label=sprintf("%.2f",pb)), data=subset(powerdf, m==5), hjust=1.1) + xlim(c(0,50)) + scale_colour_gradient(name=expression(p[B]))



library(nullabor)
n <- 100
x <- rnorm(n)
y <- rnorm(n) + 0.15*x
summary(lm(y~x))
df <- data.frame(x=x, y=y)
qplot(x, y, data = df)  %+%  lineup(null_permute("y")) + facet_wrap(~ .sample) + geom_smooth(method="lm")
