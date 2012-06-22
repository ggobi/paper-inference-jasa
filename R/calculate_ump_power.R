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
  res <- pt(q = -t_n, df = n - 3, ncp = mu) - pt(q = t_n, df = n - 3, ncp = mu) + 1
  return(res)
}

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

getx <- function(n){
  x1 <- rnorm(n,0,1)
  nc <- 15*n/100
  x2 <- rnorm(n=nc,mean=-1.75, sd=1/3)
  return(c(x2,x1))
}

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

get_ump_power_by_effect <- function(){
  set.seed(2035)
  ump <- rbind(data.frame(experiment="Experiment 1",effect=seq(0,16, by=.2),
                pow= calculate_ump_power1(beta=seq(0,16, by=.2)/10, n=100, sigma=1)),
               data.frame(experiment="Experiment 2",effect=seq(0,5.5, by=.2),
                pow= calculate_ump_power2(beta=seq(0,5.5, by=.2)/10, n=100, sigma=1)),
               data.frame(experiment="Experiment 3",effect=seq(0,4.5, by=.2),
                pow= calculate_ump_power3(beta=seq(0,4.5, by=.2)/sqrt(115), n=115, sigma=1, x=getx(100))))
  return(ump)
}


