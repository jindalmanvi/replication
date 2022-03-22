install.packages("haven")
library(haven)
library(ggplot2)
library(stargazer)
library(AER) #for ivreg
library(ivmodel)
slave_data<- read_dta("slave_trade_QJE.dta")


N<- 52
K<- 1
G<-matrix(slave_data$abs_latitude, nrow = N)
G_sums <- sapply(c(1:N), function(abs_latitude) sum(G[slave_data$abs_latitude,])) 
G <- G/G_sums
w_means <- runif(K,0,10)
w <- matrix(sapply(w_means, function(abs_latitude) rnorm(1,slave_data$abs_latitude,5)),nrow=K)

Z <- G%*%w
mu <- G%*%w_means
Z_rc <- Z-mu

mu <- as.vector(mu)
e1 <- sapply(mu,function(abs_latitude) rnorm(1,slave_data$abs_latitude,0.5))
e2 <- sapply(e1,function(abs_latitude) rnorm(1,slave_data$abs_latitude,0.01))


D_weights <- matrix(slave_data$abs_latitude,
                    nrow=N)
D <- Z+e1

Y <- D+e2

df <- tibble(
  "Y"=Y,
  "D"=D,
  "Z"=as.vector(Z),
  "Z_rc"=as.vector(Z_rc)
)
install.packages("fixest")
library(fixest)

feols(ln_maddison_pcgdp2000~1|ln_export_area~abs_latitude,
      data=slave_data)





ivreg1<-ivreg(ln_maddison_pcgdp2000~ln_export_area|saharan_distance_minimum+
                atlantic_distance_minimum+indian_distance_minimum+red_sea_distance_minimum, data=slave_data)