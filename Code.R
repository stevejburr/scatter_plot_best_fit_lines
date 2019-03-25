#exploring different ways of drawing lines of best fit on scatter plots
library(tidyverse)
library(mvtnorm)
library(patchwork)

#to do:
#update labels to use the correct names
#potential have 3 or 4 different types of data, and a plot for each
#add a references section and write up

make_plots <- function(x,y) {
#model y using x (standard best fit line)
line1 <- lm(y~x)$coef
line1.slope <- line1[2]
line1.intercept <- line1[1]
#model x using y (what if x is actually predicted by y, not true)
line2 <- lm(x~y)$coef
#y = mx + c
#(y-c) = mx
#x = (1/m)y - (c/m)
line2.slope <- (1/line2[2])
line2.intercept <- -(line2[1]/line2[2])

#calculate the principal components of x/y
pca <- prcomp(cbind(x,y))$rotation
pca.slope <- pca[2,1] / pca[1,1]
pca.intercept <- mean(y) - (pca.slope * mean(x))

ggplot() +
  geom_point(aes(x=x,y=y)) +
  #geom_abline(aes(slope=line1.slope,intercept=line1.intercept,colour="red") +
  #geom_abline(aes(slope=line2.slope,intercept=line2.intercept),colour="blue") +
  geom_abline(aes(slope=pca.slope,intercept=pca.intercept)) -> plot1



#calculate residuals of three lines at each real point x + plot these

data <- data.frame(x,y)
data %>%
  mutate(yhat_line1=(x*line1.slope+line1.intercept),
         xhat_line1=(y-line1.intercept)/line1.slope,
         residual_line1=-(yhat_line1-y),
         yhat_line2=(x*line2.slope+line2.intercept),
         xhat_line2=(y-line2.intercept)/line2.slope,
         residual_line2=-(yhat_line2-y),
         yhat_line3=(x*pca.slope+pca.intercept),
         xhat_line3=(y-pca.intercept)/pca.slope,
         residual_line3=-(yhat_line3-y),
         #https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
         a=pca.slope,
         b=-1,
         c=pca.intercept,
         xhat_line3=(b*(b*x-a*y)-(a*c))/((a*a)+(b*b)),
         yhat_line3=(a*(-b*x+a*y)-(b*c))/((a*a)+(b*b)))%>% 
  select(-c(a,b,c))-> data
#currently residual_line# shows the ls residuals for y ~ x
#calculate all the residuals using the formula...

#express these as "line start / line end" coords with different labels, then can easily do facetted residual plots + calcs

#draw residual lines for fitting y~x
data %>%
  ggplot()+
  #geom_point(aes(x=x,y=yhat_line1),shape=2) +
  geom_point(aes(x=x,y=y))+
  geom_segment(aes(x=x,y=y,xend=x,yend=yhat_line1))+
  geom_abline(aes(slope=line1.slope,intercept=line1.intercept),colour="red") -> plot1a

#draw residual lines for fitting x~y
data %>%
  ggplot()+
  geom_point(aes(x=xhat_line2,y=y),shape=2) +
  geom_point(aes(x=x,y=y))+
  geom_segment(aes(x=x,y=y,xend=xhat_line2,yend=y))+
  geom_abline(aes(slope=line2.slope,intercept=line2.intercept),colour="blue") -> plot1a


#draw residual lines for fitting via pca
data %>%
  ggplot()+
  geom_point(aes(x=xhat_line3,y=yhat_line3),shape=2) +
  geom_point(aes(x=x,y=y))+
  geom_segment(aes(x=x,y=y,xend=xhat_line3,yend=yhat_line3))+
  geom_abline(aes(slope=pca.slope,intercept=pca.intercept),colour="black") -> plot1a

plot1+plot1a

data %>%
  select(x,starts_with("resid")) %>% 
  gather(key="key",value="value",-x) %>%
  ggplot(aes(x=x,y=value,color=key)) +
  facet_wrap(key ~ .) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values=c("residual_line1"="red",
                              "residual_line2"="blue",
                              "residual_line3"="black"))-> plot2

  print(plot1 + plot2)
  

}


#simulate x/y using simple sets of distributions
set.seed(123)
x <- rnorm(50,mean=100,sd=10)
# y = 0.8 * x + noise
y <- 0.8 * x + rnorm(50,mean=10,sd=8)
make_plots(x,y)

#actual multivariate normal case:
mean <- c(100,90)
sigma <- as.matrix(rbind(c(1,0.6),c(0.6,1)))
set.seed(123)
data.matrix <- rmvnorm(1000,mean,sigma)
x <- data.matrix[,1]
y <- data.matrix[,2]

make_plots(x,y)


#highly correlated rvnorm
mean <- c(100,90)
sigma <- as.matrix(rbind(c(1,0.9),c(0.9,1)))
set.seed(123)
data.matrix <- rmvnorm(1000,mean,sigma)
x <- data.matrix[,1]
y <- data.matrix[,2]

make_plots(x,y)



#lowly correlated rvnorm
mean <- c(100,90)
sigma <- as.matrix(rbind(c(1,0.2),c(0.2,1)))
set.seed(123)
data.matrix <- rmvnorm(1000,mean,sigma)
x <- data.matrix[,1]
y <- data.matrix[,2]

make_plots(x,y)


#look at what if we are measuring the wrong way round
set.seed(123)
y <- rnorm(1000,mean=100,sd=10)
# y = 0.8 * x + noise
x <- 0.8 * y + rnorm(1000,mean=10,sd=8)
make_plots(x,y)

#look at first approach with higher noise...

set.seed(123)
x <- rnorm(1000,mean=100,sd=10)
# y = 0.8 * x + noise
y <- 0.8 * x + rnorm(1000,mean=20,sd=10)
make_plots(x,y)


#look at what if we are measuring the wrong way round + high noise
set.seed(123)
y <- rnorm(1000,mean=100,sd=10)
# y = 0.8 * x + noise
x <- 0.8 * y + rnorm(1000,mean=20,sd=10)
make_plots(x,y)



#make more stretched out
set.seed(123)
x <- rnorm(1000,mean=100,sd=20)
# y = 0.8 * x + noise
y <- 0.8 * x + rnorm(1000,mean=30,sd=5)
make_plots(x,y)



#geometrically can think of levers
#if you draw a triangle, the hypotenuse is the middle PCA which balances both residuals
#the standard linear regression line only cares about vertical residuals, so is pulls the line flatter
#the reverses regression line only cares about horizontal, so pulls the line steaper
#draw the triangle + show movements.

#so if you think the true relationship is the standard regression,
#then by eye you will naturally overstate the relationship


#add an extra set of plots, show/calculate all three types of residual for the 3 different colours + facet...
#this will show what each technique is minimising.