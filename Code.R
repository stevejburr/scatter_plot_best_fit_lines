#exploring different ways of drawing lines of best fit on scatter plots
library(tidyverse)
library(mvtnorm)
library(patchwork)


library(scales)
show_col(hue_pal()(3))

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

#calculate predicted positions on each line, and corresponding "residual" lines
data <- data.frame(x,y)
data %>%
  mutate(yhat_line1=(x*line1.slope+line1.intercept),
         xhat_line1=x,
         yhat_line2=y,
         xhat_line2=(y-line2.intercept)/line2.slope,
         #https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
         a=pca.slope,
         b=-1,
         c=pca.intercept,
         xhat_line3=(b*(b*x-a*y)-(a*c))/((a*a)+(b*b)),
         yhat_line3=(a*(-b*x+a*y)-(b*c))/((a*a)+(b*b)),
         #want slopes/intercepts in this data frame:
         slope_line1=line1.slope,
         slope_line2=line2.slope,
         slope_line3=pca.slope,
         intercept_line1=line1.intercept,
         intercept_line2=line2.intercept,
         intercept_line3=pca.intercept
         )%>% 
  select(-c(a,b,c)) %>%
  #transpose to a long form
  gather(key="key",value="value",-c(x,y)) %>% 
  # have "yhat_line1", want two colums of "yhat" "line1"
  separate(key,c("type","line"),"_") %>% 
  #then transpose so we have cols for xhat, yhat etc
  spread(key="type",value="value") %>%
  #calculate the residuals for each method
  #i.e. the distance from the point to the line
  mutate(line=case_when(
           line=="line1" ~ "y~x",
           line=="line2" ~ "x~y",
           line=="line3" ~ "PCA"
         ),
         line=factor(line,levels=c("y~x","x~y","PCA"))) -> data


#facetted scatter plot with three sets of residual lines
data %>% 
  ggplot() +
  facet_grid(line ~ .) +
  geom_point(aes(x=x,y=y,colour=line))+
  geom_abline(aes(slope=slope,intercept=intercept,colour=line))+
  geom_segment(aes(x=x,y=y,xend=xhat,yend=yhat,colour=line))+
  coord_equal() +
  theme_minimal() +
  theme(panel.grid=element_blank()) -> plot1

#basic scatter plot
ggplot() +
  geom_point(data=distinct(data,x,y),
             aes(x=x,y=y),colour="grey50")+
  geom_abline(data=distinct(data,line,slope,intercept),
              aes(slope=slope,intercept=intercept,colour=line))+
  coord_equal() +
  theme_minimal() +
  theme(panel.grid=element_blank()) -> plot2

#add conditional mean of y given a banded value of x to a basic scatter plot
data %>% 
  distinct(x,y) -> pairs
#use cut to create evenly sized groups
pairs$x_group <- cut(pairs$x,5)

pairs %>%
  group_by(x_group) %>%
  summarise(y_bar = mean(y),
            max_x=max(x),
            min_x=min(x)) -> condMeans

plot2 +
  geom_segment(data=condMeans,
               aes(x=min_x,xend=max_x,y=y_bar,yend=y_bar)) -> plot3

#gelman / hill plot, look at standardised variables
stdize <- function(x){(x-mean(x))/sd(x)}

stdData <- data.frame(x,y)
stdData %>% 
  mutate_all(funs(stdize,mean,sd)) -> stdData

stdlm <- lm(y_stdize ~ x_stdize, data=stdData)

stdData$slope <- coef(stdlm)[2]
stdData$intercept <- coef(stdlm)[1]
stdData$y_hat <- (stdData$x_stdize * stdData$slope) + stdData$intercept

ggplot(stdData) +
  geom_vline(aes(xintercept=0),colour="grey80")+
  geom_hline(aes(yintercept=0),colour="grey80")+
  geom_point(aes(x=x_stdize,y=y_stdize),colour="grey50") +
  geom_abline(aes(slope=slope,intercept=intercept),colour="#F8766D") +
  geom_abline(aes(slope=1,intercept=0),colour="#F8766D",linetype=2) +
  scale_y_continuous("Standard deviations from mean of y") +
  scale_x_continuous("Standard deviations from mean of x")+
  coord_equal() +
  theme_minimal()+
  theme(panel.grid=element_blank(),
        axis.title=element_text(colour="grey50"),
        axis.text=elemnt_text(colour="grey50"),
        text=element_text(colour="grey50")) +
  labs(title="Illustration of regression to the mean using standardised variables.",
       subtitle="With standardised variables, the gradient of the regression line is the correlation between the two variables.\nIf there is perfect correlation, then this gradient is equal to 1 (dotted line).\nIf the correlation is less than this then we will observe 'regression to the mean'.\nIf x is say 2sd from the mean of x, then y will be predicted to be 2*gradients from the mean of y.\nFor a gradient of less than 1, this will mean that y is predicted to be closer to the mean than x, this is regression to the mean.") -> plot4

print(plot1+plot2)
print(plot3)
print(plot4)
}


#simulate x/y using simple sets of distributions
set.seed(123)
x <- rnorm(500,mean=100,sd=10)
# y = 0.8 * x + noise
y <- 0.8 * x + rnorm(500,mean=10,sd=8)
make_plots(x,y)


#actual multivariate normal case:
mean <- c(100,100)
sigma <- as.matrix(rbind(c(1,0.6),c(0.6,1)))
set.seed(123)
data.matrix <- rmvnorm(100,mean,sigma)
x <- data.matrix[,1]
y <- data.matrix[,2]

make_plots(x,y)

# 
# #highly correlated rvnorm
# mean <- c(100,90)
# sigma <- as.matrix(rbind(c(1,0.9),c(0.9,1)))
# set.seed(123)
# data.matrix <- rmvnorm(1000,mean,sigma)
# x <- data.matrix[,1]
# y <- data.matrix[,2]
# 
# make_plots(x,y)
# 
# 
# 
# #lowly correlated rvnorm
# mean <- c(100,90)
# sigma <- as.matrix(rbind(c(1,0.2),c(0.2,1)))
# set.seed(123)
# data.matrix <- rmvnorm(1000,mean,sigma)
# x <- data.matrix[,1]
# y <- data.matrix[,2]
# 
# make_plots(x,y)
# 
# 
# #look at what if we are measuring the wrong way round
# set.seed(123)
# y <- rnorm(1000,mean=100,sd=10)
# # y = 0.8 * x + noise
# x <- 0.8 * y + rnorm(1000,mean=10,sd=8)
# make_plots(x,y)
# 
# #look at first approach with higher noise...
# 
# set.seed(123)
# x <- rnorm(1000,mean=100,sd=10)
# # y = 0.8 * x + noise
# y <- 0.8 * x + rnorm(1000,mean=20,sd=10)
# make_plots(x,y)
# 
# 
# #look at what if we are measuring the wrong way round + high noise
# set.seed(123)
# y <- rnorm(1000,mean=100,sd=10)
# # y = 0.8 * x + noise
# x <- 0.8 * y + rnorm(1000,mean=20,sd=10)
# make_plots(x,y)
# 
# 
# 
# #make more stretched out
# set.seed(123)
# x <- rnorm(1000,mean=100,sd=20)
# # y = 0.8 * x + noise
# y <- 0.8 * x + rnorm(1000,mean=30,sd=5)
# make_plots(x,y)



#geometrically can think of levers
#if you draw a triangle, the hypotenuse is the middle PCA which balances both residuals
#the standard linear regression line only cares about vertical residuals, so is pulls the line flatter
#the reverses regression line only cares about horizontal, so pulls the line steaper
#draw the triangle + show movements.

#so if you think the true relationship is the standard regression,
#then by eye you will naturally overstate the relationship


#add an extra set of plots, show/calculate all three types of residual for the 3 different colours + facet...
#this will show what each technique is minimising.