
# Canonical-correlation-analysis

rm(list = ls())

#import packages

library(PMA)
library(ggplot2)
library(ggExtra)


#Data Set

set.seed(123)
x1 <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
x2 <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
x3 <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
x4 <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)


v6 <- matrix(c(rep(1,25),rep(0,25)),ncol=1)
v6 <- as.factor(v6)


# permutation test_sorting

N <- 10
corPerm_1 <- numeric(length = N) 
corPerm_2 <- numeric(length = N) 
corPerm_3 <- numeric(length = N) 
corPerm_4 <- numeric(length = N) 
corPerm_5 <- numeric(length = N) 
corPerm_6 <- numeric(length = N) 
corPerm_total<- numeric(length = N) 

for(i in 1:N)
{
  #Sorting
  
  x1_sample <- matrix(sample(c(x1)), nrow = 50, ncol = 10)
  x2_sample <- matrix(sample(c(x2)), nrow = 50, ncol = 10)
  x3_sample <- matrix(sample(c(x3)), nrow = 50, ncol = 10)
  x4_sample <- matrix(sample(c(x4)), nrow = 50, ncol = 10)
  
  #Store in a list
  
  xlist_sample <- list(x1_sample, x2_sample, x3_sample, x4_sample)
  
  #SMCCA in Sorted Datasets
  
  #Execute MultiCCA.permute_sample
  
  perm_out_sample <- MultiCCA.permute(xlist_sample, nperms=10)#10permutations
  
  
  #Execute MultiCCA_sample
  
  out_sample <- MultiCCA(xlist_sample, penalty=perm_out_sample$bestpenalties)
  

  #Fitting to a Regression Model_sample
  
  x_1_sample <- x1_sample%*%out_sample$ws[[1]]
  x_2_sample <- x2_sample%*%out_sample$ws[[2]]
  x_3_sample <- x3_sample%*%out_sample$ws[[3]]
  x_4_sample <- x4_sample%*%out_sample$ws[[4]]
  
  
  corPerm_1[i] <- cor(x_1_sample,x_2_sample)
  corPerm_2[i] <- cor(x_1_sample,x_3_sample)
  corPerm_3[i] <- cor(x_1_sample,x_4_sample)
  corPerm_4[i] <- cor(x_2_sample,x_3_sample)
  corPerm_5[i] <- cor(x_2_sample,x_4_sample)
  corPerm_6[i] <- cor(x_3_sample,x_4_sample)
  corPerm_total[i] <- corPerm_1[i]+corPerm_2[i]+corPerm_3[i]+corPerm_4[i]+corPerm_5[i]+corPerm_6[i]
}


#Store in a list

xlist <- list(x1,x2,x3,x4)


# Execute MultiCCA.permute

perm.out <- MultiCCA.permute(xlist, nperms=10)#10permutations
print(perm.out)

#Execute MultiCCA

out <- MultiCCA(xlist, penalty=perm.out$bestpenalties)
print(out)


#Fitting to a Regression Model

x_1 <- x1%*%out$ws[[1]]
x_2 <- x2%*%out$ws[[2]]
x_3 <- x3%*%out$ws[[3]]
x_4 <- x4%*%out$ws[[4]]



#Sum of Correlation Coefficients

cor_1=cor(x_1,x_2)
cor_2=cor(x_1,x_3)
cor_3=cor(x_1,x_4)
cor_4=cor(x_2,x_3)
cor_5=cor(x_2,x_4)
cor_6=cor(x_3,x_4)

cor_total=cor_1+cor_2+cor_3+cor_4+cor_5+cor_6


# permutation_testのfigure

results.df <- data.frame(x = unlist(corPerm_total))
ggplot(results.df,aes(x)) + 
  geom_histogram(color="darkgreen",fill="lightseagreen") +
  geom_density(fill='green', alpha=0.3) +
  geom_vline(xintercept = cor_total, lwd=1, lty=2) +
  xlab("sum of correlation coefficients")+
  ylab("Density")+
  scale_x_continuous(limits = c(-2, 5)) +
  theme_classic()+
  theme(axis.text.x = element_text(size = 15),  
       axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15))


cor_total#2.249144
corPerm_total
p_value_Cor <- sum(unlist(corPerm_total)>2.249144)/length(corPerm_total)
p_value_Cor

# Scatter Plot

data<-data.frame(x_1,x_2,x_3,x_4,v6)

scatter_plot_color <- ggplot(data,aes(x = x_1, y = x_2))+
  geom_point(aes(x=x_1, y=x_2, color = v6)) +
  geom_smooth(method = "lm", color = "red",
              linetype = "dashed",
              se = TRUE,
              size = 0.5,
              fill = "pink")+
  xlab("x_1") +  
  ylab("x_2") + 
  theme_classic()

ggMarginal(scatter_plot_color, type = "densigram", groupFill = TRUE)
