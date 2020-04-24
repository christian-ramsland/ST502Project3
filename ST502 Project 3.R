#ST502 R Project 3
###############################
set.seed(6)

#Part 1: Comparing Two-Sample T-Tests

#Read in MPG data
mpgData <- read.csv("~/ST503/Code/mpg.txt", sep="")

#Create objects for US MPG and Japanese MPG
USmpg <- mpgData[1:249,1]
JapanMPG <- mpgData[250:328,1]

#Pooled Two-Sample T-Test
t.test(x=USmpg,y=JapanMPG,paired=FALSE,var.equal=TRUE)
print("The pooled t-test suggests there is evidence that there is a difference in MPG between US-made cars and Japanese-made cars.")

#Unequal Variances Two-Sample T-Test
t.test(x=USmpg,y=JapanMPG,paired=FALSE,var.equal=FALSE)
print("The unequal variances t-test suggests there is evidence that there is a difference in MPG between US-made cars and Japanese-made cars.")

#Plot densities for US MPG and Japanese MPG
plot(density(USmpg),xlab="MPG",main="MPG for US-Made Cars")
plot(density(JapanMPG),xlab="MPG",main="MPG for Japanese-Made Cars")
#Based on the density plots, the normality assumption is not well met for either sample.
#The US MPG data is right-skewed and multimodal.
#The Japanese MPG data is bimodal and slightly left-skewed.

#Which test is best?
#Perform an F-test to compare the two variances
var.test(x=USmpg,y=JapanMPG,alternative="two.sided")
#The F-Test suggests there is not a significant difference in variances.
#Comparison of results from the two types of t-tests:
 #same p-values and both tests rejected the null hypothesis
 #the CI for the unequal variances t-test was slightly smaller
#Based on the results from the F-test, I would use the pooled variances t-test





# Part 2 Simulation Study - equivalence 


storeresults_uneq <- matrix(nrow = 135, ncol = 6)
N = 100
sigmasq1 <- c(1,3,9)
sigmasq2 <- 1
sampleobs1 <- c(10,25,60)
sampleobs2 <- c(10,25,60)
mew <- 0
mewtwo <- c(-5,-1,0,1,5)

row <- 0
for(i in 1:length(mewtwo)){ #mewtwo
  for(j in 1:length(sigmasq1)){
    for(k in 1:length(sampleobs1)){ #n1
      for(l in 1:length(sampleobs2)){ #n2
        reject = 0 # counter for numbers of times it rejects H0
        for(m in 1:N){
          ynull = rnorm(n = sampleobs1[k], mean = mew, sd=sqrt(sigmasq1[j]))
          yalt = rnorm(n = sampleobs2[l], mean = mewtwo[i])
          test = t.test(ynull,yalt, var.equal = FALSE)
          reject = ifelse((test$p.value < 0.05), reject + 1, reject)
        }
      row = row + 1
      # filling up the storage matrix with values needed to create plots
      storeresults_uneq[row,1] = mewtwo[i]
      storeresults_uneq[row,2] = sampleobs1[k]
      storeresults_uneq[row,3] = sampleobs2[l]
      storeresults_uneq[row,4] = sigmasq1[j]
      storeresults_uneq[row,5] = reject
      storeresults_uneq[row,6] = 0
      }
    }
  }
}

storeresults_eq <- matrix(nrow = 135, ncol = 6)
row <- 0
for(i in 1:length(mewtwo)){ #mewtwo
  for(j in 1:length(sigmasq1)){
    for(k in 1:length(sampleobs1)){ #n1
      for(l in 1:length(sampleobs2)){ #n2
        reject = 0 # counter for numbers of times it rejects H0
        for(m in 1:N){
          ynull = rnorm(n = sampleobs1[k], mean = mew, sd=sqrt(sigmasq1[j]))
          yalt = rnorm(n = sampleobs2[l], mean = mewtwo[i])
          test = t.test(ynull,yalt, var.equal = TRUE)
          reject = ifelse((test$p.value < 0.05), reject + 1, reject)
        }
        row = row + 1
        # filling up the storage matrix with values needed to create plots
        storeresults_eq[row,1] = mewtwo[i]
        storeresults_eq[row,2] = sampleobs1[k]
        storeresults_eq[row,3] = sampleobs2[l]
        storeresults_eq[row,4] = sigmasq1[j]
        storeresults_eq[row,5] = reject
        storeresults_eq[row,6] = 1
      }
    }
  }
}

# plot construction

library(ggplot2)

storeresults <- as.data.frame(storeresults_uneq)
colnames(storeresults) <- c("mu2","n1","n2","sigmasq_1","reject","eq")
storeresults$sigmasq_1 <- as.factor(storeresults$sigmasq_1)

ggplot(data = storeresults, aes(x = mu2, y = reject, group=sigmasq1)) +
  geom_line(aes(linetype=sigmasq_1))


