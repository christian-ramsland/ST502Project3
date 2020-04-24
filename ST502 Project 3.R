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




# Part 2 Simulation Study


storeresults <- matrix(nrow = 135, ncol = 6)
N = 100
sigmasq1 <- c(1,3,9)
sigmasq2 <- 1
sampleobs1 <- c(10,25,60)
sampleobs2 <- c(10,25,60)
mew <- 0
mewtwo <- c(-5,-1,0,1,5)

count <- 0
for(i in 1:length(mewtwo)){ #mewtwo
  for(j in 1:length(sigmasq1)){
    for(k in 1:length(sampleobs1)){ #n1
      for(l in 1:length(sampleobs2)){ #n2
        pass = 0 # counter for numbers of times it rejects H0
        for(m in 1:N){
          ynull = rnorm(n = sampleobs1[k], mean = mew, sd=sqrt(sigmasq1[j]))
          yalt = rnorm(n = sampleobs2[l], mean = mewtwo[i])
          test = t.test(ynull,yalt, var.equal = FALSE)
          pass = ifelse((test$p.value < 0.05), pass + 1, pass)
        }
      count = count + 1
      storeresults[count,1] = count
      storeresults[count,2] = mewtwo[i]
      storeresults[count,3] = sampleobs1[k]
      storeresults[count,4] = sampleobs2[l]
      storeresults[count,5] = sigmasq1[j]
      storeresults[count,6] = pass
      }
    }
  }
}

