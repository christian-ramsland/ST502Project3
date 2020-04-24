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


storeresults <- matrix(nrow = 135, ncol = 6)
N = 100
alpha = 0.05
sigmasq1 <- c(1,3,9)
sigmasq2 <- 1
sampleobs1 <- c(10,25,60)
sampleobs2 <- c(10,25,60)
mew <- 0
mewtwo <- c(-5,-1,0,1,5)


row <- 0
for(i in 1:length(mewtwo)){
  for(j in 1:length(sigmasq1)){
    for(k in 1:length(sampleobs1)){
      for(l in 1:length(sampleobs2)){
        rejecteq = 0 # counter for numbers of times it rejects H0 w/ equal variance
        rejectuneq = 0 # counter for numbers of times it rejects H0 w/ unequal variance
        for(m in 1:N){
          ynull = rnorm(n = sampleobs1[k], mean = mew, sd=sqrt(sigmasq1[j]))
          yalt = rnorm(n = sampleobs2[l], mean = mewtwo[i])
          testuneq = t.test(ynull,yalt, var.equal = FALSE)
          testeq = t.test(ynull,yalt, var.equal = TRUE)
          rejectuneq = ifelse((testuneq$p.value < alpha), rejectuneq + 1, rejectuneq) # check that every var on this line is "uneq"
          rejecteq = ifelse((testeq$p.value < alpha), rejecteq + 1, rejecteq) # check that every var on this line is "eq"
        }
      row = row + 1
      # filling up the storage matrix with values needed to create plots
      storeresults[row,1] = mewtwo[i]
      storeresults[row,2] = sampleobs1[k]
      storeresults[row,3] = sampleobs2[l]
      storeresults[row,4] = sigmasq1[j]
      storeresults[row,5] = (rejecteq)/N
      storeresults[row,6] = (rejectuneq)/N
      }
    }
  }
}

# getting data set up to be plotted nicely - hardcoded but preferable to running all those loops twice!
storeresultsdf <- as.data.frame(storeresults)
storeresultsdf <- rbind(storeresultsdf,storeresultsdf)
storeresultsdf[136:270,5] <- storeresultsdf[1:135,6]
storeresultsdf[1:135,6] <- 0 
storeresultsdf[136:270,6] <- 1

# plot construction

library(ggplot2)

colnames(storeresultsdf) <- c("mu2","n1","n2","sigmasq_1","reject","eq")
storeresultsdf$sigmasq_1 <- as.factor(storeresultsdf$sigmasq_1)
storeresultsdf$eq <- as.factor(storeresultsdf$eq)
storeresultsdf$n1 <- as.factor(storeresultsdf$n1)
storeresultsdf$n2 <- as.factor(storeresultsdf$n2)
levels(storeresultsdf$n1) <- c("sample1 = 10","sample1 = 25","sample1 = 60")
levels(storeresultsdf$n2) <- c("sample2 = 10","sample2 = 25","sample2 = 60")


ggplot(data = storeresultsdf, aes(x = mu2, y = reject, colour = sigmasq_1)) +
  geom_line(aes(linetype=eq)) + facet_grid(vars(n2), vars(n1)) +
    labs(colour = "Group 1 Variance", x = "Mu1 - Mu2", y = "Power") +
      guides(linetype=FALSE) +
        ggtitle("Dashed Lines represent Power for the Unqual Variance Test \n Solid Lines represent Power for the Equal Variance Test") +
          theme(plot.title = element_text(size = 10))
  




