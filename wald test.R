# problem 3

# Chlorpromazine vs. Placebo

# probability of nausea in Placebo group
pla.p <- 45/80
# probability of nausea in Chlorpromazine group
chl.p <- 26/75

# Wald test
dmean <- pla.p - chl.p
se <- sqrt(pla.p*(1 - pla.p)/80 + chl.p*(1 - chl.p)/75)
W <- dmean/se
W

# p-value
chl.pv <- 2*pnorm(-abs(W))

# odds ratio
(pla.p/(1 - pla.p))/(chl.p/(1 - chl.p))

# Dimenhydrinate vs. Placebo

# probability of nausea in Placebo group
pla.p <- 45/80
# probability of nausea in Dimenhydrinate group
dim.p <- 52/85

# Wald test
dmean <- pla.p - dim.p
se <- sqrt(pla.p*(1 - pla.p)/80 + dim.p*(1 - dim.p)/85)
W <- dmean/se
W

# p-value
dim.pv <- 2*pnorm(-abs(W))

# odds ratio
(pla.p/(1 - pla.p))/(dim.p/(1 - dim.p))

# Pentobarbital (100 mg) vs. Placebo

# probability of nausea in Placebo group
pla.p <- 45/80
# probability of nausea in Pentobarbital (100 mg) group
p100.p <- 35/67

# Wald test
dmean <- pla.p - p100.p
se <- sqrt(pla.p*(1 - pla.p)/80 + p100.p*(1 - p100.p)/67)
W <- dmean/se
W

# p-value
p100.pv <- 2*pnorm(-abs(W))

# odds ratio
(pla.p/(1 - pla.p))/(p100.p/(1 - p100.p))

# Pentobarbital (150 mg) vs. Placebo

# probability of nausea in Placebo group
pla.p <- 45/80
# probability of nausea in Pentobarbital (150 mg) group
p150.p <- 37/85

# Wald test
dmean <- pla.p - p150.p
se <- sqrt(pla.p*(1 - pla.p)/80 + p150.p*(1 - p150.p)/85)
W <- dmean/se
W

# p-value
p150.pv <- 2*pnorm(-abs(W))

# odds ratio
(pla.p/(1 - pla.p))/(p150.p/(1 - p150.p))

# only the W value in Chlorpromazine group is greater than 1.96 (5% level)
# so there is significant difference in effectiveness between Placebo and Chlorpromazine
# while no difference between Placebo and other groups

# Bonferroni methods

# new level under Bonferroni methods
0.05/4

# recall the p-values
chl.pv
dim.pv
p100.pv
p150.pv

# we have the same answer above
# there is significant difference in effectiveness between Placebo and Chlorpromazine
# while no difference between Placebo and other groups

# FDR methods

# the order for p-value of each group is
# chl.pv < p150.pv < dim.pv < p100.pv

# now find out i
p <- c(chl.pv, p150.pv, dim.pv, p100.pv)
l <- rep(0, 4)
t <- NULL
for(i in 1:4){
  l[i] <- i*0.05/4
  if(p[i] < l[i]){
    t <- c(t, p[i])
  }
}
T <- max(t)
T

# we have the same answer above
# there is significant difference in effectiveness between Placebo and Chlorpromazine
# while no difference between Placebo and other groups

