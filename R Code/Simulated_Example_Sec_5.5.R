# This is code to replicate the implementation in Sections 5.5
# of the main method presented in  my dissertation.
# Code developed by Corinna Cichy

# load required packages
library(sets)
library(openxlsx)
library(readxl)
library(caret)
library(MLmetrics)

# read input data
metrics <- read.xlsx(input_file)

# Define Membership functions. In this simulated example, the values are chosen
# based on potential expert knowledge in order to show the flexibility of the
# method.

## The following part refers to Section 5.5.1

# configure the options for the sets package.
sets_options("universe", seq(0, 100, 0.1)) 

# define individual membership functions

# Gaussian membership function with three sets for comp1, comp2, cons1 and cons2
comp1 <- fuzzy_variable(low =
                          fuzzy_normal(mean = 0, sd = 30),
                        medium =
                          fuzzy_normal(mean = 80, sd = 20),
                        high =
                          fuzzy_normal(mean = 100, sd = 10))
comp2 <- fuzzy_variable(low =
                          fuzzy_normal(mean = 0, sd = 40),
                        medium =
                          fuzzy_normal(mean = 75, sd = 15),
                        high =
                          fuzzy_normal(mean = 100, sd = 5))
cons1 <- fuzzy_variable(low =
                          fuzzy_normal(mean = 0, sd = 40),
                        medium =
                          fuzzy_normal(mean = 90, sd = 15),
                        high =
                          fuzzy_normal(mean = 100, sd = 5))

cons2 <- fuzzy_variable(low =
                          fuzzy_normal(mean = 0, sd = 30),
                        medium =
                          fuzzy_normal(mean = 80, sd = 20),
                        high =
                          fuzzy_normal(mean = 100, sd = 10))

# Gaussian membership function with two sets for curr1
curr1 <- fuzzy_variable(short =
                          fuzzy_normal_gset(mean = 0, sd = 175, 
                                            universe = seq(0, 500, 0.1)),
                        long =
                          fuzzy_normal_gset(mean = 500, sd = 175, 
                                            universe = seq(0, 500, 0.1)))

# trapezoidal membership function with two sets for access1
access1 <- fuzzy_variable(inconvenient = 
                            fuzzy_trapezoid(corners = c(-10, 0, 20, 40)),
                          convenient = 
                            fuzzy_trapezoid(corners = c(20, 40, 100, 110)))

# Gaussian membership function with three sets for quality
quality <- fuzzy_variable(poor =
                          fuzzy_normal_gset(mean = 1, sd = 1.5,
                                            universe = seq(1, 10, 0.1)),
                        normal = 
                          fuzzy_normal_gset(mean = 5.5, sd = 1.5,
                                            universe = seq(1, 10, 0.1)),
                        good =
                          fuzzy_normal_gset(mean = 10, sd = 1.5,
                                            universe = seq(1, 10, 0.1))) 

# create plot of membership functions (Fig. 5.3)
plot(comp1)
plot(cons1)
plot(access1)
plot(quality)

## The following part refers to Section 5.5.2

# define fuzzy rules. In this example, simple if-then statements and
# connections with AND / OR operators are used for fuzzy basis
r1 <- fuzzy_rule(comp1 %is% high, quality %is% good)
r2 <- fuzzy_rule(comp1 %is% medium, quality %is% normal)
r3 <- fuzzy_rule(comp1 %is% low, quality %is% poor)
r4 <- fuzzy_rule(comp2 %is% high, quality %is% good)
r5 <- fuzzy_rule(comp2 %is% medium, quality %is% normal)
r6 <- fuzzy_rule(comp2 %is% low, quality %is% poor)
r7 <- fuzzy_rule(cons1 %is% high, quality %is% good)
r8 <- fuzzy_rule(cons1 %is% medium, quality %is% normal)
r9 <- fuzzy_rule(cons1 %is% low, quality %is% poor)
r10 <- fuzzy_rule(cons2 %is% high, quality %is% good)
r11 <- fuzzy_rule(cons2 %is% medium, quality %is% normal)
r12 <- fuzzy_rule(cons2 %is% low, quality %is% poor)
r13 <- fuzzy_rule(curr1 %is% short, quality %is% good)
r14 <- fuzzy_rule(curr1 %is% long, quality %is% poor)
r15 <- fuzzy_rule(access1 %is% inconvenient, quality %is% poor)
r16 <- fuzzy_rule(access1 %is% convenient, quality %is% good)

r17 <- fuzzy_rule(comp1 %is% low && comp2 %is% high, quality %is% normal)
r18 <- fuzzy_rule(comp1 %is% medium && comp2 %is% medium, quality %is% normal)
r19 <- fuzzy_rule(comp1 %is% high && comp2 %is% low, quality %is% normal)
r20 <- fuzzy_rule(comp1 %is% high && comp2 %is% high, quality %is% good)
r21 <- fuzzy_rule(comp1 %is% low && comp2 %is% low, quality %is% poor)
r22 <- fuzzy_rule(comp1 %is% low && comp2 %is% medium, quality %is% poor)
r23 <- fuzzy_rule(comp1 %is% medium && comp2 %is% low, quality %is% poor)
r24 <- fuzzy_rule(comp1 %is% medium && comp2 %is% high, quality %is% good)
r25 <- fuzzy_rule(comp1 %is% high && comp2 %is% medium, quality %is% good)

r26 <- fuzzy_rule(cons1 %is% low && cons2 %is% high, quality %is% normal)#
r27 <- fuzzy_rule(cons1 %is% medium && cons2 %is% medium, quality %is% normal)#
r28 <- fuzzy_rule(cons1 %is% high && cons2 %is% low, quality %is% normal)#
r29 <- fuzzy_rule(cons1 %is% high && cons2 %is% high, quality %is% good)
r30 <- fuzzy_rule(cons1 %is% low && cons2 %is% low, quality %is% poor)#
r31 <- fuzzy_rule(cons1 %is% low && cons2 %is% medium, quality %is% poor)##
r32 <- fuzzy_rule(cons1 %is% medium && cons2 %is% low, quality %is% poor)##
r33 <- fuzzy_rule(cons1 %is% medium && cons2 %is% high, quality %is% good)##
r34 <- fuzzy_rule(cons1 %is% high && cons2 %is% medium, quality %is% good)##


r35 <- fuzzy_rule(curr1 %is% short && access1 %is% inconvenient, quality %is% normal)
r36 <- fuzzy_rule(curr1 %is% long && access1 %is% convenient, quality %is% normal)
r37 <- fuzzy_rule(curr1 %is% short && access1 %is% convenient, quality %is% good)
r38 <- fuzzy_rule(curr1 %is% long && access1 %is% inconvenient, quality %is% poor)


# define fuzzy systems to build the basis for subsequent approximation
g1 <- function(x) {
  bf1 <- fuzzy_system(set(comp1, quality), set(r1, r2, r3))
  fi <- fuzzy_inference(bf1, list(comp1 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g2 <- function(x) {
  bf1 <- fuzzy_system(set(comp2, quality), set(r4, r5, r6))
  fi <- fuzzy_inference(bf1, list(comp2 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g3 <- function(x) {
  bf1 <- fuzzy_system(set(cons1, quality), set(r7, r8, r9))
  fi <- fuzzy_inference(bf1, list(cons1 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g4 <- function(x) {
  bf1 <- fuzzy_system(set(cons2, quality), set(r10, r11, r12))
  fi <- fuzzy_inference(bf1, list(cons2 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g5 <- function(x) {
  bf1 <- fuzzy_system(set(curr1, quality), set(r13,r14))
  fi <- fuzzy_inference(bf1, list(curr1 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g6 <- function(x) {
  bf1 <- fuzzy_system(set(access1, quality), set(r15, r16))
  fi <- fuzzy_inference(bf1, list(access1 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g7 <- function(x1, x2) {
  bf1 <- fuzzy_system(set(comp1, comp2, quality), set(r17, r18, r19, r20, r21, r22,
                                                    r23, r24, r25))
  fi <- fuzzy_inference(bf1, list(comp1 = x1, comp2 = x2))
  return(gset_defuzzify(fi, "centroid"))  
}

g8 <- function(x1, x2) {
  bf1 <- fuzzy_system(set(cons1, cons2, quality), set(r26, r27, r28, r29, r30, r31,
                                                    r32, r33, r34))
  fi <- fuzzy_inference(bf1, list(cons1 = x1, cons2 = x2))
  return(gset_defuzzify(fi, "centroid"))  
}
g8a <- function(x1, x2) {
  bf1 <- fuzzy_system(set(cons1, cons2, quality), set(r26, r27, r28, r29, r30))
  fi <- fuzzy_inference(bf1, list(cons1 = x1, cons2 = x2))
  return(gset_defuzzify(fi, "centroid"))  
}

g9 <- function(x1, x2) {
  bf1 <- fuzzy_system(set(curr1, access1, quality), set(r35, r36, r37, r38))
  fi <- fuzzy_inference(bf1, list(curr1 = x1, access1 = x2))
  return(gset_defuzzify(fi, "centroid"))  
}

# apply to data
attach(metrics)
gf1 <- sapply(Comp1, b1)
gf2 <- sapply(Comp2, b2)
gf3 <- sapply(Cons1, b3)
gf4 <- sapply(Cons2, b4)
gf5 <- sapply(Curr1, b5)
gf6 <- sapply(Access1, b6)
gf7 <- mapply(b7, Comp1, Comp2)
gf8 <- mapply(b8, Cons1, Cons2)
gf8a <- mapply(b8a, Cons1, Cons2)
gf9 <- mapply(b9, Curr1, Access1)

# generate a algorithmic plot of one fuzzy system (Fig. 5.4a)
g = seq(0,100,0.1)
pts <- lapply(g, b1)
fspline <- splinefun(g, pts,
                     method = c("fmm"),
                     ties = mean)
plot(g, pts, type = "l", ylab = 'g1', xlab = "Comp1")

# fitting the model using least squares and backwards elimination
lm1 <- lm(quality ~ gf1 + gf2 + gf3 + gf4+ gf5 + gf6+ gf7+ gf8 + gf8a + gf9)
lm1AIC <- step(lm1)
lmfuzzy <- lm(quality ~ bf1 + bf2 + bf5 + bf6 + bf8)
summary(lmfuzzy)

## diagnostic plots and evaluation via training and test data split are 
# omitted in this simulated example but are provided for validation study
# (Ch. 6)



