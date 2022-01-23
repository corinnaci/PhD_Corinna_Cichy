# This is code to replicate the implementation in Section 6.5
# of the main method presented in  my dissertation.
# Code developed by Corinna Cichy

# load required packages
library(readxl)
library(ppclust)
library(sets)

# read input data
DQ_data <- read.xlsx(input_file)

# exploratory data analysis



# construct fuzzy sets
# conduct fuzzy c-means clustering with c=3 for each feature to obtain points at
# which the membership function is defined to be equal to one. 
cl_comp1 <- fcm(DQ_data$Comp1, centers=3)
cl_comp2 <- fcm(DQ_data$Comp2, centers=3)
cl_inte1 <- fcm(DQ_data$Inte1, centers=3)
cl_inte2 <- fcm(DQ_data$Inte2, centers=3)
cl_accur1 <- fcm(DQ_data$Accur1, centers=3)
cl_accur2 <- fcm(DQ_data$Accur2, centers=3)
cl_vali1 <- fcm(DQ_data$Vali1, centers=3)
cl_vali2 <- fcm(DQ_data$Vali2, centers=3)
cl_access1 <- fcm(DQ_data$Access1, centers=3)
cl_quality <-fcm(DQ_data$Quality, centers = 3)
# details of resulting clusters for each variable can be viewed by using the 
# summary() command.

# configure the options for the sets package.
sets_options("universe", seq(0, 100, 0.01))

# define membership functions using the previously obtained cluster centers
# and the systematic procedure defined in Section 5.5.1
# cluster centers are directly inserted as points in function definition

vali1 <- 	fuzzy_variable(
  low = 
    fuzzy_trapezoid(corners = c(-10, 0, 99.05, 99.62)), 
  medium = 
    fuzzy_triangular(corners = c(99.05, 99.62, 99.99)),
  high = 
    fuzzy_trapezoid(corners = c(99.62, 99.99, 100, 110)))

vali2 <- 	fuzzy_variable(
  low = 
    fuzzy_trapezoid(corners = c(-10, 0, 93.50, 99.60)),
  medium = 
    fuzzy_triangular(corners = c(93.50, 99.60, 100)),
  high = 
    fuzzy_triangular(corners = c(99.60, 100, 110))) # triangular form, since
# highest cluster center coincides with maximum point of scale

accur2 <- 	fuzzy_variable(
  low = 
    fuzzy_trapezoid(corners = c(-10, 0, 99.2, 99.75)),
  medium = 
    fuzzy_triangular(corners = c(99.2, 99.75,100)),
  high = 
    fuzzy_triangular(corners = c(99.75, 100, 110)))

inte1 <- 	fuzzy_variable(
  low = 
    fuzzy_trapezoid(corners = c(-10, 0, 63.72, 95.04)),
  medium = 
    fuzzy_triangular(corners = c(63.72, 95.04 , 99.24)),
  high = 
    fuzzy_trapezoid(corners = c(95.04, 99.24, 100, 110)))

inte2 <- 	fuzzy_variable(
  low = 
    fuzzy_trapezoid(corners = c(-10, 0, 54.52, 98.92)),
  medium = 
    fuzzy_triangular(corners = c(54.52, 98.92, 99.97)),
  high = 
    fuzzy_trapezoid(corners = c(98.92, 99.97, 100, 110)))

comp1 <- fuzzy_variable(
  low = 
    fuzzy_trapezoid(corners = c(-10, 0, 58.74, 75.98)),
  medium = 
    fuzzy_triangular(corners = c(58.74, 75.98, 99.71)),
  high = 
    fuzzy_trapezoid(corners = c(75.98, 99.71, 100, 110)))

comp2 <- fuzzy_variable(
  low = 
    fuzzy_trapezoid(corners = c(-10, 0, 3.11, 64.31)),
  medium = 
    fuzzy_triangular(corners = c(3.11, 64.31, 99.75)),
  high = 
    fuzzy_trapezoid(corners = c(64.31, 99.75, 100, 110)))

# customize options for variables access1 and quality
sets_options("universe", seq(0, 10, 0.01)) 

access1 <- fuzzy_variable(
  low = 
    fuzzy_trapezoid(corners = c(-10, 0, 0.69, 3.08)),
  medium = 
    fuzzy_triangular(corners = c(0.69,3.08, 6.24)),
  high =
    fuzzy_trapezoid(corners = c(3.08, 6.24, 10, 100)))

quality <- fuzzy_variable(
  poor = 
    fuzzy_trapezoid(corners = c(-10, 0, 2.76, 8.40)),
  normal = 
    fuzzy_triangular(corners = c(2.76, 8.4, 9.61)),
  good = 
    fuzzy_trapezoid(corners = c(8.4, 9.61, 10, 100)))


# define fuzzy rules
r1 <- fuzzy_rule(inte1 %is% high, quality %is% good)
r2 <- fuzzy_rule(inte1 %is% low, quality %is% poor)
r3 <- fuzzy_rule(inte1 %is% medium, quality %is% normal)

r4 <- fuzzy_rule(inte2 %is% high, quality %is% good)
r5 <- fuzzy_rule(inte2 %is% low, quality %is% poor)
r6 <- fuzzy_rule(inte2 %is% medium, quality %is% normal)

r7 <- fuzzy_rule(vali1 %is% high, quality %is% good)
r8 <- fuzzy_rule(vali1 %is% low, quality %is% poor)
r9 <- fuzzy_rule(vali1 %is% medium, quality %is% normal)

r10 <- fuzzy_rule(comp1 %is% high, quality %is% good)
r11 <- fuzzy_rule(comp1 %is% low, quality %is% poor)
r12 <- fuzzy_rule(comp1 %is% medium, quality %is% normal)

r13 <- fuzzy_rule(comp2 %is% high, quality %is% good)
r14 <- fuzzy_rule(comp2 %is% low, quality %is% poor)
r15 <- fuzzy_rule(comp2 %is% medium, quality %is% normal)

r16 <- fuzzy_rule(vali2 %is% high, quality %is% good)
r17 <- fuzzy_rule(vali2 %is% low, quality %is% poor)
r18 <- fuzzy_rule(vali2 %is% medium, quality %is% normal)

r19 <- fuzzy_rule(accur2 %is% high, quality %is% good)
r20 <- fuzzy_rule(accur2 %is% low, quality %is% poor)
r21 <- fuzzy_rule(accur2 %is% medium, quality %is% normal)

r22 <- fuzzy_rule(access1 %is% low, quality %is% poor)
r23 <- fuzzy_rule(access1 %is% medium, quality %is% normal)
r24 <- fuzzy_rule(access1 %is% high, quality %is% good)

# define fuzzy systems
g1 <- function(x) {
  bf1 <- fuzzy_system(set(inte1, quality), set(r1, r2, r3))
  fi <- fuzzy_inference(bf1, list(inte1 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g2 <- function(x) {
  bf1 <- fuzzy_system(set(inte2, quality), set(r4, r5, r6))
  fi <- fuzzy_inference(bf1, list(inte2 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g3 <- function(x) {
  bf1 <- fuzzy_system(set(vali1, quality), set(r7, r8, r9))
  fi <- fuzzy_inference(bf1, list(vali1 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g4 <- function(x) {
  bf1 <- fuzzy_system(set(comp1, quality), set(r10, r11, r12))
  fi <- fuzzy_inference(bf1, list(comp1 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g5 <- function(x) {
  bf1 <- fuzzy_system(set(comp2, quality), set(r13, r14, r15))
  fi <- fuzzy_inference(bf1, list(comp2 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g6 <- function(x) {
  bf1 <- fuzzy_system(set(vali2, quality), set(r16, r17, r18))
  fi <- fuzzy_inference(bf1, list(vali2 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g7 <- function(x) {
  bf1 <- fuzzy_system(set(accur2, quality), set(r19, r20, r21))
  fi <- fuzzy_inference(bf1, list(accur2 = x))
  return(gset_defuzzify(fi, "centroid"))  
}

g8 <- function(x) {
  bf1 <- fuzzy_system(set(access1, quality), set(r22, r23, r24))
  fi <- fuzzy_inference(bf1, list(access1 = x))
  return(gset_defuzzify(fi, "centroid"))  
}


# apply to data to obtain fuzzy basis/ transformed data set with precision 
# up to 2dp
gf1 <- sapply(round(Inte1, 2), g1) 
gf2 <- sapply(round(Inte2, 2), g2)
gf3 <- sapply(round(Vali1, 2), g3)
gf4 <- sapply(round(Comp1, 2), g4)
gf5 <- sapply(round(Comp2, 2), g5)
gf6 <- sapply(round(Vali2, 2), g6)
gf7 <- sapply(round(Accur2, 2), g7)
gf8 <- sapply(round(Access1, 2), g8)

# least squares estimation using fuzzy basis
lmDQ <- lm(Quality ~ gf1 + gf2 + gf3 + gf4 +gf5 + gf6 + gf7+ gf8) #start with full model
lmDQ_AIC <- step(lmDQ) #Akaike criterion for model selection, backwards elimination

# obtain model parameters
summary(lmDQ_AIC)


# model diagnostics
