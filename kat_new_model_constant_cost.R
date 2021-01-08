#### Model using analytical solutions for E

# Packages
library(tidyverse)
library(Matrix)
library(kableExtra)
library(ggplot2)
library(plotly)
library(lpSolve)
library(lpSolveAPI)

source("stock_dynamic.R")
source("harvest_species.R")
source("harvest_tech.R")
source("profit_tech.R")
source("pvnb.R")
source("Analytical_E.R")


# First, set seed to ensure we have the same result 
set.seed(666)


# set the number of species and technologies we want to study
species_num <- 2
tech_num <- 2


# set the number of years we want to model the stock dynamic 
year = 5


# set the discount rate
delta = 0.05
rho = 1/(1+delta)

# set default values for variables in the "Our Model" section
# set parameters
species <- c(1:species_num)
parameter <- data.frame(species)
parameter$r <- c(rep(0.5,species_num))
parameter$K <- c(rep(1.2,species_num))
parameter$Xo <- c(rep(1000,species_num))
# parameter$X1 <- c(1,rep(harvest_species(E_1),year-1))
# parameter$X2 <- c(1,rep(harvest_species(E_2),year-1))
parameter$delta <- c(rep(0.05,species_num))
parameter$rho <- 1/(1+parameter$delta)

# set catchabilities

q <- c(rep(0.05,tech_num*species_num))
q_matrix <- matrix(q, nrow = tech_num, ncol = species_num)
q_matrix <- data.frame(q_matrix)
names(q_matrix) <- c("species 1", "species 2")

# set prices

p <- c(rep(100,tech_num*species_num))
p_matrix <- matrix(p, nrow = tech_num, ncol = species_num)
p_matrix <- data.frame(p_matrix)
names(p_matrix) <- c("species 1", "species 2")

# set costs

c <- c(rep(10,tech_num*species_num))
c_matrix <- matrix(c, nrow = tech_num, ncol = species_num)
c_matrix <- data.frame(c_matrix)
names(c_matrix) <- c("species 1", "species 2")

# set a Quota

Q <- 500

# find effort at a single point in time for species-based effort (not tech-based)

E <- c(Opt_E1(Q, c = c_matrix$`species 2`[1], q1 = q_matrix$`species 1`[1], q2 = q_matrix$`species 2`[1], p1 = p_matrix$`species 1`[1], p2 = p_matrix$`species 2`[1], X1 = parameter$Xo, X2 = parameter$Xo), Opt_E2(Q, c = c_matrix$`species 1`[1], q1 = q_matrix$`species 1`[1], q2 = q_matrix$`species 2`[1], p1 = p_matrix$`species 1`[1], p2 = p_matrix$`species 2`[1], X1 = parameter$Xo, X2 = parameter$Xo))

# ^ note: if prices are the same for both species, get infinite value. But as soon as prices are not the same, then can get a numerical result.