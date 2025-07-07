#### Set the working directory to the folder containing the files, 
#### or adjust the file paths in the script to match their correct locations on your system.

rm(list = ls())
library(MCMCpack)
library(readr)
library("readxl")
library(MASS)
library(R2jags)
library(ggplot2)

####Upload anicent data
Counts<- as.matrix(read_csv("Example_Ancient_data.csv",col_names = FALSE))



##### Create age class for ancient data
low<-c(0,6,15,26,36,72,96)
up<-c(6,15,26,36,72,96,120)
oldAge =(low+up)*0.5 # middle point of age class for ancient herds
oldAge
N_old=length(oldAge) # #number of age classes for ancient herds
N_old

########Modern herds
modernherds<- read_csv("modern_herds.csv",col_names = TRUE)
View(modernherds)
names(modernherds)
dim(modernherds)

######## Variables in modern herds
## cows_alive = number of surviving animals     
## Sex = sex of surviving animals            
## midinterval= middle point  of age class for modern herds      
## SubjectAtRisk = number of subjects at risk in each age group  
## herdID = herd ID 

##### Other Input 
#input N_Age#input data --- 
N_tot <- nrow(modernherds) #total number of observations in modern herds
N_S <- length(unique(modernherds$herdID)) ### number of modern herds
K <- nrow(Counts)  #number of ancient herds
total_count <- rowSums(Counts)  #number of animals in each ancient herd


####  variables to check the fit for modern data
agepred=modernherds$midinterval[1:(N_tot/2)]
predstudy=modernherds$herdID[1:(N_tot/2)]

### JAGS Model file 

filein<-"AsymmetricSexratioModel.txt"
### Initial values for the model
inits<-NULL
###output variables
params<-c("beta","beta_I","beta_S","ratio_r","omega.lambda", "rbar", "theta","gamma","fit_M","fit_F","f_old","m_old","theta_pred","f_pred","m_pred")

##### OUTPUT FOR MODERN HERDS --- Poisson regression
## beta = regression coefficient for time  
## beta_I = regression coefficient for sex-time iteraction
## beta_S= regression coefficient for sex
## ratio_r =estimate of sex ratio for modern herds
## omega.lambda= standard deviation of random effet for modern herds
## fit_F = fitted values for female animals 
## fit_M = fitted values for male animals

##### OUTPUT FOR Ancient HERDS --- Dirichlet Multinomila Model
## rbar = inferred sex ratio for ancient cows at mid point of age class
## theta = parameters of multinomial, probabilities of dieing in each age class for each ancient herd
## gamma = hyperparameter for the Dirichlet prior on theta
## f_old = proportion of females surviving per age class in each ancient herd
## m_old = proportion of males surviving per age class in each ancient herd

## theta_pred = probability of dying n each age class for a hypothetical ancient herd
## f_pred = proportion of females surviving per age class in a hypothetical ancient herd
## m_pred = proportion of males surviving per age class in a hypothetical ancient herd


# Prepare data for JAGS

data_modern <- list(
  ancient_herds = Counts,
  total_count = total_count,
  K = K,
  N_tot = N_tot,
  N_S = N_S,
  N_Age = modernherds$SubjectAtRisk,
  cows = modernherds$cows_alive,
  Sex = modernherds$Sex,
  midAge = modernherds$midinterval,
  subject = modernherds$herdID,
  N_old = N_old,
  oldAge = oldAge,
  N_fit = length(agepred),
  agepred = agepred,
  predstudy = predstudy
)


## Run the model
model <- jags(
  data = data_modern,
  inits = NULL,
  parameters.to.save = params,
  model.file = filein,
  n.chains = 2,
  n.iter = 50000,
  n.burnin = 20000,
  n.thin = 10,
  DIC = TRUE
)




# Extract posterior samples for beta, gamma and theta
attach.jags(model)

# Plot trace plots for beta, beta_I, and beta_S 
par(mfrow = c(1, 1))
plot(1:length(beta), beta, type = "l", main = "Trace plot for beta", ylab = "beta", xlab = "Iteration")
plot(1:length(beta), beta_I, type = "l", main = "Trace plot for beta_I", ylab = "beta_I", xlab = "Iteration")
plot(1:length(beta), beta_S, type = "l", main = "Trace plot for beta_S", ylab = "beta_S", xlab = "Iteration")

# Plot trace plots for some theta and gamma 
par(mfrow = c(1, 1))
plot(1:length(theta[, 1, 1]), theta[, 1, 1], type = "l", main = "Trace plot for theta - Herd", ylab = "theta", xlab = "Iteration")

# Plot trace plots for gamma
par(mfrow = c(1, 1))  
plot(1:length(gamma[, 1]), gamma[, 1], type = "l", main = "Trace plot for gamma ", ylab = "theta", xlab = "Iteration")

# Plot posterior mean and 95% credible intervals for theta (probability of death per age class per ancient herd)
theta_mean <- apply(theta, c(2, 3), mean)
theta_lower <- apply(theta, c(2, 3), function(x) quantile(x, 0.025))
theta_upper <- apply(theta, c(2, 3), function(x) quantile(x, 0.975))


# Show each plot and wait for user input to continue

for (i in seq_len(K)) {
  df_theta <- data.frame(
    oldAge = oldAge,
    mean = theta_mean[i, ],
    lower = theta_lower[i, ],
    upper = theta_upper[i, ]
  )
  #windows()
  print(
    ggplot(df_theta, aes(x = oldAge, y = mean)) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 2) +
      labs(
        x = "Age (months)",
        y = "Probability of Death (theta)",
        title = paste("Posterior Mean and 95% CI for theta - Herd", i)
      ) +
      theme_minimal()
  )
  if (interactive()) {
    # Wait for user input before moving to next plot
    readline(prompt = "Press [enter] to see the next plot, or type 'q' to quit: ")
    # The plot will appear in the RStudio Plots pane or in a new window if using R GUI.
    # If you are using RStudio, make sure the Plots pane is visible.
  }
}

# Disable interactive plotting globally
options(device.ask.default = FALSE)
devAskNewPage(FALSE)


# Extract posterior means and sds
rbar_est <- colMeans(rbar)
rbar_sd <- apply(rbar, 2, sd)

# Plot sex ratio for archaeological data 

rbar_mean <- apply(rbar, 2, mean)
rbar_lower <- apply(rbar, 2, quantile, 0.025)
rbar_upper <- apply(rbar, 2, quantile, 0.975)
rbar_df <- data.frame(oldAge = oldAge, mean = rbar_mean, lower = rbar_lower, upper = rbar_upper)

ggplot(rbar_df, aes(x = oldAge, y = mean)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 2) +
  labs(x = "Age (months)", y = "Sex Ratio (rbar)", title = "Posterior Mean and 95% Credible Interval for rbar") +
  theme_minimal()


# Plot fit for each ancient herd using a loop
plot(oldAge, theta_mean[1,], pch = 3, ylim = c(0, 0.5))
points(oldAge, Counts[1,] / sum(Counts[1,]), col = 2)

for (i in 2:K) {
  points(oldAge, theta_mean[i,], pch = 3, ylim = c(0, 0.5), col=i)
  points(oldAge, Counts[i,] / sum(Counts[i,]), col = i)
}

# Posterior inference on survival rates for females and males for ancient herds
f_old_est <- apply(f_old, c(2,3), mean)
m_old_est <- apply(m_old, c(2,3), mean)
matplot(oldAge[1:(N_old-1)], t(f_old_est), type = "l", lty = 1, col = rainbow(K), ylab = "Female survival", xlab = "Age")
matplot(oldAge[1:(N_old-1)], t(m_old_est), type = "l", lty = 1, col = rainbow(K), ylab = "Male survival", xlab = "Age")




# Plot with confidence intervals using ggplot2 for a hypothetical herd  

theta_est <- colMeans(theta_pred)
lower <- apply(theta_pred, 2, quantile, 0.025)
upper <- apply(theta_pred, 2, quantile, 0.975)
td <- data.frame(x = oldAge, y = theta_est, lower = lower, upper = upper)
ggplot(td, aes(x = x, y = y)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "age class", y = "prob of death", title = "Plot with Confidence Intervals") +
  theme_minimal()

# Prediction of male/female survival proportions
f_mean <- colMeans(f_pred)
m_mean <- colMeans(m_pred)
plot(oldAge[1:6], f_mean, col = "magenta", ylim = c(0, max(c(f_mean, m_mean))))
points(oldAge[1:6], m_mean, col = "blue")




#################################################################
### Productivity measures for ancient herds

#### Data structure for productivity measures used in the paper



# birthrate per month
birthRate = 1./18.36459152 #37/750
# month of first birth
ageFirstBirth = 42 # really is 42.19


# Legge (1991) - Age classes for ancient herds and lengths 
nAC_legge = 7
ls_legge = c(0, 6, 15, 26, 36, 72, 96) 
us_legge = c(6, 15, 26, 36, 72, 96, 192)
AC_lengths_legge = us_legge - ls_legge


yields = read.csv(file = 'TableLiveWeightMilkYield.txt', header = FALSE, sep = '\t',
                  col.names = c('age_month', 'weight_kg_per_animal', 'milk_consumed_kg_per_month',
                                'milk_yield_kg_per_month'))





### Productivity measures for ancient herds
## Compute yields   returns a list with the posterior distribution of all productivity measures:
## reproductive output, mow yield , milk yield,  calories per hay, 
## total calories, hay consumed, milk consumed, milk calories, 
## mow calories ,  total fat, milk calories per hay, 
## mow calories per hay , total protein 

## Input:
## nAC = number of age classes    
## ls = lower bounds of age classes
## AC_lengths = lengths of age classes
## males = matrix W x (nAC-1) with male survival proportions
## females = matrix W x (nAC-1) with female survival proportions
## yields_ = yields data frame with milk and mow yields
## birthRate_ = birth rate per month
## ageFirstBirth_ = month of first birth


compute_yields <- function(nAC, ls, AC_lengths, m_output, f_output,
     yields_ = yields, birthRate_ = birthRate, ageFirstBirth_ = ageFirstBirth) {
  # If m_output/f_output are vectors, convert to matrix with 1 row
  if (is.vector(m_output)) males <- matrix(m_output, nrow = 1)
  if (is.vector(f_output)) females <- matrix(f_output, nrow = 1)
  
  fs = cbind(0.5, f_output, 0)
  ms = cbind(0.5, m_output, 0)
  
  females = sapply(2:ncol(fs), function(x) fs[, x-1] - fs[, x])
  males = sapply(2:ncol(ms), function(x) ms[, x-1] - ms[, x])
  
  n_sim <- nrow(males)
  total_months <- sum(AC_lengths)
  # Expand age class info to per-month (outside loop)
  age_months <- integer(total_months)
  class_idx <- integer(total_months)
  month <- 1
  for (i in seq_len(nAC)) {
  for (j in seq_len(AC_lengths[i])) {
    age_months[month] <- ls[i] + (j - 1)
    class_idx[month] <- i
    month <- month + 1
  }
  }
  # Precompute yield columns (outside loop)
  weight_kg_per_animal <- yields_$weight_kg_per_animal
  milk_yield_kg_per_month <- yields_$milk_yield_kg_per_month
  milk_consumed_kg_per_month <- yields_$milk_consumed_kg_per_month

  # Preallocate output vectors
  out <- list(
  reproductive_output = numeric(n_sim),
  mow_yield = numeric(n_sim),
  milk_yield = numeric(n_sim),
  calories_per_hay = numeric(n_sim),
  total_calories = numeric(n_sim),
  hay_consumed = numeric(n_sim),
  milk_consumed = numeric(n_sim),
  milk_calories = numeric(n_sim),
  mow_calories = numeric(n_sim),
  total_fat = numeric(n_sim),
  milk_calories_per_hay = numeric(n_sim),
  mow_calories_per_hay = numeric(n_sim),
  total_protein = numeric(n_sim)
  )

  for (sim in seq_len(n_sim)) {
  males_row <- males[sim, ]
  females_row <- females[sim, ]
  surviving_females <- numeric(total_months + 1)
  surviving_males <- numeric(total_months + 1)
  surviving_females[1] <- 0.5
  surviving_males[1] <- 0.5
  males_row <- c(males_row, 0)
  females_row <- c(females_row, 0)
  reproductive_output <- numeric(total_months)
  milk_yield_vec <- numeric(total_months)
  milk_consumed_vec <- numeric(total_months)
  milk_calories_vec <- numeric(total_months)
  mow_yield_vec <- numeric(total_months)
  mow_calories_vec <- numeric(total_months)
  total_protein_vec <- numeric(total_months)
  total_calories_vec <- numeric(total_months)
  total_fat_vec <- numeric(total_months)
  hay_consumed_vec <- numeric(total_months)
  for (m in seq_len(total_months)) {
    i <- class_idx[m]
    surviving_females[m + 1] <- surviving_females[m] - (females_row[i] / AC_lengths[i])
    surviving_males[m + 1] <- surviving_males[m] - (males_row[i] / AC_lengths[i])
    cum_surviving_females <- surviving_females[m + 1]
    cum_surviving_males <- surviving_males[m + 1]
    cum_surviving <- cum_surviving_females + cum_surviving_males
    if (age_months[m] > ageFirstBirth_) {
    reproductive_output[m] <- birthRate_ * cum_surviving_females
    }
    milk_yield_vec[m] <- milk_yield_kg_per_month[m] * cum_surviving_females
    milk_consumed_vec[m] <- milk_consumed_kg_per_month[m] * cum_surviving
    milk_calories_vec[m] <- (milk_yield_vec[m] - milk_consumed_vec[m]) * 670
    mow_yield_vec[m] <- (weight_kg_per_animal[m] * 0.4995) * ((males_row[i] + females_row[i]) / AC_lengths[i])
    mow_calories_vec[m] <- mow_yield_vec[m] * 1980
    total_protein_vec[m] <- ((milk_yield_vec[m] - milk_consumed_vec[m]) * 0.0333) + (mow_yield_vec[m] * 0.1942)
    total_fat_vec[m] <- ((milk_yield_vec[m] - milk_consumed_vec[m]) * 0.0375) + (mow_yield_vec[m] * 0.1273)
    hay_consumed_vec[m] <- (weight_kg_per_animal[m] * 0.75331575) * cum_surviving
  }
  total_reproductive_output <- sum(reproductive_output)
  total_mow_yield <- sum(mow_yield_vec)
  total_milk_yield <- sum(milk_yield_vec)
  total_calories <- sum(milk_calories_vec + mow_calories_vec)
  total_hay_consumed <- sum(hay_consumed_vec)
  total_milk_consumed <- sum(milk_consumed_vec)
  total_milk_calories <- sum(milk_calories_vec)
  total_mow_calories <- sum(mow_calories_vec)
  total_protein <- sum(total_protein_vec)
  total_fat <- sum(total_fat_vec)
  out$reproductive_output[sim] <- total_reproductive_output
  out$mow_yield[sim] <- total_mow_yield
  out$milk_yield[sim] <- total_milk_yield
  out$calories_per_hay[sim] <- total_calories / total_hay_consumed
  out$total_calories[sim] <- total_calories
  out$hay_consumed[sim] <- total_hay_consumed
  out$milk_consumed[sim] <- total_milk_consumed
  out$milk_calories[sim] <- total_milk_calories
  out$mow_calories[sim] <- total_mow_calories
  out$total_fat[sim] <- total_fat
  out$milk_calories_per_hay[sim] <- total_milk_calories / total_hay_consumed
  out$mow_calories_per_hay[sim] <- total_mow_calories / total_hay_consumed
  out$total_protein[sim] <- total_protein
  }
  return(out)
}

output<- compute_yields(nAC_legge, ls_legge, AC_lengths_legge, m_pred, f_pred)
names(output) 

# Plot posterior distribution of reproductive output
reproductive_output <- output$reproductive_output
hist(reproductive_output, breaks = 30, probability = TRUE,
  main = "Posterior Distribution of Reproductive Output",
  xlab = "Reproductive Output", col = "lightblue", border = "white")
lines(density(reproductive_output), col = "red", lwd = 2)

# Plot posterior distribution of mow yield
mow_yield <- output$mow_yield 
hist(mow_yield, breaks = 30, probability = TRUE,
  main = "Posterior Distribution of Mow Yield",
  xlab = "Mow Yield", col = "lightgreen", border = "white")   
lines(density(mow_yield), col = "blue", lwd = 2)

# Plot joiny posterior distribution of milk yield & mow yield

milk_yield <- output$milk_yield
mow_yield <- output$mow_yield

# 2D kernel density estimate
dens2d <- kde2d(milk_yield, mow_yield, n = 100)

# Plot the joint posterior density
filled.contour(
  dens2d,
  color.palette = terrain.colors,
  xlab = "Milk Yield",
  ylab = "Mow Yield",
  main = "Joint Posterior Density of Milk Yield and Mow Yield"
)

# Plot posterior distribution of calories per hay and total calories using a 2 dimesnional kernel density estimate istogram
# 2D histogram (hexbin) of calories per hay vs total calories
calories_per_hay <- output$calories_per_hay
total_calories <- output$total_calories

library(plotly)


# Create 2D histogram bins
xbins <- cut(calories_per_hay, breaks = 20)
ybins <- cut(total_calories, breaks = 20)
counts <- table(xbins, ybins)
z <- as.matrix(counts)

# Get bin midpoints for x and y
x_mid <- (head(attr(xbins, "breaks"), -1) + tail(attr(xbins, "breaks"), -1)) / 2
y_mid <- (head(attr(ybins, "breaks"), -1) + tail(attr(ybins, "breaks"), -1)) / 2

# Surface plot
plot_ly(
  x = x_mid,
  y = y_mid,
  z = z,
  type = "surface"
) %>%
  layout(
    title = "Surface Plot of Binned Counts",
    scene = list(
      xaxis = list(title = "calories per hay"),
      yaxis = list(title = "total calories"),
      zaxis = list(title = "Count")
    )
  )


