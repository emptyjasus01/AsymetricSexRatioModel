# Bayesian Asymmetric Sex Ratio Model
This repository contains code and data to accompany the manuscript Bayesian inference of sex-specific mortality profiles and product yields from unsexed cattle zooarchaeological remains, demonstrating how to fit the Bayesian asymmetric sex ratio model described in Section “Bayesian model for inferring sexed age-at-death profiles”.

## Contents
•	Example.R – Demonstrates how to run JAGS code to fit the model, plot and summarise MCMC output, and compute productivity measures described in the manuscript.
•	modern_herds.csv – Data from modern herds used to estimate sex ratio over time, formatted for direct import into R and JAGS.
•	Example_Ancient_data.csv – Counts of dead animals per age class for each ancient herd (rows = herds, columns = age classes).
•	TableLiveWeightMilkYield.txt – Parameters (as used in the manuscript) for estimating productivity measures.
•	AsymmetricSexratioModel.txt – JAGS model used for fitting the analysis described in the manuscript.
## Notes
•	The JAGS model assumes that ancient counts are pre-assigned to age classes and does not perform automated assignment of uncertain counts to possible age classes.
•	The repository includes examples for MCMC diagnostics, plotting, and computation of productivity measures.
## Requirements
•	R (version >= 4.0 recommended)
•	rjags
•	coda
•	ggplot2 (optional for enhanced plotting)
## Install required packages in R using:
install.packages(c("rjags", "coda", "ggplot2"))
## Running the example
To run the example:
1.	Clone this repository:
2.	git clone https://github.com/yourusername/your-repo-name.git
3.	cd your-repo-name
4.	Open Example.R in R or RStudio.
5.	Run the script to fit the JAGS model, generate MCMC summaries, and compute productivity measures.
