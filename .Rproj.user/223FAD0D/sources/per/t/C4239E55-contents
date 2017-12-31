# Stefano Olgiati, PhD, MS
# Department of Quantitative Methods, via dei Caniana 2, Bergamo (ITA) 24129; tel  +39 
# 035 20 52 638; fax +39 035 20 52 549; email <stefano.olgiati@unibg.it>

# Based on:
# Kruschke, J. K. (2014). Jags-Ymet-Xmet-Mrobust.R 

# Accompanies the book:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: 
# A Tutorial with R, JAGS, and Stan 2nd Edition. Academic Press / Elsevier.
# Web Open Access: https://sites.google.com/site/doingbayesiandataanalysis/software-installation


library(rjags)

#===============================================================================
source("Jags.R")

# FUNCTION 1/3: DATA, MODEL, INITIALIZE AND RUN THE CHAINS
codaSamples <- genMCMC(data)

# FUNCTION 2/3: SUMMARY INFO 
smryMCMC(codaSamples)

# FUNCTION 3/3: PLOT
plotMCMC(codaSamples , data) 
#===============================================================================

