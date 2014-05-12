#------------------------ MEAN z-test
n = 100
x = 100

mu0 = 98
sigma = 10

alpha = 0.05
z = (x-mu0)/(sigma/sqrt(n))

#-- pvalue for Ha: mu != mu0
pvalue = 2*pnorm(z) 
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}

#-- pvalue for Ha: mu < mu0
pvalue = pnorm(z) 
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}

#-- pvalue for Ha: mu > mu0
pvalue = 1-pnorm(z) 
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}



#------------------------ MEAN t-test
mu0 = 0
n = 6
x = mean(c(-9.5,-10,-9.4,-8.3,-9,-9.9))
ss = sd(c(-9.5,-10,-9.4,-8.3,-9,-9.9))
alpha = 0.05
z = (x-mu0)/(ss/sqrt(n))

#-- pvalue for Ha: mu != mu0
pvalue = 2*pt(z, df = n-1)
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}

#-- pvalue for Ha: mu < mu0
pvalue = pt(z, df = n-1) 
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}

#-- pvalue for Ha: mu > mu0
pvalue = 1-pt(z, df = n-1) 
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}




#------------------------ PROPORTION z-test

n_samp_success <- 44
n_samp_obs <- 100
n_pop_success <- 50
n_pop_obs <- 100
alpha = 0.01

p_sample = (n_samp_success/n_samp_obs)
p_pop = (n_pop_success/n_pop_obs)


z = (p_sample-p_pop)/sqrt((p_pop*(1-p_pop))/n_samp_obs)

#-- pvalue for Ha: mu != mu0
pvalue = 2*pnorm(z) 
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}

#-- pvalue for Ha: mu < mu0
pvalue = pnorm(z) 
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}

#-- pvalue for Ha: mu > mu0
pvalue = 1-pnorm(z) 
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}
