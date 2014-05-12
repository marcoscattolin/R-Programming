#------------------------ 2 means

m1_samp = mean(c(26.1,26.5,26.5,25.8,26.7,26.2))
m1_pop = 0
s1 = sd(c(26.1,26.5,26.5,25.8,26.7,26.2))
n1 = 6

m2_samp = mean(c(16.6,16.5,17.1,17.5,17.5,17.7,16.3))
m2_pop = 0
s2 = sd(c(16.6,16.5,17.1,17.5,17.5,17.7,16.3))
n2 = 6

t = ((m1_samp-m2_samp)-(m1_pop-m2_pop))/sqrt(((s1^2)/n1)+((s2^2)/n2))

dfreedom = min(n1-1,n2-1)

#-- pvalue for Ha: mu != mu0
pvalue = 2*pt(z, df = dfreedom)
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}

#-- pvalue for Ha: mu < mu0
pvalue = pt(z, df = dfreedom) 
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}

#-- pvalue for Ha: mu > mu0
pvalue = 1-pt(z, df = dfreedom) 
pvalue < alpha
if(!pvalue < alpha){print("fail to reject Null hp")}else{print("Reject Null hp")}


#------------------------ 2 props
p1_samp = 0.12
p1_pop = 10
n1 = 45

p2_samp = 0.14
p2_pop = 10
n2 = 75

alpha = 0.1

term1 = (p1_samp*(1-p1_samp))/n1
term2 = (p2_samp*(1-p2_samp))/n2
        
z = ((p1_samp-p2_samp)-(p1_pop-p2_pop)) / sqrt(term1+term2)

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









