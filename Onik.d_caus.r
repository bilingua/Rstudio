d_caus <-  read.csv('C:/Documents/Onik/R/dutch_causatives.txt')

# 3.1
# levels of this variable (i.e. Clause) the number of observations is less then 25 .
table(d_caus$CeSynt)
fisher.test(d_caus$Aux, d_caus$CeSynt)$p.value # significant

# 3.2
chisq.test(d_caus$Aux, d_caus$CrSem)$p.value # significant
chisq.test(d_caus$Aux, d_caus$CeSem)$p.value # non-significant
chisq.test(d_caus$Aux, d_caus$CdEvSem)$p.value # significant
chisq.test(d_caus$Aux, d_caus$EPTrans)$p.value # significant
chisq.test(d_caus$Aux, d_caus$Country)$p.value # significant
chisq.test(d_caus$Aux, d_caus$Domain)$p.value # significant

# 3.3
(chi_exp <- chisq.test(d_caus$Aux, d_caus$EPTrans)$expected)

# 3.4. Calculate the odds ratio.
(chi_exp[1][1]/chi_exp[3][1])/(chi_exp[3][1]/chi_exp[4][1])

# 3.5 Calculate effect size for this test using Cramer’s V (phi).
# library(cramer)
# cramer.test(d_caus$Aux, d_caus$EPTrans)

# 3.7 Visualize the distribution using mosaic plot.
library(vcd)
vcd::mosaic(~ Aux + CrSem + Country, data=d_caus, shade=TRUE, legend=TRUE)

# 3.8
# Perfoming a multiple chi-squared test does not make it nesserary to take into account the dependence between independent variables (in the formula).

