library(readxl)
file_path <- "C:/Users/Kelly/Desktop/Stat Project/data.xlsx"
da <- read_excel(file_path, sheet = "combined data")

library(plm)
library(lmtest)
library(fUnitRoots)
install.packages("vars")
library(vars)
phom=pdata.frame(da,index=c("State","Year"))

# Define variables
y=phom$Homeless.Percentage
x1=phom$Inequality
x2=phom$Unemployment.rate
x3=phom$Inequa_Unempl
x4=phom$Gov.assist.per.capita
x5=phom$Housing.cost

data <- data.frame(phom$Homeless.Percentage, phom$Inequality, phom$Unemployment.rate,phom$Housing.cost,phom$Gov.assist.per.capita)
cor_matrix <- cor(data)
cor_matrix

#Panel unit root test
purtest(phom$Homeless.Percentage, data = phom,index = c("State", "Year"), pmax = 4, exo = "intercept", lags = "SIC",test = "madwu")
purtest(phom$Inequality, data = phom,index = c("State", "Year"), pmax = 4, exo = "intercept", lags = "SIC",test = "madwu")
purtest(phom$Unemployment.rate, data = phom,index = c("State", "Year"), pmax = 4, exo = "intercept", lags = "SIC",test = "madwu")
purtest(phom$Gov.assist.per.capita, data = phom,index = c("State", "Year"), pmax = 4, exo = "intercept",lags = "SIC", test = "madwu")
purtest(phom$Housing.cost, data = phom,index = c("State", "Year"), pmax = 4, exo = "intercept",lags = "SIC", test = "madwu")


#OLS on the pooled data
hom_pool = plm(y ~ x1+x2+x3+x4+x5, data = phom,
              model = "pooling")
summary(hom_pool)

#Robust (Clustered s.e.)
library(lmtest)
# Regression table with standard SE
ols=coeftest(hom_pool)
# Regression table with "clustered" SE (default type HC0):
robust=coeftest(hom_pool, vcovHC)
# Regression table with "clustered" SE (small-sample correction)
robust_sss=coeftest(hom_pool, vcovHC(hom_pool, type="sss"))
# Combine all in one table
library(stargazer)
stargazer(ols,robust,robust_sss, type="text", 
          column.labels=c("OLS","robust","robustsss"),keep.stat=c("n","rsq"))

#Robust standard errors increase 2 times (last column is for small sample adjustment)
#indicating within group correlation

#Fixed-effects
hom_fe = plm(y ~ x1+x2+x3+x4+x5, data = phom,index=c("State","Year"),
            model = "within")
summary(hom_fe)

#F test for common effects
pFtest(hom_fe, hom_pool)
#reject the null, implying there is substantial interfirm variation and pooled 
#model with common intercept is not appropriate.

# Random-effects
hom_re = plm(y ~ x1+x2+x3+x4+x5, data = phom,
            model = "random", random.method = "walhus")
summary(hom_re)

# Combine all in one table
library(stargazer)
stargazer(hom_pool,hom_fe,hom_re, type="text", 
          column.labels=c("OLS","FE","RE"),keep.stat=c("n","rsq"))


#Hausman Test
phtest(hom_re, hom_fe)

#panel VAR model
library(plm)
install.packages("pgmm")
library(pgmm)
install.packages("panelvar")
library(panelvar)

z1 <- pvargmm(dependent_vars = c("Homeless.Percentage","Inequality","Unemployment.rate","Inequa_Unempl","Gov.assist.per.capita","Housing.cost"), lags = 1,
              transformation = "fd",
              data = phom,
              panel_identifier=c("State", "Year"),
              steps = c("twostep"),
              system_instruments = FALSE,max_instr_dependent_vars = 99,
             max_instr_predet_vars = 99, min_instr_dependent_vars = 2L,min_instr_predet_vars = 1L,
             collapse = FALSE
             )
summary(z1)

