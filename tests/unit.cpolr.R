library(anchors)

cat("UNIT TESTS FOR CPOLR\n\n")
## a little helper function for demo

cat("Define C\n")
data(poleff)
vign <- anchors(xsayself ~ xsay1 + xsay2 + xsay3 + xsay4 + xsay5, 
                data = poleff, method="C")
#                options=anchors.options(debug=1))
summary(vign)

cat("First cpolr\n")
poleff <- insert(poleff, vign)
out <- cpolr(cbind(Cs, Ce) ~ as.factor(china) + age + male + educyrs, 
            data = poleff)
summary(out)



##
cat("Checking cpolr against ordinal probit (polr) for scalar responses\n")
data(freedom)
C <- anchors(self ~ vign1 + vign2 + vign3 + vign4 + vign5 + vign6,
             data = freedom, method="C")
freedom <- insert(freedom, C)

check2 <- polr(as.factor(Cs) ~ as.factor(country) + sex + age + educ,
               data = freedom, method = "probit")
test2 <- cpolr(cbind(Cs, Cs) ~ as.factor(country) + sex + age + educ,
              data = freedom)

cat("Comparing point estimates for the polr estimates (first column)
and the cpolr estimates (second column) for the coefficients\n")
cbind(check2$coef, test2$coef)
cat("Comparing point estimates for the polr estimates (first column)
and the cpolr estimates (second column) for the cut points\n")
cbind(check2$zeta, test2$zeta)

cat("These equalities MUST be true\n")
allequal.test(check2$coef, test2$coef)
allequal.test(check2$zeta, test2$zeta)


##
cat("Create a dataset with holes in C\n")
idx <- poleff$Cs == poleff$Ce
poleff2 <- poleff[idx,]
nrow(poleff)
nrow(poleff2)

cat("Compare with polr using data with holes in C\n")
(zc <- summary(cpolr(cbind(Cs, Cs) ~ as.factor(china) + age + male + educyrs, 
            data = poleff2, Hess=TRUE)))

(zp <- summary(polr( as.factor(Cs) ~ as.factor(china) + age + male + educyrs, 
            data = poleff2, method="probit", Hess=TRUE)))

cat("This equality MUST be true\n")
allequal.test(zc$coef, zp$coef)

##
cat("Compare with polr using data with holes in C -- one covariate\n")
(zp <- summary(polr( as.factor(Cs) ~ male, data = poleff2, method="probit", Hess=TRUE)))
(zc <- summary(cpolr( cbind(Cs, Cs) ~ male, data = poleff2, Hess=TRUE)))
cat("This equality MUST be true\n")
allequal.test(zc$coef, zp$coef)

cat("Compare with polr using data with holes in C -- no covariate\n")
(zp <- summary(polr( as.factor(Cs) ~ 1, data = poleff2, method="probit", Hess=TRUE)))
(zc <- summary(cpolr( cbind(Cs, Cs) ~ 1, data = poleff2, Hess=TRUE)))
cat("This equality MUST be true\n")
allequal.test(zc$coef, zp$coef)

## fitted values
data(table1src)
data(table1)

## replicate table1
test1 <- anchors( y ~ z1 + z2 , data=table1src, method="C")
summary(test1)
table1src2 <- insert(table1src, test1)
print(table1src2)
      
z <- cpolr( cbind(Cs, Cs) ~ 1, data = table1src2, Hess=TRUE)
print(fitted(z, average=FALSE))
print(fitted(z, average=TRUE))
print(fitted(z, test1, average=FALSE))
print(fitted(z, test1, average=TRUE))
print(fitted(z, test1, average=TRUE, unconditional=TRUE))

p <- fitted(z, average=TRUE)
print(p[2:4]/sum(p[2:4]))


