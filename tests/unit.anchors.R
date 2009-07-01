#########################################################################
## Jonathan Wand <wand(at)stanford.edu>
##
## UNIT TESTS for
## - anchors() 
## - summary.anchors()
## - print.summary.anchors() 
#########################################################################
library(anchors)
cat("TEST1: Checking anchors against the 13 examples in Table 1 of King and Wand (2007)\n")
data(table1)
dta <- data.frame(y  = c(1,3,3,4,4,2,2,3,1,2,3,3,4),
                  z1 = c(2,3,2,2,2,3,2,2,3,3,4,3,3),
                  z2 = c(3,4,4,4,3,3,2,2,2,2,2,2,2))
test1 <- anchors(y ~ z1 + z2, data = dta, method="C")
summary(test1)

cat("In the following two matricies, the true value is in the first column
and the estimated value in the second column; both columns should be
identical.\n")
cbind(table1$Cs, test1$rank$span[,"Cs"])
cbind(table1$Ce, test1$rank$span[,"Ce"])
cat("These equalities MUST be true\n")

allequal.test(table1$Cs, test1$rank$span[,"Cs"])
allequal.test(table1$Ce, test1$rank$span[,"Ce"])

cat("TEST2: Repeat but with missing values\n")
data(table1)
dta <- data.frame(y  = c(1,NA,3 ,4 ,4,2,0,3,1,0,3,3,4), 
                  z1 = c(2,3 ,NA,2 ,2,0,2,2,3,3,0,3,3),
                  z2 = c(3,4 ,4 ,NA,0,3,2,2,2,2,2,0,2))
test1 <- anchors(y ~ z1 + z2, data = dta, method="C")
print(test1)
summary(test1)

cat("These equalities MUST be false incorrect length error:\n")
allequal.test(table1$Cs, test1$rank$span[,"Cs"], FALSE) 
cat("BUT These equalities MUST be true\n")
allequal.test(table1$Cs[c(1,8,9,13)], test1$rank$span[,"Cs"])
allequal.test(table1$Ce[c(1,8,9,13)], test1$rank$span[,"Ce"])
# ## using internal values returned by anchors()
# allequal.test(table1$Cs[-test1$omit.row], test1$rank$span[,"Cs"])
# allequal.test(table1$Ce[-test1$omit.row], test1$rank$span[,"Ce"])
# ## using internal values returned by anchors()
# allequal.test(table1$Cs[!test1$omit.logical], test1$rank$span[,"Cs"])
# allequal.test(table1$Ce[!test1$omit.logical], test1$rank$span[,"Ce"])


cat("TEST3: Repeat but with missing values\n")
data(table1)
dta <- data.frame(y  = c(1,3,3,NA,4,2,2,3,1,2,3,3,4),
                  z1 = c(2,3,2,2 ,2,3,2,2,3,3,4,3,3),
                  z2 = c(3,4,4,4 ,3,3,2,0,2,2,2,2,2))
test1 <- anchors(y ~ z1 + z2, data = dta, method="C")
print(test1)
summary(test1)

cat("These equalities MUST be false incorrect length error:\n")
allequal.test(table1$Cs, test1$rank$span[,"Cs"],FALSE)
cat("BUT These equalities MUST be true\n")
allequal.test(table1$Cs[-c(4,8)], test1$rank$span[,"Cs"])
allequal.test(table1$Ce[-c(4,8)], test1$rank$span[,"Ce"])
# ## using internal values returned by anchors()
# allequal.test(table1$Cs[-test1$omit.row], test1$rank$span[,"Cs"])
# allequal.test(table1$Ce[-test1$omit.row], test1$rank$span[,"Ce"])
# ## using internal values returned by anchors()
# allequal.test(table1$Cs[!test1$omit.logical], test1$rank$span[,"Cs"])
# allequal.test(table1$Ce[!test1$omit.logical], test1$rank$span[,"Ce"])


cat("TEST4: Repeat but with missing values -- string names\n")
data(table1)
dta <- data.frame(y  = c(1,3,3,NA,4,2,2,3,1,2,3,3,4),
                  z1 = c(2,3,2,2 ,2,3,2,2,3,3,4,NA,3),
                  z2 = c(3,4,4,4 ,3,3,2,0,2,2,2,2,2))
rownames(dta) <- letters[1:13]
test1 <- anchors(y ~ z1 + z2, data = dta, method="C")
print(test1)
summary(test1)

cat("These equalities MUST be false incorrect length error:\n")
allequal.test(table1$Cs, test1$rank$span[,"Cs"], FALSE)
cat("BUT These equalities MUST be true\n")
allequal.test(table1$Cs[-c(4,8,12)], test1$rank$span[,"Cs"])
allequal.test(table1$Ce[-c(4,8,12)], test1$rank$span[,"Ce"])
# ## using internal values returned by anchors()
# allequal.test(table1$Cs[-test1$omit.row], test1$rank$span[,"Cs"])
# allequal.test(table1$Ce[-test1$omit.row], test1$rank$span[,"Ce"])
# ## using internal values returned by anchors()
# allequal.test(table1$Cs[!test1$omit.logical], test1$rank$span[,"Cs"])
# allequal.test(table1$Ce[!test1$omit.logical], test1$rank$span[,"Ce"])


cat("TEST5: \n")
data(table1)
dta <- data.frame(y  = c(1,3,3,4,4,2,2,3,1,2,3,3,4),
                  z1 = c(2,3,2,2,2,3,2,2,3,3,4,3,3),
                  z2 = c(3,4,4,4,3,3,2,2,2,2,2,2,2),
                  z3 = c(2,3,2,2,2,3,2,2,3,3,4,3,3))
## can handle one vignette
test1 <- anchors(y ~ z1, data = dta, method="C")
allequal.test( test1$rank$max, 3 )
## duplicates are simply ignored
test1 <- anchors(y ~ z1 + z1 + z1 + z1, data = dta, method="C")
allequal.test( test1$rank$max, 3 )
## additional cases...
test1 <- anchors(y ~ z1 + z2, data = dta, method="C")
allequal.test( test1$rank$max, 5 ) ## should be TRUE
test1 <- anchors(y ~ z1 + z2 + z3, data = dta, method="C")
allequal.test( test1$rank$max, 7 ) ## should be TRUE


