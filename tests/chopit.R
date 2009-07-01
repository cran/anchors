## R Code to estimate CHOPIT model of political efficacy
##
## Example: default model
## 
## Author:    Jonathan Wand <wand(at)stanford.edu>
##
## Created:   2002-08-01
## Modified:  $Date: 2005/12/19 21:12:53 $
## Revision:  $Revision: 1.2 $
## RCS-ID:    $Id: chopit.default2.R,v 1.2 2005/12/19 21:12:53 jwand Exp $
##
library(anchors)

cat("\n\nchopit() Demo\n\n")

## Step 1. Get library and data
# library(anchors)
data(mexchn)

cat("\nSpecify list of model components\n")
## Step 2. List of names of columns from dataset which will be used in analysis
fo <- list(self =  xsayself ~ china + age + male + educyrs  ,
           vign = cbind(xsay1,xsay2,xsay3,xsay4,xsay5) ~ 1  ,
           tau  =           ~ china + age + male + educyrs  )

cat("\nDefault invocation of chopit\n")
## Step 3. Invoke chopit() function
out0  <- chopit( fo, mexchn, options=anchors.options(verbose=TRUE))

cat("\nSummary of default chopit\n")
summary(out0)

cat("\nTake a look at some additional information\n")
cat("Gradients\n")
print(out0$gr)
