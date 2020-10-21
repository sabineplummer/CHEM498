#  You will need to submit this script as part of the assignment.  Put your name/ID number below
# rename the script to: 
#  NAME:  here        ID#: here

# Clear all variables out of the R environment - good for cleaning-up BAD if you want to keep temporary calculations big data in memory etc...
rm(list=ls())

result = tryCatch({
  graphics.off()

}, warning = function(w) {
  message("no plots to clear")
}, error = function(e) {
  message("no plots to clear - but no problem!")
}, finally = {
})


# clear the console - let the destruction continue!
cat("\014")


## _____________________________ooOOoo_________________________________________
## EVERYTHING in the data generation portion is COPIED and slightly modified 
## from the jat-16-2053-File005 script from the paper:
## Desharnais, Brigitte & Camirand Lemyre, Felix & Mireault, Pascal & Skinner, Cameron. (2017).
## Procedure for the Selection and Validation of a Calibration Model I-Description and Application.
## Journal of Analytical Toxicology.

#Indicate concentration levels to be used.
## VERY IMPORTANT - BLANKS THROW ERRORS, USE A VERY SMALL CONCENTRATION INSTEAD
## SOMEDAY i'LL CHASE THAT ERROR OUT

x <- c(1, 5, 10, 15, 35, 50, 75, 100, 200, 400, 500, 800,1000)

#Number of levels.
n <- length(x)

# _____________ Setup the calibration parameters used to synthesize data________________
# setup all three then later choose if linear or quad

#Random selection of b0 (intercept). Set boundaries of parameter.
## choose a value within the max to min using a uniform probability
b0 <- runif(1, min=-0.009, max=0.5)

#Random selection of b1 (linear parameter). Set boundaries of parameter.
b1 <- runif(1, min=0.002, max=0.8)

#Random selection of b2 (quadratic parameter). Set boundaries of parameter.
b2 <- runif(1, min=-0.00007, max=-0.00000007)

# NOW decide if data is linear or quadratic and synthesize "perfect data" in y

# LINEAR  my old friend!
# NOTE no quadratic component - uncomment the line below, comment out the quad
#y <- (b1*x) + b0


# QUADRATIC - a litte curveball....
# Generation of predicted responses. NOTE inclusion of quadratic component
y <- (b2*x*x) + (b1*x) + b0



# ____________ Establish the magnitude of the SD at each level________
# three types of standard deviation behaviour therefore three weight types

# For all weight, program is set to generate a maximum of 20% RSD, but this can be changed in max=.

# ______ Weighting 1: uniform sd (noise and also variance)

#For a w=1 (uniform weight) weighting, uncomment the following lines.


# #Generate the expected sd at each concentration level
#percent <- runif(1, min=1, max=20)
#absolute <- (percent/100)*y[1]
#sd <- rep(absolute, n)
#RSD <- sd/y*100

# ______ Weighting 2: root-proportional noise 

#For a w=1/x weighting, uncomment the following lines.


# #Generate the expected sd at each concentration level
percent <- runif(1, min=1, max=20)
percent <- 15
varabs <- ((percent/100)*y[1])
zsq <- varabs/sqrt(x[1])
sd <- zsq*sqrt(x)
RSD <- sd/y*100

# _______ Weighting 3: proportional noise

#For a w=1/(x^2) weighting, uncomment the following lines.
# #Generate the expected sd at each concentration level
#percent <- runif(1, min=1, max=20)
#sd <- (percent/100)*y
#RSD <- sd/y*100

# _____________________________________________________________________________
# Now go and modify the signals with appropriate sd

#Generation of an experimental data matrix.
#Set number of replicates.
rep = 12

#Create empty matrix to store the results. one row for each std, many columns, one for each replicate measurement
T <- matrix(nrow=n, ncol=rep)

#Generation of random normally distributed measurements and storage in T matrix.
for(i in 1:n) 
{
  Temp <- rnorm(rep, y[i], sd[i])
  T[i,] <- Temp
}

#Append the concentrations to the results.
# this matrix is
# col 1 = stds concentrations
# col 2 = first measurement
# col 3+ = replicate measurements

A <- cbind(x, T)


plot(rep(A[,1],rep),as.vector(A[ ,-1]),xlab="Concentration",ylab="Signal",main="Calibration data")




# Save above parameters/data in a compact format (for later comparisons).
original_parameters <- cbind(x, n, rep, b0, b1, b2, sd, RSD)
#parameters_file <- paste("P", result_file, sep="_")
#write.table(P, parameters_file, sep="\t", row.names=F)

## let's see what the S/N looks like
SN <-(original_parameters[,8]/100)^-1
plot(x,SN,xlab="Concentration",ylab="Signal/noise",main="SN - input")
#plot(x,sd,xlab="Concentration",ylab="noise",main="noise - input")

#C Clear all variables except the synthesized data and the essential parameters 
rm(list= ls()[!(ls() %in% c('A','original_parameters', 'SN'))])



### _____________________________xOOOx______________________________________________
## EVERYTHING BELOW HERE IS COPIED FROM THE Run_me script from the paper:
## Desharnais, Brigitte & Camirand Lemyre, Felix & Mireault, Pascal & Skinner, Cameron. (2017).
## Procedure for the Selection and Validation of a Calibration Model I-Description and Application.
## Journal of Analytical Toxicology.

## the copied material has been modified enough to make it run with the above
## mostly this means not loading a data file and printing-out a results file


## Parameters to be set
# directory <- "C:/Temp/calib_selection/DC1/Scripts" # set work directory
## folder where the two R scripts: Calibration2Ks and Calibration2CVM


## filename <- "test_calib.txt" # name of the file that contains the raw data (calibration data)
result_filname <- "My_Results.txt" # name of the file that contains the results
dec <- "." # , if the decimals are ex. 0,5 . if the decimals are ex 0.5
nbgrille = 50 # global paramter for integral approximations
b <- 300 #global paramter for bootstraping RECOMMENDED VALUE IS 1000
stat <- 2 # 1: KS, 2: CVM
alpha <- 0.05 ## The alpha to use in hypothesis testing
## end of parameter setting


#################################################################################################
####################################### Code ####################################################
#################################################################################################


# setwd(directory)
source("CodeV5.R") ## Contains the main coded functions

stats = c("Kolmogorov Smirnov","Cramer von Mises")


Decision <- rep(0,3)
DCV <- rep(0,3)


## run all combinations of the regression (linear/quad) and weighting options
## calculate essential parameters and store in "List#"
## later pick/choose the "best" combination

## List holds (not a complete list here)
## Fitted parameters b2,b1,b0 for both linear and quadratic regression
## P value for the linear fit
## P value of the partial F-test (for model order selection)



## run BOTH an linear and quadratic fit on the data using uniform weight


# weight : 0
weight <- 0;
P0 = fitPlus(A,weight,b,nbgrille,stat)
coP0 <- matrix(c(rev(P0$Linear$param),0,rev(P0$Quadratic$param)),ncol=3,byrow=T) ## Contains the fitted coefficients
colnames(coP0) <- c("b0","b1","b2")
rownames(coP0) <- c("Linear","Quadratic")

## extract out only the parameters we'll use later (there are more inside P0, e.g. Fcalc value)
## put these selected parameters into one of three "Lists" 

## NB that the partial F-test for model order is carried-out in fitplus and the p value is returned here

List0 <- list(Fitted_Param = coP0, Pval_Lin = P0$Linear$pvalN,Stat_Lin = P0$Linear$statN ,
              Pval_Quad = P0$Quadratic$pvalN,Stat_Quad = P0$Quadratic$statN, Pval_PartialF = P0$pvalFtest)

Decision[1] = 1 + (List0$Pval_PartialF<0.05)

## run BOTH an linear and quadratic fit on the data using 1/X weight

# weight : 1
weight <- 1;

P1 = fitPlus(A,weight,b,nbgrille,stat)

coP1 <- matrix(c(rev(P1$Linear$param),0,rev(P1$Quadratic$param)),ncol=3,byrow=T) ## Contains the fitted coefficients
colnames(coP1) <- c("b0","b1","b2")
rownames(coP1) <- c("Linear","Quadratic")


List1 <- list(Fitted_Param = coP1, Pval_Lin = P1$Linear$pvalN,Stat_Lin = P1$Linear$statN ,
              Pval_Quad = P1$Quadratic$pvalN,Stat_Quad = P1$Quadratic$statN, Pval_PartialF = P1$pvalFtest)

Decision[2] = 1 + (List1$Pval_PartialF<0.05)

## run BOTH an linear and quadratic fit on the data using 1/X^2 weight

# weight : 2
weight <- 2;
P2 = fitPlus(A,weight,b,nbgrille,stat)

coP2 <- matrix(c(rev(P2$Linear$param),0,rev(P2$Quadratic$param)),ncol=3,byrow=T) ## Contains the fitted coefficients
colnames(coP2) <- c("b0","b1","b2")
rownames(coP2) <- c("Linear","Quadratic")


List2 <- list(Fitted_Param = coP2, Pval_Lin = P2$Linear$pvalN,Stat_Lin = P2$Linear$statN ,
              Pval_Quad = P2$Quadratic$pvalN,Stat_Quad = P2$Quadratic$statN, Pval_PartialF = P2$pvalFtest)

Decision[3] = 1+ (List2$Pval_PartialF<0.05)



## Now  identify/find which is the best of the available models/weighting

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Variance Test for weight selection

## uniform
var0 = apply(A[,-1]/length(A[,1]),1,var)

## 1/X
Ano  = A[,-1]/sqrt(A[,1])
spoids = sum(1/sqrt(A[,1]))
var1 = apply(Ano/spoids,1,var)

## 1/X^2
Ano2 = A[,-1]/A[,1]
spoids2 = sum(1/(A[,1]))
var2 = apply(Ano2/spoids2,1,var)

plot(A[ ,1],var0, xlab = "Concentration", ylab = "Weighted Variance", main = "Uniform weighing")
plot(A[ ,1],var1, xlab = "Concentration", ylab = "Weighted Variance", main = "1/x weighing")
plot(A[ ,1],var2, xlab = "Concentration", ylab = "Weighted Variance", main = "1/x^2 weighing")

## determine which of the three weighting schemes yields the smallest variance of the variances
## Round the numbers to a reasonable # of decimals
s1 = signif(sum((var0-mean(var0))^2),3)
s2 = signif(sum((var1-mean(var1))^2),3)
s3 = signif(sum((var2-mean(var2))^2),3)
sss = c(s1,s2,s3)

# build text for later display

z1 = paste("Variance test for weight selection" )
z2 = paste("Scores:" , "No weight:",s1,"   x^(-1):",s2, "    x^(-2):",s3 )
ccc = c("no weight","x^(-1)","x^(-2)")

# which one of S1, S2 or S3 is smallest - retain it and incorporate into text
ccb = paste("Selected weight: ",ccc[which(sss==min(sss))],sep="")
zaa = paste(z1,z2,ccb,sep="\n")



## now we have found the best weighting scheme "remember" it so we can select the 
## appropriate List (0,1 or 2) that holds the linear & quadratic regression parameters
## best_wt is 1,2 or 3
best_wt <- which(sss==min(sss))
## Choose between List1 (uniform), List2 (1/X) or List3 (1/X^2)
## copy those winning parameters from the correct "List#" into a_winner
a_winner = eval(parse(text = paste("List",best_wt-1,sep="")))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Ftest for Heteroscedasticity

pop1 = A[1,-1] ## look at the set of lowest signals
pop2 = A[nrow(A),-1]  ## look at the set of highest signals
ttt = var.test(pop1,pop2,alternative = "less") ## do they have significantly different variances?
bb = paste("F-test for heteroscedasticity")
bb1 = paste("p-value: ",signif(ttt$p.value,4)) ## round the p-value
ddd = "No"
if(ttt$p.value<alpha ){ddd="Yes"} ## decide if heteroscedastic or uniform
bb2 = paste("Weighting needed:",ddd,sep=" ")
## build text with results (weighting/no weighting needed)
popl = paste(bb,bb1,bb2,sep="\n")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## partial Ftest (ratio of sums of squares rather than variances really) for model order selection
# a_winner contains both the linear and quadratic regression parameters and the results of the
# partial F-test between these two models (now just need to decide if quad is better than linear)
# only really need to build the text below

v = paste("Partial F-test for model order selection")
## extract the value
v1 = paste("p-value",signif(a_winner$Pval_PartialF,4),sep=": ")
## remember "best_wt" holds the index value indicating which type of weighting is best
v2 = paste("Model selected: ",c("linear","quadratic")[Decision[best_wt]],sep="")
vv = paste(v,v1,v2,sep="\n")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## okay now start looking at the residuals to verify that the selected model & weighting actually produce 
## residuals with a normal distribution
## If they don't then the model has failed, maybe the data has higher order trends
## maybe it is just bad luck (sampling statistics are always risky)

## use one of the two normality tests

w = paste("Normality of the standerdized residuals")
w1 = paste("Test used: ",stats[stat],sep="") # stats hold the names of the two tests
aw= eval(parse(text = paste("a_winner$Pval_",c("Lin","Quad")[Decision[best_wt]],sep="")))
w2 = paste("p-value: ",signif(aw,4))


# if the value of aw is "large" then the residuals appear to be normally distributed
# and the model is "validated"
ww = "No" # start by assuming not validated
if( alpha<aw) # but if aw is larger than alpha (0.05)
{
  ww = "Yes" # set the model to validated
}
ww1 = paste("Validation test passed:",ww,sep=" ")
vw =paste(w,w1,w2,ww1,sep="\n")


## start building text for the grand reveal at end of program

md = c("Linear","Quadratic")[Decision[best_wt]]  # model chosen
c("1","1/x","1/x^2")[best_wt]  # weight chosen
if(ww=="Yes")
{
  zw=paste("Model selected: ",md,", ",c("1","1/x","1/x^2")[best_wt],sep="" )
  zw2 = paste("Calibration equation:")
## pull out the calibration parameters for display
  
  if(md=="Linear")
  {
    co1 = a_winner$Fitted_Param[1,1]
    co2 = a_winner$Fitted_Param[1,2]
    ## build the text string
    zw1 = paste(signif(co2,4)," x + ",signif(co1,4),sep="")
  }
  if(md=="Quadratic")
  {
    co1 = a_winner$Fitted_Param[2,1]
    co2 = a_winner$Fitted_Param[2,2]
    co3 = a_winner$Fitted_Param[2,3]
    ## build the text string
    zw1 = paste(signif(co3,4)," x^2 + ",signif(co2,4)," x + ",signif(co1,4),sep="")
  }
  ## build overall string
  vw2 = paste(zw,zw2,zw1,sep="\n")
}
if(ww == "No"){
  ## since its a failure, don't show any calibration parameters just the fail msg
  vw2 = paste("No model selected, validation test failed")
}

## Plots

## NB I have commented out some of the lines below to force plotting into the 
## RStudio window, uncomment them to generate PDF files instead

## namePlot = strsplit(result_filname,".",fixed=TRUE)[[1]][1]
# Plot of variance
var_level = apply(A[,-1],1,var)
## testing if this works pdf(file=paste(namePlot,"Variance.pdf",sep="_"))
plot(A[,1],var_level,xlab="Concentration",ylab="Variance",main="Variance plot",mgp = c(2, 0.8, 0), axes = T)
## dev.off()
## Calibration curve
f <- function(x)
{
  predic(rev(a_winner$Fitted_Param[Decision[best_wt],]),x)
}
Cal.dots = predic(rev(a_winner$Fitted_Param[Decision[best_wt],]),A[,1])
## pdf(file=paste(namePlot,"Calibration curve.pdf",sep="_"))
plot(rep(A[,1],each=(ncol(A[,-1]))),c(t(A[,-1])),xlab="Concentration",ylab="Signal",main="Calibration curve",mgp = c(2, 0.8, 0), axes = T)
curve(f,add=T)
## dev.off()
mt = paste(popl,zaa,vv,vw,vw2,sep="\n \n")
## write(mt,file=result_filname)

## put/print the summary text onto the console - yes, this is the only place it actually outputs
cat(mt)





