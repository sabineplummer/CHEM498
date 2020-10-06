#  You will need to submit this script as part of the assignment.  Put your name/ID number below
# rename the script to: 
#  NAME:  here        ID#: here
# Clear all variables out of the R environment - good for cleaning-up BAD if you want to keep temporary calculations big data in memory etc...
rm(list=ls())
#_________________________________________ Setting general parameters for LR -------------------------
# STANDARDS

C_levels <- c(0,1,2,4,7,10,20,25,50) # concentration levels of the standards
Replicates <- 5 # number of replicates
Stds <-c()

# SIGNALS
Slope <-27.6
Intercept <- 43  ## ALWAYS MAKE POSITIVE AND ROUGHLY 1-5% OF MAX STD * SLOPE (50*27 = 1350)  otherwise bad things happen in WLS
p_known <- c(Intercept,Slope)  # setup the "known" parameters in matrix form 2 rows x 1 column

rm(Slope, Intercept) # don't need them anymore - remove
# ERROR/NOISE

Types_of_noise <- c("uniform","root-proportional","proportional")  # what types of noise are encountered?
Noise_type <-Types_of_noise[1] # the type of noise that will be in the calibration/sample data


print(paste("the type of noise added to the signals is:",Noise_type))
# DEFAULT RSD_at_max <-0.05
RSD_at_max <-0.05  # approx the RSD found on the highest calibration standard = sigma/signal(last std)

# prepare to use random numbers (for noise values) BUT setting the seed guarantees everyone gets same random sequence 
# DEFAULT set.seed(653)
set.seed(653) # force the random number generator to always produce the same sequence of numbers.  Allows everyone to generate the SAME noise values

rm(Types_of_noise)  # remove this now that we have selected one of the options - clean up
#-------------------------------------- ENd of parameter definitions ------------------------------


# ______________________________________ BUILD STANDARDS & SIGNALS  ___________________________________ 

for (lev in c(1:length(C_levels))) {  # create a vector with the standards concentrations, one for each replicate
  for (rep in c(1:Replicates)) {
    Stds<-c(Stds,C_levels[lev])
  }
}

rm(lev, rep)  # clean-up


# setup the standards in a matrix math compatible format 
Stds_array<-array(1,c(2,length(Stds)))  # create a matrix 2 x # of stds and fill with "1"
Stds_array[2, ]<-Stds # load in the standard concentrations into the 2nd row 
Stds_array<-t(Stds_array) # need to reorient this matrix  so we can use matrix math, t(X) is the transpose function


# now create a set of signals using the desired calibration function
Signals <- Stds_array%*%p_known  # in R this is a MATRIX multiplication: using just * is a point by point multiplication - not the same


# do we see the expected "perfect" calibration?  un comment to see
#  plot(Stds,Signals)  #take a look - verify it is as expected.  This is the NOISE free "perfect" data

# -------------------------------------------- End of Stds and Signals -----------------------------

# _____________________________________________ PREPARE A SET OF NOISE TO ADD TO THE SIGNALS ________

# now add onto the "perfect signals" the expected noise
# choose between the possibilities as set above in the parameter area

# FOR EXERCISE 1  use "uniform" noise i.e. HOMOSCEDASTIC data


if (Noise_type =="uniform"){
  # the noise is uniform throughout ALL standards (Homoscedastic) and is set to RSD_at_max
  # create an "error" (or deviation or noise) to add to each of the signals later
  # the rnorm function generates a Normally distributed set of values at the specified mean (0) and SD
  Noise_vector <-rnorm(length(Stds),0,RSD_at_max*max(Signals))  # find the max signal, multiply by RSD and use this as the SD for the normal distribution 
  
  # ignore this for the moment - only needed for calculating the SN plot
  Noise_scalar<-(Signals/max(Signals))^0
  
  
  # since the noise is uniform we can calculate an %RSD and compare against expected - always check your data against expectations, It can help find errors
  print(paste("the observed RSD is:",round((sd(Noise_vector)/max(Signals)),digits = 5))) # the sd function is the sample standard deviation (Dof = N-1)
  print(paste("the expected RSD is:",RSD_at_max))
  # are they the same?? close enough?
  
  # if the type of noise is NOT "uniform" then....  We'll use these others for weighted regression
} else if (Noise_type =="root-proportional") {
  # first start off with a noise vector with uniform noise throughout and large enough to produce the expected %RSD on the maximum signal
  Noise_vector <-rnorm(length(Stds),0,RSD_at_max*max(Signals))  # find the max signal, multiply by RSD and use this as the SD for the normal distribution   
  # now modify the noise to be "root proportional" i.e. noise at std of interest = sqrt(Sig of interest/max sig) x noise
  Noise_scalar<-(Signals/max(Signals))^0.5 
  
  
  plot(Stds,Noise_scalar)  # take a look, it should go from small value to 1 in a root relationship
  # update the noise vector to now be root proportional
  Noise_vector <- Noise_vector*Noise_scalar # note, this is using the "regular" point by point multiplication - not matrix multiplication
  
  # if the type of noise is NOT "uniform" or "prportional" then.... 
} else  { #noise must be "proportional"
  # first start off with a noise vector with uniform noise throughout and large enough to produce the expected %RSD on the maximum signal
  Noise_vector <-rnorm(length(Stds),0,RSD_at_max*max(Signals))  # find the max signal, multiply by RSD and use this as the SD for the normal distribtion   
  # now modify the noise to be "proportional" i.e. noise at std of interest = (Sig of interest/max sig) x noise
  Noise_scalar <-(Signals/max(Signals)) 
  
  plot(Stds,Noise_scalar)  # take a look, it should go from small value to 1 in a proportional relationship
  # update the noise vector to now be proportional
  Noise_vector <- Noise_vector*Noise_scalar   
}
#----------------------------------- We now have a vector with JUST the noise in it ---------------------------

# _________________________________ Add the noise onto the signals to produce "realistic" signals __________

Signals <-Signals+Noise_vector
Signals<-Signals[ ,1]  # not sure why but "Signals" is in Data frame format and we'll need it as a simple vector

plot(Stds,Signals)  #take a look - verify it is as expected.


# Also very helpful to look at the (blank) signal/noise - high values tell you where you are obtaining your most trustworthy data
# When S/N = approx 3 is the DL

SN<-((Signals-p_known[1])/(Noise_scalar*RSD_at_max*max(Signals)))
plot(Stds,SN)

# --------------------------------- We now have some realistic signals for calibration ------------






# ___   EXERCISE 1: LR using OLS see slide 6  _________________________________________________

# let's solve for the parameters using simple matrix operations: a DIY experiment


# the "solve" function is equivalent to inverting a matrix i.e.(matrix)^-1
# the %*% is R's way of denoting a matrix multiplication
# t(X) produces the transpose of X

# DIY_LR_p = (X^T * X)^-1 * X^T * Y = p
#So the "do it yourself" LR is:

DIY_LR_p <- solve(t(Stds_array)%*%Stds_array)%*%t(Stds_array)%*%Signals

# should (assuming defaults) = 48.41316, 27.12816

# ---------------- Now you have your DIY parameters --------------------------


# Use R's built-in LR function "lm" or linear model

# the LR function needs to know the dependent vs. independent and the data frame holding the data
# here the dependent is "Stds" and the independent is "Signals" and I have assembled the data into a dataframee IN the second arguement

Rs_LR_p <- lm(Signals ~ Stds, data=data.frame(Stds,Signals))  # build linear regression model on full data
# Rs_LR_p contains all the raw output from the regression - some of it is useful "as is" some needs additional work

# R has a function (summary) that can extract/calculate some of the most commonly used parameters we need 

Compact_p<-summary(Rs_LR_p)  


# Take a look if you want
# print(Compact_p)  # output just SOME of the LR parameters



# _________ EXERCISE #2:  Compare and contrast see slide 7_____________________________________________________________


# COMPARE YOUR parameter RESULTS TO R's LR FUNCTION 
# Use the "print" and "paste" functions

# Change PO to TRUE to print-out the values on screen - make FALSE to silence the printing
PO <- TRUE

if (PO) {
  #EXPECTED VALUES
  print(paste("the EXPECTED intercept is:",round((p_known[1]),2)))
  print(paste("the EXPECTED slope is:",round((p_known[2]), digits = 2)))
  
  #WHAT YOU HAVE CALCULATED
  print(paste("the DIY calculated intercept is:",round((DIY_LR_p[1]), digits = 2)))
  print(paste("the DIY calculated slope is:",round((DIY_LR_p[2]), digits = 2)))
  
  #What R has calculated
  print(paste("R's calculated intercept is:",round((Rs_LR_p$coefficients[1]), digits = 2)))
  print(paste("R's calculated slope is:",round((Rs_LR_p$coefficients[2]), digits = 2)))
  
  
  #Calc the SD of the residuals found in lm (equivalent to the Sy or Sr)
  # Careful, what are the correct # of Degrees of Freedom?  Why not use  the built-in sd function??
  DIY_Sr <- sum(((Rs_LR_p$residuals)^2)/Rs_LR_p$df.residual)^.5
  # How does that compare to what we expect using the input noise? again, use the same DoF as previous (to permit a fair comparison)
  Expected_Sr <- (sum((Noise_vector)^2/43)^.5)
  # should equal 35.41002 if defaults used
  
  
  print(paste("CALCULATED Sr from regression residuals:",round((DIY_Sr), digits = 2)))
  print(paste("EXPECTED Sr INPUT noise:",round((Expected_Sr), digits = 2)))
  print(paste("R's reported Sr value:",round((Compact_p$sigma), digits = 2)))
  
  
  
  
  # make sure the regression residuals have a mean of 0 (or close to) - as expected!
  Mean_resid_close_to_0 <-NA
  
  # since we know the noise that we added onto the signal (our "input" residuals), do they have a mean = 0?
  Expected_resid <-NA
  
  print(paste("CALCULATED mean regression residual is:",round((Mean_resid_close_to_0), digits = 2)))
  print(paste("EXPECTED mean regression residual is:",round((Expected_resid), digits = 2)))
  
  
  
  
  rm(DIY_Sr, Expected_Sr,Mean_resid_close_to_0,Expected_resid)  # clean these up - don't use them anywhere below
  
  
  
}  # ending bracket for the "if" print statement

#  OKAY nOW THAT WE TRUST R TO COMPUTE A LINEAR REGRESSION - HOW ABOUT THE ERROR?


#_________EXERCISE 3  ERROR PROPIGATION   See slide 32 _________________________________________________________________

#                  SET UP A SAMPLE

Smpl_a_Sig <-c(915.2915, 941.3969, 939.0855, 984.2708, 952.9782)


Mean_a_Sig <- mean(Smpl_a_Sig)


Conc_a <- NA  # apply the calib eqn to calc the mean concentration 
Conc_a <-unname(Conc_a)  # unname strips off the residual names from the coefficients (could also have just used [[]] in previous line)



# ______________________________ Calc the "textbook" variance see slide 33________________________________________

#Var_unk_txtbk
# Calc the expected variance in the unknown using the "textbook" equation

## *********   ## REMOVE RH SIDE OF THIS EQN BEFORE RELEASING THE SCRIPT!!
Var_unk_text <- NA
# should =  0.4225204 assuming defaults


#_________________________________ Calc the variance using the VCV matrix equation see slide 33__________________

#Var_unk_VCV
# Break this down into bite sized pieces

# Step 1: calc the derivative vector

Deriv_f <- NA
# should =  -0.03686206 -1.22047272


# Step 2: Now calc the VCV

VCV <- NA


# Step 3: caclulate the variance due to calibration alone

Var_calib <-NA

# Step 4: calculate the total variance by adding the contributions from both
# also make the substitution for the sample variance (sigma_Yunk ^2) for the variance from the calibration (sigma_Y_calib^2/M)
# Careful: when looking at the slides recall N is the total number of standards measurements and M is the total number of unknown measurements
# Nearly all the texts and CHEM 312 notes use N for the unknowns and M for the standards - Sorry

Var_unk_VCV <- NA

#______________________________________ Now calc the variance using the "algorithm" equation see slide 33__________________

#Var_unk_alg

# Calc the determinant of (X^t * X)
Det_Xs<- NA
# should = 477350


Var_unk_algo <- NA



# _____________________________________________ Now compare all of your results to each other ___________________

# print-out the calculations of the variances using the print command similar to what was done above 

# uncomment and put in appropriate text/variables for the following: 
# print(paste("The Blah Blah:",(Var_unk_text)))








#---------------------------------------------- XXX ---------------------------------------------------------



## ASSIGNMENT 1
## Extend the OLS equations to carry-out a WLS regression using matrix math to calculate the best fit parameters
## use R's lm function to carry-out the SAME WLS regression
## calculate the true concentration of the unknown (i.e. with a confidence interval - see CHEM 312 notes if needed)


#--------------------------------------------------  WLS - an extension to OLS ---------------------------

# YOu will need to adjust the OLS math from above to perform a WLS calculation
# you will also need to USE THE CORRECT TYPE OF weighting DEPENDING ON THE TYPE OF NOISE
# The possibilities are: "uniform","root-proportional","proportional"

# Here is something to try and think through
# Ideally we would use the actual variance at each measurement (signal) as the weighting factor (i.e. Wi = 1/sigma^2)
# BUT when we have replicates (such as here) we only have access to the sample variance so Wi = 1/SDi^2
# (in many instances we only have one measurement at each concentration so we don't even have the SD (no replicates, can't calc a SD) - oh dear!)
# BUT we have agreed that when we have a well behaved instrument the variances are
# variance = constant (use OLS) or
# variance = linear with concentration (the SD is "root-proportional" case), suggesting the use of Wi = 1/X or
# variance = scales with concentration^2 (the SD is "proportional" case), suggesting the use of Wi = 1/X^2

# in practice the noise (variance) is not just one or the other but really a mixture of all three
# the three cases above are really just about which of the three types of noise (variance) is dominant in the experiment that we are performing
# for example, and in practice, if the blank noise is dominant (and its a constant) then the noise overall is constant and OLS makes sense
# on the other hand if (at higher concentrations & signals) the noise on the (analyte) signal is dominant then the noise scales with the signal and we have the other two cases (WLS makes sense)

# As hinted at above, we often don't have access to the actual sigma but since we know that the sigma depends on the SD which depends on the signal which depends on the (concentration + blank)
# we can make the above substitutions (i.e. Wi = 1/sigmai^2 is replaced by Wi = 1/const (OLS) or 1/Xi (root) or 1/Xi^2 (proportional))

# If we make these substitutions it has implications for when X=0 i.e. the blanks, in the simple WLS case (Wi = 1/X or Wi = 1/X^2) the W at X=0 blows up - not good
# So in practice we can't really use 1/X or 1/X^2 as the weighting factors
# instead we recognize that the SD it tied to the signals (which is linear with X) so we use the expected signal instead of X
# I know, it's confusing but notice the signal doesn't go to 0 when x goes to zero, it goes to a constant (the intercept) and it (obviously) scales with the signal
# so In the real world (read calibration software) the appropriate thing to do is first run an OLS to obtain a first estimate of the regression equation parameters (m, b, Sr)
# then if OLS is used the Wi = 1/sigma^2 = 1/const =  1/Sr^2
# if WLS (root proportional) is used then Wi = 1/sigma^2 = 1/(sqrt(mx+b))^2 = 1/(mx+b)
# if WLS (proportional) is used Wi = 1/sigma^2 = 1/(mx+b)^2
# notice that none of these blow-up when x=0 (assuming of course that b is a reasonable, > 0 value which is the case with this synthetic data)
# Hmmmm... if only we knew the ideal values for mx+b we could easily build W


# okay, now start building the W vector/matrix

if (Noise_type =="uniform"){
  W <-NA
  print("Doing weighted regression with UNIFORM weights aka OLS")
  
} else if (Noise_type =="root-proportional") {
  # This is the situation where the commercial calibration software would say that it is using "1/X" weighting.
  # as discussed above, this runs into TROUBLE when x=0 (i.e. the blanks), instead we will weight using the best estimate of the signal
  # If this was "real" data (i.e. collected in the lab, with noise) then we would perform a preliminary OLS to obtain the LR_p and then calculate a predicted/expected set of signals
  W <- NA
  
  print("Weighted regression, Weights = 1/variance and variance is proportional to the signal i.e. root proportional noise")
} else  { #noise must be "proportional"
  W <- NA
  print("Weighted regression with weights = 1/variance and variance is proportional to the signal^2 i.e. proportional noise")
}

# Careful here, we will be using a Weights vector/matrix with values proportional to the EXPECTED variance (i.e. uniform, root proportional or proportional)


DIY_WLS_p <- NA  #<-- MODIFY ME (see above) TO INCLUDE WEIGHTING!



# ________________________________ COMPARE TO R's weighted LR ______________________________________________________

# Use R's built-in LR function "lm" or linear model BUT using weighted LR
# See the additional arguments you can pass lm - one is for weighting!!

# Same call as above  EXCEPT using a weights vector (1/variance) which can be extracted from the diagonal of the W matrix

# careful on the syntax - including the "weights" in the lm call requires including the "lm(other arguments, weights = ABC)"

Rs_WLR_p <- NA  # build linear regression model on full data, use the weights option


Compact_wp<-summary(Rs_WLR_p)  # R has a summary function that helps with calculating some useful summary statistics
# Take a look at all the parameters ,if you want,
# print(Compact_wp)  # output just SOME of the LR parameters

# but at least compare the slope/intercept
print(paste("DIY_slope and intercept are:", round(DIY_WLS_p,3)," and R finds:",round(Rs_WLR_p$coefficients,3)))

# __________________________________  SIMPLE PLOT OF CALIBRATION DATA AND THE OLS vs WLS lines_____________


plot(Stds,Signals)  #take a look - verify it is as expected.
abline(Rs_WLR_p,col=4)  # add a line extracting the coefficients from the lm output
abline(Rs_LR_p, col=3)


# _____________________________ CALCULATE THE ERROR ON A SAMPLE using the matrix approach _____________________________________
# Var_unk_wVCV
# Break this down into bite sized pieces

# Step 1: calc the derivative vector
# DONE ABOVE no need to recalculate it  we called it "Deriv_f"

# Step 2: Now calc the weighted VCV

WVCV <- NA

# Doing the manual multiplication out of: solve(t(Stds_array)%*%W%*%Stds_array)
# produces:
# [sum(WX^2) , -sum(WX)
#   -sum(WX)  , sum (W)] ALL divided by the Det

#where the Det = sum(W)*Sum(wX^2) - (sum(WX))^2

# THIS IS A DIRECT EXTENSION TO THE OLS CASE
# Be careful  (Compact_wp$sigma)^2  is the WEIGHTED variance (i.e. the weighted variance is uniform up/down the curve)
# weighted variance = sum(Wi*(Yi - (b0+b1Xi))^2)/(Dof = N-2)     Where N = # of stds measured, Wi, Xi and Yi are the ith value

#Let's prove this:
W_Var <-NA
print(paste("manually calulated weighted variance is:",round(W_Var,3),", R's value is:",round((Compact_wp$sigma)^2,3) ))
# these should be the same!


### see: https://stats.stackexchange.com/questions/138938/correct-standard-errors-for-weighted-linear-regression


# Step 3: calculate the Weighted variance due to calibration alone

WVar_calib <-NA

# Step 4: calculate the total variance by adding the contributions from both
# CAREFUL - the equation used above in the OLS case makes a critical simplification - the variance in measuring the unknown is the same as the variance
# from the calibration - this works when all measurements have the SAME variance (i.e. the uniform case) 

# Sadly we can't make the same substitution here but, it isn't too big a deal since it is easy to calculate the expected variance
# associated with measuring the unknown. - see slides!



Var_unk_wVCV <- NA

#__________________________________ other stuff - do not forget _________________________________________________

#  You will need to calculate the unknown's confidence interval in the script above under OLS, and WLS (both cases)
# verify you have included your name / ID# IN the script file above
# verify you have prepared all the necessary plots and values and put them into your report/assignment (a Word document)

