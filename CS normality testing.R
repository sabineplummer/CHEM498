# rename the script to: LAST NAME_ID# 
#  You will need to submit this script as part of the assignment.  Put your name/ID number below
#  NAME:  here        ID#: here
# Clear all variables out of the R environment - good for cleaning-up BAD if you want to keep temporary calcuations big data in memory etc...
rm(list=ls())

# Clear all plots  YES ALL PLOTS

result = tryCatch({
  graphics.off()
  
}, warning = function(w) {
  message("no plots to clear")
}, error = function(e) {
  message("no plots to clear - but no problem!")
}, finally = {
  
})


Exercise<-"five" # ("one", "two", "three", "four" etc.)  LOWERCASE ONLY

print(paste("RUNNING CODE FOR EXERCISE",Exercise))

switch (Exercise,  # the SWITCH statement allows you to run selected pieces od code depending on the value of the "Exercise" (one, two, three, four)
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        
        one = {  # If Exercise = one then do the following 
          
          ## EXERCISE 1: CAN YOU SPOT A NORMAL DISTRIBUTION??
          Num<- 1000 # Number of data points in the set
          Data<-rnorm(Num, mean = 0, sd = 1)  # create a data set of random numbers that follow the Normal distribution
          
          if (Num>30){ # When plotting a histogram use a "reasonable" number of bins rather than the default
            bins <- round(Num^.5,0)}
          else
          {bins <-10}
          
          
          hist(Data,bins) # Show a histogram of the data
          rm(Data,bins, Num)
        },  # END OF EXERCISE one
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
        
        two = {  # If Exercise = two then do the following 
          
          ## EXERCISE 2: CAN YOU SPOT A NORMAL DISTRIBUTION??
          Num<- 1000 # Number of data points in the set
          
          if (runif(1)>0.5) { # 50% of the time it will be a uniform distribution 50% of the time it will be a unifirm distribution
            Data<-rnorm(Num, mean = 0, sd = 1)
            Dist_type<-"Normal"
          }
          
          else {
            Data<-runif(Num,-4,4)
            Dist_type<-"Uniform"
          }
          
          if (Num>30){# When plotting a histogram use a "reasonable" number of bins rather than the default
            bins <- round(Num^.5,0)}
          else
          {bins <-10}
          
          
          hist(Data,bins) # Show a histogram of the data
          Sys.sleep(2)  # wait for 2 seconds before displaying the actual distribution type as a legend on the histogram
          legend("topleft", legend=paste("The distribution is: ", Dist_type))
          rm(Dist_type,bins,Data, Num)  # housekeeping so as to not clutter up the environment
        },  # END OF EXERCISE two
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        three = {  # If Exercise = three then do the following 
          
          ## EXERCISE 3: Meet the Shapiro-Wilk test for Normally distributed data
          Num<- 1000 # Number of data points in the set
          
          Data<-rnorm(Num, mean = 0, sd = 1)  # create a data set of random numbers that follow the Normal distribution
          Dist_type<-"Normal"
          
          if (Num>30){# When plotting a histogram use a "reasonable" number of bins rather than the default
            bins <- round(Num^.5,0)}
          else
          {bins <-10}
          hist(Data,bins) # Show a histogram of the data
          
          # RUN THE SW test on the Data
          
          SW_result<-shapiro.test(Data)
          
          # Build a text string to print on the histogram
          boo<-c(paste("The input distribution is: ", Dist_type),
                 paste("The value of W is:",round(SW_result$statistic[[1]],3)),
                 paste("The probabiliy (p) value is:",round(SW_result$p.value,3)))
          
          legend("topleft", legend=boo) # display the text on the histogram
          
          alpha_val<-0.05  #Set the alpha value.  A value of 0.05 corresponds to the 95% confidence limit/interval 
          # on the CONSOLE print if the Data appears to be Normal
          # When SW_result$p.value is "large" then we can't tell if it ISN'T normal therefore we call it "Normal" - Love the double negatives in Stats!!
          if (SW_result$p.value>=alpha_val) { # if the p value is larger than the cutoff value then
            Normal_TF<-paste("Normal b/c the p value is >= ",alpha_val)}
          else {Normal_TF<-paste("non-Normal because the p value is < ",alpha_val)}
          
          
          print(paste("Based on the p-value the data (N=",Num,"), appears to be:",Normal_TF),quote=FALSE)
          
          rm(Data,bins,Num,boo,SW_result)  # housekeeping so as to not clutter up the environment
        },  # END OF EXERCISE 
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        four = {  # If Exercise = four then do the following 
          
          ## EXERCISE 4: Shapiro-Wilk test for Normally distributed data on NON-Normal data
          Num<- 500 # Number of data points in the OVERALL set
          
          ## Build to separate groups of data and bring them together into one set of data
          G1_mean<-0
          G2_mean<-G1_mean+5 # Set the difference between the two means
          
          Num_per_set<-round(Num/2,0)  # Split the total number of points into two groups - needs to be an integer
          
          Group1<-rnorm(Num_per_set, G1_mean, sd = 1)  # create a data set of random numbers that follow the Normal distribution
          Group2<-rnorm(Num-Num_per_set, G2_mean, sd = 1)  # create a data set of random numbers that follow the Normal distribution
          ##  finish building two groups of data
          
          Data<-c(Group1,Group2) # combine them together
          Dist_type<-paste("two separate normals with delta mean:",G2_mean-G1_mean)
          
          if (Num>30){# When plotting a histogram use a "reasonable" number of bins rather than the default
            bins <- round(Num^.6,0)}
          else
          {bins <-10}
          hist(Data,bins) # Show a histogram of the data
          
          # RUN THE SW test on the Data
          
          SW_result<-shapiro.test(Data)
          
          # Build a text string to print on the histogram
          boo<-c(paste("The input distribution is: ", Dist_type),
                 paste("The value of W is:",round(SW_result$statistic[[1]],3)),
                 paste("The probabiliy (p) value is:",round(SW_result$p.value,3)))
          
          legend("topleft", legend=boo) # display the text on the histogram
          
          alpha_val<-0.05  #Set the alpha value.  A value of 0.05 corresponds to the 95% confidence limit/interval
          
          # on the CONSOLE print if the Data appears to be Normal
          # When SW_result$p.value is "large" then we can't tell if it ISN'T normal therefore we call it "Normal" - Love the double negatives in Stats!!
          if (SW_result$p.value>=alpha_val) { # if the p value is larger than the cutoff value then
            Normal_TF<-paste("Normal b/c the p value is >= ",alpha_val)}
          else {Normal_TF<-paste("non-Normal because the p value is < ",alpha_val)}
          
          
          print(paste("Based on the p-value the data (N=",Num,"), appears to be:",Normal_TF),quote=FALSE)
          
          rm(Data,bins,Num,boo,SW_result, Num_per_set,Dist_type)  # housekeeping so as to not clutter up the environment
        },  # END OF EXERCISE 
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        five ={  
          ## EXERCISE 5: rune the Shapiro-Wilk test for Normally distributed data on NON-Normal data MANY TIMES
          # We are going to run multiple SW tests on data sets with a known difference in means
          # We will then vary the difference in the means and repeat
          # we want to discover the ability of the SW test to detect the fact that the data ins't from a single Normal distribution
          
          # Need to initialize some variables for later use - DON'T fiddle with these!
          Ps = numeric() #empty vector to hold p values - can plot a histogram if you want
          second_mean = numeric() #initalize the variable
          fract = numeric() #emptry vector to hold the fration of p's <0.05
          # Done initalizing varibles
          
          Num<- 200 # Number of data points in the OVERALL set
          
          
          Num_per_set<-round(Num/2,0)  # Split the total number of points into two groups - needs to be an integer
          
          
          ## CAREFUL  SETTING n_iter >300 can really slow the system down!!
          n_iter = 200 #serves two purposes 1- #of times to do the SW test 2-# of steps for the second mean
          Max_diff_of_means <-20
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means  
          
          
          for (m in 1:n_iter){ #number of different distances btw the two means = 1 up to n_iter
            
            try_at_mean = (m-1)/n_iter*Max_diff_of_means #vary the second mean from 0 to Max_diff_of_means  in n_iter steps
            second_mean = c(second_mean,try_at_mean) #build vector containing all of the means in the second distribution
            
            # Run the SW test on a set of data, repeat n_iter times
            for (n in 1:n_iter){ #number of SW tests to run
              #The syntax is: rnorm(n, mean = 0, sd = 1)
              #build one set at mean = 0 and SD=1 and a second at mean = "sec_mean" and SD=1
              ## Build to separate groups of data and bring them together into one set of data
              G1_mean<-0
              G2_mean<-G1_mean+second_mean[m] # Set the difference between the two means
              
              Group1<-rnorm(Num_per_set, G1_mean, sd = 1)  # create a data set of random numbers that follow the Normal distribution
              Group2<-rnorm(Num-Num_per_set, G2_mean, sd = 1)  # create a data set of random numbers that follow the Normal distribution
              ##  finish building two groups of data
              Data<-c(Group1,Group2) # combine them together
              
              # RUN THE SW test on the Data
              SW_result<-shapiro.test(Data)
              
              Ps = c(Ps,round(SW_result$p.value,3)) #extract out the p-value and store in a vector
              
            } # finish one SW test (for (n in 1:n_iter))
            
            
            #if the p-value is <0.05 then the SW test finds a significant difference btw the data and a Normal dist
            fract[m] = sum(Ps<.05)/n_iter #fraction of tests that rejected the data as coming from ONE normal dist
            Ps = numeric() #clear the vector for next round
          }
          fract_norm=1-fract # fraction of data sets counted as Normal
          plot(second_mean,fract_norm, main="fraction of data sets counted as Normal distributions",xlab="distance between the two means",ylab="fraction")
          
        },  # END OF EXERCISE 
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        six ={  
          ## EXERCISE 6: Let's see if the t-test is performing as expected
          
          
          #   INITIALIZE SOME VARIABLES TO HOLD OUR STATISTICAL GOODNESS
          Ps = numeric() #empty vector to hold p values
          fract = numeric() #emptry vector to hold the fration of p's <0.05
          
          Num<- 8 # Number of data points in the OVERALL set
          G_mean <-12  # an arbitrary mean value
          
          Num_per_set<-round(Num/2,0)  # Split the total number of points into two groups - needs to be an integer
          
          
          
          n_iter = 5000 # number of times to run the t-test on radomly generated sets of data
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means  
          
          
          # Run the t test on a set of data, repeat n_iter times
          for (n in 1:n_iter){ #number of t tests to run
            #The syntax is: rnorm(n, mean = 0, sd = 1)
            Data<-rnorm(Num, G_mean, sd = 1)  # create a data set of random numbers that follow the Normal distribution
            # out of this SINGLE NORMAL set extract two sub sets
            Group1<-head(Data,Num_per_set)  # the head function gets from 1 to Num_per_set out of Data
            Group2<-tail(Data,Num_per_set)  # the tail function gets from Num_per_set to the last element out of Data
            # RUN THE t test on the Data
            T_result<-t.test(Group1,Group2, var.equal=TRUE) # need to FORCE R to use the equal variance option
            
            Ps = c(Ps,round(T_result$p.value,3)) #extract out the p-value and store in a vector
            
            
            
          } # finish one t test (for (n in 1:n_iter))
          
          #if the p-value is <0.05 then the t test finds a significant difference btw the two groups of data
          fract = sum(Ps<=.05)/n_iter #fraction of tests that XXX   
          
          fract_norm=1-fract # fraction of data sets counted as Normal
          #  plot(second_mean,fract_norm, main="fraction of data sets counted as Normal distributions",xlab="distance between the two means",ylab="fraction")
          
        }, # END OF EXERCISE 
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        seven ={  
          ## EXERCISE 7: Let's see if the t-test is performing as expected when there is an actual difference between the groups
          
          
          #   INITIALIZE SOME VARIABLES TO HOLD OUR STATISTICAL GOODNESS
          Ps = numeric() #empty vector to hold p values
          fract = numeric() #emptry vector to hold the fration of p's <0.05
          second_mean<- numeric() #emptry vector to hold the value of the second mean
          
          Num<- 8 # Number of data points in the OVERALL set
          Max_diff_of_means <-10
          Std_dev <-1
          alpha <-0.05
          
          Num_per_set<-round(Num/2,0)  # Split the total number of points into two groups - needs to be an integer
          
          
          
          n_iter = 100 # number of times to run the t-test on radomly generated sets of data
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means  
          
          
          # Run the t test on a set of data, repeat n_iter times
          for (m in 1:n_iter){ #number of different distances btw the two means = 1 up to n_iter
            
            try_at_mean = (m-1)/n_iter*Max_diff_of_means #vary the second mean from 0 to Max_diff_of_means  in n_iter steps
            second_mean = c(second_mean,try_at_mean) #build vector containing all of the means in the second distribution
            
            # Run the t test on a set of data, repeat n_iter times
            for (n in 1:n_iter){ #number of t tests to run
              #The syntax is: rnorm(n, mean = 0, sd = 1)
              #build one set at mean = 0 and SD=1 and a second at mean = "sec_mean" and SD=1
              ## Build two separate groups of data and bring them together into one set of data
              G1_mean<-0
              G2_mean<-G1_mean+second_mean[m] # Set the difference between the two means
              
              Group1<-rnorm(Num_per_set, G1_mean, sd = Std_dev)  # create a data set of random numbers that follow the Normal distribution
              Group2<-rnorm(Num-Num_per_set, G2_mean, sd = Std_dev)  # create a data set of random numbers that follow the Normal distribution
              
              # RUN THE t test on the two groups
              TT_result<-t.test(Group1,Group2, var.equal=TRUE) # need to FORCE R to use the equal variance option
              
              Ps[n] = round(TT_result$p.value,3) #extract out the p-value and store in a vector
              
            } # finish one t test (for (n in 1:n_iter))
            
            #if the p-value is <0.05 then the t test finds a significant difference btw the two groups of data
            fract[m] = sum(Ps<=alpha)/n_iter #fraction of tests that XXX  
            
            
          }
          
          
          print(paste("t-test where:N=",Num_per_set,"and",Num-Num_per_set,", alpha=",alpha,"and both SD=",Std_dev)) 
          
          plot(second_mean,fract, main="fraction of data sets where the difference in the means was SUCCESSFULY detected",xlab="distance between the two means",ylab="fraction")
          
          fract_norm=1-fract # fraction of data sets where the two means did not differ
          plot(second_mean,fract_norm, main="fraction of data sets where no difference between the means was detected",xlab="distance between the two means",ylab="fraction")
          
        },# END OF EXERCISE 
        
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        eight ={  
          ## EXERCISE 8: Let's see if the t-test is performing as expected when we use NON-NORMAL data
          
          
          #   INITIALIZE SOME VARIABLES TO HOLD OUR STATISTICAL GOODNESS
          Ps = numeric() #empty vector to hold p values
          fract = numeric() #emptry vector to hold the fration of p's <0.05
          alpha <-0.05
          Num<- 8 # Number of data points in the OVERALL set
          
          
          Num_per_set<-round(Num/2,0)  # Split the total number of points into two groups - needs to be an integer
          
          
          
          n_iter = 10000 # number of times to run the t-test on randomly generated sets of data
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the t test is   
          hist(rlnorm(1500),1500^.7, main="A Log-Normal distribution",xlab="values (when log-mean = 0 and log-SD = 0)")  ## SHOW A LOG-NORMAL DISTRIBUTION
          
          # Run the t test on a set of data, repeat n_iter times
          for (n in 1:n_iter){ #number of t tests to run
            
            Data<-rlnorm(Num)  # create a data set of random numbers that follow the Log-Normal distribution
            
            # out of this SINGLE UNIFORM set extract two sub sets
            Group1<-head(Data,Num_per_set)  # the head function gets from 1 to Num_per_set out of Data
            Group2<-tail(Data,Num_per_set)  # the tail function gets from Num_per_set to the last element out of Data
            # RUN THE t test on the Data
            T_result<-t.test(Group1,Group2, var.equal=TRUE) # need to FORCE R to use the equal variance option
            
            Ps = c(Ps,round(T_result$p.value,3)) #extract out the p-value and store in a vector
            
            
            
          } # finish one t test (for (n in 1:n_iter))
          
          #if the p-value is <0.05 then the t test finds a significant difference btw the two groups of data
          fract = sum(Ps<=alpha)/n_iter #fraction of tests that XXX   
          
          fract_norm=1-fract # fraction of data sets counted as Normal  
          
          print(paste("t-test where:N=",Num_per_set,"and",Num-Num_per_set,", alpha=",alpha,"and both from a log-normal distribution")) 
          print(paste("the expected fraction is", alpha,",but the observed fraction is:", fract)) 
        },# END OF EXERCISE 
        
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        nine ={  
          ## EXERCISE 8: Let's see if the t-test is performing as expected when we use NON-NORMAL data
          #   INITIALIZE SOME VARIABLES TO HOLD OUR STATISTICAL GOODNESS
          Ps_W = numeric() #empty vector to hold p values
          Ps_T = numeric()
          fract_W = numeric() #emptry vector to hold the fration of p's <0.05
          fract_T = numeric() #emptry vector to hold the fration of p's <0.05
          second_mean<- numeric() #emptry vector to hold the value of the second mean
          
          Num<- 8 # Number of data points in the OVERALL set
          Max_diff_of_means <-10
          Std_dev <-1
          alpha <-0.05
          
          Num_per_set<-round(Num/2,0)  # Split the total number of points into two groups - needs to be an integer
          
          
          
          n_iter = 100 # number of times to run the t-test on radomly generated sets of data
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means
          
          
          
          # Run the t test on a set of data, repeat n_iter times
          for (m in 1:n_iter){ #number of different distances btw the two means = 1 up to n_iter
            
            try_at_mean = (m-1)/n_iter*Max_diff_of_means #vary the second mean from 0 to Max_diff_of_means  in n_iter steps
            second_mean = c(second_mean,try_at_mean) #build vector containing all of the means in the second distribution
            
            # Run the t test on a set of data, repeat n_iter times
            for (n in 1:n_iter){ #number of t tests to run
              
              Group1<-rlnorm(Num_per_set,meanlog = 0, sdlog = Std_dev)  
              Group2<-rlnorm(Num-Num_per_set,meanlog = second_mean[m], sdlog = Std_dev)
              
              # RUN THE Wilcox-U test on the Data
              WT_result<-wilcox.test(Group1,Group2) 
              # RUN THE t test on the SAME Data
              T_result<-t.test(Group1,Group2, var.equal=TRUE) # need to FORCE R to use the equal variance option
              
              Ps_W[n] = round(WT_result$p.value,3) #extract out the p-value and store in a vector
              Ps_T[n] = round(T_result$p.value,3) #extract out the p-value and store in a vector
              
            } # finish one t test (for (n in 1:n_iter))
            
            #if the p-value is <0.05 then the test finds a significant difference btw the two groups of data
            fract_W[m] = sum(Ps_W<=alpha)/n_iter #fraction of tests that have an p value below alpha  
            fract_T[m] = sum(Ps_T<=alpha)/n_iter #fraction of tests that have an p value below alpha
            
          }
          
          
          plot(second_mean,fract_W, main="fraction of data sets where the difference in the groups was SUCCESSFULY detected",xlab="distance between the two means",ylab="fraction",col="red")
          points(second_mean,fract_T, col="blue")
          legend("topleft", legend=c("The Wilcox test", "The t test"),col=c("red", "blue" ),lty=1)
          
          #  Let's see what the two distributions actually look like 
          
          plot(seq(-1,5*Max_diff_of_means,by=0.1),dlnorm(seq(-1,5*Max_diff_of_means,by=0.1)),
               main="Comparison of the distributions for Group1 and Group2",
               xlab="Values",ylab="fraction", col="red")
          points(seq(-1,5*Max_diff_of_means,by=0.1),dlnorm(seq(-1,5*Max_diff_of_means,by=0.1),meanlog = log(Max_diff_of_means)),col="blue")
          legend("topleft", legend=c("Group 1", "Group 2"),col=c("red", "blue" ),lty=1)
          
        }# END OF EXERCISE 
        
        
        
        
        
        
        
) # end of the switch statement
