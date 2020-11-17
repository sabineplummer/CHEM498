# rename the script to: LAST NAME_ID# 
#  You will need to submit this script as part of the assignment.  Put your name/ID number below
#  NAME:  Sabine Plummer       ID#: 40087050
# Clear all variables out of the R environment - good for cleaning-up BAD if you want to keep temporary calcuations big data in memory etc...
rm(list=ls())

# # Clear all plots  YES ALL PLOTS
# 
 result = tryCatch({
   graphics.off()
#  
 }, warning = function(w) {
   message("no plots to clear")
 }, error = function(e) {
   message("no plots to clear - but no problem!")
 }, finally = {
   
 })

Exercise<-"nine" # ("one", "two", "three", "four" etc.)  LOWERCASE ONLY

print(paste("RUNNING CODE FOR EXERCISE",Exercise))
print("",quote=FALSE)
print("",quote=FALSE)
switch (Exercise,  # the SWITCH statement allows you to run selected pieces of code depending on the value of the "Exercise" (one, two, three, four)
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        
        one = {  # If Exercise = one then do the following 
          set.seed(567)
          Num<- 8 # Number of data points in the OVERALL set
          
          ## Build two separate groups of data and bring them together into one set of data
          
          ## GROUP 1
          G1_mean<-0
          SD1<- 1
          
          ## GROUP 2
          offset <- 5
          G2_mean<-G1_mean+offset # Set the difference between the two means
          SD2<-1
          
          
          
          Group1<-rnorm(Num, G1_mean, sd = SD1)  # create a data set of random numbers that follow the Normal distribution
          Group2<-rnorm(Num, G2_mean, sd = SD2)  # create a data set of random numbers that follow the Normal distribution
          ##  finish building two groups of data
          
          mean_G1<-mean(Group1)
          mean_G2<-mean(Group2)
          
          Dist_type<-paste("two separate normals with delta mean:",G2_mean-G1_mean)
          
          bins=(2*Num)^0.6
          hist(Group1, bins, col=rgb(1,0,0,0.5), xlim=c(min(c(Group1,Group2))-1,max(c(Group1,Group2))+1),main="Group 1 and Group2", xlab="measured variable")
          hist(Group2, bins, col=rgb(0,0,1,0.5), add=T)
          
          # Run the t-test
          
          t_result <-t.test(Group1, Group2,alternative = "two.sided",var.equal = TRUE)
          
          
          # the confidence interval is centered at (mean(group1)-mean(Group1))
          # the range of the confidence interval is (mean(group1)-mean(Group1)) +/- t* SD(mean(Group1)-mean(Group2))
          # when deciding whether (or not) to reject the null hypothesis, based on the confidence interval, look to see if the null hypothesized value (0) is inside the confidence interval
          
          #A couple of ways of calculating the center of the CI
          center_CI<-(t_result$conf.int[1]-t_result$conf.int[2])/2+t_result$conf.int[2]
          alt_center_CI<-mean(t_result$conf.int)
          expected_center_CI<-(mean_G1-mean_G2)
          
          #print(paste("FYI the center of the CI is: ",round(center_CI,3), "or:",round(alt_center_CI,3), " and is expected at:",round(expected_center_CI,3)),quote=FALSE)
          
          print(paste("R's t calc is: ",round(t_result$statistic,3),
                      "with a calculated p-value of:",t_result$p.value), quote=FALSE)
          print(paste("The CI extends from:",round(t_result$conf.int[1],3),"to",round(t_result$conf.int[2],3),
                      "centered at:",round(alt_center_CI,3)), quote=FALSE)
          print(paste("The test is run using a null hypothesis of: mean(Group1)-mean(Group2) =",t_result$null.value),quote=FALSE)
          print("Does the null hypothesis value lie inside the CI?", quote=FALSE)
          
          
          
          # How many times would you expect to have this test "fail" to detect the difference in means?
          # How many times would you expect to have this test "fail" to when there is in fact no difference in the means?
          
        },  # END OF EXERCISE one
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
        
        two = {  # If Exercise = two then do the following 
          ## EXERCISE: run the t test for difference of means on Normal data MANY TIMES
          # We are going to run multiple t tests on data sets with a known difference in means
          # We will then vary the difference in the means and repeat
          # we want to discover the ability of the t test to detect the fact that the two groups have different means
          
          # Need to initialize some variables for later use - DON'T fiddle with these!
          Ps = numeric() #empty vector to hold p values - can plot a histogram if you want
          second_mean = numeric() #initalize the variable
          fract = numeric() #emptry vector to hold the fration of p's <0.05
          # Done initalizing varibles
          
          Num<- 8 # Number of data points in each group
          alpha <-0.05
          
          G1_mean<-0
          SD1<- 1
          
          Max_diff_of_means <-5
          second_mean = G1_mean+Max_diff_of_means 
          G2_mean<-numeric()  # just initialize for now
          SD2<-1
          
          
          ## CAREFUL  SETTING n_iter >200 can really slow the system down!!
          n_iter = 100 #serves two purposes 1- #of times to do the SW test 2-# of steps for the second mean
          G2_mean =seq(G1_mean, to = second_mean, length.out = n_iter)
          #build vector containing all of the means in the second distribution
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means  
          for (m in 1:n_iter){ #number of different distances btw the two means 
            
            # Run the test on a set of data, repeat n_iter times
            for (n in 1:n_iter){ #number of SW tests to run
              #The syntax is: rnorm(n, mean = X, sd = Y)
              #build one set at mean = 0 and SD=1 and a second at mean = "G2_mean[m]" 
              
              
              Group1<-rnorm(Num, G1_mean, sd = SD1)  # create a data set of random numbers that follow the Normal distribution
              Group2<-rnorm(Num, G2_mean[m], sd = SD2)  # create a data set of random numbers that follow the Normal distribution
              
              # RUN THE test on the Data
              t_result <-t.test(Group1, Group2,alternative = "two.sided",var.equal = TRUE)
              
              
              Ps[n] = t_result$p.value #extract out the p-value and store in a vector
              
            } # finish one test (for (n in 1:n_iter))
            
            
            #if the p-value is <0.05 then the test finds a significant difference btw the means
            fract[m] = sum(Ps<alpha)/n_iter #fraction of tests that found the two means differed
            
          }
          
          plot(G2_mean,fract, main="fraction of data sets where the difference between the means was detected",xlab="distance between the two means",ylab="fraction")
          
          
        },  # END OF EXERCISE two
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        three = {  # If Exercise = three then do the following 
          
          # Need to initialize some variables for later use - DON'T fiddle with these!
          Ps = numeric() #empty vector to hold p values - can plot a histogram if you want
          second_mean = numeric() #initalize the variable
          fract = numeric() #emptry vector to hold the fration of p's <0.05
          # Done initalizing varibles
          
          Num<- 8 # Number of data points in each group
          alpha <-0.05
          
          G1_mean<-0
          SD1<- 1
          
          Max_diff_of_means <-5
          second_mean = G1_mean+Max_diff_of_means 
          G2_mean<-numeric()  # just initialize for now
          SD2<-1
          
          
          ## CAREFUL  SETTING n_iter >200 can really slow the system down!!
          n_iter = 100 #serves two purposes 1- #of times to do the SW test 2-# of steps for the second mean
          G2_mean =seq(G1_mean, to = second_mean, length.out = n_iter)
          #build vector containing all of the means in the second distribution
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means  
          for (m in 1:n_iter){ #number of different distances btw the two means 
            
            # Run the test on a set of data, repeat n_iter times
            for (n in 1:n_iter){ #number of SW tests to run
              #The syntax is: rnorm(n, mean = X, sd = Y)
              #build one set at mean = 0 and SD=1 and a second at mean = "G2_mean[m]" 
              
              
              Group1<-rnorm(Num, G1_mean, sd = SD1)  # create a data set of random numbers that follow the Normal distribution
              Group2<-rnorm(Num, G2_mean[m], sd = SD2)  # create a data set of random numbers that follow the Normal distribution
              
              # ADD AN OUTLIER (most often, this finds the maximum deviation in the set, makes it MUCH larger, and puts it into the first element)        
                     Group2[1]<-max(abs(Group2-G2_mean[m]))*SD2*5+G2_mean[m]  # replace the first element with an outlier that has a deviation 5 SD's bigger
              
              # RUN THE test on the Data
              t_result <-t.test(Group1, Group2,alternative = "two.sided",var.equal = TRUE)
              
              
              Ps[n] = t_result$p.value #extract out the p-value and store in a vector
              
            } # finish one test (for (n in 1:n_iter))
            
            
            #if the p-value is <0.05 then the test finds a significant difference btw the means
            fract[m] = sum(Ps<alpha)/n_iter #fraction of tests that found the two means differed
            Ps = numeric() #clear the vector for next round
          }
          
          plot(G2_mean,fract, main="fraction of data sets where the difference between the means was detected",xlab="distance between the two means",ylab="fraction") 
          
        },  # END OF EXERCISE 
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        four = {  # If Exercise = four then do the following 
          # Need to initialize some variables for later use - DON'T fiddle with these!
          Ps_W = numeric() #empty vector to hold p values
          Ps_T = numeric()
          fract_W = numeric() #emptry vector to hold the fration of p's <0.05
          fract_T = numeric() #emptry vector to hold the fration of p's <0.05
          
          second_mean = numeric() #initalize the variable
          
          # Done initalizing varibles
          
          Num<- 5 # Number of data points in each group
          alpha <-0.05
          
          G1_mean<-0
          SD1<- 1
          
          Max_diff_of_means <-5
          second_mean = G1_mean+Max_diff_of_means 
          G2_mean<-numeric()  # just initialize for now
          SD2<-1
          
          
          ## CAREFUL  SETTING n_iter >200 can really slow the system down!!
          n_iter = 100 #serves two purposes 1- #of times to do the SW test 2-# of steps for the second mean
          G2_mean =seq(G1_mean, to = second_mean, length.out = n_iter)
          #build vector containing all of the means in the second distribution
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means  
          for (m in 1:n_iter){ #number of different distances btw the two means 
            
            # Run the test on a set of data, repeat n_iter times
            for (n in 1:n_iter){ #number of SW tests to run
              #The syntax is: rnorm(n, mean = X, sd = Y)
              #build one set at mean = 0 and SD=1 and a second at mean = "G2_mean[m]" 
              
              
              Group1<-rnorm(Num, G1_mean, sd = SD1)  # create a data set of random numbers that follow the Normal distribution
              Group2<-rnorm(Num, G2_mean[m], sd = SD2)  # create a data set of random numbers that follow the Normal distribution
              
              #ADD AN OUTLIER (most often, this finds the maximum deviation in the set, makes it MUCH larger, and puts it into the first element)              
              Group2[1]<-max(abs(Group2-G2_mean[m]))*SD2*15+G2_mean[m]  # replace the first element with an outlier that has a deviation 5 SD's bigger
              
              
              # Should you run the t-test with/without ver.equal = TRUE or =FALSE??
              equal_var <-NA
              
              
              # RUN THE test on the Data
              t_result <-t.test(Group1, Group2,alternative = "two.sided",var.equal = TRUE)
              
              # RUN THE Wilcox-U test on the Data
              wt_result<-wilcox.test(Group1,Group2) 
              
              
              Ps_W[n] = round(wt_result$p.value,3) #extract out the p-value and store in a vector
              Ps_T[n] = round(t_result$p.value,3) #extract out the p-value and store in a vector
              
              
              
            } # finish one test (for (n in 1:n_iter))
            
            
            #if the p-value is <0.05 then the test finds a significant difference btw the means
            
            fract_W[m] = sum(Ps_W<=alpha)/n_iter #fraction of tests that have an p value below alpha  
            fract_T[m] = sum(Ps_T<=alpha)/n_iter #fraction of tests that have an p value below alpha
            
          }
          
          
          
          
          
          plot(G2_mean,fract_W, main="fraction of data sets where the difference between the means was detected",xlab="distance between the two means",ylab="fraction",col="red")
          points(G2_mean,fract_T, col="blue")
          legend("topleft", legend=c("The Wilcox test", "The t test"),col=c("red", "blue" ),lty=1)
          
        },  # END OF EXERCISE 
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        five ={  
          # Need to initialize some variables for later use - DON'T fiddle with these!
          Ps_W = numeric() #empty vector to hold p values
          Ps_T = numeric()
          fract_W = numeric() #emptry vector to hold the fration of p's <0.05
          fract_T = numeric() #emptry vector to hold the fration of p's <0.05
          
          second_mean = numeric() #initalize the variable
          
          # Done initalizing varibles
          
          Num<- 5 # Number of data points in each group
          alpha <-0.05
          
          G1_mean<-0
          SD1<- 1
          
          Max_diff_of_means <-5
          second_mean = G1_mean+Max_diff_of_means 
          G2_mean<-numeric()  # just initialize for now
          SD2<-1
          
          
          ## CAREFUL  SETTING n_iter >200 can really slow the system down!!
          n_iter = 100 #serves two purposes 1- #of times to do the SW test 2-# of steps for the second mean
          G2_mean =seq(G1_mean, to = second_mean, length.out = n_iter)
          #build vector containing all of the means in the second distribution
          
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means  
          for (m in 1:n_iter){ #number of different distances btw the two means 
            
            # Run the test on a set of data, repeat n_iter times
            for (n in 1:n_iter){ #number of SW tests to run
              #The syntax is: rnorm(n, mean = X, sd = Y)
              #build one set at mean = 0 and SD=1 and a second at mean = "G2_mean[m]" 
              ## Build two separate groups of data and bring them together into one set of data
              
              Group1<-rlnorm(Num,meanlog = G1_mean, sdlog = SD1)  
              Group2<-rlnorm(Num,meanlog = G2_mean[m], sdlog = SD2)
              
              # Should you run the t-test with/without ver.equal = TRUE or =FALSE??
              equal_var <-NA
              
              
              # RUN THE test on the Data
              t_result <-t.test(Group1, Group2,alternative = "two.sided",var.equal = FALSE)
              
              # RUN THE Wilcox-U test on the Data
              wt_result<-wilcox.test(Group1,Group2) 
              
              
              Ps_W[n] = round(wt_result$p.value,3) #extract out the p-value and store in a vector
              Ps_T[n] = round(t_result$p.value,3) #extract out the p-value and store in a vector
              
              
              
            } # finish one test (for (n in 1:n_iter))
            
            #if the p-value is <0.05 then the test finds a significant difference btw the means
            
            fract_W[m] = sum(Ps_W<=alpha)/n_iter #fraction of tests that have an p value below alpha  
            fract_T[m] = sum(Ps_T<=alpha)/n_iter #fraction of tests that have an p value below alpha
            
          }
          
          
          ## let's have a look at the raw data  BUT increase the number of points DRAMATICALLY so we can see the shape of the distributions
          Num<-10*Num
          Group1<-rlnorm(Num,meanlog = G1_mean, sdlog = SD1)  
          Group2<-rlnorm(Num,meanlog = G2_mean[m], sdlog = SD2)
          bins=(2*Num)^0.6
          Nim<-Num/10  # restore the value for num in case we use it later
          
          hist(Group1, bins, col=rgb(1,0,0,0.5), xlim=c(min(c(Group1,Group2))-1,max(c(Group1,Group2))+1),main="Group 1 and Group2", xlab="measured variable")
          hist(Group2, 2*bins, col=rgb(0,0,1,0.5), xlim=c(min(c(Group1,Group2))-1,max(c(Group1,Group2))+1),add=T)
          
          
          plot(G2_mean,fract_W, main="fraction of data sets where the difference between the means was detected",xlab="distance between the two means",ylab="fraction",col="red")
          points(G2_mean,fract_T, col="blue")
          legend("topleft", legend=c("The Wilcox test", "The t test"),col=c("red", "blue" ),lty=1)
          
        },  # END OF EXERCISE 
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        six ={  
          
          
          # Need to initialize some variables for later use - DON'T fiddle with these!
          Ps_W = numeric() #empty vector to hold p values
          Ps_T = numeric()
          fract_W = numeric() #emptry vector to hold the fration of p's <0.05
          fract_T = numeric() #emptry vector to hold the fration of p's <0.05
          
          second_mean = numeric() #initalize the variable
          
          # Done initalizing varibles
          
          Num<- 4 # Number of data points in each group
          alpha <-0.05
          
          G1_mean<-0
          SD1<- 1  # OKAY NOT REALLY THE SD WHEN USING A UNIFORM DISTRIBUTION  See the runif call below to see how its actually used
          
          Max_diff_of_means <-5
          second_mean = G1_mean+Max_diff_of_means 
          G2_mean<-numeric()  # just initialize for now
          SD2<-1 # OKAY NOT REALLY THE SD WHEN USING A UNIFORM DISTRIBUTION  See the runif call below to see how its actually used
          
          
          ## CAREFUL  SETTING n_iter >200 can really slow the system down!!
          n_iter = 100 #serves two purposes 1- #of times to do the SW test 2-# of steps for the second mean
          G2_mean =seq(G1_mean, to = second_mean, length.out = n_iter)
          #build vector containing all of the means in the second distribution
          
          
          ## 1 create a loop that runs n_iter times.  Each time test how successful the SW test is at a given difference of means  
          for (m in 1:n_iter){ #number of different distances btw the two means 
            
            # Run the test on a set of data, repeat n_iter times
            for (n in 1:n_iter){ #number of SW tests to run
              
              
              #The syntax is: runif(n, min = 0, max = 1)
              Group1<-rnorm(Num, G1_mean, sd = SD1)  # create a data set of random numbers that follow the Normal distribution
              Group2<-runif(Num,G2_mean[m]-3*SD2,G2_mean+3*SD2)
              #      Group2[1]<-max(Group2-G2_mean[m])*SD2*5+G2_mean[m]  # replace the first element with an outlier that has a deviation 5 SD's bigger
              
              
              
              # Should you run the t-test with/without ver.equal = TRUE or =FALSE??
              equal_var <-NA
              
              
              # RUN THE test on the Data
              t_result <-t.test(Group1, Group2,alternative = "two.sided",var.equal = TRUE)
              
              # RUN THE Wilcox-U test on the Data
              wt_result<-wilcox.test(Group1,Group2) 
              
              
              Ps_W[n] = round(wt_result$p.value,3) #extract out the p-value and store in a vector
              Ps_T[n] = round(t_result$p.value,3) #extract out the p-value and store in a vector
              
              
              
            } # finish one test (for (n in 1:n_iter))
            
            
            
            #if the p-value is <0.05 then the test finds a significant difference btw the means
            
            fract_W[m] = sum(Ps_W<=alpha)/n_iter #fraction of tests that have an p value below alpha  
            fract_T[m] = sum(Ps_T<=alpha)/n_iter #fraction of tests that have an p value below alpha
            
          }
          
          
          ## let's have a look at the raw data  BUT increase the number of points DRAMATICALLY so we can see the "shape" of the data
          Num<-40*Num
          Group1<-rnorm(Num, G1_mean, sd = SD1)  # create a data set of random numbers that follow the Normal distribution
          Group2<-runif(Num,G2_mean[m]-3*SD2,G2_mean[m]+3*SD2)
          bins=(2*Num)^0.6
          
          Num<-Num/40 # restore the original value
          
          
          hist(Group1, bins, col=rgb(1,0,0,0.5), xlim=c(min(c(Group1,Group2))-1,max(c(Group1,Group2))+1),main="Group 1 and Group2", xlab="measured variable")
          hist(Group2, 2*bins, col=rgb(0,0,1,0.5), xlim=c(min(c(Group1,Group2))-1,max(c(Group1,Group2))+1),add=T)
          
          
          plot(G2_mean,fract_W, main="fraction of data sets where the difference between the means was detected",xlab="distance between the two means",ylab="fraction",col="red")
          points(G2_mean,fract_T, col="blue")
          legend("topleft", legend=c("The Wilcox test", "The t test"),col=c("red", "blue" ),lty=1)
        }, # END OF EXERCISE 
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        seven ={  
          
          # MEET THE ANOVA test
          
          Num<- 10
          G_means <-c(2,2.2,3.5)  # a vector holding the group means in order
          G_SDs <-c(1,1,1)
          
          # Store each groups data (the dependant data) in a vector called "Variable"
          Variable<-c((rnorm(Num, G_means[1], sd = G_SDs[1])),(rnorm(Num, G_means[2], sd = G_SDs[2])),(rnorm(Num, G_means[3], sd = G_SDs[3])))  # create a data set of random numbers that follow the Normal distribution
          # Store the independent data in the "condition" vector named cond
          cond<-c(rep(1,10),rep(2,10),rep(3,10))
          
          
          test_data<-data.frame(Variable,cond) # assemble into a simple data frame
          
          
          # run the ANOVA using pretty much every default setting 
          A_results<-aov(Variable~cond, data = test_data)
          Compact_p<-summary(A_results)
          print(Compact_p)
          p_val<-Compact_p[[1]]$`Pr(>F)`[1]
          
          f_val<- Compact_p[[1]]$`F value`[1]
          
          print(paste("the p-value is:",round(p_val,6),"the F-ratio is:",round(f_val,2)))
          
          Data_together<-list((rnorm(Num, G_means[1], sd = G_SDs[1])),(rnorm(Num, G_means[2], sd = G_SDs[2])),(rnorm(Num, G_means[3], sd = G_SDs[3])))  # create a data set of random numbers that follow the Normal distribution
          boxplot(Data_together)
          
          
        },# END OF EXERCISE 
        
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        eight ={  
          # MEET THE Kruskal-Wallis test
          
          Num<- 10
          G_means <-c(2,2.2,3.5)  # a vector holding the group means in order
          G_SDs <-c(1,1,1)
          
          # Store each groups data (the dependant data) in a vector called "Variable"
          Variable<-c((rnorm(Num, G_means[1], sd = G_SDs[1])),(rnorm(Num, G_means[2], sd = G_SDs[2])),(rnorm(Num, G_means[3], sd = G_SDs[3])))  # create a data set of random numbers that follow the Normal distribution
          # Store the independent data in the "condition" vector named cond
          cond<-c(rep(1,10),rep(2,10),rep(3,10))
          
          
          test_data<-data.frame(Variable,cond) # assemble into a simple data frame
          
          
          # run the Kruskal-Wallis test using pretty much every default setting 
          
          
          KW_results<-kruskal.test(Variable~cond, data = test_data)
          
          p_val<-KW_results$p.value
          
          KW_val<- KW_results$statistic
          
          print(paste("the p-value is:",round(p_val,6),"the KW Statistic is:",round(KW_val,2)))
          
          Data_together<-list((rnorm(Num, G_means[1], sd = G_SDs[1])),(rnorm(Num, G_means[2], sd = G_SDs[2])),(rnorm(Num, G_means[3], sd = G_SDs[3])))  # create a data set of random numbers that follow the Normal distribution
          boxplot(Data_together)
          
          pairwise.wilcox.test(KW_results)
          
        },# END OF EXERCISE 
        
        
        #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        nine ={  
          #  NAME: Sabine Plummer
          #  ID#: 40087050
          #  EMAIL ADDRESS: sabine.plummer@mail.concordia.ca
          
          #  DO THE ASSIGNMENT CODING IN HERE!!! 
        
                 # Need to initialize some variables for later use 
                Ps_A = numeric() #empty vector to hold p values
                Ps_K = numeric() #empty vector to hold p values
                fract_A = numeric() #emptry vector to hold the fration of p's <0.05
                fract_K = numeric() #emptry vector to hold the fration of p's <0.05
                third_mean = numeric() #initalize the variable
                # Done initalizing varibles
                
                Num <- 5 # Number of data points in each group
                alpha <- 0.05
                
                G1_mean <- 0
                SD1 <- 1
                
                G2_mean <- 0
                SD2 <- 1
                
                Max_diff_of_means <-5
                third_mean = ((G1_mean + G2_mean) / 2) + Max_diff_of_means 
                G3_mean <- numeric()  # initalizing varible
                SD3 <- 1
                
                
                n_iter = 100 # no. of times to do the test and no. of steps for the second mean
                G3_mean = seq(G1_mean, to = third_mean, length.out = n_iter)
                # build vector containing all of the means in the second distribution
                
                # 1 create a loop that runs n_iter times.  Each time test how successful the test is at a given difference of means  
                for (m in 1:n_iter){ #number of different distances btw the two means 
                        
                        # Run the test on a set of data, repeat n_iter times
                        for (n in 1:n_iter){ #number of SW tests to run
                                #The syntax is: rnorm(n, mean = X, sd = Y)
                         
                                # Store each groups data (the dependent data) in a vector called "Variable"
                                Variable <- c((rnorm(Num, G1_mean, sd = SD1)),(rnorm(Num, G2_mean, sd = SD2)),(rnorm(Num, mean = G3_mean[m], sd = SD3)))  # create a data set of random numbers that follow the Normal distribution
                                # Store the independent data in the "condition" vector named cond
                                cond<-c(rep(1,5),rep(2,5),rep(3,5))
                                
             
                                test_data<-data.frame(Variable,cond) # assemble into a simple data frame
                                
                                # RUN THE test on the Data
                                
                                # run the ANOVA using pretty much every default setting 
                                
                                A_results<-aov(Variable~as.factor(cond), data = test_data)
                                Compact_p<-summary(A_results)
                                p_val<-Compact_p[[1]]$`Pr(>F)`[1]

                                
                                # run the Kruskal-Wallis test using pretty much every default setting 
                                
                                KW_results<-kruskal.test(Variable~cond, data = test_data)
                                

                                Ps_A[n] = round(p_val,3) #extract out the p-value and store in a vector
                                Ps_K[n] = round(KW_results$p.value,3) #extract out the p-value and store in a vector
                                
                                
                        } # finish one test (for (n in 1:n_iter))
                        
                        #if the p-value is <0.05 then the test finds a significant difference btw the means
                        
                        fract_A[m] = sum(Ps_A<=alpha)/n_iter #fraction of tests that have an p value below alpha  
                        fract_K[m] = sum(Ps_K<=alpha)/n_iter #fraction of tests that have an p value below alpha
                        
                }
                
                plot(G3_mean,fract_A, main="fraction of data sets where the difference between the means was detected",xlab="distance between the means",ylab="fraction",col="red")
                points(G3_mean,fract_K, col="blue")
                legend("bottomright", legend=c("The Anova test", "The KW test"),col=c("red", "blue" ),lty=1)
        
                plot(TukeyHSD(A_results))
                print(pairwise.wilcox.test(Variable, cond))
                
        }# END OF EXERCISE nine
        
        
) # end of the switch statement
