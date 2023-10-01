
 In order to reproduce the results presented in the article entitled "Robust bivariate random-effects model for accommodating 
 outlying and influential studies in meta-analysis of diagnostic test accuracy studies," follow the following steps.

 1. To reproduce the figures given in Sections 2 and 5, run the R code in the "RealData_Results.R" file.

   - to do so, you need to load the source R files "RealData_Functions_BNNM.R," "RealData_Functions_BMSOM.R," and "RealData_Functions.R".

   - thus, save "RealData_Functions_BNNM.R," "RealData_Functions_BMSOM.R," and "RealData_Functions.R" files in the same folder
     as the "RealData_Results.R" file.

 2. To reproduce the tables presented in Section 4, run the R code in the "Simulation_Results_BL.R" and "Simulation_Results_BN.R" to
   reproduce the simulation results when assuming the bivariate Laplace and Normal distribution, respectively. Note that we used parallel computing.

   - to do so, you need to load both the "Simulation_Functions_BL.R" and "DataGeneration_BL.R" file to run the code in the "Simulation_Results_BL.R" file,
     and load the "Simulation_Functions_BN.R" and "DataGeneration_BN.R" file to run the code in the "Simulation_Results_BN.R" file.

   - thus, save all the appropriate files in the same folder as the main files - "Simulation_Results_BL.R" and "Simulation_Results_BN.R."


 *** Please forward any questions you may have to the authors without hesitation. *** 