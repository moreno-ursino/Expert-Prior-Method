The scripts in this folder are the ones used to obtain the results in our paper. The scripts can be run in all operating system. R must be installed with the following libraries: rstan (we used rstan_2.6.0), mvtnorm, stats4.  

We worked on the following environment:
R version 3.2.1 (2015-06-18)Platform: x86_64-unknown-linux-gnu (64-bit)

Folders:

*	**Stan_models**: the Stan models used in the analysis are grouped in this folder. The user must to add them in the same folder of the R analysis session. 
    * betareg_noninf.stan is the model for the regression with non-informative prior distributions used to compute the first weight scheme;
    * hyperparam.stan is the model to estimate the hyper-parameters of Method 2;
    * mixture_prior_integration.stan is the model to do regression for Method 1 and 2.

*	**Data**: data used in the work are collected in this folder. 
    * endox.txt: number of stickers for each expert in each interval for cyclophosphamide;
    * mmf.txt: number of stickers for each expert in each interval for MMF;
    * experts_table.txt: table of experts’ covariates;
    * gen_scenario.R: script to generate scenarios for simulations; scenario 1 is given as example.

*   **Simulations**: scripts used to run our simulations.
    * analyse.r: first script to run, to read the experts’ data and to find the fitted beta distributions for each expert and for each arm;
    * graphique_experts.r: to plot the marginal histograms and the fitted beta distributions;
    * regression_mugamma.R: to estimate Sigma and the regression coefficients for Method 2; 
    * 3dpriors_bis.R: to plot the resulting prior distribution in case of equal weight;
    * run_sim.R: to run simulations in the chosen scenario (the one previously generated);
    * sensitivity_script.R: to perform the sensitivity analysis with phi and lambda;
    * analysis_sensitivity.R: to extract the results;
    * run_sim_rho.R: to see the impact of the correlation parameter in Sigma.

How to use the scripts to perform a clinical trial analysis
1.	Update the table of experts’ covariates and the sticker counting from the elicitation process (files: endox.txt, mmf.txt, experts_table.txt)
2.	Use the analyse.r to fit beta distributions to histograms (take care about changing the number of experts) and store the mu and gamma obtained.
3.	Chose the hyperparameters for Sigma or run regression_mugamma.R to estimate the hyperparameters for Method 2.
4.	Chose the weight scheme and define the weight vector w.
5.	Define y1 and y2 as the response for the first and the second arm.
6.	Update sensitivity_script.R with these data and run it. 
7.	Use analysis_sensitivity.R to extract the posterior results.


For any issue in updating the models for new version of rstan, please contact moreno.ursino@gmail.com.
