# CARRoT
CARRoT: R package for predictive modelling by means of regression adjusted for multiple regularisation methods
The package itself is available at https://cran.r-project.org/web/packages/CARRoT/index.html

carrot_function_rsq.r - the original CARRoT script. Dependencies: doParallel, nnet

carrot_functions_mod_si_las.r - the file allowing to compare CARRoT output with the output of the models constituted by only statistically significant predictors and lasso regression. Dependencies: car, afex, nnet, doParallel

carrot_functions_mod_si_las_ada.r - the file allowing to compare CARRoT output with the output of the models constituted by only statistically significant predictors and adaptive lasso regression. Dependencies: car, afex, nnet, doParallel, glmnet

carrot_functions_mod_step_las.r - the file allowing to compare CARRoT output with the output of the models constituted by onlystepwise forward selection based on Akaike Information Criteria. Dependencies:car, afex, nnet, doParallel


table3_carrot.r - script to replicate Table 11 from the paper. Dependencies: same as for carrot_function_rsq.r plus the packages containing the datasets required for reproduction (listed in Table 11 of the paper)

carrot_table_new.r - script to replicate the results of Tables S1 and S2. Dependencies: same as forcarrot_functions_mod_si_las.r plus the packages containing the datasets required for reproduction (listed in S1 and S2 Tables of the paper)

carrot_table_new_ada.r - script to replicate the results of S1 and S2 Tables for stepwise forward selection. Dependencies: same as carrot_functions_mod_step_las.r plus the packages containing the datasets required for reproduction (listed in S1 and S2 Tables of the paper)

License GPL-2
