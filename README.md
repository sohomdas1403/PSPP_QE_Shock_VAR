***Macroeconomics\_Group\_Project.R:***



This script runs the estimation of the VAR model, plots respective IRFs and the isolated resultant QE Shocks, and the correlation graph with financial variables for the first-differences specification. All plots created after running this code are saved in the "Macroeconomics Project Data" folder.



***Macroeconomics\_Group\_Project (Levels).R:***



This script runs the estimation of the VAR model, plots respective IRFs and the isolated resultant QE Shocks, and the correlation graph with financial variables for the first-levels specification. All plots created after running this code are also saved in the "Macroeconomics Project Data" folder. All plots with names including "\_levels" come from running this R script and come from the levels specification.





*Note that the two R files can be run in any order — run "**Macroeconimics\_Group\_Project.R"** if you wish to yield results from the first-differences specification, and run "<b>Macroeconomics\_Group\_Project (Levels).R"</b>* *if you wish to yield results from the levels specification. If you wish to replicate all the results referenced and shown in the report, you must run both.*

**Abstract:**
This study aims to analyze the macroeconomic effects of the European Central Bank’s (ECB) quantitative easing (QE) measures taken through the Public Sector Purchase Programme (PSPP). We do so by estimating a VAR(1) model including Euro area PSPP purchases, ECB deposit facility rates, industrial production indices, unemployment, and inflation as endogenous regressors. The impulse response functions show statistically insignificant responses of the endogenous variables to a one-standard-deviation PSPP shock across all horizons. Subsequently, we isolate the clean structural QE shocks from the PSPP purchases through Cholesky decomposition, and study their correlation with key financial variables selected from the ECB’s Altavilla dataset.


