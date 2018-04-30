#--------------------------------------------#
# R CODE FOR ENTRY MODEL - BERRY (1992)
# Mai-Anh Dang, Hoang Tran, Xiahua Wang
# M2 EEE - Toulouse School of Economics
# April, 2018
#--------------------------------------------#


Code files:
-------------------------
1. [entry_mle_simplified.R] 
estimate entry model by MLE (for heter, no heter, no cor) 
Column (1-3), Table 2 of the report

2. [entry_simulated.R]
estimate entry model by Simulated MLE
Column (4), Table 2 of the report

3. [predictions_entry.R]
predict the number of entering firms
Table 3 of the report

4. [ols_probit_regression.do]
estimate the OLS and Probit model
Table 2 & 3 of the report

5. [ols_probit_prediction.do]
predict the number of entering firms
Table 5 of the report

6.[airpresence_route_firm.do] and [firm_city_operate.do]
files to clean data from the orginal files


Data files:
-------------------------
1. [markets_wide_new.csv]
The final data used for the estimating the OLS, in the wide format

2. [markets_long_new.csv]
The final data used for the estimating the Probit, in the long format

3. [566275675_T_DB1B_MARKET.csv]
CSV data files in raw_data folder includes the DB1B market dataset from 2001 Q1 to Q4. (Note the names are the same for four quarters, so rename the files manually before processing the raw data)
