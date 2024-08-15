# Sanctions and Russian Online Prices

## Steps for replication
1. Merge data and code repositories
2. Run TPD script for CPI and Stock data, saving to DB (remember to change flag in the file) -\> `tpd_index_calculation.R`. If some aggregation is too large for your RAM, you may split the data according to brand and reconsolidate the index
3. Run BEAST on all indexes for CPI and stock (remember to change flag in the file) --\> `beast.R`
4. Merge official CPI data in the DB tables --\> `merge_official.py`
5. Calculate differences between webscraping index and official data; Imputation of missing webscraping data points via Kalman Smoothing; cointegration test betwen webscraping and official index; stationarity test on differences; mape, malpe, and t-test pre-post; -\> `differences.R`
6. Calculate the difference between CPI from webscraping and trend projections before the war start --\> `beast2.R` and `diff_analysis.R` , second one also includes images
7. Detailed analysis of differences between excess CPI for official data and webscraping data, including scatterplot graph --\> `diff_analysis_detail.R`
8. Causality between sanctions and CPI and PSI breaks for each COICOP category --\> `new_causality.R`
9. Causality between sanctions and Forex breaks --\> `causality_sanctions_forex.R`
10. Causality between Forex breaks and CPI and PSI breaks for each COICOP category --\> `forex_causality.R`
11. Remake BEAST to derive only positive breakpoints in the CPI --\> `beast_positive.R`
12. Causality between sanctions and positive CPI breaks for each COICOP category --\> `new_causality_positive.R`
13. Causality between sanctions and positive Forex breaks --\> `causality_sanctions_forex_positive.R`
14. Causality between Forex positive breaks and CPI positive breaks for each COICOP category --\> `forex_causality_positive.R`
15. Causality between sanctions and excess inflation --\> `sanctions_excess_inflation.R`
16. Causality between Forex positive breaks and excess inflation --\> `forex_causality_excess_inflation.R`
17. Asked from reviewer: Causality between interest rate breaks and forex breaks (and reverse) --\> `causality_interest_breaks_forex.R`
18. Asked from reviewer: Causality between sanctions and interest rate breaks  --\> `causality_sanctions_interest_breaks.R`
19. Asked from reviewer: Causality between interest rate breaks and positive CPI breaks --\> `causality_interest_breaks_positive_breaks.R`
20. Asked from reviewer: Causality between interest rate breaks and PSI breaks --\> `causality_interest_breaks_stock_breaks.R`
21. DiD between Champagne and Prosecco: run `did_preparation.py` --\> generates base data for DiD analysis
22. DiD between Champagne and Prosecco: run `champagne_did.R`
23. ARDL test for cointegration `ardl_test.R`
24. Maximum entrpy bootstrap for stationarity `bootstrap_me.R` and `bootstrap_me_kpss.R`
25.  Folder LLM: file `sanctions_match.R` to pre-process sanction data and file `direct_sanctions.py` to classify brands according to sanctions exposure
26.  Folder LLM: file `brand_product_matching.R` to merge prices with sanctions and origin information
27.  Folder LLM: file `cpi_psi_sanctions.R` to calculate disaggregated CPI/PSI with interacted TPD and make graphs
28. Rosstat procedure: scripts in folder `rosstat_procedure`
29. Comparison with Italian products: script in folder `italian_match_products`
30. Simple correlation between official and WS-CPI: script `simple_correlation.R`

