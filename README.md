Case study in Business Analytics and Quantitative Marketing 


I. SQL codes provide guidance in collecting and cleaning the data(done by BigQuery)

In order to run the SQL codes of this section, we need to follow the order starting from A1 to A16. After each "A# Table:" the table name is presented. It's important that the table names are consistent through the codes since the tables created latter may depend on the previous tables.
We need to first run the code below the line "##A(step number) Table:(table name)" and then save the table with the corresponding (table name). 
The tables that we will use in our analysis are "trans_table_new2" and "tree_table_new2".        


II: R script contains the code of the main analysis(data visualization and data analysis)
In order to do RFM based segmentation during Holidays, we have to had the correct data set. We create transactions tables for the periods of December, Sinterklaas, Father's day and Easter for the years of 2017 and 2018. We create these tables from the trans_table_new, and these new tables have the name H_December_17/H_December_18, H_Sinterklaas_17/H_Sinterklaas_18, H_Fathersday_17/H_Fathersday_18 and H_Easter_17/H_Easter_18, respectively. Then in with R codes we calculated the RFM based segmentations and the results are represent in the Table 18.
Afterwards, through the tables in SQL, we calculated the RFM scores for each table. This code is the second part of each code of each Holiday. We seperate the codes with the sign "######". The new tables are H_December_17_rfm/H_December_18_rfm, H_Sinterklaas_17_rfm/H_Sinterklaas_18_rfm, H_Fathersday_17_rfm/H_Fathersday_18_rfm and H_Easter_17_rfm/H_Easter_18_rfm. Subsequently, with the use of R codes we calculated the K-means for each Holiday. The results can be found in Table 19-20-21-22.

The comments in the R file are given to explain all the steps. To run the R codes, we need to follow the comments and the order of the file. Please consult to the R file for detailed explanation.   
