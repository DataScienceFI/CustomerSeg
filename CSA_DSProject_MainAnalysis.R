# Installing and loading packages 
install.packages("bigrquery")
install.packages("readr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("rfm")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("VIM")
install.packages("mice")
install.packages("psych")
install.packages("cluster")
install.packages("scatterplot3d") 
install.packages("arsenal")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("flexmix")
install.packages("countreg", repos="http://R-Forge.R-project.org")
install.packages("amap")
install.packages("tidyr")
install.packages("clues")
library(bigrquery)
library(readr)
library(lubridate)
library(dplyr)
library(rfm)
library(ggplot2)
library(magrittr) 
library(VIM)
library(mice)
library(psych)
library(cluster)
library(scatterplot3d) 
library(arsenal)
library(rpart)
library(rpart.plot)
library(flexmix)
library(countreg)
library(tidyr)
library(clues)
library(amap)
#-----------------------------------------------------------------------------------------------------------
# Setting the project and filename
#-----------------------------------------------------------------------------------------------------------
filename <-"/content/datalab/notebooks/config.json"
token = readChar(filename, file.info(filename)$size)
set_service_token(token)
project = "edc-gall-en-gall"
analysis_date = lubridate::as_date('2018-12-31', tz = 'UTC') #setting the analysis date benchmark
#-----------------------------------------------------------------------------------------------------------
# Graph: The orders histogram
#-----------------------------------------------------------------------------------------------------------
sql3 <- "SELECT *
FROM `edc-gall-en-gall.data.figure33`"
order_data2 <- query_exec(sql3, project, use_legacy_sql = FALSE, max_pages = Inf)
orders=order_data2[,1]
customers=order_data2[,2]
newdata=order_data2[order(orders),]
newdata=newdata[1:20,]

ggplot(newdata, aes(x=orders, y=customers)) + geom_bar(stat = "identity")+geom_text(aes(label=customers), vjust=1.6, color="orange", size=3.5)

#-----------------------------------------------------------------------------------------------------------
# Imorting the demographic varaibles from Big Query table tree_table_new2
#-----------------------------------------------------------------------------------------------------------
sql10 = "SELECT cast(lkuid as string) as customer_id , cast(allow_analysis as string) as allow_analysis, gender as gender,
cast(opt_in_commercial_email as string)as opt_in,age as age,age_category as age_category,loyal_time as loyal_time
FROM `edc-gall-en-gall.data.tree_table_new2`
Order by lkuid"
demdata = query_exec(sql10, project, use_legacy_sql = FALSE, max_pages = Inf)
colnames(demdata) = names(demdata)
allow_analysis = demdata[,2]
gender = demdata[,3]
opt_in = demdata[,4]
age = demdata[,5] 
age_category = demdata[,6] #categorical age variable
loyal_time = demdata[,7]
#-----------------------------------------------------------------------------------------------------------
# Creating dummy varaible for GENDER from variable gender 1:female 0:male
#-----------------------------------------------------------------------------------------------------------
n = nrow(demdata)
GENDER = rep(0,n) 
for(i in 1:n){
  if(demdata[i,3] =="m"){ #replace 0's by 1 if not male
    GENDER[i] = 0
    
  }
  else if(demdata[i,3]=="f"){
    GENDER[i] = 1
  }
}
#-----------------------------------------------------------------------------------------------------------
# Defining the segments for customers for the benchmark method RFM-based Segmnentation
#-----------------------------------------------------------------------------------------------------------

segment_names <- c("BestCustomers", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising Customers", "Need Attention", "About To Sleep",
                   "At Risk", "Can not be lost" ,"Lost Customer")
recency_lower <-   c(4, 3, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <-   c(5, 5, 5, 5, 4, 3, 3, 2, 2, 1)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 2 ,1, 3, 2, 5, 5, 1)
monetary_lower <-  c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <-  c(5, 4, 3, 2, 1, 3, 2, 5, 5, 1)
#-----------------------------------------------------------------------------------------------------------
# Importing the transactional data from Big Query table trans_table_new2
#-----------------------------------------------------------------------------------------------------------
sql1 = "SELECT cast(lkuid as string) as customer_id, cast(trans_nr as string) as TransNo, trans_amount as revenue ,cast(date(betaal_datum)as string)as order_date
FROM `edc-gall-en-gall.data.trans_table_new2`
Order by lkuid "
order_data = query_exec(sql1, project, use_legacy_sql = FALSE, max_pages = Inf)
rfm_data_orders = data.frame(order_data[,1],as.Date(order_data[,4]), order_data[,3])
colnames(rfm_data_orders) = c("customer_id","order_date","revenue")
#-----------------------------------------------------------------------------------------------------------
# RFM-based Segmentation 
#-----------------------------------------------------------------------------------------------------------
rfm_order_result = rfm_table_order(rfm_data_orders,customer_id,order_date,revenue,analysis_date)
RFMsegments_main = rfm_segment(rfm_order_result, segment_names, recency_lower, recency_upper,frequency_lower, frequency_upper, monetary_lower, monetary_upper)
#Percentages per segment
table = RFMsegments_main %>% count(segment) %>%arrange(desc(n)) %>%rename(Segment = segment, Count = n)
total = sum(table[,2])
percentages = NULL
for(i in 1:nrow(table[,2])){
  percentages[i] = table[i,2]/total*100
}
table
percentages
# RFM graphs
rfm_heatmap(rfm_order_result,plot_title = "RFM Heat Map",plot_title_justify = 0.5, xaxis_title = "Frequency",yaxis_title = "Recency", legend_title = "Mean Monetary Value",brewer_n = 5, brewer_name = "Oranges")
rfm_bar_chart(rfm_order_result,bar_color = "gray23",xaxis_title = "Monetary Score", sec_xaxis_title = "Frequency Score",yaxis_title = " ", sec_yaxis_title = "Recency Score")
rfm_rm_plot(rfm_order_result,point_color = "gray23", xaxis_title = "Monetary",yaxis_title = "Recency", plot_title = "Recency vs Monetary")
rfm_fm_plot(rfm_order_result,point_color = "gray23", xaxis_title = "Monetary",yaxis_title = "Frequency", plot_title = "Frequency vs Monetary")
rfm_rf_plot(rfm_order_result,point_color = "gray23", xaxis_title = "Frequency",yaxis_title = "Recency", plot_title = "Recency vs Frequency")


#RFM-based segmentation on holidays only
#December 2017
sql1 = "SELECT cast(lkuid as string) as customer_id , cast(trans_nr as string) as TransNo, trans_amount as revenue ,cast(date(betaal_datum)as string)as order_date
FROM `edc-gall-en-gall.data.H_December_17`"
#December 2018
sql1 = "SELECT cast(lkuid as string) as customer_id , cast(trans_nr as string) as TransNo, trans_amount as revenue ,cast(date(betaal_datum)as string)as order_date
FROM `edc-gall-en-gall.data.H_December_18`"
#Sinterklaas 2017
sql1 = "SELECT cast(lkuid as string) as customer_id , cast(trans_nr as string) as TransNo, trans_amount as revenue ,cast(date(betaal_datum)as string)as order_date
FROM `edc-gall-en-gall.data.H_Sinterklass_17`"
#Sinterklaas 2018
sql1 = "SELECT cast(lkuid as string) as customer_id , cast(trans_nr as string) as TransNo, trans_amount as revenue ,cast(date(betaal_datum)as string)as order_date
FROM `edc-gall-en-gall.data.H_Sinterklass_18`"
#Easter 2017
sql1 = "SELECT cast(lkuid as string) as customer_id , cast(trans_nr as string) as TransNo, trans_amount as revenue ,cast(date(betaal_datum)as string)as order_date
FROM `edc-gall-en-gall.data.H_Easter_17`"
#Easter 2018
sql1 = "SELECT cast(lkuid as string) as customer_id , cast(trans_nr as string) as TransNo, trans_amount as revenue ,cast(date(betaal_datum)as string)as order_date
FROM `edc-gall-en-gall.data.H_Easter_18`"

#for each sql1 following code should be repeated
order_data1 = query_exec(sql1, project, use_legacy_sql = FALSE, max_pages = Inf)
rfm_data_orders1 = data.frame(order_data1[,1],as.Date(order_data1[,4]), order_data1[,3])
colnames(rfm_data_orders1) = c("customer_id","order_date","revenue")
rfm_Holidays = rfm_table_order(rfm_data_orders1,customer_id,order_date,revenue,analysis_date)
RFMsegments = rfm_segment(rfm_Holidays , segment_names, recency_lower, recency_upper,frequency_lower, frequency_upper, monetary_lower, monetary_upper)
#table with amount of customers per segment
table1 = RFMsegments %>%count(segment) %>%arrange(desc(n)) %>%rename(Segment = segment, Count = n)
total = sum(table1[,2])
percentages = NULL
for(i in 1:nrow(table1[,2])){
  percentages[i] = table1[i,2]/total*100
}
table1
percentages


#-----------------------------------------------------------------------------------------------------------
# Removing customers from Lost Customer segment
#-----------------------------------------------------------------------------------------------------------
alldata = data.frame(RFMsegments_main, allow_analysis, GENDER, age_category, opt_in,loyal_time)

#List of exceptionally best customers with RFM score 555
Excep_best = RFMsegments_main[RFMsegments_main[,3]=='555',1]
#List of customers from BEst Customer segment
Best_cust_data_index = as.numeric(rownames(alldata[alldata[,2] == 'BestCustomers',]))
Best_segment_id = RFMsegments_main[Best_cust_data_index,1]

lost_cust_data_index = as.numeric(rownames(alldata[alldata[,2] == 'Lost Customer',])) #indices of customers from Lost customer group
alldata = alldata[-lost_cust_data_index,] #new data without inactive customers
customerid = alldata[,1]
Frequency = alldata[,4] 
Recency = alldata[,5] 
Monetary = alldata[,6] 
Allow_analysis = alldata[,11]
Female = alldata[,12]
Age = alldata[,13]
Opt_in_com = alldata[,14]
Loyal_time = alldata[,15]
#scacling the R,F,M variables
Frequency_s = scale(Frequency) 
Recency_s = scale(Recency) 
Monetary_s = scale(Monetary) 
#------------------------------------------------------------------------------------------------------------------
#PCA analysis
#------------------------------------------------------------------------------------------------------------------
data_pca = data.frame(Recency,Frequency,Monetary)
colnames(data_pca) = c("Recency","Frequency","Monetary")
cov(data_pca) 
cor(data_pca) 
#pca with and without scaling
pca.data = prcomp(data_pca,scale = F)
pca.dataScale = prcomp(data_pca,scale = T) #with scaling
pca.dataScale$rotation
#the pca plots 
biplot(pca.data, cex = 0.6)
biplot(pca.dataScale, cex = 0.6)
eig = eigen(cor(data_pca))
screeplot(pca.dataScale, type="l", npcs = 3, main = NULL)
pve = rep(NA, dim(data_pca)[2]) # proportion of variance explained
for(i in 1:3)
{
  pve[i] = print(sum(eig$values[1:i])/3)
}
eig$values
pve
# generating new varaibles using the weights/loadings of pca
pca_Recency = pca.dataScale$rotation[1,1]*Recency_s 
pca_Frequency = (-1)*pca.dataScale$rotation[2,1]*Frequency_s 
pca_Monetary = (-1)*pca.dataScale$rotation[3,1]*Monetary_s 
data_pca = data.frame(Recency,Frequency,Monetary)
colnames(data_pca) = c("Recency","Frequency","Monetary")
cov(data_pca) 
cor(data_pca) 
#pca with and without scaling
pca.data = prcomp(data_pca,scale = F)
pca.dataScale = prcomp(data_pca,scale = T) #with scaling
pca.dataScale$rotation
#the pca plots 
biplot(pca.data, cex = 0.6)
biplot(pca.dataScale, cex = 0.6)
eig = eigen(cor(data_pca))
screeplot(pca.dataScale, type="l", npcs = 3, main = NULL)
pve = rep(NA, dim(data_pca)[2]) # proportion of variance explained
for(i in 1:3)
{
  pve[i] = print(sum(eig$values[1:i])/3)
}
eig$values
pve
# generating new varaibles using the weights/loadings of pca
pca_Recency = pca.dataScale$rotation[1,1]*Recency_s 
pca_Frequency = (-1)*pca.dataScale$rotation[2,1]*Frequency_s 
pca_Monetary = (-1)*pca.dataScale$rotation[3,1]*Monetary_s 
#-----------------------------------------------------------------------------------------------------------
# Missing Values imputation, apply KNN one by one to avoid death kernel
#-----------------------------------------------------------------------------------------------------------
missingdat = data.frame(Frequency_s, Recency_s, Monetary_s, Age, Allow_analysis, Opt_in_com)
colnames(missingdat) = c("Frequency","Recency","Monetary","Age"," Allow_analysis", "Opt_in_com")
#the plot of missing varaibles
aggr(missingdat,numbers = TRUE,col = c("gray88","darkorange"),bars = TRUE, sortVars=TRUE, labels=names(missingdat), cex.axis=.65, gap=3, ylab=c("Histogram of missing data","Pattern"))
#partitioning data to 6 small parts otherwise kNN function in GCP can't handle it and leads to kernel death in case applied on whole data
n = nrow(missingdat)
n1 = floor(n/11)
n2 = 2*n1
n3 = 3*n1
n4 = 4*n1
n5 = 5*n1
n6 = 6*n1
n7 = 7*n1
n8 = 8*n1
n9 = 9*n1
n10 = 10*n1
n11 = n

#taking only the parts of the imputed data the remaing parts of KNN object conssits of the object with TRUE/FALSE logicals indicating value imputed or not
#KNN imputataions, running one by one
KNN_1 = kNN(data = missingdat[1:n1,])
KNN_2 = kNN(data = missingdat[n1+1:n2,])
KNN_3 = kNN(data = missingdat[n2+1:n3,])
KNN_4 = kNN(data = missingdat[n3+1:n4,])
KNN_5 = kNN(data = missingdat[n4+1:n5,])
KNN_6 = kNN(data = missingdat[n5+1:n6,])
KNN_7 = kNN(data = missingdat[n6+1:n7,])
KNN_8 = kNN(data = missingdat[n7+1:n8,])
KNN_9 = kNN(data = missingdat[n8+1:n9,])
KNN_10 = kNN(data = missingdat[n9+1:n10,])
KNN_11 = kNN(data = missingdat[n10+1:n11,])
imputed1 = KNN_1[,1:6]
imputed2 = KNN_2[,1:6]
imputed3 = KNN_3[,1:6]
imputed4 = KNN_4[,1:6]
imputed5 = KNN_5[,1:6]
imputed6 = KNN_6[,1:6]
imputed7 = KNN_7[,1:6]
imputed8 = KNN_8[,1:6]
imputed9 = KNN_9[,1:6]
imputed10 = KNN_10[,1:6]
imputed11 = KNN_11[,1:6]
#updating new imputed Age, Allow_analysis, Opt_in_com variables other three varaibles had no missing values
Imputed_data = rbind(imputed1,imputed2, imputed3, imputed4, imputed5, imputed6, imputed7, imputed8 ,imputed9, imputed10, imputed11)
Age = Imputed_data[,4]
Allow_analysis = Imputed_data[,5]
Opt_in_com = Imputed_data[,6]

#-----------------------------------------------------------------------------------------------------------
# K-means Analysis
#-----------------------------------------------------------------------------------------------------------
#data's for standard K-means and K-means PCA
data_kmeans = data.frame(customerid,Frequency_s, Recency_s, Monetary_s)
data_kmeans_pca = data.frame(customerid,pca_Recency, pca_Frequency, pca_Monetary)
set.seed(20190218) # we set the seed to get the same clusters each time
km = kmeans(x = data_kmeans[,-1], centers = 3) #note customer ID won't be used in kmeans
set.seed(20190218) # we set the seed to get the same clusters each time
km_pca = kmeans(x = data_kmeans_pca[,-1], centers = 3)
summary(km)
classkm = km$cluster #class varaible where 1:Good 2:Best 3: Better
table(classkm)
#k-means pca-based results
summary(km_pca)
classkm_pca = km_pca$cluster
table(classkm_pca)
# in order to have the desired order of class variable
# Creating new categorical varaible which will take following order of segments 1:Good, 2:Better, 3:Best
n = nrow(data_kmeans)
class = rep(0,n)
for(i in 1:n){
  if(classkm[i] == 1){
    class[i] = 1
  }
  else if(classkm[i] == 2){
    class[i] = 3 #Best
  }
  else if(classkm[i] == 3){
    class[i] = 2
  }
}
table(class)
#the real pca k-means class
class_pca = rep(0,n)
for(i in 1:n){
  if(classkm_pca[i] == 1){
    class_pca[i] = 1 
  }
  else if(classkm_pca[i] == 2){
    class_pca[i] = 3 #Best
  }
  else if(classkm_pca[i] == 3){
    class_pca[i] = 2
  }
}
table(class_pca)

# Plotting the resuts of K-means
plot(data_kmeans[,2:4],col = class,main="k-means clusters") #the standard kmeans results
plot(data_kmeans_pca[,2:4],col = class_pca,main="PCA k-means clusters") #the standard kmeans results
colors = c("#FFCC66", "#CC6633","#993300")
colors = colors[as.numeric(class)]
scatterplot3d(data_kmeans[,2:4], color=colors,angle = 155)
scatterplot3d(data_kmeans_pca[,2:4], color=colors, angle = 110)  

########Kmeans Father's day 2018
sql7 <- "SELECT cast(lkuid as string) as customer_id, total_revenue as totrevenue, last_purchase_date as most_recent_visit, recency as recency_days, frequency as number_of_orders 
FROM `edc-gall-en-gall.data.H_Fathersday_18_rfm`"
#######Kmeans Father's day 2017  
sql7 <- "SELECT cast(lkuid as string) as customer_id, total_revenue as totrevenue, last_purchase_date as most_recent_visit, recency as recency_days, frequency as number_of_orders 
FROM `edc-gall-en-gall.data.H_Fathersday_17_rfm`"
########Kmeans Easter 2018
sql7 <- "SELECT cast(lkuid as string) as customer_id, total_revenue as totrevenue, last_purchase_date as most_recent_visit, recency as recency_days, frequency as number_of_orders 
FROM `edc-gall-en-gall.data.H_Easter_18_rfm`"
#######Kmeans Easter 2017
sql7 <- "SELECT cast(lkuid as string) as customer_id, total_revenue as totrevenue, last_purchase_date as most_recent_visit, recency as recency_days, frequency as number_of_orders 
FROM `edc-gall-en-gall.data.H_Easter_17_rfm`"
#######Kmeans Sinterklass 2018
sql7 <- "SELECT cast(lkuid as string) as customer_id, total_revenue as totrevenue, last_purchase_date as most_recent_visit, recency as recency_days, frequency as number_of_orders 
FROM `edc-gall-en-gall.data.H_Sinterklass_18_rfm`"
#######Kmeans Sinterklass 2017
sql7 <- "SELECT cast(lkuid as string) as customer_id, total_revenue as totrevenue, last_purchase_date as most_recent_visit, recency as recency_days, frequency as number_of_orders 
FROM `edc-gall-en-gall.data.H_Sinterklass_17_rfm`"
#######Kmeans December 2017
sql7 <- "SELECT cast(lkuid as string) as customer_id, total_revenue as totrevenue, last_purchase_date as most_recent_visit, recency as recency_days, frequency as number_of_orders 
FROM `edc-gall-en-gall.data.H_December_17_rfm`"
#######Kmeans December 2018
sql7 <- "SELECT cast(lkuid as string) as customer_id, total_revenue as totrevenue, last_purchase_date as most_recent_visit, recency as recency_days, frequency as number_of_orders 
FROM `edc-gall-en-gall.data.H_December_18_rfm`"

#########After we choose the desirable holiday, we run the things below##########
comb_data <- query_exec(sql7, project, use_legacy_sql = FALSE, max_pages = Inf)
#varaible definitions for data(1)
customer_id = comb_data[,1]
totrevenue = comb_data[,2]
most_recent_visit = comb_data[,3]
recency_days = comb_data[,4]
number_of_orders = comb_data[,5]
data_customer <- data.frame(customer_id,totrevenue,most_recent_visit,number_of_orders,recency_days)
##########
data=data.frame(data_customer$totrevenue,data_customer$number_of_orders,data_customer$recency_days)
datascale=scale(data)
Clusters=kmeans(datascale, 3)
Clusters


#------------------------------------------------------------------------------------------------------------------
# Decision-tree analysis
#------------------------------------------------------------------------------------------------------------------
#data preparation for the tree, now we also include demographics
tree_data = data.frame(customerid,Frequency, Recency, Monetary, Female, Age, Allow_analysis,Opt_in_com, Loyal_time ,class)
colnames(tree_data) = c("ID","Frequency","Recency","Monetary","Female", "Age","Opt_Analysis", "Opt_Commercial","Loyal_time","Class")
#data with pca_class from k-means pca
tree_data_pca = data.frame(customerid,Frequency,Recency,Monetary,Female,Age,Allow_analysis,Opt_in_com,Loyal_time,class_pca)
colnames(tree_data_pca) = c("ID","Frequency","Recency","Monetary","Female", "Age","Opt_Analysis", "Opt_Commercial","Loyal_time","Class_pca")

#Standard tree
tree.fit = rpart(Class ~ Recency+ Frequency + Monetary + Female + Age + Allow_analysis + Opt_in_com + Loyal_time, data = tree_data, method = "class") #the default uses the gini index
tree.fit
rpart.plot(tree.fit,box.palette=list("peachpuff1", "gray75","darkorange"))
#pca tree
tree.fit_pca = rpart(Class_pca ~ Recency+ Frequency + Monetary + Female + Age + Allow_analysis + Opt_in_com + Loyal_time, data = tree_data_pca, method = "class")
tree.fit_pca
rpart.plot(tree.fit_pca,box.palette=list("peachpuff1", "gray75","darkorange"))

#Tree pruning
printcp(tree.fit)
plotcp(tree.fit)
printcp(tree.fit_pca)
plotcp(tree.fit_pca)

#Pruning the tree and plotting the pruned tree by using the minimal gini index criteria
tree.pruned = prune(tree.fit, cp = 0.063609)
rpart.plot(tree.pruned, box.palette=list("peachpuff1", "gray75","darkorange"))
tree.pruned_pca = prune(tree.fit_pca, cp = 0.10374)
rpart.plot(tree.pruned_pca, box.palette=list("peachpuff1", "gray75","darkorange"))

#Prediction of the classes and transitional probabilities
pred = predict(tree.pruned,data = tree_data, method = "class")
pred_pca = predict(tree.pruned_pca,data = tree_data_pca, method = "class")
rate_class = data.frame(tree_data[,1], pred, class)
rate_class_pca = data.frame(tree_data_pca[,1], pred_pca, class_pca)
colnames(rate_class) = c("customerid","trans.class1","trans.class2","trans.class3","kmeans.class")
colnames(rate_class_pca) = c("customerid","trans.class1","trans.class2","trans.class3","kmeans.class_pca")

#------------------------------------------------------------------------------------------------------------------
# Algorithm to identifiy potential customers, cussutomers who are likely to move up or down to other class
#------------------------------------------------------------------------------------------------------------------
rate = rate_class[,-c(1,5)]
rate_pca = rate_class_pca[,-c(1,5)]

realClass = as.character(rate_class[,5])
realClass_pca = as.character(rate_class_pca[,5])

customerid = rate_class[,1]
customerid_pca = rate_class_pca[,1]

newClass = rep(NA,nrow(rate))
newClass_pca = rep(NA,nrow(rate_pca))

max.prob = rep(0,nrow(rate))
max.prob_pca = rep(0,nrow(rate_pca))

max.position = rep(NA,nrow(rate))
max.position_pca = rep(NA,nrow(rate_pca))

for(i in 1:nrow(rate)){
  new = NULL
  max.prob[i] = max(rate[i,])
  new = which(rate[i,] == max.prob[i])
  max.position[i] = paste(new,collapse="--")
  if(length(new)==1) {newClass[i] = new} 
  if (length(new)!=1) {
    if(sum(new==realClass[i]) == 1) newClass[i] = realClass[i]
    if(sum(new==realClass[i]) == 0) newClass[i] = max.position[i]
  }
}
for(i in 1:nrow(rate_pca)){
  new_pca = NULL
  max.prob_pca[i] = max(rate_pca[i,])
  new_pca = which(rate_pca[i,] == max.prob_pca[i])
  max.position_pca[i] = paste(new_pca,collapse="--")
  if(length(new_pca)==1) {newClass_pca[i] = new_pca} 
  if (length(new_pca)!=1) {
    if(sum(new_pca==realClass_pca[i]) == 1) newClass_pca[i] = realClass_pca[i]
    if(sum(new_pca==realClass_pca[i]) == 0) newClass_pca[i] = max.position_pca[i]
  }
}

table = cbind(customerid,max.prob,max.position,realClass,newClass,tree_data)
table_pca = cbind(customerid_pca,max.prob_pca,max.position_pca,realClass_pca,newClass_pca,tree_data_pca)

#------------------------------------------------------------------------------------------------------------------
# Determining the data and movement in Good class PCA
#------------------------------------------------------------------------------------------------------------------
Class1ind_pca = as.numeric(rownames(table_pca[table_pca[,4] ==1,])) #indicies of good customers from k-means
Class1data_pca = table_pca[Class1ind_pca,]
nk = nrow(Class1data_pca)
Good_pca = rep(0,nk) #staying in good
UpGoodToBetter_pca = rep(0,nk) #From Good to Better
UpGoodToBest_pca = rep(0,nk) #From Good to Best

for(i in 1:nk){
  if(Class1data_pca[i,5] == 2){
    UpGoodToBetter_pca[i] = 1
  }
  else if( Class1data_pca[i,5] == 3){
    UpGoodToBest_pca[i] = 1
  }
  else if(Class1data_pca[i,5] == 1){
    Good_pca[i] = 1
  }
} 

#updating the data
Trans_class1_data_pca = data.frame(Class1data_pca,Good_pca, UpGoodToBetter_pca, UpGoodToBest_pca)
#corresponding indecies and data's of these customers
GoodInd_pca = as.numeric(rownames(Trans_class1_data_pca[Trans_class1_data_pca[,16]==1,]))
UpGoodToBetterInd_pca = as.numeric(rownames(Trans_class1_data_pca[Trans_class1_data_pca[,17]==1,]))
UpGoodToBestInd_pca = as.numeric(rownames(Trans_class1_data_pca[Trans_class1_data_pca[,18]==1,]))
dataGood_pca = Trans_class1_data_pca[GoodInd_pca,]
dataUpGoodToBetter_pca =  Trans_class1_data_pca[UpGoodToBetterInd_pca,]
dataUpGoodToBest_pca =  Trans_class1_data_pca[UpGoodToBestInd_pca,] 


#------------------------------------------------------------------------------------------------------------------
#Determining the data and movement in Better class PCA
#------------------------------------------------------------------------------------------------------------------
Class2ind_pca = as.numeric(rownames(table_pca[table_pca[,4] ==2,])) #indicies of good customers from k-means
Class2data_pca = table[Class2ind_pca,]
nk = length(Class2ind_pca)
Better_pca = rep(0,nk) #staying in good
UpBetterToBest_pca = rep(0,nk) #From Better to Best
DownBetterToGood_pca = rep(0,nk) #From Better to Good

for(i in 1:nk){
  if(Class2data_pca[i,5] == "2"){
    Better_pca[i] = 1
  }
  else if(Class2data_pca[i,5] == "3"){
    UpBetterToBest_pca[i] = 1
  }
  else if(Class2data_pca[i,5] == "1"){
    DownBetterToGood_pca[i] = 1
  }
} 

#updating the data
Trans_class2_data_pca = data.frame(Class2data_pca, Better_pca, UpBetterToBest_pca, DownBetterToGood_pca)
#corresponding indecies and data's of these customers
BetterInd_pca = as.numeric(rownames(Trans_class2_data_pca[Trans_class2_data_pca[,16]==1,]))
UpBetterToBestInd_pca = as.numeric(rownames(Trans_class2_data_pca[Trans_class2_data_pca[,17]==1,]))
DownBetterToGoodInd_pca = as.numeric(rownames(Trans_class2_data_pca[Trans_class2_data_pca[,18]==1,]))
#the data of each group
dataBetter_pca = Trans_class2_data_pca[BetterInd_pca,]
dataUpBetterToBest_pca =  Trans_class2_data_pca[UpGoodToBetterInd_pca,]
dataDownBetterToGood_pca =  Trans_class2_data_pca[DownBetterToGoodInd_pca,] 


#------------------------------------------------------------------------------------------------------------------
#Determining the data and movement in Best class PCA
#------------------------------------------------------------------------------------------------------------------
Class3ind_pca = as.numeric(rownames(table[table_pca[,4] ==3,])) #indicies of good customers from k-means
Class3data_pca = table[Class3ind_pca,]
nk = length(Class3ind_pca)
Best_pca = rep(0,nk) #staying in good
DownBestToBetter_pca = rep(0,nk) #from best to better
DownBestToGood_pca = rep(0,nk) #from best to better

for(i in 1:nk){
  if( Class3data_pca[i,5] == "3"){
    Best_pca[i] = 1
  }
  else if(Class3data_pca[i,5] == "2"){
    DownBestToBetter_pca[i] = 1
  }
  else if(Class3data_pca[i,5] == "1"){
    DownBestToGood_pca[i] = 1
  }
} 
#updating the data
Trans_class3_data_pca = data.frame(Class3data_pca, Best_pca, DownBestToBetter_pca, DownBestToGood_pca)
#corresponding indecies and data's of these customers
BestInd_pca = as.numeric(rownames(Trans_class3_data_pca[Trans_class3_data_pca[,16]==1,]))
DownBestToBetterInd_pca = as.numeric(rownames(Trans_class3_data_pca[Trans_class3_data_pca[,17]==1,]))
DownBestToGoodInd_pca = as.numeric(rownames(Trans_class3_data_pca[Trans_class3_data_pca[,18]==1,]))
#the data of each group
dataBest_pca = Trans_class3_data_pca[BestInd_pca,]
dataDownBestToBetter_pca =  Trans_class3_data_pca[DownBestToBetterInd_pca,]
dataDownBestToGood_pca =  Trans_class3_data_pca[DownBestToGoodInd_pca,] 

#------------------------------------------------------------------------------------------------------------------
# Finite Mixture Model with Negative Binomial Distribution
#------------------------------------------------------------------------------------------------------------------
set.seed(20190218)
Model = FLXMRziglm(family = "poisson")
control = list(verbose = 10, iter.max = 300, minprior = 0.1, tol = 0.01)  
formula = Frequency ~ 1 + Monetary
formula2 = Frequency ~ 1+ Monetary + Recency
data = data.frame(customerid,Frequency,Monetary)
data2 = data.frame(customerid,Frequency,Monetary,Recency)

#formula 1 with k = 3
Fitted1 = flexmix(formula, data = data, k = 3 ,model = Model, control = control)
summary(Fitted1)
Fitted1
refit1 = refit(Fitted1, method = 'optim')
summary(refit1) 
#formula 1 with k = 4
Fitted2 = flexmix(formula, data = data, k = 4 ,model = Model, control = control)
summary(Fitted2)
Fitted2
refit2 = refit(Fitted2, method = 'optim')
summary(refit2)   
#formula 1 with k = 5
Fitted3 = flexmix(formula, data = data, k = 5 ,model = Model, control = control)
summary(Fitted3)
Fitted3
refit3 = refit(Fitted3, method = 'optim')
summary(refit3)
#formula 1 with k = 6
Fitted4 = flexmix(formula, data = data, k = 6 ,model = Model, control = control)
summary(Fitted4)
Fitted4
refit4 = refit(Fitted4, method = 'optim')
summary(refit4) 

#formula 2 with k = 3
Fitted1 = flexmix(formula2, data = data2, k = 3 ,model = Model, control = control)
summary(Fitted1)
Fitted1
refit1 = refit(Fitted1, method = 'optim')
summary(refit1) 
#formula 2 with k = 4
Fitted2 = flexmix(formula2, data = data2, k = 4 ,model = Model, control = control)
summary(Fitted2)
Fitted2
refit2 = refit(Fitted2, method = 'optim')
summary(refit2)   
#formula 2 with k = 5
Fitted3 = flexmix(formula2, data = data2, k = 5 ,model = Model, control = control)
summary(Fitted3)
Fitted3
refit3 = refit(Fitted3, method = 'optim')
summary(refit3)
#formula 2 with k = 6
Fitted4 = flexmix(formula2, data = data2, k = 6 ,model = Model, control = control)
summary(Fitted4)
Fitted4
refit4 = refit(Fitted4, method = 'optim')
summary(refit4)  

#getting the clusters k = 6 case
cluster = clusters(Fitted4)
d = data.frame(data2,cluster)
data = data.frame(d,Age,Female,Allow_analysis,Opt_in_com)
Cluster1Ind = as.numeric(rownames(d[d[,5]==1,]))
Cluster2Ind = as.numeric(rownames(d[d[,5]==2,]))
Cluster3Ind = as.numeric(rownames(d[d[,5]==3,]))
Cluster4Ind = as.numeric(rownames(d[d[,5]==4,]))
Cluster5Ind = as.numeric(rownames(d[d[,5]==5,]))
Cluster6Ind = as.numeric(rownames(d[d[,5]==6,]))
datac1 = data[Cluster1Ind,]
datac2 = data[Cluster2Ind,]
datac3 = data[Cluster3Ind,]
datac4 = data[Cluster4Ind,]
datac5 = data[Cluster5Ind,]
datac6 = data[Cluster6Ind,]  

#------------------------------------------------------------------------------------------------------------------  
# Robustness checks  
#------------------------------------------------------------------------------------------------------------------  
# Cheking the best customers and comparing for k-means and RFM-based segmentation
kmeans = tree_data[tree_data[,9]=='3',1:2]
kmeans_pca = tree_data_pca[tree_data_pca[,9]=='3',1:2]
colnames(kmeans) = c("ID","v1")
colnames(kmeans_pca) = c("ID","v2")
# best customers from RFM-based segmentation
bestRFM = data.frame(bestbestcustomers[,1:2])
colnames(bestRFM) = c("ID","v3")  
nrow(bestRFM)
nrow(k)
nrow(k2)
#comparing RFM best customers with best customers from k-means and k-means PCA
compare(bestRFM,kmeans,by = "ID") 
compare(bestRFM,kmeans_pca,by = "ID")
compare(kmeans,kmeans_pca,by = "ID")  

#K-means with Euclidean distance and pre-specified initial centroid
Clusters.eucl=Kmeans(data = data_kmeans_pca[,-1], 3,method="euclidean",nstart=25)
Clusters.eucl$withinss
Clusters.eucl$betweenss

#K-means with Manhattan distance and random initial centroid
Clusters.manh.cendroid=Kmeans(data = data_kmeans_pca[,-1], 3,method="manhattan")
Clusters.manh.cendroid$withinss
Clusters.manh.cendroid$betweenss

#K-means with Manhattan distance and pre-specified initial centroid
Clusters.manh.cendroidN=Kmeans(data = data_kmeans_pca[,-1], 3,method="manhattan",nstart=25)
Clusters.manh.cendroidN$withinss
Clusters.manh.cendroidN$betweenss

#CH for PCA
data = as.matrix(datascale)
get_CH(data, ClustersPCA$cluster, disMethod = "Euclidean")



#Extra**
#------------------------------------------------------------------------------------------------------------------
# Determining the data and movement in Good class
# Prediction estimation of K-means standard class
#------------------------------------------------------------------------------------------------------------------
Class1ind = as.numeric(rownames(table[class==1,])) #indicies of good customers from k-means
Class1data = table[Class1ind,]
n1 = length(Class1ind)
Good = rep(0,n1) #staying in good
UpGoodToBetter = rep(0,n1) #From Good to Better
UpGoodToBest = rep(0,n1) #From Good to Best

for(i in 1:n1){
  if(Class1data[i,4] == "1" & Class1data[i,5] == "2"){
    UpGoodToBetter[i] = 1
  }
  else if(Class1data[i,4] == "1" & Class1data[i,5] == "3"){
    UpGoodToBest[i] = 1
  }
  else if(Class1data[i,4] == "1" & Class1data[i,5] == "1"){
    Good[i] = 1
  }
} 
sum(Good)*100/n1
sum(UpGoodToBetter)*100/n1
sum(UpGoodToBest)*100/n1
#updating the data
Trans_class1_data = data.frame(Class1data[,1],Class1data[,4:5],Class1data[,7:14],Good,UpGoodToBetter, UpGoodToBest)
#corresponding indecies and data's of these customers
GoodInd = as.numeric(rownames(Trans_class1_data[Trans_class1_data[,12]==1,]))
UpGoodToBetterInd = as.numeric(rownames(Trans_class1_data[Trans_class1_data[,13]==1,]))
UpGoodToBestInd = as.numeric(rownames(Trans_class1_data[Trans_class1_data[,14]==1,]))
dataGood = Trans_class1_data[GoodInd,]
dataUpGoodToBetter =  Trans_class1_data[UpGoodToBetterInd,]
dataUpGoodToBest =  Trans_class1_data[UpGoodToBestInd,] 

#------------------------------------------------------------------------------------------------------------------
#Determining the data and movement in Better class
#------------------------------------------------------------------------------------------------------------------
Class2ind = as.numeric(rownames(table[class ==2,])) #indicies of good customers from k-means
Class2data = table[Class2ind,]
n2 = length(Class2ind)
Better = rep(0,n2) #staying in good
UpBetterToBest = rep(0,n2) #From Better to Best
DownBetterToGood = rep(0,n2) #From Better to Good

for(i in 1:n2){
  if(Class2data[i,4] == "2" & Class2data[i,5] == "2"){
    Better[i] = 1
  }
  else if(Class2data[i,4] == "2" & Class2data[i,5] == "3"){
    UpBetterToBest[i] = 1
  }
  else if(Class2data[i,4] == "2" & Class2data[i,5] == "1"){
    DownBetterToGood[i] = 1
  }
} 
sum(Better)*100/n2
sum(UpBetterToBest)*100/n2
sum(DownBetterToGood)*100/n2

#updating the data
Trans_class2_data = data.frame(Class2data[,1],Class2data[,4:5],Class2data[,7:14],Better,UpBetterToBest,DownBetterToGood)
#corresponding indecies and data's of these customers
BetterInd = as.numeric(rownames(Trans_class2_data[Trans_class2_data[,12]==1,]))
UpBetterToBestInd = as.numeric(rownames(Trans_class2_data[Trans_class2_data[,13]==1,]))
DownBetterToGoodInd = as.numeric(rownames(Trans_class2_data[Trans_class2_data[,14]==1,]))
#the data of each group
dataBetter = Trans_class2_data[BetterInd,]
dataUpBetterToBest =  Trans_class2_data[UpGoodToBetterInd,]
dataDownBetterToGood =  Trans_class2_data[DownBetterToGoodInd,] 

#------------------------------------------------------------------------------------------------------------------
#Determining the data and movement in Best class
#------------------------------------------------------------------------------------------------------------------
Class3ind = as.numeric(rownames(table[class ==3,])) #indicies of good customers from k-means
Class3data = table[Class3ind,]
n3 = length(Class3ind)
Best = rep(0,n3) #staying in good
DownBestToBetter = rep(0,n3) #from best to better
DownBestToGood = rep(0,n3) #from best to good

for(i in 1:n3){
  if(Class3data[i,4] == "3" & Class3data[i,5] == "3"){
    Best[i] = 1
  }
  else if(Class3data[i,4] == "3" & Class3data[i,5] == "2"){
    DownBestToBetter[i] = 1
  }
  else if(Class3data[i,4] == "3" & Class3data[i,5] == "1"){
    DownBestToGood[i] = 1
  }
} 
sum(Best)*100/n3
sum(DownBestToBetter)*100/n3
sum(DownBestToGood)*100/n3
#updating the data
Trans_class3_data = data.frame(Class3data[,1],Class3data[,4:5],Class3data[,7:14],Best,DownBestToBetter,DownBestToGood)
#corresponding indecies and data's of these customers
BestInd = as.numeric(rownames(Trans_class3_data[Trans_class3_data[,12]==1,]))
DownBestToBetterInd = as.numeric(rownames(Trans_class3_data[Trans_class3_data[,13]==1,]))
DownBestToGoodInd = as.numeric(rownames(Trans_class3_data[Trans_class3_data[,14]==1,]))
#the data of each group
dataBest = Trans_class3_data[BestInd,]
dataDownBestToBetter =  Trans_class3_data[DownBestToBetterInd,]
dataDownBestToGood =  Trans_class3_data[DownBestToGoodInd,] 

