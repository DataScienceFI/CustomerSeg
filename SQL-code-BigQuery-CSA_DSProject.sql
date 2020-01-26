########### A. CLEANING DATA & PREPARING VARIABLES ###########

##B2B customers###############################################
##A1. Table: b2b_customers_ID: 
select distinct lkuid, cast(c.uuid as string) uuid
from data.customer_table c inner join 
(select cast(uuid as string) uuid 
from data.b2b_customers) b2b 
on c.uuid = b2b.uuid
order by lkuid

##Gender variable#############################################
##A2. Table: gender_table 
select lkuid, gender, 
(case when gender = "f" or gender = "m" then gender else null end) as gender_adjust
from data.customer_table
group by lkuid, gender
order by lkuid

##Age variable################################################
##A3. Table: age_table
select lkuid, birthday, 		date_diff('2018-12-31',date(birthday),year) as age
from data.customer_table
order by lkuid

##Age category variable#######################################
##A4. Table: age_table_category
select lkuid, age, 
(case when age between 18 and 24 then 1 else 0 end) + (case when age between 25 and 34 then 2 else 0 end) + (case when age between 35 and 44 then 3 else 0 end) + (case when age between 45 and 54 then 4 else 0 end) + (case when age between 55 and 64 then 5 else 0 end) + (case when age between 65 and 100 then 6 else 0 end)as age_category
from data.age_table
group by lkuid, age
order by lkuid

##Total sales of all customers################################
##A5. Table:total_sales
select date(betaal_datum) as purchase_date, sum(verkoopwaarde_inc_vat) total_sales
from data.sales_table
where betaal_datum between '2017-01-01' and '2018-12-31'
group by betaal_datum
order by betaal_datum

##Transaction table for all customers#########################
##A6. Table: trans_table_orginal 
select cast(lkuid as string) as lkuid, betaal_datum, betaal_datum_tijd, trans_nr, kassabon_nummer, sum(verkoopwaarde_inc_vat) as trans_amount
from data.sales_table
where betaal_datum between '2017-01-01' and '2018-12-31'
group by lkuid, betaal_datum, betaal_datum_tijd, trans_nr, kassabon_nummer
order by lkuid, betaal_datum

##R,F,M values for all customers##############################
##A7. Table: 2years_RFMvalue_original
select distinct cast(lkuid as string) as lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2018-12-31',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(verkoopwaarde_inc_vat) as total_revenue 
from data.sales_table
where betaal_datum between '2017-01-01' and '2018-12-31'
group by lkuid
order by lkuid

##Transaction table for customers with total spend below 10k##
##A8. Table: trans_table_below10k
select t.lkuid, betaal_datum, betaal_datum_tijd, trans_nr, kassabon_nummer, trans_amount
from data.trans_table_original t
where t.lkuid not in
(select lkuid 
from data.b2b_customers_ID b2b) 
and t.lkuid in 
(select lkuid
from data.2years_RFMvalue_original
where total_revenue < 10000)
order by lkuid

##Get number of orders per day################################
##A9. Table: count_order_per_day
select lkuid, betaal_datum, count(distinct betaal_datum_tijd) as count
from data.trans_table_below10k
group by lkuid, betaal_datum
order by lkuid

##Transaction table for customers with total spend below 10k and number of order below 3##
##A10. Table: trans_table_new
select * 
from data.trans_table_below10k
where lkuid not in
(select lkuid
from data.count_order_per_day
where count > 3) 
order by lkuid

##Seasonality adjustment using 2016 sales data################
##A11. Table:avg_amount_16
select date(betaal_datum) as datum, 
extract(YEAR from betaal_datum) as year16,
extract(MONTH from betaal_datum) as month,
extract(DAY from betaal_datum) as day,
avg(verkoopwaarde_inc_vat) as average_amount
from data.sales_table
where betaal_datum between '2016-01-01' and '2016-12-31' and verkoopwaarde_inc_vat > 0
group by betaal_datum
order by betaal_datum

##A12. Table: avg_amount_16to1718
select date(year16+1, month, day) as betaal_datum17, date(year16+2, month, day) as betaal_datum18, average_amount
from data.avg_amount_16

##A13. Table: avg_amount_1718
select s.purchase_date, average_amount
from  data.total_sales s inner join 
(select purchase_date
from data.total_sales) t on t.purchase_date = s.purchase_date
inner join
(select betaal_datum17, betaal_datum18,
(case when betaal_datum17 between '2017-11-20' and '2017-12-31' or betaal_datum18 between '2018-11-19' and '2018-12-31' then average_amount else 0 end) as average_amount
from data.avg_amount_16to1718)  
on betaal_datum17 = t.purchase_date or betaal_datum18 = t.purchase_date
order by purchase_date

##Taking difference between the transaction amount of 2017(2018) and the daily expected amount 2017(2018)###########
##A14. Table: trans_table_new2
select lkuid, s.betaal_datum, betaal_datum_tijd, trans_nr, kassabon_nummer, trans_amount-average_amount as trans_amount
from data.trans_table_new s inner join 
(select purchase_date, average_amount
from data.avg_amount_1718) a on a.purchase_date = date(s.betaal_datum)
order by lkuid

##Inserting into trans_table_new2 the rows of transactions when betaal_datum is between 2017-01-01 and 2017-10-01 or it is between 2018-01-01 and 2018-10-01##########################
insert data.trans_table_new2 (lkuid, betaal_datum, betaal_datum_tijd, trans_nr, kassabon_nummer, trans_amount)
select lkuid, betaal_datum, betaal_datum_tijd, trans_nr, kassabon_nummer, trans_amount from data.trans_table_new
where betaal_datum between '2017-01-01' and '2017-10-01' or betaal_datum between '2018-01-01' and '2018-10-01'

##R,F,M values for customers original from trans_table_new after adjusting for seasonality##
##A15. Table: 2years_RFMvalue_new2 
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2018-12-31',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.trans_table_new2
group by lkuid
order by lkuid

##A combination of demographic data and behavioural data of the selected customers for decision tree######################
##A16. Table: tree_table_new2
select distinct s.lkuid, g.gender, g.gender_adjust, s.allow_analysis, s.opt_in_commercial_email, a.age, a.age_category, t.loyal_time, d.recency, d.frequency, d.total_revenue
from data.customer_table s 
inner join (
select lkuid ,gender, gender_adjust
from data.gender_table) g on g.lkuid = s.lkuid
inner join(
select lkuid, recency, frequency, total_revenue
from data.2years_RFMvalue_new2) d on s.lkuid = cast(d.lkuid as string)
inner join (
select lkuid, age, 
(case when age_category between 1 and 6 then age_category else null end) as age_category
from data.age_table_category
group by lkuid, age,age_category) a on a.lkuid = d.lkuid
inner join (
select lkuid, date_diff('2018-12-31',DATE(min(betaal_datum)),day) as loyal_time
from data.trans_table_new2
group by lkuid) t on t.lkuid = s.lkuid  
order by s.lkuid


##############################################################

################ B. SEASONAL RFM SEGMENTATION ################

##December##
##17
select *
from data.trans_table_new 
where betaal_datum between '2017-12-04' and '2017-12-31'
######
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2017-12-31',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.H_December_17 
where cast(lkuid as string) in 
(select distinct lkuid
from data.H_December_17)
group by lkuid
order by lkuid
##18
select *
from data.trans_table_new 
where betaal_datum between '2018-12-03' and '2018-12-31'
######
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2018-12-31',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.H_December_18 
where cast(lkuid as string) in 
(select distinct lkuid
from data.H_December_18)
group by lkuid
order by lkuid

##Sinterklass##
##17
select *
from data.trans_table_new 
where betaal_datum between '2017-11-20' and '2017-12-03'
######
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2017-12-03',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.H_Sinterklass_17 
where cast(lkuid as string) in 
(select distinct lkuid
from data.H_Sinterklass_17)
group by lkuid
order by lkuid
##18
select *
from data.trans_table_new 
where betaal_datum between '2018-11-19' and '2018-12-02'
######
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2018-12-02',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.H_Sinterklass_18 
where cast(lkuid as string) in 
(select distinct lkuid
from data.H_Sinterklass_18)
group by lkuid
order by lkuid

##Fathers day##
##17
select *
from data.trans_table_new 
where betaal_datum between '2017-06-05' and '2017-06-18'
######
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2017-06-18',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.H_Fathersday_17 
where cast(lkuid as string) in 
(select distinct lkuid
from data.H_Fathersday_17)
group by lkuid
order by lkuid
##18
select *
from data.trans_table_new 
where betaal_datum between '2018-06-04' and '2018-06-17'
######
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2018-06-17',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.H_Fathersday_18
where cast(lkuid as string) in 
(select distinct lkuid
from data.H_Fathersday_18)
group by lkuid
order by lkuid

##Easter##
##17
select *
from data.trans_table_new 
where betaal_datum between '2017-04-10' and '2017-04-16'
######
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2017-04-16',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.H_Easter_17 
where cast(lkuid as string) in 
(select distinct lkuid
from data.H_Easter_17)
group by lkuid
order by lkuid
##18
select *
from data.trans_table_new 
where betaal_datum between '2018-03-26' and '2018-04-01'
######
select distinct cast(lkuid as string) lkuid, 
       date(max(betaal_datum)) as last_purchase_date,
       date_diff('2018-04-01',date(max(betaal_datum)),day) as recency,
       count(distinct betaal_datum_tijd) as frequency,
       sum(trans_amount) as total_revenue 
from data.H_Easter_18
where cast(lkuid as string) in 
(select distinct lkuid
from data.H_Easter_18)
group by lkuid
order by lkuid