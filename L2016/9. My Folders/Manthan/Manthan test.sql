sel top 10*  from cust_vm.I1695E_CUS_SLS_DTL where bus_dt = '2016-12-10'
sel top 10* from lowes_vm.I0036K_FSC_CAL_CNV
sel top 10* from CUST_VM.I1577C_GLD_CUS_ADR

create table ca_hold.Test as (sel gld_cus_typ_id, fsc_wk_end_dt, sum(TOT_SAL_AMT) as sales from cust_vm.I1695E_CUS_SLS_DTL a 
inner join lowes_vm.I0036K_FSC_CAL_CNV b on a.cal_dt=b.cal_dt
inner join CUST_VM.I1577C_GLD_CUS_ADR c on a.cus_id=c.cus_id
where a.cal_dt between '2016-01-01' and '2016-12-31' group by 1,2) with data;

sel distinct fsc_wk_end_dt from ca_hold.Test

-- Question 1
sel gld_cus_typ_id,sum(case when fsc_wk_end_dt between  '2016-01-01' and '2016-01-31' then sales end) as Jan_sales,
sum(case when fsc_wk_end_dt between  '2016-02-01' and '2016-02-29' then sales end) as Feb_sales,
sum(case when fsc_wk_end_dt between  '2016-03-01' and '2016-03-31'  then sales end) as Mar_sales,
sum(case when fsc_wk_end_dt between  '2016-04-01' and '2016-04-30'  then sales end) as Apr_sales,
sum(case when fsc_wk_end_dt between  '2016-05-01' and '2016-05-31'  then sales end) as May_sales,
sum(case when fsc_wk_end_dt between  '2016-06-01' and '2016-06-30'  then sales end) as Jun_sales,
sum(case when fsc_wk_end_dt between  '2016-07-01' and '2016-07-31'  then sales end) as Jul_sales,
sum(case when fsc_wk_end_dt between  '2016-08-01' and '2016-08-31'  then sales end) as Aug_sales,
sum(case when fsc_wk_end_dt between  '2016-09-01' and '2016-09-30'  then sales end) as Sep_sales,
sum(case when fsc_wk_end_dt between  '2016-10-01' and '2016-10-31'  then sales end) as Oct_sales,
sum(case when fsc_wk_end_dt between  '2016-11-01' and '2016-11-30'  then sales end) as Nov_sales,
sum(case when fsc_wk_end_dt between  '2016-12-01' and '2016-12-31'  then sales end) as Dec_sales
from ca_hold.Test group by 1;

-- Question 2
-- Add column to table
sel * from ca_hold.Test
Alter table ca_hold.Test drop new_column;
Alter table ca_hold.Test add new_column Char(50);
Update ca_hold.Test set new_column = 'C:\Test\ABC' where gld_cus_typ_id = 'B';
Update ca_hold.Test set new_column = 'C:\Test\ABCD' where gld_cus_typ_id = 'I';

-- Replace Test by concatenate new column
-- http://www.info.teradata.com/htmlpubs/DB_TTU_14_00/index.html#page/SQL_Reference/B035_1145_111A/Regular_Expr_Functions.085.12.html#ww14955792
sel * from ca_hold.Test
Alter table ca_hold.Test add con_new_column Char(50);
Update ca_hold.Test set con_new_column = (gld_cus_typ_id || '_' || fsc_wk_end_dt);
Update ca_hold.Test set new_column =  regexp_replace(new_column,'Test', con_new_column); -- sql query
Update ca_hold.Test set new_column = con_new_column where regexp_replace(new_column,'[Test]*', con_new_column,1,0,'c');
Update ca_hold.Test set new_column = con_new_column where oreplace(new_column,'Test', con_new_column);
sel oreplace(new_column,'Test', con_new_column) as new_column from ca_hold.Test
 --'<Test*>'
 
 -- Question 3
 -- Pick all customers whose gld_typ_Id average sales are greater than overall average.
 sel * from ca_hold.Test
create table ca_hold.Test_Avg as (sel gld_cus_typ_id, average(sales) as Avg_sales from ca_hold.Test group by 1) with data;
sel * from ca_hold.Test_Avg
sel average(sales)  as Avg_sales from ca_hold.Test;
create table ca_hold.Test_All_Avg as (sel average(sales)  as Avg_sales from ca_hold.Test) with data;
sel * from ca_hold.Test_All_Avg

sel gld_cus_typ_id, (case when a.Avg_sales<b.Avg_sales then 'Yes' end) as Flag 
from ca_hold.Test_Avg a, ca_hold.Test_All_Avg b  group by 1,2;
