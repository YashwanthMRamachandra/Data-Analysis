-- IDENTIFY THE CUSTOMERS WHO HAS 80% OF THE PROFITS(MARGIN %) FROM 20% CUSTOMER BASE(80-20 RULE). LOYALTY CUSTOMERS WHO PROACTIVELY PURCHASES ITEMS(SMALL) & VISITS THE STORES FREQUENTLY.--

-- Input data
create table  ca_hold.Kob_cust as (sel  a.brn_cd, g.cus_id, h.gld_cus_id, i.fsc_wk_end_dt,

sum(tot_sal_amt) as sales, sum(tot_itm_qty) as units, sum(tot_cst_amt) as cost, sum(tot_sal_amt)-sum(tot_cst_amt) as margin_wo_corr,
count(distinct(trim(g.lct_nbr) || trim(g.cal_dt) || trim(mas_sal_tns_nbr)))  as trans_count,sum(tot_sal_amt)/count(distinct(trim(g.lct_nbr) || trim(g.cal_dt) || trim(mas_sal_tns_nbr))) as avg_basket_size

from  LOWES_VM.I0166A_VBU_ITM a 
inner join CUST_VM.I1695E_CUS_SLS_DTL  g on a.itm_nbr=g.itm_nbr
inner join CUST_VM.I1577C_GLD_CUS_ADR  h on g.cus_id=h.cus_id
inner join lowes_vm.I0036K_FSC_CAL_CNV i on i.cal_dt=g.cal_dt
where g.cal_dt between '2015-10-31' and '2016-04-29'  and a.BRN_CD = 188 and TNS_TYP_CD = 1
			and i.fsc_wk_end_dt between '2015-10-31' and '2016-04-29'  group by 1,2,3,4) with data;



sel  a.brn_cd, e.asr_nbr, g.cus_id, g.lct_nbr,i.fsc_wk_end_dt,

sum(tot_sal_amt) as sales, sum(tot_itm_qty) as units, sum(tot_cst_amt) as cost, sum(tot_sal_amt)-sum(tot_cst_amt)+sum(mrg_crt_amt) as margin
from  LOWES_VM.I0166A_VBU_ITM a 
inner join lowes_vm.I0041A_ITM_HRC e on a.itm_nbr=e.itm_nbr
inner join CUST_VM.I1695E_CUS_SLS_DTL  g on a.itm_nbr=g.itm_nbr --and a.itm_nbr=e.itm_nbr
inner join lowes_vm.I0036K_FSC_CAL_CNV i on i.cal_dt=g.cal_dt
/*inner join lowes_vm.I0101B_LCT_MRG_CRT j on a.itm_nbr=j.itm_nbr and i.fsc_wk_end_dt=j.fsc_wk_end_dt and g.lct_nbr=j.lct_nbr*/
where g.cal_dt between '2015-10-31' and '2016-11-06'  and a.BRN_CD = 188 and TNS_TYP_CD = 1
			and i.fsc_wk_end_dt between '2015-10-31' and '2016-11-06' and e.mer_dvs_nbr = 33 and g.cus_id =  1391594  group by 1,2,3,4;
			

sel top 100* from lowes_vm.I0101B_LCT_MRG_CRT where itm_nbr= 464627 and fsc_wk_end_dt='2014-10-24' and lct_nbr=1598


sel  a.brn_cd, e.asr_nbr, g.cus_id,g.lct_nbr, i.fsc_wk_end_dt,

sum(tot_sal_amt) as sales, sum(tot_itm_qty) as units, sum(tot_cst_amt) as cost, sum(tot_sal_amt)-sum(tot_cst_amt)as margin
from  LOWES_VM.I0166A_VBU_ITM a 
inner join lowes_vm.I0041A_ITM_HRC e on a.itm_nbr=e.itm_nbr
inner join CUST_VM.I1695E_CUS_SLS_DTL  g on a.itm_nbr=g.itm_nbr
inner join lowes_vm.I0036K_FSC_CAL_CNV i on i.cal_dt=g.cal_dt
/*inner join lowes_vm.I0101B_LCT_MRG_CRT j on a.itm_nbr=j.itm_nbr and i.fsc_wk_end_dt=j.fsc_wk_end_dt and g.lct_nbr=j.lct_nbr*/
where g.cal_dt between '2015-10-31' and '2016-11-06'  and a.BRN_CD = 188 and TNS_TYP_CD = 1
			and i.fsc_wk_end_dt between '2015-10-31' and '2016-11-06' and e.mer_dvs_nbr = 33 and g.cus_id =  1391594  group by 1,2,3,4,5;

 
create table  ca_hold.Kob_cust as (sel  a.brn_cd, g.cus_id, h.gld_cus_id, i.fsc_wk_end_dt,

sum(tot_sal_amt) as sales, sum(tot_itm_qty) as units, sum(tot_cst_amt) as cost, sum(tot_sal_amt)-sum(tot_cst_amt) as margin_wo_corr, (sum(tot_sal_amt)-sum(tot_cst_amt))/sum(tot_sal_amt) as margin_rate, 
count(distinct(trim(g.lct_nbr) || trim(g.cal_dt) || trim(mas_sal_tns_nbr)))  as trans_count,sum(tot_sal_amt)/count(distinct(trim(g.lct_nbr) || trim(g.cal_dt) || trim(mas_sal_tns_nbr))) as avg_basket_size

from  LOWES_VM.I0166A_VBU_ITM a 
inner join lowes_vm.I0041A_ITM_HRC e on a.itm_nbr=e.itm_nbr
inner join CUST_VM.I1695E_CUS_SLS_DTL  g on a.itm_nbr=g.itm_nbr
inner join CUST_VM.I1577C_GLD_CUS_ADR  h on g.cus_id=h.cus_id
inner join lowes_vm.I0036K_FSC_CAL_CNV i on i.cal_dt=g.cal_dt
inner join lowes_vm.I0101B_LCT_MRG_CRT j on a.itm_nbr=j.itm_nbr
where g.cal_dt between '2015-10-31' and '2016-04-29'  and a.BRN_CD = 188 and TNS_TYP_CD = 1
			and i.fsc_wk_end_dt between '2015-10-31' and '2016-04-29' and e.mer_dvs_nbr = 33 group by 1,2,3,4) with data;
 
-- Assortmenr list
sel  a.brn_cd, c.brn_nme, b.pvt_brn_cgy_cd, d.des_txt as brnd_txt,f.asr_nbr,f.asr_des_txt
from  LOWES_VM.I0166A_VBU_ITM a 
inner join Lowes_VM.I1788A_PVT_BRN b on a.brn_cd = b.brn_cd
inner join Lowes_VM.I0410A_BRN c on a.brn_cd = c.brn_cd
inner join LOWES_VM.I1787A_PVT_BRN_CGY d on b.pvt_brn_cgy_cd = d.pvt_brn_cgy_cd
inner join lowes_vm.I0041A_ITM_HRC e on a.itm_nbr=e.itm_nbr
inner join lowes_vm.I0046A_ASR f on e.asr_nbr=f.asr_nbr
inner join CUST_VM.I1695E_CUS_SLS_DTL g on a.itm_nbr=g.itm_nbr
and g.cal_dt between '2015-10-31' and '2016-10-28'  and a.BRN_CD = 188 and TNS_TYP_CD = 1 and a.des_txt not like '%SOS%'  group by 1,2,3,4,5,6;


 --inner join CUST_VM.I1577C_GLD_CUS_ADR  h on g.cus_id=h.cus_id
 , (sum(tot_sal_amt)-sum(tot_cst_amt)+sum(mrg_crt_amt))/sum(tot_sal_amt) as margin_rate, 
count(distinct(trim(g.lct_nbr) || trim(g.cal_dt) || trim(mas_sal_tns_nbr)))  as trans_count,sum(tot_sal_amt)/count(distinct(trim(g.lct_nbr) || trim(g.cal_dt) || trim(mas_sal_tns_nbr))) as avg_basket_size

 and a.des_txt not like '%SOS%'  
sel top 10* from lowes_vm.I0101B_LCT_MRG_CRT
sel top 10* from cust_vm.I1695E_CUS_SLS_DTL where cal_dt = '2016-01-01'
sel top 10*  from LOWES_VM.I0166A_VBU_ITM
sel top 10* from lowes_vm.I1788A_PVT_BRN
sel * from lowes_vm.I0043A_MER_DVS
sel top 100* from CUST_VM.I1577C_GLD_CUS_ADR

case when sum(tot_sal_amt) =0 then 0
	else sum((tot_sal_amt)-sum(tot_cst_amt))/sum(tot_sal_amt) end as  margin_rate

sel * from cust_vm.I1695E_CUS_SLS_DTL where cus_id=1391594 