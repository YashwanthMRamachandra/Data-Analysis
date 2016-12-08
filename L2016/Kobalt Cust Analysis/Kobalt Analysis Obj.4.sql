--  Examine sales volume of new customers & demistify their behaviour, hereafter who resides as Kobalt customers

sel  a.brn_cd, e.asr_nbr, g.cus_id, i.fsc_wk_end_dt,
sum(tot_sal_amt) as sales, sum(tot_itm_qty) as units, sum(tot_cst_amt) as cost, sum(tot_sal_amt)-sum(tot_cst_amt)+sum(mrg_crt_amt) as margin
from  LOWES_VM.I0166A_VBU_ITM a 
inner join lowes_vm.I0041A_ITM_HRC e on a.itm_nbr=e.itm_nbr
inner join CUST_VM.I1695E_CUS_SLS_DTL  g on a.itm_nbr=g.itm_nbr
inner join lowes_vm.I0036K_FSC_CAL_CNV i on i.cal_dt=g.cal_dt
/*inner join lowes_vm.I0101B_LCT_MRG_CRT j on a.itm_nbr=j.itm_nbr*/
where g.cal_dt between '2015-10-31' and '2016-10-28'  and a.BRN_CD = 188 and TNS_TYP_CD = 1
			and i.fsc_wk_end_dt between '2015-10-31' and '2016-10-28' and e.mer_dvs_nbr = 33 
			group by 1,2,3,4,5;
			
-- 11,590,438
sel  a.brn_cd, g.cus_id, h.gld_cus_id, i.fsc_wk_end_dt
from  LOWES_VM.I0166A_VBU_ITM a 
inner join CUST_VM.I1695E_CUS_SLS_DTL  g on a.itm_nbr=g.itm_nbr
inner join CUST_VM.I1577C_GLD_CUS_ADR  h on g.cus_id=h.cus_id
inner join lowes_vm.I0036K_FSC_CAL_CNV i on i.cal_dt=g.cal_dt
where g.cal_dt between '2015-10-31' and '2016-10-28'  and a.BRN_CD = 188 and TNS_TYP_CD = 1
			and i.fsc_wk_end_dt between '2015-10-29' and '2016-10-28'  group by 1,2,3,4;

