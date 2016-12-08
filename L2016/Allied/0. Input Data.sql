
--######################################################## Orlando Sprint Analysis ##########################################################################
-- Sample data of all tables
SELECT TOP 5 * FROM ea_perm.ab_tlog_header;
SELECT TOP 5 * FROM ea_perm.ab_tlog_detail;
SELECT TOP 5 * FROM ea_perm.ab_tlog_tender;
--SELECT TOP 5 * FROM ea_perm.ab_tlog_control; does not exist
SELECT TOP 5 * FROM ea_perm.ab_inventory;
--SELECT TOP 5 * FROM ea_perm.ab_inv_control; does not exist
SELECT TOP 5 * FROM ea_perm.ab_product;
SELECT TOP 5 * FROM ea_perm.ab_store;
SELECT * FROM ea_perm.ab_store;
SELECT TOP 500 * FROM ea_perm.ab_loyalty;
SELECT TOP 5 * FROM ea_perm.ab_ad_header;
SELECT TOP 5 * FROM ea_perm.ab_tos_header;
SELECT TOP 5 * FROM ea_perm.ab_tender;
sel top 100* from ea_perm.ab_loyalty where mosaic not in (' ')

-- Filter table based on registraion_date between ''2015--09-29'' and ''2016-08-26''
SELECT distinct a1.create_date,a1.shopper_status,a1.loyalty_no,a1.first_name,a1.middle_name,a1.last_name,a1.email,a1.points,a1.signup_dt,a1.signup_store,a1.mosaic,a1.phone,
a1.address_1,a1.address_2,a1.city,a1.state,a1.zip_code,a1.ADR_NBR,a1.LTD_MSR,a1.LNG_MSR
FROM ea_perm.ab_loyalty as a1 where a1.registration_date between '2015-09-29' and '2016-08-26' ; -- " and mosaic not in (' ') "  to exclude empty cases

-- Merge above filtered table with Lowes Demographic table


SELECT distinct a1.create_date,a1.shopper_status,a1.loyalty_no,a1.first_name,a1.middle_name,a1.last_name,a1.email,a1.points,a1.signup_dt,a1.signup_store,a1.mosaic,a1.phone,
a1.address_1,a1.address_2,a1.city,a1.state,a1.zip_code,a1.ADR_NBR,a1.LTD_MSR,a1.LNG_MSR,a2.store_no, a2.reg_id, a2.tran_no,a2.tos_cd,a2.start_time, a2.sale_trans,a2.tran_dt
FROM ea_perm.ab_loyalty as a1 inner join ea_perm.ab_tlog_header as a2 on a1.loyalty_no=a2.loyalty_no
where a1.registration_date between '2015-09-29' and '2016-08-26'  group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27;
 

select * from ea_perm.ab_loyalty where loyalty_no='4089845395';
select * from ea_perm.ab_loyalty where loyalty_no='9254768942';
select * from ea_perm.ab_tlog_header where loyalty_no='5593698634';
select * from ea_perm.ab_loyalty where registration_date='2016-06-12';

--########################### END ############################### Orlando Sprint Analysis ############################### END ##################################
