/*Script for exporting RUSLANA data to separate files with respect to region_id and include the name of the region*/

alter table firms_rusl_new add column region_id varchar 

update firms_rusl_new f
set region_id= r.region_id
from regions r
where f.region=r.region;

create table region_code as
select 
region as reg_name,
count(bvd_num) as firm_num
from firms_rusl_new
group by region;

alter table region_code
add column reg_code varchar;

create table firms_rusl_shrink_reg as
select
f._id,
f.name_long,
f.name_short,
f.bvd_num,
f.okpo,
f.inn,
f.region,
f.city,
f.status,
f.stat_ch_dt,
f.found_dt,
f.jur_form,
r.region_id
from firms_rusl_shrink f
left join regions_rusl r
on r.region=f.region;

CREATE INDEX firm_bvd_num_sh_idx ON firms_rusl_shrink_reg (bvd_num);
CREATE INDEX firm_inn_sh_idx ON firms_rusl_shrink_reg (inn);
CREATE INDEX firm_okpo_sh_idx ON firms_rusl_shrink_reg (okpo);
CREATE INDEX firm_region_sh_idx ON firms_rusl_shrink_reg (region);
CREATE INDEX firm_city_sh_idx ON firms_rusl_shrink_reg (city);
CREATE INDEX firm_name_short_sh_idx ON firms_rusl_shrink_reg (name_short);
CREATE INDEX firm_status_sh_idx ON firms_rusl_shrink_reg (status);
CREATE INDEX firm_jur_form_sh_idx ON firms_rusl_shrink_reg (jur_form);
CREATE INDEX firm_stat_ch_dt_sh_idx ON firms_rusl_shrink_reg (stat_ch_dt);
CREATE INDEX firm_found_dt_sh_idx ON firms_rusl_shrink_reg (found_dt);
CREATE INDEX firm_region_id_sh_idx ON firms_rusl_shrink_reg (region_id);

Select * from firms_rusl_shrink_reg
where COALESCE (region_id,  '') = '';

Select * from firms_rusl_fnl
where region_id = '100';

create table firms_rusl_fnl as
select
_id, 
name_long,
name_short,
bvd_num,
okpo,
inn,
region,
city,
status,
stat_ch_year,
to_number(found_year, '9999') as found_year,
jur_form,
region_id
from firms_rusl_shrink_reg

CREATE INDEX firm_bvd_num_fnl_idx ON firms_rusl_fnl (bvd_num);
CREATE INDEX firm_inn_fnl_idx ON firms_rusl_fnl (inn);
CREATE INDEX firm_okpo_fnl_idx ON firms_rusl_fnl (okpo);
CREATE INDEX firm_region_fnl_idx ON firms_rusl_fnl (region);
CREATE INDEX firm_city_fnl_idx ON firms_rusl_fnl (city);
CREATE INDEX firm_name_short_fnl_idx ON firms_rusl_fnl (name_short);
CREATE INDEX firm_status_fnl_idx ON firms_rusl_fnl (status);
CREATE INDEX firm_jur_form_fnl_idx ON firms_rusl_fnl (jur_form);
CREATE INDEX firm_stat_ch_year_fnl_idx ON firms_rusl_fnl (stat_ch_year);
CREATE INDEX firm_found_year_fnl_idx ON firms_rusl_fnl (found_year);
CREATE INDEX firm_region_id_fnl_idx ON firms_rusl_fnl (region_id);
