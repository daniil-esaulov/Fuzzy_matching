/*Script for creating PostgreSQL database containing Ruslana data*/

CREATE TABLE firms_rusl_shrink
( _id character varying,
  _id2 character varying,
  name_long character varying,
  name_short character varying,
  bvd_num character varying,
  okpo character varying,
  inn character varying,
  region character varying,
  city character varying,
  status character varying,
  stat_ch_dt character varying,
  found_dt character varying,
  jur_form character varying);
  



  COPY firms_rusl_shrink
	FROM 'D:\Esaulov\Ruslana_comb\ruslana_5.csv' 
	DELIMITER ';' 
	CSV HEADER;


CREATE INDEX firm_bvd_num_idx_shrink ON firms_rusl_shrink (bvd_num);
CREATE INDEX firm_inn_idx_shrink ON firms_rusl_shrink (inn);
CREATE INDEX firm_okpo_idx_shrink ON firms_rusl_shrink (okpo);
CREATE INDEX firm_region_idx_shrink ON firms_rusl_shrink (region);
CREATE INDEX firm_city_idx_shrink ON firms_rusl_shrink (city);
CREATE INDEX firm_name_short_idx_shrink ON firms_rusl_shrink (name_short);
CREATE INDEX firm_status_idx_shrink ON firms_rusl_shrink (status);
CREATE INDEX firm_jur_form_idx_shrink ON firms_rusl_shrink (jur_form);

ALTER TABLE firms_rusl_shrink ADD COLUMN found_dt_new character varying
UPDATE firms_rusl_shrink SET found_dt_new = right(found_dt, 4)
ALTER TABLE firms_rusl_shrink ALTER COLUMN found_dt_new TYPE DATE 
using to_date(found_dt_new, 'YYYY/MM/DD');


