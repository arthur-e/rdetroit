﻿--CREATE TABLE census_crosswalk (trtid00 character varying(255), trtid10 character varying(255), weight numeric, placefp10 integer, cbsa10 integer, metdiv10 integer, ccflag10 integer, changetype integer)
COPY census_crosswalk (trtid00, trtid10, weight, placefp10, cbsa10, metdiv10, ccflag10, changetype) FROM '/home/arthur/Downloads/crosswalk_2000_2010.csv' WITH (FORMAT csv, HEADER)