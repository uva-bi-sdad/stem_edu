SELECT hd2012_2017.unitid, hd2012_2017.year, hd2012_2017.instnm, hd2012_2017.addr, hd2012_2017.city,
hd2012_2017.stabbr, hd2012_2017.zip, hd2012_2017.fips, hd2012_2017.sector,
hd2012_2017.iclevel, hd2012_2017.hloffer, hd2012_2017.ugoffer,
hd2012_2017.groffer, hd2012_2017.hdegofr1, hd2012_2017.deggrant,
hd2012_2017.instcat, hd2012_2017.countycd, hd2012_2017.longitud,
hd2012_2017.latitude, ic2012_2017.level1, ic2012_2017.level2,
ic2012_2017.level3,
ic2012_2017.level4,
ic2012_2017.level5,
ic2012_2017.level6,
ic2012_2017.level7,
ic2012_2017.level8,
ic2012_2017.level12,
ic2012_2017.level17,
ic2012_2017.level18,
ic2012_2017.level19,
ic2012_2017.distcrs,
ic2012_2017.distnced,
ic2012_2017.dstnced1,
ic2012_2017.dstnced2,
ic2012_2017.dstnced3
INTO temp_hd_ic_2012_2017
FROM hd2012_2017
FULL OUTER JOIN ic2012_2017
ON hd2012_2017.unitid = ic2012_2017.unitid
AND hd2012_2017.year = ic2012_2017.year




SELECT *
INTO temp_hd_ic_efa_2012_2017
FROM temp_hd_ic_2012_2017
FULL OUTER JOIN
     (SELECT
      efa2012_2016.unitid AS efa2012_2016_unitid,
      efa2012_2016.year AS efa2012_2016_year,
      efa2012_2016.efaianm,
      efa2012_2016.efaianw,
      efa2012_2016.efasiam,
      efa2012_2016.efasiaw,
      efa2012_2016.efbkaam,
      efa2012_2016.efbkaaw,
      efa2012_2016.efhispm,
      efa2012_2016.efhispw,
      efa2012_2016.efnhpim,
      efa2012_2016.efnhpiw,
      efa2012_2016.efwhitm,
      efa2012_2016.efwhitw,
      efa2012_2016.ef2morm,
      efa2012_2016.ef2morw,
      efa2012_2016.efunknm,
      efa2012_2016.efunknw,
      efa2012_2016.efnralm,
      efa2012_2016.efnralw
     FROM efa2012_2016) as e
ON temp_hd_ic_2012_2017.unitid = e.efa2012_2016_unitid
AND temp_hd_ic_2012_2017.year = e.efa2012_2016_year


SELECT *
INTO temp_hd_ic_efa_efb_2012_2017
FROM temp_hd_ic_efa_2012_2017
FULL OUTER JOIN
     (SELECT
      efb2012_2016.unitid AS efb2012_2016_unitid,
      efb2012_2016.year AS efb2012_2016_year,
      efb2012_2016.efbage,
      efb2012_2016.efage01,
      efb2012_2016.efage02,
      efb2012_2016.efage03,
      efb2012_2016.efage04
     FROM efb2012_2016) as e
ON temp_hd_ic_efa_2012_2017.unitid = e.efb2012_2016_unitid
AND temp_hd_ic_efa_2012_2017.year = e.efb2012_2016_year
