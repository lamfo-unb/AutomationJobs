/*-----------------------------------------------------------------------------*/
/*---- 			Creation of CBOS database of employees from RAIS		   ----*/
/*-----------------------------------------------------------------------------*/

libname rais "\\Storage6\Bases\DADOS\RESTRITO\RAIS\SAS";
libname auto "C:\Users\B2657804\Desktop\AutomationJobs\data";

/*========   Part I: Create the panel data   ========*/
%macro create_data;
%do ano = 1986 %to 2016;
  %if &ano. < 2003 %then %let cbo = cbo1994;
  %else %let cbo = cbo2002;

  %if 2002 < &ano. and &ano. < 2007 %then %let renda = rem_media;
  %else %let renda = rem_med_sm;

  proc sql ;*inobs=1000;
 	create table auto&ano. as
     	select &ano. as ano, &cbo., count(*) as empregados
	from rais.brasil&ano.(where=(mes_deslig='00' and &cbo. ne ''))
	group by ano, &cbo.;
  quit;
%end;
    data auto_painel; set auto1986-auto2016; run;
%mend create_data;
%create_data;

/*--- Convertion CBO1994 to CBO2002 ---*/
data cbo_trad;
  infile "C:\Users\rafal\Google Drive\LAMFO\AutomationJobs\Data\CBO94 - CBO2002 - Conversao com 90.csv"
  FIRSTOBS=2 dsd truncover delimiter = ";";
  length CBO1994 $5 CBO2002 $6 ;
  input CBO1994 CBO2002;
run;

PROC SORT DATA=auto_painel(where=(ano=2003)) out=cbos; BY CBO2002; RUN;
PROC SORT DATA=cbo_trad ; BY CBO2002; RUN;
DATA convert; MERGE cbos cbo_trad;    
    BY CBO2002;
RUN;

proc sql;    * Create a weighted convertion table: CBO1994 to CBO2002;
create table weight as
  select a.cbo1994, cbo2002 as cbo_2002, emp / tot as prop
from convert a, (select cbo1994, sum(emp) as tot from convert group by cbo1994) as b
where a.cbo1994 = b.cbo1994 and a.cbo1994 ne ''
order by a.cbo1994;
quit;

data weight; set weight; if prop = . then prop = 0; run;

proc sql;  * Merge the weight to auto_painel ;
create table auto_painel2 as
  select a.*, b.*
from auto_painel as a full outer join weight b
on a.cbo1994 = b.cbo1994;
quit;

data auto_painel2; set auto_painel2;   * Make conversion ;
  if prop = . then prop = 1;
  if cbo2002 = '' then cbo2002 = cbo_2002;
  if empregados = . then delete;
run;

proc sql;  * Calculate the proportional number of employees for each CBO 2002 ;
create table auto_painel2 as
  select ano, cbo2002, sum(empregados*prop) as empregados
from auto_painel2
group by ano, cbo2002;
quit;

/*---- Export the data ----*/
proc export outfile='C:\Users\B2657804\Desktop\AutomationJobs\Data\RAIS_CBO_1986-2016.csv'
  data = auto_painel2 DBMS = CSV REPLACE;
run;

/*========   Part II: Assing the Job Zones   ========*/
proc import datafile = "C:\Users\b2657804\Documents\Meu Drive\LAMFO\AutomationJobs\Conversions\CBO_OK.csv" 
    out = CBO_JOB DBMS = CSV REPLACE; delimiter = ';'; 
run;

proc sql;
  create table auto as 
    select a.*, b.*
  from auto.automation2(drop=job_zone) as a left join cbo_job as b on a.CBO2002 = b.CBO2002;

  * Calculando empregados e renda esperados ;
  create table auto as 
    select UF, codemun, regiao_metro, ano, CBO2002, job_zone, empregados/rep as empregados, 
           renda/rep as renda, renda_sm/rep as renda_sm
  from auto;
quit;

* Agregações sem CBO ; 
PROC SQL;
CREATE TABLE AUTOMATION AS 
	SELECT UF, codemun, regiao_metro, ano, cbo2002, Job_Zone, SUM(empregados) AS empregados, 
           sum(renda) as renda, sum(renda_sm) as renda_sm
FROM auto
WHERE codemun ne ''
GROUP BY UF, codemun, regiao_metro, ano, cbo2002, Job_Zone;

CREATE TABLE JobZone_Painel AS 
	SELECT UF, codemun, regiao_metro, ano, Job_Zone, SUM(empregados) AS empregados, 
		   sum(renda) as renda, sum(renda_sm) as renda_sm
	FROM AUTOMATION
	GROUP BY UF, codemun, regiao_metro, ano, Job_Zone;
QUIT;

/*---- Salvando ----*/
data auto.automation2(compress=YES); set automation; run;
data auto.JobZone_Painel2(compress=YES); set JobZone_Painel; run;







