/*-----------------------------------------------------------------------------*/
/*---- 			Creation of CBOS database of employees from RAIS		   ----*/
/*-----------------------------------------------------------------------------*/

libname rais "\\Storage6\Bases\DADOS\RESTRITO\RAIS\SAS";
libname auto "\\sbsb2\dpti\Usuarios\Rafael Morais\Auto";

/*========   Part I: Create the panel data   ========*/
%macro create_data;
%do ano = 1986 %to 2017;
  %if &ano. < 2003 %then %let cbo = cbo1994;
  %else %let cbo = cbo2002;
  proc sql ;*inobs=1000;
 	create table auto&ano. as
     	select &ano. as ano, &cbo., count(*) as empregados
	from rais.brasil&ano.(where=(mes_deslig='00' and &cbo. ne ''))
	group by ano, &cbo.;
  quit;
%end;
    data auto_painel; set auto1986-auto2017; run;
%mend create_data;
%create_data;

/*--- Convertion CBO1994 to CBO2002 ---*/
data cbo_trad;
  infile "\\sbsb2\dpti\Usuarios\Rafael Morais\Auto\CBO94 - CBO2002 - Conversao com 90.csv"
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
proc export outfile='\\sbsb2\dpti\Usuarios\Rafael Morais\Auto\RAIS_CBO_1986-2017.csv'
  data = auto_painel2 DBMS = CSV REPLACE;
run;

/*========   Part II: Assing the Job Zones   ========*/
proc import datafile = "\\sbsb2\dpti\Usuarios\Rafael Morais\Auto\CBO_OK.csv"
    out = CBO_JOB DBMS = CSV REPLACE; delimiter = ';'; 
run;
proc sql; insert into CBO_JOB values('030115', '4', 1); quit;


proc sql;
  create table auto as 
    select a.*, b.*
  from auto_painel2 as a left join cbo_job as b on a.CBO2002 = b.CBO2002;

  * Calculando empregados e renda esperados ;
  create table auto as 
    select ano, CBO2002, job_zone, empregados/rep as empregados
  from auto;
quit;

* Agregações sem CBO ; 
PROC SQL;
CREATE TABLE JobZone_Painel AS 
	SELECT ano, Job_Zone, SUM(empregados) AS empregados
	FROM auto
	GROUP BY ano, Job_Zone;
QUIT;

/*---- Salvando ----*/
data auto.JobZone_Painel(compress=YES); set JobZone_Painel; run;
proc export data=JobZone_Painel outfile="\\sbsb2\dpti\Usuarios\Rafael Morais\Auto\jobzone_painel.csv"
 dbms=csv replace; 
run;






