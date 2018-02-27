
libname rais "\\Storage6\Bases\DADOS\RESTRITO\RAIS\SAS";
libname local "C:\Users\b2657804\Documents\temp";
libname auto "C:\Users\b2657804\Documents\Meu Drive\LAMFO\Adicionar a automation\Data";

%macro munic;
%do ano = 1986 %to 2016;
  %if &ano. < 2003 %then %do;
	%if &ano. < 1999 %then %do;
		 proc sql ;*inobs=1000;
		 	create table local.auto_all&ano.(compress=YES) as
		 	select UF, codemun, regiao_metro, &ano. as ano, cbo1994, grau_instr, sum(peso) as empregados, 
				   sum(rem_med_sm*peso)/sum(peso) as renda_m_sm, sum(rem_med_sm*peso) as renda_t_sm
		 	from rais.brasil&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo1994, grau_instr;

		 	create table local.auto&ano.(compress=YES) as 
		 	select UF, codemun, regiao_metro, ano, cbo1994, sum(empregados) as empregados, 
				   sum(renda_t_sm*empregados)/sum(empregados) as renda_m_sm, sum(renda_t_sm*empregados) as renda_t_sm 
		 	from local.auto_all&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo1994;
		 quit;
	%end;
  %else %do;
	 proc sql ;*inobs=1000;
		 	create table local.auto_all&ano.(compress=YES) as
		 	select UF, codemun, regiao_metro, &ano. as ano, cbo1994, grau_instr, sum(peso) as empregados, 
				   sum(rem_med_r*peso)/sum(peso) as renda_m, sum(rem_med_r*peso) as renda_t 
		 	from rais.brasil&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo1994, grau_instr;

		 	create table local.auto&ano.(compress=YES) as 
		 	select UF, codemun, regiao_metro, ano, cbo1994, sum(empregados) as empregados, 
				   sum(renda_t*empregados)/sum(empregados) as renda_m, sum(renda_t*empregados) as renda_t 
		 	from local.auto_all&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo1994;
		 quit;
  %end;
  %end;

    %if &ano. > 2002 %then %do;
		 proc sql ;*inobs=1000;
		 	create table local.auto_all&ano.(compress=YES) as
		 	select UF, codemun, regiao_metro, &ano. as ano, cbo2002, grau_instr, sum(peso) as empregados, 
				   sum(rem_med_r*peso)/sum(peso) as renda_m, sum(rem_med_r*peso) as renda_t 
		 	from rais.brasil&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo2002, grau_instr;

		 	create table local.auto&ano.(compress=YES) as 
		 	select UF, codemun, regiao_metro, ano, cbo2002, sum(empregados) as empregados, 
				   sum(renda_t*empregados)/sum(empregados) as renda_m, sum(renda_t*empregados) as renda_t 
		 	from local.auto_all&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo2002;
		 quit;
	%end;
%end;
    data auto.auto_painel(compress=YES); set local.auto1986-local.auto2016; run;
	data local.auto_all_painel; set local.auto_all1986-local.auto_all2016; run;
%mend munic;
%munic;


/* Conversao CBO94 para CBO2002 */
data cbo_trad;
  infile "C:\Users\b2657804\Documents\Meu Drive\LAMFO\AutomationJobs\Data\CBO94 - CBO2002 - Conversao com 90.csv" 
  FIRSTOBS=2 dsd truncover delimiter = ";";
  length CBO1994 $5 CBO_2002 $6   ;
  input CBO1994 CBO_2002;
run;

PROC SORT DATA=auto.auto_painel OUT=auto; BY CBO1994; RUN;
PROC SORT DATA=cbo_trad 				; BY CBO1994; RUN;

DATA AUTO_PAINEL; MERGE auto(IN=A) cbo_trad; 
    BY CBO1994;
	IF A;
	IF CBO2002 = '' THEN CBO2002 = CBO_2002;
	drop CBO1994 CBO_2002;
RUN;


/* Conversao da renda em Salrarios Minimos*/
data SM;
  infile "C:\Users\b2657804\Documents\Meu Drive\LAMFO\Adicionar a automation\ipeadata[27-02-2018-05-27].csv"
  FIRSTOBS=2 dsd truncover delimiter = ",";
  length DATA $7 ;
  input DATA SM;
run;

PROC SQL;
	CREATE TABLE SM AS 
	SELECT input(SUBSTR(Data, 1, 4), best12.) AS ANO FORMAT BEST12., AVG(SM) AS SM
	FROM SM
	GROUP BY ANO;
QUIT;

PROC SORT DATA = AUTO_PAINEL OUT = auto; BY ANO; RUN;
DATA AUTO_PAINEL; MERGE auto(IN=A) SM;
    BY ANO;
	IF renda_m = . THEN DO;
	    renda_m = renda_m_sm*sm;
		renda_t = renda_t_sm*sm;
    END;
	IF A;

	drop renda_m_sm renda_t_sm sm renda_m;

	if cbo2002 = '' or codemun = '' then delete;

	rename renda_t = renda;
RUN;

/*---- Salvando ----*/
data auto.auto_painel(compress=YES); set AUTO_PAINEL; run;


/* Acrescentando Job Zones */
proc import datafile="C:\Users\b2657804\Documents\Meu Drive\LAMFO\AutomationJobs\Data\CBO2002_SOC.csv"
			out = CBO_SOC dbms=CSV replace; delimiter=";";
run; 

PROC SQL;
CREATE TABLE CBO_SOC AS 
SELECT CBO2002, ROUND(AVG(input(Job_Zone, best12.))) as Job_Zone
FROM CBO_SOC
group by CBO2002;

SELECT Job_Zone, count(*)
from CBO_SOC
group by Job_Zone;
QUIT;

PROC SORT DATA = AUTO_PAINEL OUT = auto; BY CBO2002; RUN;
DATA AUTO_PAINEL; MERGE auto(IN=A) cbo_soc;
    BY CBO2002;
	IF A;
RUN;

DATA AUTO_PAINEL; MERGE auto(IN=A) cbo_soc;
    BY CBO2002;
	IF A;
RUN;

PROC SQL;
CREATE TABLE AUTOMATION AS 
SELECT UF, codemun, regiao_metro, ano, cbo2002, Job_Zone, SUM(empregados) AS empregados, sum(renda*empregados) as renda
FROM AUTO_PAINEL
GROUP BY UF, codemun, regiao_metro, ano, cbo2002, Job_Zone;

SELECT Job_Zone, count(*)/28965177 AS FREQ
from AUTOMATION
group by Job_Zone;

CREATE TABLE JobZone_Painel AS 
SELECT UF, codemun, regiao_metro, ano, Job_Zone, SUM(empregados) AS empregados, sum(renda*empregados) as renda
FROM AUTOMATION
GROUP BY UF, codemun, regiao_metro, ano, Job_Zone;

QUIT;


/*---- Salvando ----*/
data auto.automation(compress=YES); set automation; run;
data auto.JobZone_Painel(compress=YES); set JobZone_Painel; run;


