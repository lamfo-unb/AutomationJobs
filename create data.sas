/*-----------------------------------------------------------------------------------------*/
/*---- 			Criação de Base de dados de CBOS de empregados vindo da RAIS		   ----*/
/*----	    Nível: Ano, UF, RM, Município, CBO2002/Job_Zone -- Renda, Nº Empregados	   ----*/
/*-----------------------------------------------------------------------------------------*/

/*==== 						UTILIZANDO EMPREGADOS EM DEZEMBRO DO ANO CORRENTE  		   ====*/


libname rais "\\Storage6\Bases\DADOS\RESTRITO\RAIS\SAS";
libname local "C:\Users\b2657804\Documents\temp\temp";
libname auto "C:\Users\b2657804\Documents\Meu Drive\LAMFO\Adicionar a automation\Data";

%macro munic;
%do ano = 1986 %to 2016;
  %if &ano. < 2003 %then %do;
	%if &ano. < 1999 %then %do;
		 proc sql ;*inobs=1000;
		 	create table local.auto_all&ano.(compress=YES) as
		 	select UF, codemun, regiao_metro, &ano. as ano, cbo1994, grau_instr, count(*) as empregados, 
				    sum(rem_med_sm) as renda_t_sm
		 	from rais.brasil&ano.(where=(mes_deslig='00'))
		 	group by UF, codemun, regiao_metro, ano, cbo1994, grau_instr;

		 	create table local.auto&ano.(compress=YES) as 
		 	select UF, codemun, regiao_metro, ano, cbo1994, sum(empregados) as empregados, 
				   sum(renda_t_sm) as renda_t_sm 
		 	from local.auto_all&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo1994;
		 quit;
	%end;
  %else %do;
	 proc sql ;*inobs=1000;
		 	create table local.auto_all&ano.(compress=YES) as
		 	select UF, codemun, regiao_metro, &ano. as ano, cbo1994, grau_instr, count(*) as empregados, 
				    sum(rem_med_r) as renda_t 
		 	from rais.brasil&ano.(where=(mes_deslig='00'))
		 	group by UF, codemun, regiao_metro, ano, cbo1994, grau_instr;

		 	create table local.auto&ano.(compress=YES) as 
		 	select UF, codemun, regiao_metro, ano, cbo1994, sum(empregados) as empregados, 
				   sum(renda_t) as renda_t 
		 	from local.auto_all&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo1994;
		 quit;
  %end;
  %end;

    %if &ano. > 2002 %then %do;
		 proc sql ;*inobs=1000;
		 	create table local.auto_all&ano.(compress=YES) as
		 	select UF, codemun, regiao_metro, &ano. as ano, cbo2002, grau_instr, count(*) as empregados, 
				  sum(rem_med_r) as renda_t 
		 	from rais.brasil&ano.(where=(mes_deslig='00'))
		 	group by UF, codemun, regiao_metro, ano, cbo2002, grau_instr;

		 	create table local.auto&ano.(compress=YES) as 
		 	select UF, codemun, regiao_metro, ano, cbo2002, sum(empregados) as empregados, 
				    sum(renda_t) as renda_t 
		 	from local.auto_all&ano.
		 	group by UF, codemun, regiao_metro, ano, cbo2002;
		 quit;
	%end;
%end;
    data auto_painel2(compress=YES); set local.auto1986-local.auto2016; run;
	data local.auto_all_painel2; set local.auto_all1986-local.auto_all2016; run;
%mend munic;
%munic;

/*--- Conversao CBO94 para CBO2002 ---*/
data cbo_trad;
  infile "C:\Users\b2657804\Documents\Meu Drive\LAMFO\AutomationJobs\Data\CBO94 - CBO2002 - Conversao com 90.csv" 
  FIRSTOBS=2 dsd truncover delimiter = ";";
  length CBO1994 $5 CBO_2002 $6 ;
  input CBO1994 CBO_2002;
run;

PROC SORT DATA=auto_painel2 OUT=auto; BY CBO1994; RUN;
PROC SORT DATA=cbo_trad 				; BY CBO1994; RUN;
DATA AUTO_PAINEL; MERGE auto(IN=A) cbo_trad; 
    BY CBO1994;
	IF A;
	IF CBO2002 = '' THEN CBO2002 = CBO_2002;
	drop CBO1994 CBO_2002;
RUN;

/*--- Conversao da renda em Salrarios Minimos ---*/
data SM;
  infile "C:\Users\b2657804\Documents\Meu Drive\LAMFO\Adicionar a automation\ipeadata[27-02-2018-05-27].csv"
  FIRSTOBS=2 dsd truncover delimiter = ",";
  length DATA $7 ;
  input DATA SM;
run;

PROC SQL; * Tornando a série de Salário Mínimo anual;
	CREATE TABLE SM AS 
	SELECT input(SUBSTR(Data, 1, 4), best12.) AS ANO FORMAT BEST12., AVG(SM) AS SM
	FROM SM
	GROUP BY ANO;
QUIT;

PROC SORT DATA = AUTO_PAINEL OUT = auto; BY ANO; RUN;
DATA AUTO_PAINEL; MERGE auto(IN=A) SM;
    BY ANO;
	IF renda_m = . THEN renda_t = renda_t_sm*sm;
 	IF A;

	renda_sm = renda_t/sm; * Renda em SM ;

	drop renda_m_sm renda_t_sm sm renda_m;

	if cbo2002 = '' or codemun = '' then delete;

	rename renda_t = renda;
RUN;

/*---- Salvando ----*/
data auto.auto_painel2(compress=YES); set AUTO_PAINEL; run;

/*--- Acrescentando Job Zones ---*/
proc import datafile = "C:\Users\b2657804\Documents\Meu Drive\LAMFO\AutomationJobs\Conversions\CBO_OK.csv" 
    out = CBO_JOB DBMS = CSV REPLACE; delimiter = ';'; 
run;

proc sql;
  create table auto as 
    select a.*, b.*
  from auto.automation2(drop=job_zone) as a left join cbo_job as b on a.CBO2002 = b.CBO2002;

  * Calculando empregados e renda esperados ;
  create table auto as 
    select UF, codemun, regiao_metro, ano, CBO2002, empregados/rep as empregados, 
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







