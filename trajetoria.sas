libname rais "\\storage6\bases\DADOS\RESTRITO\rais\SAS";
libname salva "C:\Users\b2657804\Documents\Meu Drive\LAMFO\trajetoria";
libname local "C:\Users\b2657804\Documents\trajetoria";


%macro trajetoria;
%do ano = 1986 %to 2017;
%if &ano. < 2003 %then %let cbo = cbo1994;
%else %let cbo = cbo2002;
  proc sql inobs=1000;
    create table tra&ano.(compress=YES) as
	  select pis, &cbo. as &cbo.&ano.
	from rais.brasil&ano.(where=(mes_deslig='00' and pis ne ''))
	order by pis;
  quit;
%end;

data traj_all(compress=YES); merge tra1986-tra2017;
  by pis;
  n = cmiss(of cbo19941986--cbo20022017);
run;
%mend trajetoria;
%trajetoria;


/*------------------------------------------------------------------------*/
/*  Investigar a trajetoria dos empregados da RAIS */
/*------------------------------------------------------------------------*/
/* Passo 1: Traduzir CBO1994 para CBO2002 */
data cbo_trad;
  infile "\\sbsb2\dpti\Usuarios\Rafael Morais\automation\conversions\CBO94 - CBO2002 - Conversao com 90.csv"
  FIRSTOBS=2 dsd truncover delimiter = ";";
  length CBO1994 $5 CBO_2002 $6 ;
  input CBO1994 CBO_2002;
run;

PROC SORT DATA = cbo_trad  ; BY CBO1994; RUN;
%macro traduz;
%do ano = 1986 %to 2002;
PROC SORT DATA = traj_all; BY CBO1994&ano.; RUN;
DATA traj_all; MERGE traj_all(IN=A rename = CBO1994&ano.=CBO1994) cbo_trad; 
    BY CBO1994;
	IF A;
	rename CBO_2002 = cbo2002&ano.;
	drop CBO1994;
RUN;
%end;
%mend traduz;
%traduz;

/* Passo 2: Traduzir para Job Zones */
proc import datafile = "\\sbsb2\dpti\Usuarios\Rafael Morais\Auto\CBO_OK.csv"
    out = CBO_JOB DBMS = CSV REPLACE; delimiter = ';'; 
run;

proc sort data = CBO_JOB   ; by CBO2002; run; 
%macro JZ;
%do ano = 1986 %to 2017;
PROC SORT DATA = traj_all      ; BY CBO2002&ano.; RUN;
DATA traj_all; MERGE traj_all(IN=A rename = CBO2002&ano.=CBO2002) CBO_JOB; 
    BY CBO2002;
	IF A;
	rename JOB_ZONE = JZ&ano.;
	drop CBO2002;
RUN;
%end;
%mend traduz;
%JZ;

data trajetoria; set traj_all; if n < 31; run;
data traj_incom; set traj_all; if n = 31; run;


/* Passo 3: Contagem */
%macro probab;
%do ano = 1986 %to 2000;
proc sql; 
create table test%eval(&ano.+1) as 
select &ano. as ano, JZ&ano. as inicio, JZ%eval(&ano.+1) as fim, count(*) as freq
from trajetoria
where JZ&ano. <> '' and JZ%eval(&ano.+1) <> ''
group by JZ&ano., JZ%eval(&ano.+1);

create table count as 
select ano, inicio, sum(freq) as total
from test%eval(&ano.+1)
group by ano, inicio;

create table test%eval(&ano.+1) as 
select a.ano, a.inicio, a.fim, a.freq/b.total as prob
from test%eval(&ano.+1) a left join count b on a.ano = b.ano and a.inicio = b.inicio;

create table inco%eval(&ano.+1) as 
  select &ano. as ano, 'income' as inicio, JZ%eval(&ano.+1) as fim, count(*) as freq
from traj_incom
where JZ%eval(&ano.+1) <> ''
group by JZ&ano., JZ%eval(&ano.+1);

create table count_inc as 
select ano, inicio, sum(freq) as total
from inco%eval(&ano.+1)
group by ano, inicio;

create table test_inc%eval(&ano.+1) as 
select a.ano, a.inicio, a.fim, a.freq/b.total as prob
from inco%eval(&ano.+1) a left join count_inc b on a.ano = b.ano;

quit;

%end;

data prob; set test1987-test2017 ; run;
data prob_inc; set test_inc1987-test_inc2017; run;
%mend probab;
%probab;

data prob; set prob prob_inc; run;

proc sql; 
create table inco as 
  select 'income' as inicio, JZ%eval(&ano.+1) as fim, count(*) as freq

/* Passo 4: Visualização */
proc sort data = prob(where=(inicio='1' and fim<>'1')) out = graph; by ano fim; run;

proc sgplot data = graph;
   series x = ano y = prob/ group = fim; 
   yaxis label="Probabilidade de Transição";
run;
