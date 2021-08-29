/*nale¿y zmieniæ scie¿ke na wlasn¹ */
libname wyn "C:\Users\macie\Documents\SAS\zab";

/*WYŒWIETLENIE PODSTAWOWYCH W£ASNOŒCI ZBIORU*/
proc contents data=wyn.data out=gh_types;
run;
/*Usuniêcie zbêdnej zmiennej - var1, która stanowi Id*/
data dane_surowe;
	set wyn.data;
	drop var1;
run;
/*Sprawdzenie zawartoœci zbioru "na szybko"*/
proc freq nlevels data= dane_surowe;
run;
proc corr data= dane_surowe;
	var _numeric_;
	with price;
run;
/*MACRO WYZNACZAJ¥CE BRAKI DANYCH DLA POSZCZEGÓLNYCH ZMIENNYCH*/
%macro overview(dset);
proc iml;
	use &dset;
	read all var _NUM_ into x[colname=nNames]; 
	n = countn(x,"col");
	nmiss = countmiss(x,"col");
	read all var _CHAR_ into x[colname=cNames]; 
	close data.abt_sam_beh_train;
	c = countn(x,"col");
	cmiss = countmiss(x,"col");
	Names = cNames || nNames;
	rNames = {"Missing", "NotMissing"};
	cnt = (cmiss // c) || (nmiss // n);
	create missing_gh from cnt[r=rNames c=Names];
	append from cnt[r=rNames];
close;

proc transpose data=missing_gh out=missing_gh_t;
	id rnames;
run;

data missing_ov;
set missing_gh_t;
	format
	percent percent8.2;
	percent=Missing/(Missing+NotMissing);
	total=(Missing+NotMissing);
run;
proc sort data=missing_ov;
	by descending percent;
run;
%mend;

/*UTWORZENIE LISTY ZMIENNYCH*/
proc sql noprint;
        select name into :vlist separated by ' '
        from dictionary.columns
        where memname = upcase("dane_surowe");
quit;

/*MACRO PODSUMOWUJ¥CE PODSTAWOWE W£AŒCIWOŒCI DOT BRAKÓW DANYCH I TYPÓW ZMIENNYCH*/
%macro missing_ov(dset, title_1);
%overview(&dset)
ods exclude all; 
ods output nlevels=work.nlev_all;
proc freq data=&dset nlevels;
tables _all_;
run;
ods exclude none; 
proc sql;
	create table raw_report as
	select missing_ov._NAME_, missing_ov.Missing, missing_ov.NotMissing,missing_ov.percent, nlev_all.NLevels, case when gh_types.TYPE=1 then "NUM" else "CHAR" end as type
	from missing_ov
	inner join nlev_all on missing_ov._NAME_ = nlev_all.TableVar
	inner join gh_types on missing_ov._NAME_ = gh_types.NAME;
run;
title2 &title_1;
/*title "Podstawowe informacje na temat zbioru danych";*/
proc print data=raw_report label;
	var _NAME_ Missing NotMissing percent NLevels type;
	label 
	_NAME_="Nazwa zmiennej"
	Missing="Liczba brakuj¹cych obserwacji"
	NotMissing="Liczba nie brakuj¹cych obserwacji"
	percent = "Zawartoœæ brakuj¹cych obserwacji [%]"
	NLevels = "Liczba poziomów zmiennej"
	type = "Typ zmiennej";
run;
title;
title2;
%mend;
/*pozostawienie zmiennych, które bêd¹ uwzglêdnione w modelu*/
/*odrzucenie na podstawie badañ na zbiorze wejœciowym oraz opinii eksperckiej*/
data ds_transformed;
	set dane_surowe;
	keep price living_space rooms bathrooms floors condition;
run;


/*odrzucenie obserwacji, które nie mia³y sensu, prawdopodobnie b³êdy podczas wprowadzania danych*/
data outliers_out;
	set ds_transformed;
	if condition="" then delete;
	if Price = 0 then delete;
	if Living_space = 0 then delete;
	if floors = 0 then delete;
	if bathrooms = 0 then delete;
run;
/*MACRO DO USUWANIA OUTLIEROW */
%macro outliers(input=, var=, output= );
%let Q1=;%let Q3=;%let varL=;%let varH=;
%let n=%sysfunc(countw(&var));
%do i= 1 %to &n;
%let val = %scan(&var,&i);
%let Q1 = &Q1 &val._P25;%let Q3 = &Q3 &val._P75;%let varL = &varL &val.L;%let varH = &varH &val.H;
%end;

proc means data=&input nway noprint;
var &var;
output out=temp P25= P75= / autoname;
run;
data temp;
set temp;
ID = 1;
array varb(&n) &Q1;
array varc(&n) &Q3;
array lower(&n) &varL;
array upper(&n) &varH;
do i = 1 to dim(varb);
lower(i) = varb(i) - 1.5 * (varc(i) - varb(i));
upper(i) = varc(i) + 1.5 * (varc(i) - varb(i));
end;
drop i _type_ _freq_;
run;

data temp1;
set &input;
ID = 1;
run;

data &output;
merge temp1 temp;
by ID;
array var(&n) &var;
array lower(&n) &varL;
array upper(&n) &varH;
do i = 1 to dim(var);
if not missing(var(i)) then do;
if var(i) >= lower(i) and var(i) <= upper(i);
end;
end;
drop &Q1 &Q3 &varL &varH ID i;
run;
%mend;
%outliers(input=outliers_out, var= price living_space rooms bathrooms floors, output=outliers_out_final);


%let char_list = Condition;
%let num_list =  price living_space rooms bathrooms floors;
proc format ;
	value $ missfmt ' '="Brak danych";
run;
%macro char_bar(dset,_list,title_1);
%local i next_name;
%do i=1 %to %sysfunc(countw(&_list));
%let next_name = %scan(&_list, &i);
title "Podstawowe informacje o zmiennej" %sysfunc(upcase(&next_name)) &title_1;
proc freq data=&dset nlevels;
	tables &next_name /plots=freqplot() missing;
	format _character_ $missfmt.;
run;
%end;
title;
%mend;

%macro outliers_boxplot(dset,_list,title_1);
%local i next_name;
%do i=1 %to %sysfunc(countw(&_list));
%let next_name = %scan(&_list, &i);
title "Wykres pude³kowy dla zmiennej" %sysfunc(upcase(&next_name)) &title_1;
proc sgplot data=&dset;
	hbox &next_name;
run;
%end;
title;
%mend;
/*Raporty robocze*/
proc corr data=dane_surowe;
	var _numeric_;
	with Price;
run;
proc corr data=outliers_out_final;
	var _numeric_;
	with Price;
run;


/*BLOK RAPORTOWY*/
/*Scie¿ka do ustalenia/nale¿y wprowadzic swoj¹ scie¿kê*/
ods excel file="C:\Users\macie\Documents\SAS\zab\RepP1.xlsx" options(Sheet_Interval='none');
%missing_ov(dane_surowe,"Podstawowe informacje na temat zbioru danych przed usuniêciem obserwacji odstaj¹cych")
%missing_ov(outliers_out_final,"Podstawowe informacje na temat zbioru danych po usuniêciu obserwacji odstaj¹cych")
%outliers_boxplot(dane_surowe,&num_list,"przed usuniêciem wartoœci odstaj¹cych")
%outliers_boxplot(outliers_out_final,&num_list,"po usuniêciu wartoœci odstaj¹cych")
ods graphics on;
%char_bar(ds_transformed,&char_list,"przed usuniêciem wartoœci odstaj¹cych");
ods graphics off;
ods graphics on;
%char_bar(outliers_out_final,&char_list,"po usuniêciu wartoœci odstaj¹cych");
ods graphics off;

ODS EXCEL CLOSE;

/*--------------------------------------------------------------------- */

/*Sprawdzenie zawartoœci zbioru */
proc freq nlevels data= outliers_out_final;
run;



/* Liczba imputacji 0*/

*#[1] Imputacja;
proc mi data=outliers_out_final out=b02 nimpute=0;
	var Rooms Floors Bathrooms Living_space;
run;


*#[2] Estymacja;
proc mixed data=outliers_out_final;
 class Condition;
 model Price = rooms Living_space Floors  Bathrooms / solution covb;
   ods output  SOLUTIONF=parms01 CovB=mixcovb LSMEANS=lsm01 DIFFS=lsmdiffs01;
run;

*#[2.1] Sprawdzanie statystyk;
proc means data=outliers_out_final n mean stderr lclm uclm alpha=0.05;
 var Price Rooms Floors Bathrooms Living_space;
run;



/* Liczba imputacji 30 */
*#[1] Imputacja;
proc mi data=outliers_out_final out=b02 nimpute=30;
	var Rooms Floors Bathrooms Living_space;
run;

*#[2] Estymacja;
proc mixed data=b02;
 class Condition;
 model Price = rooms Living_space Floors  Bathrooms / solution covb;
 by _Imputacja_;
   ods output  SOLUTIONF=parm_01 CovB=mixcovb LSMEANS=lsm01 DIFFS=lsmdiffs01;
run;

*#[2.1] Sprawdzanie statystyk;
proc means data=b02 n mean stderr lclm uclm alpha=0.05;
	var Price Rooms Floors Bathrooms Living_space;
run;

*#[3] Podsumowanie;
*# Summarizing model estimates;
data parm_02;
	set parm_01;
	rename _Imputacja_=_imputation_;
run;

proc mianalyze parms=parm_02 ;
   class Condition;
   modeleffects Intercept rooms Living_space Floors  Bathrooms;
   STDERR ;

run;

