/***                             INTERVIEW                                      ***/
/***   THIS PROGRAM WILL GENERATE THE MEAN, VARIANCE, STANDARD ERROR OF MEAN,   ***/
/***   AND COEFFICIENT OF VARIATION FOR INTERVIEW DATA FOR A SPECIFIC UCC.      ***/
/***   ANOTHER PROGRAM ON THE CD-ROM  WILL GENERATE DATA FOR ALL UCC's.         ***/
/***   THE REQUESTED UCC MUST BE ADDED IN THE SECOND LINE BELOW.                ***/


%let y =96;
%let ucc=260212;



%let y2=%eval(&y+1);

libname a "d:\intrvw&y";

options linesize=153 pagesize=55 missing='' ;


data fmlyall(keep=newid finlwt21 wtrep1-wtrep44 uspop uspop1-uspop44);
  set a.fmli&y.1x (keep=newid qintrvmo finlwt21 wtrep01-wtrep44 in=in1)
      a.fmli&y.2 (keep=newid qintrvmo finlwt21 wtrep01-wtrep44)
      a.fmli&y.3 (keep=newid qintrvmo finlwt21 wtrep01-wtrep44)
      a.fmli&y.4 (keep=newid qintrvmo finlwt21 wtrep01-wtrep44)
      a.fmli&y2.1(keep=newid qintrvmo finlwt21 wtrep01-wtrep44 in=in5);

   if in1 then mo_scope=qintrvmo-1;
   else if in5 then mo_scope=4-qintrvmo;
   else mo_scope=3;

   uspop  = finlwt21* mo_scope/12;
   wtrep1 = wtrep01 ;
   wtrep2 = wtrep02 ;
   wtrep3 = wtrep03 ;
   wtrep4 = wtrep04 ;
   wtrep5 = wtrep05 ;
   wtrep6 = wtrep06 ;
   wtrep7 = wtrep07 ;
   wtrep8 = wtrep08 ;
   wtrep9 = wtrep09 ;
   %macro halfpops;
     %do i=1 %to 44;
       uspop&i = wtrep&i * mo_scope/12;
     %end;
   %mend halfpops;
   %halfpops;
   proc sort; by newid;

   proc summary nway data = fmlyall (drop = finlwt21 wtrep1-wtrep44);
      var uspop uspop1-uspop44;
      output out = totpop sum = ;


data mtab1 (drop = ref_yr);
  set a.mtbi&y.1x (keep = newid ref_yr ucc cost )
      a.mtbi&y.2 (keep = newid ref_yr ucc cost )
      a.mtbi&y.3 (keep = newid ref_yr ucc cost )
      a.mtbi&y.4 (keep = newid ref_yr ucc cost )
      a.mtbi&y2.1(keep = newid ref_yr ucc cost ) ;

        if ucc = &ucc;

        if ref_yr="19&y";
        proc sort; by newid;
data itab1 (drop = refyr rename=(value=cost));
  set a.itbi&y.1x (keep = newid refyr ucc value )
      a.itbi&y.2 (keep = newid refyr ucc value )
      a.itbi&y.3 (keep = newid refyr ucc value )
      a.itbi&y.4 (keep = newid refyr ucc value )
      a.itbi&y2.1(keep = newid refyr ucc value );
        if refyr="19&y";

        if ucc = &ucc;

        proc sort; by newid;

data expend ;
  set mtab1 itab1;
   by newid;
   if ucc='710110' then cost=cost*4;
   proc sort; by newid;

   proc datasets; delete mtab1 itab1;

data pubfile (drop= finlwt21 wtrep1-wtrep44 cost) ;
    merge fmlyall (in = infam drop=uspop uspop1-uspop44)
          expend  (in = inexp)
          ;
    by newid ;
    if not inexp then delete;
    if cost='.' then cost=0;

    wtcost  = finlwt21* cost;
    %macro halfcost;
     %do i=1 %to 44;
      wtcost&i = wtrep&i * cost;
     %end;
    %mend halfcost;
    %halfcost;

    proc summary nway data = pubfile (drop=newid);
     class ucc ;
     var wtcost wtcost1-wtcost44;
     output out = aggcst sum = ;

    proc datasets;
      delete expend pubfile;


data cstpop ;
    if _n_ = 1 then set totpop;
     set aggcst;

       array ex wtcost wtcost1-wtcost44;
       array wt uspop uspop1-uspop44;

       do over ex;
          ex = ex/wt;
       end;

data stats (drop = wtcost1-wtcost44 sse1-sse44) ;
  set cstpop (drop=uspop uspop1-uspop44);

  %macro sse;
    %do i = 1 %to 44;
      sse&i  = (wtcost&i-wtcost)**2;
    %end;
  %mend sse;
  %sse;

  sse = sum (of sse1-sse44);
  var = sse / 44;
  sem = var**.5;
  cv  = 100 * sem / wtcost;

data mean;
  set stats (drop = var sem cv);
    stat = 'mean($)';
    n = _n_;

data var  (rename=(var=wtcost));
  set stats (drop = wtcost sem cv);
    stat = 'var';
    n = _n_;

data sem  (rename=(sem=wtcost));
  set stats (drop = wtcost var cv);
    stat = 'se';
    n = _n_;

data cv  (rename=(cv=wtcost));
  set stats (drop = wtcost sem var);
    stat = 'cv(%)';
    n = _n_;

data complete;
  length stat $7. ;
  set mean var sem cv ;
    by n;

proc print data=complete noobs;
  var stat wtcost;
  title "CE Interview Survey Microdata, Average Annual Expenditure UCC=&ucc";



 run;
