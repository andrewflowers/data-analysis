  /***************************************************************************/
  /* PROGRAM NAME:  CEX INTEGRATED SURVEYS SAMPLE PROGRAM (SAS)              */
  /* FUNCTION: CREATE AN INTEGRATED SURVEY EXPENDITURE TABLE BY INCOME CLASS */
  /*           USING MICRODATA FROM THE BUREAU OF LABOR STATISTICS' CONSUMER */
  /*           EXPENDITURE SURVEY.                                           */
  /*                                                                         */
  /* WRITTEN B&DRIVE. BUREAU OF LABOR STATISTICS         APRIL 7 2003        */
  /*             CONSUMER EXPENDITURE SURVEY                                 */
  /* MODIFICATIONS:                                                          */
  /* DATE-      MODIFIED BY-        REASON-                                  */
  /* -----      ------------        -------                                  */
  /* 02/14/08   LAURA PASZKIEWICZ   UPDATED TO ACCOUNT FOR ZIPPED FILES ON CD*/
  /*                                                                         */
  /*                                                                         */
  /*  NOTE:  FOR SAS VERSION 8 OR HIGHER                                     */
  /*                                                                         */
  /***************************************************************************/

  
/*Enter Data Year*/
%LET YEAR = 2006;
/*Enter location of the unzipped microdata file*/
/*Be sure to keep the same file structure as on the CD*/
%LET DRIVE = C:\2006_CEX;



  /***************************************************************************/
  /* STEP1: READ IN THE STUB PARAMETER FILE AND CREATE FORMATS               */
  /* ----------------------------------------------------------------------- */
  /* 1 CONVERTS THE STUB PARAMETER FILE INTO A LABEL FILE FOR OUTPUT         */
  /* 2 CONVERTS THE STUB PARAMETER FILE INTO AN EXPENDITURE AGGREGATION FILE */
  /* 3 CREATES FORMATS FOR USE IN OTHER PROCEDURES                           */
  /***************************************************************************/


%LET YR1 = %SUBSTR(&YEAR, 3, 2);
%LET YR2 = %SUBSTR(%EVAL(&YEAR + 1), 3, 2);



DATA STUBFILE (KEEP= COUNT TYPE LEVEL TITLE UCC SURVEY GROUP LINE);
  INFILE "&DRIVE.\PROGRAMS\INTSTUB&YEAR..TXT"
  PAD MISSOVER;
  INPUT @1 TYPE $1. @ 4 LEVEL $1. @7 TITLE $60. @70 UCC $6.
        @80 SURVEY $1. @86 GROUP $7.;
  IF (TYPE = '1');
  IF GROUP IN ('CUCHARS' 'FOOD' 'EXPEND' 'INCOME');
  IF SURVEY = 'T' THEN DELETE;

    RETAIN COUNT 9999;
    COUNT + 1;
    LINE = PUT(COUNT, $5.)||LEVEL ;
	/* READS IN THE STUB PARAMETER FILE AND CREATES LINE NUMBERS FOR UCCS */
	/* A UNIQUE LINE NUMBER IS ASSIGNED TO EACH EXPENDITURE LINE ITEM     */
RUN;


DATA AGGFMT1 (KEEP= UCC LINE LINE1-LINE10);
  SET STUBFILE;
  LENGTH LINE1-LINE10 $6.;
    ARRAY LINES(9) LINE1-LINE9;
      IF (UCC > 'A') THEN
        LINES(SUBSTR(LINE,6,1)) = LINE;
	  RETAIN LINE1-LINE9;
      IF (UCC < 'A')  THEN 
        LINE10 = LINE;
  IF (LINE10);
  /* MAPS LINE NUMBERS TO UCCS */
RUN;


PROC SORT DATA= AGGFMT1 (RENAME=(LINE= COMPARE));
  BY UCC;
RUN;


PROC TRANSPOSE DATA= AGGFMT1 OUT= AGGFMT2 (RENAME=(COL1= LINE));
  BY UCC COMPARE;
  VAR LINE1-LINE10;
RUN;


DATA AGGFMT (KEEP= UCC LINE);
  SET AGGFMT2;
    IF LINE;
    IF SUBSTR(COMPARE,6,1) > SUBSTR(LINE,6,1) OR COMPARE=LINE;
	/* AGGREGATION FILE. EXTRANEOUS MAPPINGS ARE DELETED */
	/* PROC SQL WILL AGGANGE LINE#/UCC PAIRS FOR USE IN PROC FORMAT */
RUN;


PROC SQL NOPRINT;
  SELECT UCC, LINE, COUNT(*)
  INTO  :UCCS SEPARATED BY " ",
        :LINES SEPARATED BY " ",          
        :CNT
  FROM AGGFMT;
  QUIT;
RUN;


%MACRO MAPPING;
  %DO  i = 1  %TO  &CNT;
    "%SCAN(&UCCS,&i,%STR( ))" = "%SCAN(&LINES,&i,%STR( ))"
  %END;
%MEND MAPPING;


DATA LBLFMT (RENAME=(LINE= START TITLE= LABEL));
  SET STUBFILE (KEEP= LINE TITLE);
  RETAIN FMTNAME 'LBLFMT' TYPE 'C';
  /* LABEL FILE. LINE NUMBERS ARE ASSIGNED A TEXT LABEL */
  /* DATASET CONSTRUCTED TO BE READ INTO A PROC FORMAT  */
RUN;


PROC FORMAT;

  VALUE $AGGFMT (MULTILABEL)
    %MAPPING
    OTHER= 'OTHER'
    ;

  VALUE $INC (MULTILABEL)
    '01' = '01'
    '01' = '10'
    '02' = '02'
    '02' = '10'
    '03' = '03'
    '03' = '10'
    '04' = '04'
    '04' = '10'
    '05' = '05'
    '05' = '10'
    '06' = '06'
    '06' = '10'
    '07' = '07'
    '07' = '10'
    '08' = '08'
    '08' = '10'
    '09' = '09'
    '09' = '10';
	/* CREATE INCOME CLASS FORMAT */
RUN;


PROC FORMAT LIBRARY= WORK  CNTLIN= LBLFMT;
RUN;


  /***************************************************************************/
  /* STEP2: READ IN ALL NEEDED DATA FROM THE CD-ROM                          */
  /* ----------------------------------------------------------------------- */
  /* 1 READ IN THE INTERVIEW AND DIARY FMLY FILES & CREATE MO_SCOPE VARIABLE */
  /* 2 READ IN THE INTERVIEW MTAB/ITAB AND DIARY EXPN/DTAB FILES             */
  /* 3 MERGE FMLY AND EXPENDITURE FILES TO DERIVE WEIGHTED EXPENDITURES      */
  /***************************************************************************/




%MACRO READFMLI (FILE=, QTR=);

DATA FMLYI&QTR. (KEEP = NEWID SOURCE INCLASS WTREP01-WTREP44 FINLWT21 REPWT1-REPWT45);
	INFILE &FILE END=LAST LS=3326; 
    DO WHILE (LAST = 0);	 
	  INPUT @1 NEWID 8.          @663 QINTRVMO $2.    @331 FINLWT21 11.3     
            @841  WTREP01  11.3  @852  WTREP02  11.3  @863  WTREP03  11.3
            @874  WTREP04  11.3  @885  WTREP05  11.3  @896  WTREP06  11.3
            @907  WTREP07  11.3  @918  WTREP08  11.3  @929  WTREP09  11.3
            @940  WTREP10  11.3  @951  WTREP11  11.3  @962  WTREP12  11.3
            @973  WTREP13  11.3  @984  WTREP14  11.3  @995  WTREP15  11.3
            @1006 WTREP16  11.3  @1017 WTREP17  11.3  @1028 WTREP18  11.3
            @1039 WTREP19  11.3  @1050 WTREP20  11.3  @1061 WTREP21  11.3
            @1072 WTREP22  11.3  @1083 WTREP23  11.3  @1094 WTREP24  11.3
            @1105 WTREP25  11.3  @1116 WTREP26  11.3  @1127 WTREP27  11.3
            @1138 WTREP28  11.3  @1149 WTREP29  11.3  @1160 WTREP30  11.3
            @1171 WTREP31  11.3  @1182 WTREP32  11.3  @1193 WTREP33  11.3
            @1204 WTREP34  11.3  @1215 WTREP35  11.3  @1226 WTREP36  11.3
            @1237 WTREP37  11.3  @1248 WTREP38  11.3  @1259 WTREP39  11.3
            @1270 WTREP40  11.3  @1281 WTREP41  11.3  @1292 WTREP42  11.3
            @1303 WTREP43  11.3  @1314 WTREP44  11.3  @3324 INCLASS $2.;
	QUARTER = &QTR;
    IF QUARTER = 1 THEN 
      MO_SCOPE = (QINTRVMO - 1);
    ELSE IF QUARTER = 5 THEN 
      MO_SCOPE = (4 - QINTRVMO);
    ELSE 
      MO_SCOPE = 3;
	/* CREATE MONTH IN SCOPE VARIABLE (MO_SCOPE) */

    ARRAY REPS_A(45) WTREP01-WTREP44 FINLWT21;
    ARRAY REPS_B(45) REPWT1-REPWT45;

      DO i = 1 TO 45;
	  IF REPS_A(i) > 0 THEN
         REPS_B(i) = (REPS_A(i) * MO_SCOPE / 12); 
		 ELSE REPS_B(i) = 0;	
	  END;
	  /* ADJUST WEIGHTS BY MO_SCOPE TO ACCOUNT FOR SAMPLE ROTATION */
      SOURCE = 'I';
	  OUTPUT;
	END;
RUN;

%IF &QTR = 1 %THEN %DO;
	DATA FMLYI;
		SET FMLYI&QTR;
	RUN;
%END;
%ELSE %DO;
PROC APPEND BASE = FMLYI DATA=FMLYI&QTR.; RUN;
%END;
%MEND;

%READFMLI (FILE="&DRIVE.\intrvw&YR1.\fmlyi&YR1.1x.txt",QTR=1);
%READFMLI (FILE="&DRIVE.\intrvw&YR1.\fmlyi&YR1.2.txt" ,QTR=2);
%READFMLI (FILE="&DRIVE.\intrvw&YR1.\fmlyi&YR1.3.txt",QTR=3);
%READFMLI (FILE="&DRIVE.\intrvw&YR1.\fmlyi&YR1.4.txt" ,QTR=4);
%READFMLI (FILE="&DRIVE.\intrvw&YR1.\fmlyi&YR2.1.txt",QTR=5);
RUN;

PROC SORT DATA=FMLYI;
  BY NEWID;
RUN;

%MACRO READFMLD (FILE=, QTR=);

DATA FMLYD&QTR (KEEP = NEWID SOURCE INCLASS WTREP01-WTREP44 FINLWT21 REPWT1-REPWT45);
  INFILE &FILE END=LAST LS=1518;
    DO WHILE (LAST = 0);	 
	  INPUT @1 NEWID 8.         @1516 INCLASS $2.   @148  FINLWT21 11.3
            @687  WTREP01 11.3  @698  WTREP02 11.3  @709  WTREP03 11.3
            @720  WTREP04 11.3  @731  WTREP05 11.3  @742  WTREP06 11.3
            @753  WTREP07 11.3  @764  WTREP08 11.3  @775  WTREP09 11.3
            @786  WTREP10 11.3  @797  WTREP11 11.3  @808  WTREP12 11.3
            @819  WTREP13 11.3  @830  WTREP14 11.3  @841  WTREP15 11.3
            @852  WTREP16 11.3  @863  WTREP17 11.3  @874  WTREP18 11.3
            @885  WTREP19 11.3  @896  WTREP20 11.3  @907  WTREP21 11.3
            @918  WTREP22 11.3  @929  WTREP23 11.3  @940  WTREP24 11.3
            @951  WTREP25 11.3  @962  WTREP26 11.3  @973  WTREP27 11.3
            @984  WTREP28 11.3  @995  WTREP29 11.3  @1006 WTREP30 11.3
            @1017 WTREP31 11.3  @1028 WTREP32 11.3  @1039 WTREP33 11.3
            @1050 WTREP34 11.3  @1061 WTREP35 11.3  @1072 WTREP36 11.3
            @1083 WTREP37 11.3  @1094 WTREP38 11.3  @1105 WTREP39 11.3
            @1116 WTREP40 11.3  @1127 WTREP41 11.3  @1138 WTREP42 11.3
            @1149 WTREP43 11.3  @1160 WTREP44 11.3;

	/* READ IN FMLY FILE DATA */
    ARRAY REPS_A(45) WTREP01-WTREP44 FINLWT21;
    ARRAY REPS_B(45) REPWT1-REPWT45;

      DO i = 1 TO 45;
	  IF REPS_A(i) > 0 THEN
         REPS_B(i) = (REPS_A(i) / 4); 
		 ELSE REPS_B(i) = 0;	
	  END;
      /* ADJUST WEIGHTS TO COMPENSATE FOR HAVING FOUR QUARTERS OF DATA */
      SOURCE = 'D';
      OUTPUT;
	END;
RUN;

%IF &QTR = 1 %THEN %DO;
	DATA FMLYD;
		SET FMLYD&QTR;
	RUN;
%END;
%ELSE %DO;
PROC APPEND BASE = FMLYD DATA=FMLYD&QTR.; RUN;
%END;

%MEND;

%READFMLD (FILE="&DRIVE.\Diary&YR1.\fmlyd&YR1.1.txt", QTR=1);
%READFMLD (FILE="&DRIVE.\Diary&YR1.\fmlyd&YR1.2.txt" , QTR=2);
%READFMLD (FILE="&DRIVE.\Diary&YR1.\fmlyd&YR1.3.txt", QTR=3);
%READFMLD (FILE="&DRIVE.\Diary&YR1.\fmlyd&YR1.4.txt", QTR=4);

PROC SORT DATA=FMLYD;
  BY NEWID;
RUN;


DATA FMLY;
  SET FMLYI FMLYD;
  BY NEWID;
RUN;

%MACRO READEXP (FILE=, QTR=, TYPE=);
DATA IEXPEND&QTR.&TYPE. (KEEP=NEWID UCC REFYR COST SOURCE);
  INFILE &FILE  END=LAST LRECL=35;

  %IF &TYPE = M  %THEN %DO;
    DO WHILE (LAST = 0);	 
	  INPUT @1 NEWID 8.  @9 UCC $6.  @15 COST 12.4  @32 REFYR $4.  @29 PUBFLAG $1.;
        IF UCC = '710110' THEN COST = (COST * 4); 
        SOURCE = 'I';  
      IF REFYR = &YEAR AND PUBFLAG = '2'  THEN OUTPUT;
	END;
  %END;
  %ELSE %IF &TYPE = I  %THEN %DO;    
    DO WHILE (LAST = 0);	 
	  INPUT @1 NEWID 8.  @15 UCC $6.  @22 COST 12.4  @11 REFYR $4.  @21 PUBFLAG $1.;
	    SOURCE = 'I';
      IF REFYR = &YEAR AND PUBFLAG = '2'  THEN OUTPUT;	  
	END;
  %END;
  /* READ IN MTAB AND ITAB EXPENDITURE AND INCOME DATA */
  /* ADJUST UCC 710110 TO ANNUALIZE     */
%IF &QTR = 1 AND &TYPE = M %THEN %DO;
	DATA IEXPEND;
		SET IEXPEND&QTR.&TYPE.;
	RUN;
%END;
%ELSE %DO;
PROC APPEND BASE = IEXPEND DATA=IEXPEND&QTR.&TYPE.; RUN;
%END;
%MEND;
%READEXP (FILE="&DRIVE.\intrvw&YR1.\mtabi&YR1.1x.txt", QTR=1, TYPE = M);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\mtabi&YR1.2.txt" , QTR=2, TYPE = M);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\mtabi&YR1.3.txt", QTR=3, TYPE = M);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\mtabi&YR1.4.txt", QTR=4, TYPE = M);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\mtabi&YR2.1.txt", QTR=5, TYPE = M);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\itabi&YR1.1x.txt", QTR=1, TYPE = I);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\itabi&YR1.2.txt" , QTR=2, TYPE = I);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\itabi&YR1.3.txt", QTR=3, TYPE = I);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\itabi&YR1.4.txt", QTR=4, TYPE = I);
%READEXP (FILE="&DRIVE.\intrvw&YR1.\itabi&YR2.1.txt", QTR=5, TYPE = I);


PROC SORT DATA=IEXPEND;
  BY NEWID;
RUN;


%MACRO READEXPD (FILE=, QTR=, TYPE=);
DATA DEXPEND&QTR.&TYPE. (KEEP = NEWID UCC COST SOURCE);

  INFILE &FILE  END=LAST  MISSOVER;

  %IF &TYPE = M  %THEN %DO;
    DO WHILE (LAST = 0);	 
	  INPUT @1 NEWID 8.  @35 UCC $6.  @10 COST 12.  @23 PUB_FLAG $1.;
        SOURCE = 'D';
        COST = (COST * 13);
	  IF PUB_FLAG = '2' THEN OUTPUT;
	END;
  %END;
  %ELSE %DO;
    DO WHILE (LAST = 0);	 
	  INPUT @1 NEWID 8.  @9 UCC $6.  @15 COST 12.  @28 PUB_FLAG $1.;
        SOURCE = 'D';
        COST = (COST * 13);
	  IF PUB_FLAG = '2' THEN OUTPUT;
	END;
  %END;
RUN;
%IF &QTR = 1 AND &TYPE = M %THEN %DO;
	DATA DEXPEND;
		SET DEXPEND&QTR.&TYPE.;
	RUN;
%END;
%ELSE %DO;
PROC APPEND BASE = DEXPEND DATA=DEXPEND&QTR.&TYPE.; RUN;
%END;
%MEND;
%READEXPD (FILE="&DRIVE.\Diary&YR1.\expnd&YR1.1.txt", QTR=1, TYPE = M);
%READEXPD (FILE="&DRIVE.\Diary&YR1.\expnd&YR1.2.txt", QTR=2, TYPE = M);
%READEXPD (FILE="&DRIVE.\Diary&YR1.\expnd&YR1.3.txt", QTR=3, TYPE = M);
%READEXPD (FILE="&DRIVE.\Diary&YR1.\expnd&YR1.4.txt", QTR=4, TYPE = M);
%READEXPD (FILE="&DRIVE.\Diary&YR1.\dtabd&YR1.1.txt" , QTR=1, TYPE = I);
%READEXPD (FILE="&DRIVE.\Diary&YR1.\dtabd&YR1.2.txt", QTR=2, TYPE = I);
%READEXPD (FILE="&DRIVE.\Diary&YR1.\dtabd&YR1.3.txt", QTR=3, TYPE = I);
%READEXPD (FILE="&DRIVE.\Diary&YR1.\dtabd&YR1.4.txt", QTR=4, TYPE = I);


PROC SORT DATA=DEXPEND;
  BY NEWID;
RUN;


DATA EXPEND (KEEP= NEWID SOURCE UCC COST);
  SET IEXPEND DEXPEND;
  BY NEWID;
RUN;



DATA PUBFILE (KEEP= NEWID SOURCE INCLASS UCC RCOST1-RCOST45);
  MERGE FMLY   (IN= INFAM)
        EXPEND (IN= INEXP);
  BY NEWID;
  IF (INEXP AND INFAM);

  IF (COST = .)  THEN 
     COST = 0;
	 
     ARRAY REPS_A(45) WTREP01-WTREP44 FINLWT21;
     ARRAY REPS_B(45) RCOST1-RCOST45;

     DO i = 1 TO 45;
	   IF REPS_A(i) > 0
         THEN REPS_B(i) = (REPS_A(i) * COST);
	     ELSE REPS_B(i) = 0; 	
	 END; 
RUN;



  /***************************************************************************/
  /* STEP3: CALCULATE POPULATIONS                                            */
  /* ----------------------------------------------------------------------- */
  /*  SUM ALL 45 WEIGHT VARIABLES TO DERIVE REPLICATE POPULATIONS            */
  /*  FORMATS FOR CORRECT COLUMN CLASSIFICATIONS                             */
  /***************************************************************************/


PROC SUMMARY NWAY DATA=FMLY SUMSIZE=MAX;
  CLASS INCLASS SOURCE / MLF;
  VAR REPWT1-REPWT45;
  FORMAT INCLASS $INC.;
  OUTPUT OUT = POP (DROP = _TYPE_ _FREQ_) SUM = RPOP1-RPOP45;
RUN;

 

  /***************************************************************************/
  /* STEP4: CALCULATE WEIGHTED AGGREGATE EXPENDITURES                        */
  /* ----------------------------------------------------------------------- */
  /*  SUM THE 45 REPLICATE WEIGHTED EXPENDITURES TO DERIVE AGGREGATES/UCC    */
  /*  FORMATS FOR CORRECT COLUMN CLASSIFICATIONS                             */
  /***************************************************************************/


PROC SUMMARY NWAY DATA=PUBFILE SUMSIZE=MAX COMPLETETYPES;
  CLASS SOURCE UCC INCLASS / MLF;
  VAR RCOST1-RCOST45;
  FORMAT INCLASS $INC.;
   OUTPUT OUT= AGG (DROP= _TYPE_ _FREQ_) 
   SUM= RCOST1-RCOST45;
RUN;



  /***************************************************************************/
  /* STEP5: CALCULTATE MEAN EXPENDITURES                                     */
  /* ----------------------------------------------------------------------- */
  /* 1 READ IN POPULATIONS AND LOAD INTO MEMORY USING A 3 DIMENSIONAL ARRAY  */
  /*   POPULATIONS ARE ASSOCIATED BY INCLASS, SOURCE(t), AND REPLICATE(j)    */
  /* 2 READ IN AGGREGATE EXPENDITURES FROM AGG DATASET                       */
  /* 3 CALCULATE MEANS BY DIVIDING AGGREGATES BY CORRECT SOURCE POPULATIONS  */
  /*   EXPENDITURES SOURCED FROM DIARY ARE CALULATED USING DIARY POPULATIONS */
  /*   WHILE INTRVIEW EXPENDITURES USE INTERVIEW POPULATIONS                 */
  /* 4 SUM EXPENDITURE MEANS PER UCC INTO CORRECT LINE ITEM AGGREGATIONS     */
  /***************************************************************************/


DATA AVGS1 (KEEP = SOURCE INCLASS UCC MEAN1-MEAN45);

  /* READS IN POP DATASET. _TEMPORARY_ LOADS POPULATIONS INTO SYSTEM MEMORY  */
  ARRAY POP{01:10,2,45} _TEMPORARY_ ;
  IF _N_ = 1 THEN DO i = 1 TO 20;
    SET POP;
	ARRAY REPS{45} RPOP1--RPOP45;
	IF SOURCE = 'D' THEN t = 1;
	ELSE t = 2;
	  DO j = 1 TO 45;
	    POP{INCLASS,t,j} = REPS{j};
	  END;
	END;

  /* READS IN AGG DATASET AND CALCULATES MEANS BY DIVIDING BY POPULATIONS  */
  SET AGG (KEEP = UCC INCLASS SOURCE RCOST1-RCOST45);
	IF SOURCE = 'D' THEN t = 1;
	ELSE t = 2;
  ARRAY AGGS(45) RCOST1-RCOST45;
  ARRAY AVGS(45) MEAN1-MEAN45;
	DO k = 1 TO 45;
	  IF AGGS(k) = .  THEN AGGS(k) = 0;
	  AVGS(k) = AGGS(k) / POP{INCLASS,t,k};
	END;
RUN;


PROC SUMMARY DATA=AVGS1 NWAY;
  CLASS INCLASS UCC / MLF;
  VAR MEAN1-MEAN45;
  FORMAT UCC $AGGFMT.;
  OUTPUT OUT=AVGS2 (DROP= _TYPE_ _FREQ_  RENAME=(UCC= LINE)) SUM= ;
  /* SUM UCC MEANS TO CREATE AGGREGATION SCHEME */
RUN;


  /***************************************************************************/
  /* STEP6: CALCULTATE STANDARD ERRORS                                       */
  /* ----------------------------------------------------------------------- */
  /*  CALCULATE STANDARD ERRORS USING REPLICATE FORMULA                      */
  /***************************************************************************/


DATA SE (KEEP = INCLASS LINE MEAN SE);
  SET AVGS2;
  ARRAY RMNS(44) MEAN1-MEAN44;
  ARRAY DIFF(44) DIFF1-DIFF44;
    DO i = 1 TO 44;
      DIFF(i) = (RMNS(i) - MEAN45)**2;
    END;
  MEAN = MEAN45;
  SE = SQRT((1/44)*SUM(OF DIFF(*)));
RUN;


  /***************************************************************************/
  /* STEP7: TABULATE EXPENDITURES                                            */
  /* ----------------------------------------------------------------------- */
  /* 1 ARRANGE DATA INTO TABULAR FORM                                        */
  /* 2 SET OUT INTERVIEW POPULATIONS FOR POPULATION LINE ITEM                */
  /* 3 INSERT POPULATION LINE INTO TABLE                                     */
  /* 4 INSERT ZERO EXPENDITURE LINE ITEMS INTO TABLE FOR COMPLETENESS        */
  /***************************************************************************/


PROC SORT DATA=SE;
  BY LINE INCLASS;

PROC TRANSPOSE DATA=SE OUT=TAB1
  NAME = ESTIMATE PREFIX = INCLASS;
  BY LINE;
  VAR MEAN SE;
  /*ARRANGE DATA INTO TABULAR FORM */
RUN;


PROC TRANSPOSE DATA=POP (KEEP = SOURCE RPOP45) OUT=CUS
  NAME = LINE PREFIX = INCLASS;
  VAR RPOP45;
  WHERE SOURCE = 'I';
  /* SET ASIDE POPULATIONS FROM INTERVIEW */
RUN;


DATA TAB2;
  SET CUS TAB1;
  IF LINE = 'RPOP45' THEN DO;
    LINE = '100001';
	ESTIMATE = 'N';
	END;
  /* INSERT POPULATION LINE ITEM INTO TABLE AND ASSIGN LINE NUMBER */
RUN;


DATA TAB;
  MERGE TAB2 STUBFILE;
  BY LINE;
    IF LINE NE '100001' THEN DO;
	  IF SURVEY = 'S' THEN DELETE;
	END;
	ARRAY CNTRL(10) INCLASS1-INCLASS10;
	  DO i = 1 TO 10;
	    IF CNTRL(i) = . THEN CNTRL(i) = 0;
		IF SUM(OF CNTRL(*)) = 0 THEN ESTIMATE = 'MEAN';
	  END;

	IF GROUP IN ('CUCHARS' 'INCOME') THEN DO;
	  IF LAG(LINE) = LINE THEN DELETE;
	END;
  /* MERGE STUBFILE BACK INTO TABLE TO INSERT EXPENDITURE LINES */
  /* THAT HAD ZERO EXPENDITURES FOR THE YEAR                    */
RUN;


PROC TABULATE DATA=TAB;
  CLASS LINE / GROUPINTERNAL ORDER=DATA;
  CLASS ESTIMATE;
  VAR INCLASS1-INCLASS10;
  FORMAT LINE $LBLFMT.;

    TABLE (LINE * ESTIMATE), (INCLASS10 INCLASS1 INCLASS2 INCLASS3 INCLASS4 
                              INCLASS5  INCLASS6 INCLASS7 INCLASS8 INCLASS9) 
    *SUM='' / RTS=25;
    LABEL ESTIMATE=ESTIMATE LINE=LINE
          INCLASS1='LESS THAN $5,000'   INCLASS2='$5,000 TO $9,999' 
          INCLASS3='$10,000 TO $14,999' INCLASS4='$15,000 TO $19,999'
          INCLASS5='$20,000 TO $29,999' INCLASS6='$30,000 TO $39,999'
          INCLASS7='$40,000 TO $49,999' INCLASS8='$50,000 TO $69,999'
          INCLASS9='$70,000 AND OVER'   INCLASS10='ALL CONSUMER UNITS';
	OPTIONS NODATE NOCENTER NONUMBER LS=167 PS=MAX;
	WHERE LINE NE 'OTHER';
    TITLE "INTEGRATED EXPENDITURES FOR &YEAR BY INCOME BEFORE TAXES";
RUN;
