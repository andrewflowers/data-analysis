  /***************************************************************************/
  /* PROGRAM NAME:  CEX INTEGRATED SURVEYS SAMPLE PROGRAM (SAS)              */
  /* FUNCTION: CREATE AN INTEGRATED SURVEY EXPENDITURE TABLE BY INCOME CLASS */
  /*           USING MICRODATA FROM THE BUREAU OF LABOR STATISTICS" CONSUMER */
  /*           EXPENDITURE SURVEY.                                           */
  /*                                                                         */
  /* WRITTEN BY: BUREAU OF LABOR STATISTICS         APRIL 7 2003             */
  /*             CONSUMER EXPENDITURE SURVEY                                 */
  /* MODIFICATIONS:                                                          */
  /* DATE-      MODIFIED BY-        REASON-                                  */
  /* -----      ------------        -------                                  */
  /*                                                                         */
  /*                                                                         */
  /*                                                                         */
  /*  NOTE:  FOR SAS VERSION 8 OR HIGHER                                     */
  /*                                                                         */
  /*  DATA AND INPUT FILES USED IN THIS SAMPLE PROGRAM WERE UNZIPPED         */
  /*  OR COPIED TO THE LOCATIONS BELOW:                                      */
  /*                                                                         */
  /*  INTERVIEW DATA -- C:\2009_CEX\INTRVW09                                 */
  /*  DIARY DATA -- C:\2009_CEX\DIARY09                                      */
  /*  INTSTUB2009.TXT -- C:\2009_CEX\Programs                                */
  /*                                                                         */
  /***************************************************************************/


  /*Enter Data Year*/
  %LET YEAR = 2009;


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
  INFILE "C:\&YEAR._CEX\Programs\INTSTUB&YEAR..TXT"
  PAD MISSOVER;
  INPUT @1 TYPE $1. @ 4 LEVEL $1. @7 TITLE $60. @70 UCC $6.
        @80 SURVEY $1. @86 GROUP $7.;
  IF (TYPE = "1");
  IF GROUP IN ("CUCHARS" "FOOD" "EXPEND" "INCOME");
  IF SURVEY = "T" THEN DELETE;

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
      IF (UCC > "A") THEN
        LINES(SUBSTR(LINE,6,1)) = LINE;
	  RETAIN LINE1-LINE9;
      IF (UCC < "A")  THEN 
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
  RETAIN FMTNAME "LBLFMT" TYPE "C";
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


PROC FORMAT LIBRARY=WORK  CNTLIN=LBLFMT;
RUN;


  /***************************************************************************/
  /* STEP2: READ IN ALL NEEDED DATA FROM THE CD-ROM                          */
  /* ----------------------------------------------------------------------- */
  /* 1 READ IN THE INTERVIEW AND DIARY FMLY FILES & CREATE MO_SCOPE VARIABLE */
  /* 2 READ IN THE INTERVIEW MTAB/ITAB AND DIARY EXPN/DTAB FILES             */
  /* 3 MERGE FMLY AND EXPENDITURE FILES TO DERIVE WEIGHTED EXPENDITURES      */
  /***************************************************************************/


  /*** IMPORT INTRVW FMLY  **************************/

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\FMLI&YR1.1x.CSV"
            OUT=FMLYI&YR1.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\FMLI&YR1.2.CSV"
            OUT=FMLYI&YR1.2
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\FMLI&YR1.3.CSV"
            OUT=FMLYI&YR1.3
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\FMLI&YR1.4.CSV"
            OUT=FMLYI&YR1.4
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\FMLI&YR2.1.CSV"
            OUT=FMLYI&YR2.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;



  /*** IMPORT DIARY FMLY   **************************/

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\FMLD&YR1.1.CSV"
            OUT=FMLYD&YR1.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\FMLD&YR1.2.CSV"
            OUT=FMLYD&YR1.2
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\FMLD&YR1.3.CSV"
            OUT=FMLYD&YR1.3
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\FMLD&YR1.4.CSV"
            OUT=FMLYD&YR1.4
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;


  /*** COMBINE INTRVW AND DIARY FMLY AND ADJUST WEIGHTS  ***/

DATA FMLY;
  SET 
      FMLYI&YR1.1 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 QINTRVYR QINTRVMO IN=INTRVW) 
      FMLYI&YR1.2 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 QINTRVYR QINTRVMO IN=INTRVW) 
      FMLYI&YR1.3 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 QINTRVYR QINTRVMO IN=INTRVW) 
      FMLYI&YR1.4 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 QINTRVYR QINTRVMO IN=INTRVW)
      FMLYI&YR2.1 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 QINTRVYR QINTRVMO IN=INTRVW)

      FMLYD&YR1.1 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 IN=DIARY) 
      FMLYD&YR1.2 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 IN=DIARY) 
      FMLYD&YR1.3 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 IN=DIARY) 
      FMLYD&YR1.4 (KEEP = NEWID INCLASS WTREP01-WTREP44 FINLWT21 IN=DIARY);

    ARRAY REPS_A(45) WTREP01-WTREP44 FINLWT21;
    ARRAY REPS_B(45) REPWT1-REPWT45;

	IF INTRVW THEN DO;

      IF (QINTRVYR="&YEAR") AND (QINTRVMO <= "03") THEN MO_SCOPE = (QINTRVMO - 1);
      ELSE IF (QINTRVYR = %EVAL(&YEAR+1)) THEN MO_SCOPE = (4 - QINTRVMO);
      ELSE MO_SCOPE = 3;
	  /* CREATE MONTH IN SCOPE VARIABLE (MO_SCOPE) */

      DO i = 1 TO 45;
	  IF REPS_A(i) > 0 THEN
         REPS_B(i) = (REPS_A(i) * MO_SCOPE / 12); 
		 ELSE REPS_B(i) = 0;	
	  END;
      /* ADJUST WEIGHTS BY MO_SCOPE TO ACCOUNT FOR SAMPLE ROTATION */
	  SOURCE = "I";
	END;

    IF DIARY THEN DO;

      DO i = 1 TO 45;
	  IF REPS_A(i) > 0 THEN
         REPS_B(i) = (REPS_A(i) / 4); 
		 ELSE REPS_B(i) = 0;	
	  END;
	SOURCE = "D";
	END;	
RUN;

PROC SORT DATA=FMLY;
  BY NEWID;
RUN;



  /*** IMPORT INTRVW MTAB  **************************/

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\MTBI&YR1.1x.CSV"
            OUT=MTABI&YR1.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\MTBI&YR1.2.CSV"
            OUT=MTABI&YR1.2
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\MTBI&YR1.3.CSV"
            OUT=MTABI&YR1.3
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\MTBI&YR1.4.CSV"
            OUT=MTABI&YR1.4
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\MTBI&YR2.1.CSV"
            OUT=MTABI&YR2.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;


  /*** IMPORT INTRVW ITAB  **************************/

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\ITBI&YR1.1x.CSV"
            OUT=ITABI&YR1.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\ITBI&YR1.2.CSV"
            OUT=ITABI&YR1.2
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\ITBI&YR1.3.CSV"
            OUT=ITABI&YR1.3
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\ITBI&YR1.4.CSV"
            OUT=ITABI&YR1.4
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Intrvw&YR1\ITBI&YR2.1.CSV"
            OUT=ITABI&YR2.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;


  /*** IMPORT DIARY EXPN   **************************/

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\EXPD&YR1.1.CSV"
            OUT=EXPND&YR1.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\EXPD&YR1.2.CSV"
            OUT=EXPND&YR1.2
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\EXPD&YR1.3.CSV"
            OUT=EXPND&YR1.3
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\EXPD&YR1.4.CSV"
            OUT=EXPND&YR1.4
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;


  /*** IMPORT DIARY DTAB   **************************/

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\DTBD&YR1.1.CSV"
            OUT=DTABD&YR1.1
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\DTBD&YR1.2.CSV"
            OUT=DTABD&YR1.2
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\DTBD&YR1.3.CSV"
            OUT=DTABD&YR1.3
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;

PROC IMPORT FILE="C:\&YEAR._CEX\Diary&YR1\DTBD&YR1.4.CSV"
            OUT=DTABD&YR1.4
  DBMS = CSV
  REPLACE;
  GETNAMES=YES;
RUN;


  /*** COMBINE INTRVW AND DIARY INCOME AND EXPENDITURE FILES  *******/

DATA EXPEND;
  SET 
      MTABI&YR1.1 (KEEP = NEWID UCC COST REF_YR PUBFLAG)
      MTABI&YR1.2 (KEEP = NEWID UCC COST REF_YR PUBFLAG)
	  MTABI&YR1.3 (KEEP = NEWID UCC COST REF_YR PUBFLAG)
	  MTABI&YR1.4 (KEEP = NEWID UCC COST REF_YR PUBFLAG)
      MTABI&YR2.1 (KEEP = NEWID UCC COST REF_YR PUBFLAG)

      ITABI&YR1.1 (KEEP = NEWID UCC VALUE REFYR PUBFLAG RENAME=(VALUE=COST))
      ITABI&YR1.2 (KEEP = NEWID UCC VALUE REFYR PUBFLAG RENAME=(VALUE=COST))
	  ITABI&YR1.3 (KEEP = NEWID UCC VALUE REFYR PUBFLAG RENAME=(VALUE=COST))
	  ITABI&YR1.4 (KEEP = NEWID UCC VALUE REFYR PUBFLAG RENAME=(VALUE=COST))
      ITABI&YR2.1 (KEEP = NEWID UCC VALUE REFYR PUBFLAG RENAME=(VALUE=COST))

      EXPND&YR1.1 (KEEP = NEWID UCC COST PUB_FLAG)
      EXPND&YR1.2 (KEEP = NEWID UCC COST PUB_FLAG)
	  EXPND&YR1.3 (KEEP = NEWID UCC COST PUB_FLAG)
	  EXPND&YR1.4 (KEEP = NEWID UCC COST PUB_FLAG)

      DTABD&YR1.1 (KEEP = NEWID UCC AMOUNT PUB_FLAG RENAME=(AMOUNT=COST))
      DTABD&YR1.2 (KEEP = NEWID UCC AMOUNT PUB_FLAG RENAME=(AMOUNT=COST))
	  DTABD&YR1.3 (KEEP = NEWID UCC AMOUNT PUB_FLAG RENAME=(AMOUNT=COST))
	  DTABD&YR1.4 (KEEP = NEWID UCC AMOUNT PUB_FLAG RENAME=(AMOUNT=COST));

        IF (PUBFLAG = '2') THEN DO;
		  SOURCE = 'I';
		  IF UCC = '710110'  THEN COST = (COST * 4);
		  IF ((REF_YR = &YEAR) OR (REFYR = &YEAR));  
          OUTPUT;
		END;

		ELSE IF (PUB_FLAG = '2') THEN DO;
          SOURCE = 'D';
		  COST = (COST * 13);          
		  OUTPUT;
		END;

		ELSE DELETE;
RUN;

PROC SORT DATA=EXPEND;
  BY NEWID;
RUN;



  /*** COMBINE ALL DATA AND WEIGHT EXPNDITURES AND INCOMES ************/

DATA PUBFILE (KEEP= NEWID SOURCE INCLASS UCC RCOST1-RCOST45);
  MERGE FMLY   (IN= INFAM)
        EXPEND (IN= INEXP);
  BY NEWID;
  IF (INEXP AND INFAM);

  IF (COST = .) THEN COST = 0;
	 
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
