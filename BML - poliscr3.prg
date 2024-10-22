*******************************************************************
*
*  POLISCR3.PRG
*
*
*	10/26/2006:  Steven Bennett
*		Based on PoliScrn.PRG but now includes the "Other Policies" selections
*	04/27/2016:  Steven Bennett
*		- now provides a Individual;Firm choice then SS# cSSN4 or cEIN
*
*******************************************************************
PARAMETERS m.mMode, m.xcLName, m.xcFName, m.xcSSN4, m.xcLicense, m.xcTipe
PRIVATE m.llF8Update, m.lcSetEscape, m.lcOnError, m.lnViewRow, m.lnViewCol, m.llHasNewPolicy, m.lcWindowTitle, 	;
	m.lnOrigRec, m.lnNewRecno, m.lcBatchID, m.llUsedEnd_Indv, m.lcOrderEnd_Indv, m.lnRecnoEnd_Indv, m.lcOnF12, m.lcOnF11,	;
	m.llUsedClaims, m.lcOrderClaims, m.lnRecnoClaims, m.lcValidMsg, m.lcOnF4, m.llTaxUsed, 	;
	m.lnSelect, m.lcTaxOrder, m.lnTax_Rate, m.llUsedPoliLic, m.llCancelNewPolicy, m.llPoliTask,	;
	m.lcOrderPoliTask, m.lnRecnoPoliTask, m.llUsedPoliEmail, m.cbKeyOriginalRec

	
m.llCancelNewPolicy	=.F.	&&	default to FALSE
ON KEY LABEL F8
m.lcOnError	=ON("ERROR")
ON ERROR DO PoliScrnError WITH ERROR(), MESSAGE(), LINENO(), PROGRAM()

IF FILE("C:\BML\STEP.ON")
	SET STEP ON 
ENDIF

m.lcValidMsg	=""	&&	default

* Open WebCSV\WEB as 'extended data'
IF !USED("PoliExt")
	SELECT 0
	USE WebCSV\Web AGAIN ALIAS PoliExt
*	SET ORDER TO CBPOLIIND   && CBPOLIIND
ENDIF

* Establish Multi-Licence data ---------*
m.llUsedPoliLic	=USED("PoliLic")

IF m.llUsedPoliLic
	SELECT PoliLic
	m.lcOrdPoliLic	=ORDER()
	m.lnRecPoliLic	=RECNO()
ELSE
	SELECT 0
	USE PoliLic
ENDIF

*SET ORDER TO POLILIC   && POLINUMB+POLIID
SET ORDER TO IDNUMB   && UPPER(POLIID+POLINUMB)
*---------------------------------------*

* Establish Multi-Email data ---------*
m.llUsedPoliEmail	=USED("PoliEmail")

IF m.llUsedPoliEmail
	SELECT PoliEmail
	m.lcOrdPoliEmail	=ORDER()
	m.lnRecPoliEmail	=RECNO()
ELSE
	SELECT 0
	USE PoliEmail
ENDIF

SET ORDER TO CPOLIID   && CPOLIID
*---------------------------------------*

* Establish Claims data ----------------*
m.llUsedClaims	=USED("Claims")

IF m.llUsedClaims	&& If it was open, save the order and recno.
	SELECT Claims
	m.lcOrderClaims	=ORDER()
	m.lnRecnoClaims	=RECNO()
	USE IN Claims	&& Close is because it's next going to be opened in NOUPDATE mode
ENDIF

SELECT 0
USE Claims	NOUPDATE
*SET ORDER TO POLINUM_ID   	DESCENDING && UPPER(POLINUMB)+POLIID
SET ORDER TO POLIID   && POLIID
*---------------------------------------*

* Establish Back-End data --------------*
m.llUsedEnd_Indv	=USED("End_Indv")

IF m.llUsedEnd_Indv
	SELECT End_Indv
	m.lcOrderEnd_Indv	=ORDER()
	m.lnRecnoEnd_Indv	=RECNO()
ELSE
	SELECT 0
	USE End_Indv
ENDIF

SET ORDER TO ENDORSE   && POLINUMB+POLIID
*---------------------------------------*


* Establish Tasks ----------------------*
m.llPoliTask	=USED("PoliTask")

IF m.llPoliTask
	SELECT PoliTask
	m.lcOrderPoliTask	=ORDER()
	m.lnRecnoPoliTask	=RECNO()
ELSE
	SELECT 0
	USE PoliTask
ENDIF

SET ORDER TO POLIDUE   && CBPOLIINDK+TTOC(TDUE,1)
*---------------------------------------*


SELECT PoliInd

m.lnOrigRec	=RECNO("PoliInd")
m.lnNewRecno=0			&&	default to Zero
m.lcBatchID	=SPACE(6)	&&	default to c(6)
*m.lnViewRow	=IIF(mMode="V", 4, 0)
*m.lnViewCol	=IIF(mMode="V", 1, 0)
m.lnViewRow	=0
m.lnViewCol	=0

            
m.llF8Update 	=.F.	&&	default to FALSE
m.llHasNewPolicy=.F.	&&	default to FALSE
m.lcWindowTitle	=""		&&	default to [blank]

m.lcSetEscape	=SET("Escape")
SET ESCAPE OFF

mamtrcvd	= 0
mtax_pct	= 0
m.prem		= 0
m.taxes		= 0

*-----------------------------------------------*
* Open Chart tables and disable the relations	|
=Chart("OPEN")

SELECT ChartB
SET RELATION TO

SELECT ChartA
SET RELATION TO
*-----------------------------------------------*

SELECT PoliInd

IF mmode="A"
	SCATTER MEMVAR MEMO BLANK
	m.PoliNumb	=mpolicy
*	m.AddTl_Key	=Policy.AddTl_Key
	SELECT ChartA
	SET ORDER TO PoliNumb
	m.AddTl_Key	=IIF(SEEK(m.PoliNumb, "ChartA", "PoliNumb"), ChartA.AddTl_Key, m.AddTl_Key)
	
	SELECT PoliInd
	m.lnPremiumRecno	=0	&&	default to Zero, this mvar is used by BackEndUpdt() and BackEndTaxes(in Policy.prg)
ELSE
	SCATTER MEMVAR MEMO
	m.lnPremiumRecno	=GetPremiumRecno()	&& this mvar is used by BackEndUpdt() and BackEndTaxes(in Policy.prg)
	
*	IF SUBSTR(mpolicy,11,2)='AL'
*		DO programs\tax_pct	WITH m.polinumb
*		mamtrcvd = m.prem+m.taxes
*	ENDIF
	
ENDIF

m.cbKeyOriginalRec	=m.cbKey

*-------------------------------------------------------*
* A tax state?											|
* Is the Tax.DBF table currently open?					|
m.llTaxUsed	=USED("TAX")

DO CASE
* Tax table not currently open
CASE !m.llTaxUsed
	m.lnSelect	=SELECT()
	
	SELECT 0
	USE Tax
	SET ORDER TO AUTHORITY   && STATE+TAX_AUTH
	
* Tax table required and is currently open
CASE m.llTaxUsed
	m.lnSelect	=SELECT()
	
	SELECT Tax
	* Preserve order
	m.lcTaxOrder	=ORDER()
	SET ORDER TO AUTHORITY   && STATE+TAX_AUTH
	
ENDCASE

m.llTaxes	=SEEK(SUBSTR(m.polinumb,11,2))		&&	SELECT is Tax.dbf

IF !m.llTaxes
	USE IN Tax
ELSE
	DIMENSION aTaxCity[1]
	DIMENSION aTaxCounty[1]
	m.lnTax_Rate	=(PoliInd.nTxCtRate +PoliInd.nTxCoRate)
	=SetupTaxLists(SUBSTR(m.polinumb,11,2))		&& in Policy.prg
ENDIF

SELECT (m.lnSelect)
*-------------------------------------------------------*


@0,0 CLEAR
DO PROMPT WITH "Enter Information"


PRIVATE m.lcSetTalk

IF SET("TALK") = "ON"
	SET TALK OFF
	m.lcSetTalk = "ON"
ELSE
	m.lcSetTalk = "OFF"
ENDIF


m.llF8Update=.F.

* If (A)dding...
IF m.mMode="A"
	* ...set as TRUE to prevent and F8Update capabilities
	m.llHasNewPolicy	=.T.
ELSE
	m.lcNextPolicy	=ALLTRIM(NextChartNumb(PoliInd.PoliNumb))
		
	IF EMPTY(m.lcNextPolicy) .OR. ALLTRIM(m.lcNextPolicy)=ALLTRIM(PoliInd.PoliNumb)
		m.llHasNewPolicy	=.T.
	ELSE
		m.llHasNewPolicy	=HasNewPolicy(PoliInd.PoliNumb, PoliInd.PoliID, @lcWindowTitle)
	ENDIF
	
ENDIF


IF !m.llHasNewPolicy .AND. m.mMode="C"
	m.lcF8	=":"+SPACE(4)+"[F8]=Update to newer policy -> "+ALLTRIM(NextChartNumb(PoliInd.PoliNumb))
ELSE
	m.lcF8	=""
	
	DO CASE
	CASE m.mMode="A"
		m.lcNote	=""
	
	CASE m.mMode="C" .AND. EMPTY(m.lcNextPolicy) .OR. (ALLTRIM(m.lcNextPolicy)=ALLTRIM(PoliInd.PoliNumb))
		m.lcNote	="    [NOTE: Current policy]"
		
	CASE m.mMode="C"
		m.lcNote	="    [NOTE: Has newer policy]"
	
	OTHERWISE
		m.lcNote	=""
		
	ENDCASE
	
ENDIF

m.lcWindowTitle		=IIF(m.mMode="A", "Add", IIF(m.mMode="C", "Change", "View")) + " Individual" +IIF(m.llHasNewPolicy, m.lcNote, m.lcF8)
m.llwPoliScrnExists	=WEXIST("lwPoliScrn")

IF !m.llwPoliScrnExists
	DEFINE WINDOW lwPoliScrn	;
		FROM 3,1 TO WROWS()-1,100 		;
		TITLE m.lcWindowTitle	;
		NOFLOAT 	;
		NOCLOSE 	;
		SHADOW 		;
		NOMINIMIZE 	;
		COLOR SCHEME 1
ENDIF

IF WVISIBLE("lwPoliScrn")
	ACTIVATE WINDOW lwPoliScrn SAME
ELSE
	ACTIVATE WINDOW lwPoliScrn NOSHOW
ENDIF


* Create "Jump-to" lists of other policies for this Individual
PRIVATE m.laPoliID, m.laLicense, m.laSSN4, m.lnPoliIDCnt, m.lnLicenseCnt, m.lnSSN4Cnt, m.laClaims, m.lnClaimsCnt, m.lnEmail, m.laEIN
STORE .F. 	TO laPoliID, laLicense, laSSN4, laClaims, laEIN
DIMENSION laPoliID[1, 2]
laPoliID[1,1]=""
laPoliID[1,2]=0
DIMENSION laLicense[1, 2]
laLicense[1,1]=""
laLicense[1,2]=0
DIMENSION laSSN4[1, 2]
laSSN4[1,1]=""
laSSN4[1,2]=0
DIMENSION laEIN[1, 2]
laEIN[1,1]=""
laEIN[1,2]=0
DIMENSION laClaims[1, 2]
laClaims[1,1]=""
laClaims[1,2]=0
STORE 0 	TO m.lnPoliIDCnt, m.lnLicenseCnt, m.lnSSN4Cnt, m.lnClaimsCnt, m.lnEmailCnt

=JumpToLists()

* Create "Yes/No" dropdown for "Notify" GET fields
DIMENSION laYN[2,2]
laYN[1,1]="Yes"
laYN[1,2]=1
laYN[2,1]="No"
laYN[2,2]=2

IF mMode="C" .AND. !m.llHasNewPolicy .AND. !EMPTY(PoliInd.PoliID)
*	ON KEY LABEL f8 DO NewPolicy
	ON KEY LABEL f8 x=NewPolicy()
ELSE 
	ON KEY LABEL F8	x=MsgCurrent()
ENDIF

* Name & Address -------------------------------------------------------*
@00+m.lnViewRow,01+m.lnViewCol SAY "Policy Type" SIZE 1,11, 0
*@01+m.lnViewRow,02+m.lnViewCol SAY "First Name" SIZE 1,10, 0
*@02+m.lnViewRow,03+m.lnViewCol SAY "Last Name"  SIZE 1,9, 0
*@03+m.lnViewRow,05+m.lnViewCol SAY "Company" 	SIZE 1,7, 0
* Display labels for 	First Name	vs. <DISABLE>
*						Last Name		Company
*						Company			DBA
=SetIndvOrFirm(m.nPolicyTyp)
@04+m.lnViewRow,05+m.lnViewCol SAY "Address" 	SIZE 1,7, 0
@06+m.lnViewRow,08+m.lnViewCol SAY "City" 		SIZE 1,4, 0
@07+m.lnViewRow,07+m.lnViewCol SAY "State" 		SIZE 1,5, 0
@08+m.lnViewRow,09+m.lnViewCol SAY "Zip" 		SIZE 1,3, 0


* PoliID#, Type, License, SS#, Contact#s, email ------------------------*
@00+m.lnViewRow,49+m.lnViewCol SAY "POLI ID" 	SIZE 1,7,0
@01+m.lnViewRow,52+m.lnViewCol SAY "Type"		SIZE 1,4, 0
@03+m.lnViewRow,46+m.lnViewCol SAY "SS4" 		SIZE 1,3,0
@03+m.lnViewRow,58+m.lnViewCol SAY "EIN" 		SIZE 1,3,0
@04+m.lnViewRow,66+m.lnViewCol SAY "Ext#"		SIZE 1,4,0
@04+m.lnViewRow,71+m.lnViewCol SAY "Notify"		SIZE 1,6,0
@05+m.lnViewRow,44+m.lnViewCol SAY "Phone 1" 	SIZE 1,7, 0
@06+m.lnViewRow,44+m.lnViewCol SAY "Phone 2" 	SIZE 1,7, 0
@07+m.lnViewRow,48+m.lnViewCol SAY "Fax" 		SIZE 1,3, 0
@08+m.lnViewRow,47+m.lnViewCol SAY "Cell"		SIZE 1,4,0
*s@09.5+m.lnViewRow,26+m.lnViewCol SAY "Email" 		SIZE 1,4,0
@11+m.lnViewRow,49+m.lnViewCol SAY "Premium"	SIZE 1,7, 0


*IF SUBSTR(m.polinumb,11,2)='AL'
*	@08+m.lnViewRow,49+m.lnViewCol SAY "Amt Rcvd"	SIZE 1 ,7, 0
*ENDIF

* Taxes ----------------------------------------------------------------*
IF m.llTaxes
*	@08+m.lnViewRow,00+m.lnViewCol SAY "Tax Auth" 				SIZE 1,13, 0
	@12+m.lnViewRow,66+m.lnViewCol SAY "Tax"	
	@13+m.lnViewRow,19+m.lnViewCol TO 13+m.lnViewRow,62+m.lnViewCol
	@13+m.lnViewRow,19+m.lnViewCol SAY "<Tax Authority>"
	@13+m.lnViewRow,55+m.lnViewCol SAY "<Rate>" 
	@13+m.lnViewRow,62+m.lnViewCol SAY "<Amount>"
	@14.25+m.lnViewRow,11+m.lnViewCol SAY "  City"
	@15.75+m.lnViewRow,11+m.lnViewCol SAY "County"
ENDIF

* Dates ----------------------------------------------------------------*
@18+m.lnViewRow,02+m.lnViewCol SAY "Original Effective Date" 	SIZE 1,23, 0
*@13+m.lnViewRow,38+m.lnViewCol SAY "[F4]->Notes, [Tab]=Next Field"
@19+m.lnViewRow,03+m.lnViewCol SAY "Current Effective Date" 	SIZE 1,22, 0
@20+m.lnViewRow,09+m.lnViewCol SAY "Current End Date" 			SIZE 1,16, 0
@21+m.lnViewRow,08+m.lnViewCol SAY "Cancellation Date" 			SIZE 1,17, 0
@22+m.lnViewRow,10+m.lnViewCol SAY "Reinstated Date" 			SIZE 1,15, 0
@23+m.lnViewRow,12+m.lnViewCol SAY "Tail End Date" 				
@24+m.lnViewRow,09+m.lnViewCol SAY "Retroactive Date" 			SIZE 1,16, 0
@25+m.lnViewRow,15+m.lnViewCol SAY "Entry Date" 				SIZE 1,10, 0
@26+m.lnViewRow,02+m.lnViewCol SAY "Purchased on" 				SIZE 1,13, 0
*@21+m.lnViewRow,41+m.lnViewCol SAY "Print Envelope: " 			SIZE 1,16, 0

* Company Owner, AddTL, PDA#, [x] Alert Claims -------------------------*
@23+m.lnViewRow,40+m.lnViewCol SAY "Company Owner" 				SIZE 1,13, 0
@24+m.lnViewRow,40+m.lnViewCol SAY "    Addtl Key" 
@25+m.lnViewRow,40+m.lnViewCol SAY "         PDA#"
*@25+m.lnViewRow,51+m.lnViewCol 	GET m.claimalert	FUNCTION "*C Alert Claims Dept."	COLOR ,,,,,,,,R+/W*



DO CASE
* "View"
CASE m.mMode$'V'
	@28+m.lnViewRow,26+m.lnViewCol SAY "        (View only)          " 	SIZE 1,31, 0
	
* "Note View"
CASE m.mMode$'N'
	@28+m.lnViewRow,26+m.lnViewCol SAY "       [Esc] to Exit         " 	SIZE 1,31, 0
	
* "Add" or "Chg"
CASE m.mMode$'AC'
	@28+m.lnViewRow,26+m.lnViewCol SAY "[Ctrl+W]=Save º [Esc]=Cancel" 	SIZE 1,31, 0
	m.lcOnF4	=ON("KEY", "F4")
	ON KEY LABEL F4	 l=F4ToNotes()
	m.lcOnF12	=ON("KEY", "F12")
	ON KEY LABEL F12 l=ToEndorsements()
	m.lcOnF11	=ON("KEY", "F11")
	ON KEY LABEL F11 l=ToPoliTask()
	
ENDCASE

*? IF mMode#"V"
	@27,00 TO 27,77
	@00,77 TO 30,77
	@31,00 TO 31,WCOLS()
*? ENDIF

*--------------------------------------------------------------------*
* Do the GETs here
* Name & Address -------------------------------------------------------*
@00+m.lnViewRow,14+m.lnViewCol GET m.nPolicyTyp FUNCTION "*RH Individual;Firm"	DEFAULT IIF(!EMPTY(m.FirstName), 1, 2)  VALID SetIndvOrFirm(m.nPolicyTyp)
* Place this on the GET stack as either ENABLED or DISABLED.  It will get updated according by SetIndvOrFirm(). ------------->^^^^^^^^^^^^^
IF m.nPolicyTyp <= 1
	@01+m.lnViewRow,14+m.lnViewCol GET m.firstname 	SIZE 1,30 DEFAULT " " PICTURE "@!"	VALID BackEndUpdt()	&&	(m.mMode#"A" .OR. BackEndUpdt())
ELSE	
	@01+m.lnViewRow,14+m.lnViewCol GET m.firstname 	SIZE 1,30 DEFAULT " " PICTURE "@!"	VALID BackEndUpdt()	DISABLE &&	(m.mMode#"A" .OR. BackEndUpdt())
ENDIF
@02+m.lnViewRow,14+m.lnViewCol GET m.lastname 	SIZE 1,30 DEFAULT " " PICTURE "@!"	VALID IIF(m.llF8Update .OR. m.mMode="A", ChkInsured(), .T.) .AND. BackEndUpdt()	&&	(m.mMode#"A" .OR. BackEndUpdt()) .AND. chkinsured()
@03+m.lnViewRow,14+m.lnViewCol GET m.company 	SIZE 1,30 DEFAULT " " PICTURE "@!"	VALID BackEndUpdt()	&&	(m.mMode#"A" .OR. BackEndUpdt())
@04+m.lnViewRow,14+m.lnViewCol GET m.address1 	SIZE 1,30 DEFAULT " " PICTURE "@!"
@05+m.lnViewRow,14+m.lnViewCol GET m.address2 	SIZE 1,30 DEFAULT " " PICTURE "@!"
@06+m.lnViewRow,14+m.lnViewCol GET m.city 		SIZE 1,30 DEFAULT " " PICTURE "@!"	
@07+m.lnViewRow,14+m.lnViewCol GET m.state 		SIZE 1,02 DEFAULT " " PICTURE "@!"	
@08+m.lnViewRow,14+m.lnViewCol GET m.zip 		SIZE 1,05 DEFAULT " " PICTURE "#####"

* PoliID#, Type, License, SS#, Contact#s, email ------------------------*
@00+m.lnViewRow,58+m.lnViewCol GET m.poliid 	SIZE 1,8	DISABLED
@01+m.lnViewRow,58+m.lnViewCol GET m.tipe		WHEN TipeSetVars() VALID TipeChk()	PICTURE "@!"	
@02+m.lnViewRow,58+m.lnViewCol GET m.license	SIZE 1,9 	FUNCTION "!" DEFAULT " " VALID IIF(m.llF8Update .OR. m.mMode="A", ChkLicense(), .T.) .AND. IIF(m.License#PoliInd.License, JumpToLists(), .T.) ERROR "Possible duplicate License# entry. Please investigate."
@01.90+m.lnViewRow,45+m.lnViewCol GET m.lnLicense DEFAULT 1 	FUNCTION '* License+'+LicenseCnt(m.polinumb, m.poliid) 	SIZE 1.15,12	VALID ToLicense()

*@03+m.lnViewRow,48+m.lnViewCol SAY IIF(!EMPTY(m.cSSN5), REPLICATE(CHR(149),5),"    ")
*@03+m.lnViewRow,51+m.lnViewCol GET m.cSSN4 		SIZE 1,4 	WHEN m.nPolicyTyp=1 VALID IIF(m.llF8Update .OR. m.mMode="A", ChkSSN4(), .T.) .AND. IIF(m.cSSN4#PoliInd.cSSN4, BlankTheSSN5(03+m.lnViewRow,48+m.lnViewCol) .AND. JumpToLists(), .T.)
@03+m.lnViewRow,51+m.lnViewCol GET m.cSSN4 		SIZE 1,4 	WHEN m.nPolicyTyp=1 VALID IIF(m.llF8Update .OR. m.mMode="A", ChkSSN4(), .T.) .AND. IIF(m.cSSN4#PoliInd.cSSN4, JumpToLists(), .T.)
@03+m.lnViewRow,62+m.lnViewCol GET m.cEIN 					WHEN m.nPolicyTyp=2 VALID IIF(m.llF8Update .OR. m.mMode="A", ChkEIN(), .T.)  .AND. IIF(m.cEIN#PoliInd.cEIN, JumpToLists(), .T.) ERROR "Possible duplicate EIN entry. Please investigate."

@05+m.lnViewRow,51+m.lnViewCol GET m.phone1		SIZE 1,14	DEFAULT " "	PICTURE "(###) ###-####"
@05+m.lnViewRow,66+m.lnViewCol GET m.cPhnExt1	

@06+m.lnViewRow,51+m.lnViewCol GET m.phone2		SIZE 1,14	DEFAULT " "	PICTURE "(###) ###-####"
@06+m.lnViewRow,66+m.lnViewCol GET m.cPhnExt2		

@07+m.lnViewRow,51+m.lnViewCol GET m.fax		SIZE 1,14	DEFAULT " "	PICTURE "(###) ###-####"
@08+m.lnViewRow,51+m.lnViewCol GET m.cCellPhn		SIZE 1,14	DEFAULT " "	PICTURE "(###) ###-####"
@08+m.lnViewRow,66+m.lnViewCol GET m.cCellExt
@06.75+m.lnViewRow,71+m.lnViewCol GET m.nCellNoti 	PICTURE "@^" FROM laYN	SIZE 1,6

@09.5+m.lnViewRow,32+m.lnViewCol GET m.email PICTURE "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
@09.40+m.lnViewRow,21+m.lnViewCol GET m.lnEmail DEFAULT 1 	FUNCTION '* Email+'+EmailCnt(PoliInd.PoliID) 	SIZE 1.15,10	VALID ToEmail()

@08.25+m.lnViewRow,71+m.lnViewCol GET m.nEmailNoti	PICTURE "@^" FROM laYN	SIZE 1,6

@11+m.lnViewRow,58+m.lnViewCol GET m.prem	SIZE 1,7	DEFAULT 0	;
	VALID IIF(m.mMode#"V", ChkEffPrem(m.polinumb), .T.)	;
	.AND. IIF(m.llF8Update .OR. m.mMode="A", KYSurchgCalc() .AND. BackEndUpdt(), .T.)	;
	.AND. IIF(m.llTaxes .AND. (m.mMode="A" .OR. m.llF8Update), CalcCityAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes(), .T.)

* Taxes ----------------------------------------------------------------*
IF m.llTaxes
	@12.50+m.lnViewRow,19+m.lnViewCol GET m.cTxCtAuth	PICTURE "@^"	FROM aTaxCity		VALID SetCityRate() .AND. IIF(m.mMode="A" .OR. m.llF8Update, CalcCityAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes(), .T.)
*	@09.25+m.lnViewRow,55+m.lnViewCol GET m.nTxCtRate	SIZE 1,6,0		WHEN BizRuleTaxes(m.cTxCtAuth, m.nTxCtRate, 36, 40)	VALID BizRuleTaxes("OFF") .AND. IIF(m.mMode="A" .OR. m.llF8Update, CalcCityAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes(), .T.)
	@14.25+m.lnViewRow,55+m.lnViewCol GET m.nTxCtRate	SIZE 1,6,0		WHEN (m.mMode="A" .OR. m.llF8Update) VALID IIF(m.mMode="A" .OR. m.llF8Update, CalcCityAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes(), .T.)
	@14.25+m.lnViewRow,63+m.lnViewCol GET m.nTxCtAmt	SIZE 1,6,0		DISABLED	&&	WHEN .F.			VALID TotalTaxes() .AND. BackEndTaxes()
														
	@14.25+m.lnViewRow,19+m.lnViewCol GET m.cTxCoAuth	PICTURE "@^"	FROM aTaxCounty		VALID SetCountyRate() .AND. IIF(m.mMode="A" .OR. m.llF8Update, CalcCityAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes(), .T.)
*	@10.75+m.lnViewRow,55+m.lnViewCol GET m.nTxCoRate	SIZE 1,6,0		WHEN BizRuleTaxes(m.cTxCoAuth, m.nTxCoRate, 36, 40)	VALID BizRuleTaxes("OFF") .AND. IIF(m.mMode="A" .OR. m.llF8Update, CalcCountyAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes(), .T.)
*	@10.75+m.lnViewRow,55+m.lnViewCol GET m.nTxCoRate	SIZE 1,6,0		WHEN (m.mMode="A" .OR. m.llF8Update) VALID BizRuleTaxes("OFF") .AND. IIF(m.mMode="A" .OR. m.llF8Update, CalcCountyAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes(), .T.)
	@15.75+m.lnViewRow,55+m.lnViewCol GET m.nTxCoRate	SIZE 1,6,0		WHEN (m.mMode="A" .OR. m.llF8Update) VALID BizRuleTaxes("OFF") .AND. IIF(m.mMode="A" .OR. m.llF8Update, CalcCityAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes(), .T.)
	@15.75+m.lnViewRow,63+m.lnViewCol GET m.nTxCoAmt	SIZE 1,6,0		DISABLED	&&	WHEN .F.			VALID TotalTaxes() .AND. BackEndTaxes()

	@12+m.lnViewRow,70+m.lnViewCol GET m.taxes	SIZE 1,6	DISABLED
ENDIF

* Dates ----------------------------------------------------------------*
@18+m.lnViewRow,27+m.lnViewCol GET m.origdate    	SIZE 1,10 DEFAULT {  /  /  }

@19+m.lnViewRow,27+m.lnViewCol GET m.effective 	SIZE 1,10 DEFAULT {  /  /  }		;
	VALID IIF(m.mMode#"V", ChkEffRange(m.PoliNumb, m.Effective) .AND. ChkEffPrem(m.polinumb, "SET PREM"), .T.) .AND. IIF(m.llF8Update .OR. m.mMode$"AC", BackEndUpdt(), .T.)	;
	MESSAGE m.lcValidMsg	&&	(m.mMode#"A" .OR. BackEndUpdt())	
@20+m.lnViewRow,27+m.lnViewCol GET m.end 		SIZE 1,10 DEFAULT {  /  /  }
@21+m.lnViewRow,27+m.lnViewCol GET m.cancdate 	SIZE 1,10 DEFAULT {  /  /  }
@22+m.lnViewRow,27+m.lnViewCol GET m.reindate 	SIZE 1,10 DEFAULT {  /  /  }
@23+m.lnViewRow,27+m.lnViewCol GET m.renew 		SIZE 1,10 DEFAULT {  /  /  }	DISABLED
@24+m.lnViewRow,27+m.lnViewCol GET m.retroactiv SIZE 1,10 DEFAULT {  /  /  }
@25+m.lnViewRow,27+m.lnViewCol GET m.entrydate 	SIZE 1,10 DEFAULT {  /  /  }
@26+m.lnViewRow,15+m.lnViewCol GET m.tPurchase 	

* Company Owner, AddTL, PDA#, [x] Alert Claims -------------------------*
@23+m.lnViewRow,54+m.lnViewCol GET m.compowner 	SIZE 1,06 DEFAULT "      "
@24+m.lnViewRow,54+m.lnViewCol GET m.AddTl_Key
@25+m.lnViewRow,54+m.lnViewCol GET m.pdanum
*@26+m.lnViewRow,54+m.lnViewCol GET m.claimalert	FUNCTION "*C Alert Claims Dept."	COLOR ,,,,,,,,R+/W*
@26+m.lnViewRow,54+m.lnViewCol GET m.claimalert	FUNCTION "*C Alert Claims Dept."


* Notes ----------------------------------------------------------------*
@32+m.lnViewRow,01+m.lnViewCol 	EDIT m.notes	SIZE (WROWS()-32),WCOLS()-2,0	DEFAULT " "	SCROLL

* 
IF m.mMode='A'
	m.entrytime	=TIME()
	m.entryby 	=mstafcode
ENDIF

	
IF SUBSTR(m.polinumb,11,2)="KY" 	&& *? .AND. ALLTRIM(m.mMode)$"AC" 
	PRIVATE m.lnKYSurcharge
	m.lnKYSurcharge	=0	&&	default to 0
ENDIF



@00+m.lnViewRow,79+m.lnViewCol 	SAY "[You are viewing]"
@02+m.lnViewRow,81+m.lnViewCol 	GET m.polinumb 		DISABLE
@03+m.lnViewRow,83+m.lnViewCol	SAY GetOfferAs(m.PoliNumb)	COLOR RGB(255,0,0)
@04+m.lnViewRow,83+m.lnViewCol 	GET m.lnEndorsements DEFAULT 1 	FUNCTION '*BN End1.BMP' 	SIZE 2,10	VALID ToEndorsements()
IF USED("PoliExt") .and. SEEK(PoliInd.cbkey,"PoliExt","cbPoliInd")
	@03+m.lnViewRow,94+m.lnViewCol	GET m.lnPoliExt	DEFAULT 1	FUNCTION '*BN Save.BMP' SIZE 2,3	VALID ToPoliExt()
ENDIF
@05+m.lnViewRow,93+m.lnViewCol	SAY "[F12]" COLOR RGB(155,155,155,255,255,255)

@06+m.lnViewRow,78+m.lnViewCol 	TO 06+m.lnViewRow,99+m.lnViewCol
@07+m.lnViewRow,79+m.lnViewCol 	SAY " OTHER POLICIES"
	
@08+m.lnViewRow,79+m.lnViewCol 	SAY "By PoliID:"
*09+m.lnViewRow,79+m.lnViewCol 	GET cJumpPoliID		FROM laPoliID	SIZE 3.5,18 FUNCTION '&N' DEFAULT PoliInd.PoliNumb 	VALID JumpThisPoliID(cJumpPoliID) 	WHEN m.mMode#"A"	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"
@09+m.lnViewRow,79+m.lnViewCol 	GET nJumpPoliID		FROM laPoliID	SIZE 3.5,18 FUNCTION '&N'     DEFAULT IIF(EMPTY(laPoliID), 0, ASUBSCRIPT(laPoliID,ASCAN(laPoliID,RECNO("PoliInd")),1)) 	   VALID JumpThisPoliID(nJumpPoliID) 	WHEN m.mMode#"A"	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"

@12.75+m.lnViewRow,79+m.lnViewCol 	SAY "By " +SUBSTR(m.polinumb, 11, 2) +" License#:"
*13.75+m.lnViewRow,79+m.lnViewCol 	GET cJumpLicense	FROM laLicense	SIZE 3.5,18 FUNCTION '&N' DEFAULT PoliInd.License 	VALID JumpThisLicense(cJumpLicense) WHEN m.mMode#"A"	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"
@13.75+m.lnViewRow,79+m.lnViewCol 	GET nJumpLicense	FROM laLicense	SIZE 3.5,18 FUNCTION '&N' DEFAULT IIF(EMPTY(laLicense), 0, ASUBSCRIPT(laLicense,ASCAN(laLicense,RECNO("PoliInd")),1))  VALID JumpThisLicense(nJumpLicense) WHEN m.mMode#"A"	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"

@17.5+m.lnViewRow,79+m.lnViewCol 	SAY "By SS4#:"
*18.5+m.lnViewRow,79+m.lnViewCol 	GET cJumpSSN4 		FROM laSSN4 	SIZE 3.5,18 FUNCTION '&N' DEFAULT PoliInd.cSSN4 	VALID JumpThisSSN4(cJumpSSN4) 	WHEN m.mMode#"A" .AND. m.nPolicyTyp=1	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"
@18.5+m.lnViewRow,79+m.lnViewCol 	GET nJumpSSN4 		FROM laSSN4 	SIZE 3.5,18 FUNCTION '&N' DEFAULT IIF(EMPTY(laSSN4), 0, ASUBSCRIPT(laSSN4,ASCAN(laSSN4,RECNO("PoliInd")),1)) 	       VALID JumpThisSSN4(nJumpSSN4) 	WHEN m.mMode#"A" .AND. m.nPolicyTyp=1	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"
		
@22.25+m.lnViewRow,79+m.lnViewCol 	SAY "By EIN#:"
*23.25+m.lnViewRow,79+m.lnViewCol 	GET cJumpEIN 		FROM laEIN  	SIZE 3.5,18 FUNCTION '&N' DEFAULT PoliInd.cEIN 		VALID JumpThisEIN(cJumpEIN) 	WHEN m.mMode#"A" .AND. m.nPolicyTyp=2	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"
@23.25+m.lnViewRow,79+m.lnViewCol 	GET nJumpEIN 		FROM laEIN  	SIZE 3.5,18 FUNCTION '&N' DEFAULT IIF(EMPTY(laEIN), 0, ASUBSCRIPT(laEIN,ASCAN(laEIN,RECNO("PoliInd")),1)) 		       VALID JumpThisEIN(nJumpEIN) 	WHEN m.mMode#"A" .AND. m.nPolicyTyp=2	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"

@27+m.lnViewRow,79+m.lnViewCol 	SAY "    CLAIMS"
@28+m.lnViewRow,79+m.lnViewCol 	GET cJumpClaims 	FROM laClaims	SIZE 3,18 FUNCTION '&N' DEFAULT ""					VALID JumpThisClaim(cJumpClaims) 	WHEN m.mMode#"A" 	MESSAGE "[Dbl-Click] or [Enter] to display, [Tab] or [Ctrl+Tab] for next/previous field"

@28+m.lnViewRow,61+m.lnViewCol 	SAY "Tasks" GET m.lnPoliTask DEFAULT 1 	FUNCTION '*BN Graphics\bitmap_type.bmp'	SIZE 2,10	VALID ToPoliTask() .AND. PoliTaskCnt(PoliInd.cbKey, 29+m.lnViewRow,61+m.lnViewCol)
=PoliTaskCnt(PoliInd.cbKey, 29+m.lnViewRow,61+m.lnViewCol)
@30+m.lnViewRow,01+m.lnViewCol	SAY "NOTES:"
@30+m.lnViewRow,55+m.lnViewCol	SAY "Complete|" 
@30+m.lnViewRow,69+m.lnViewCol	SAY "[F11]" COLOR RGB(155,155,155,255,255,255)

*? IF !WVISIBLE("lwPoliScrn") .AND. mMode#"V"
	ACTIVATE WINDOW lwPoliScrn
*? ENDIF

*SET COVERAGE TO c:\temp\Cvrg.log	
READ CYCLE 	WHEN ReadLevelWhen() .AND. PoliIndAudit("ON")	VALID ReadOK()		&& ReadOK() is in Policy.PRG
*READ CYCLE 	WHEN ReadLevelWhen()	VALID ReadOK()		&& ReadOK() is in Policy.PRG
*SET COVERAGE TO 

m.lnLastKey	=LASTKEY()

* Allows you to scroll thru a Notes field when in "N"otesView mode
* Allows scroll thru Notes and/or display of Endorsements when in "V"iew mode
DO CASE
CASE m.mMode$"N"
    @32+m.lnViewRow,01+m.lnViewCol 	EDIT m.notes	SIZE (WROWS()-32),WCOLS()-2,0	DEFAULT " "	SCROLL NOMODIFY
	@04+m.lnViewRow,83+m.lnViewCol 	GET m.lnEndorsements DEFAULT 1 	FUNCTION '*BN End1.BMP' 	SIZE 2,10	VALID ToEndorsements()
	
	READ 

CASE m.mMode$"V"
	*READ
	
OTHERWISE
	* "Add" or "Chg"
	IF m.mMode$'AC'
		ON KEY LABEL F4	&lcOnF4
		ON KEY LABEL F12 &lcOnF12
		ON KEY LABEL F11 &lcOnF11
	ENDIF
	
ENDCASE

DO CASE
* Tax table was not previously open, so close it.
CASE !m.llTaxUsed .AND. USED("Tax")
	USE IN Tax

* Tax table was previously open and is still open, so restore prior order	
CASE m.llTaxUsed .AND. USED("Tax")
	m.lnSelect	=SELECT()
	
	SELECT Tax
	SET ORDER TO (m.lcTaxOrder)
	
	SELECT (m.lnSelect)

ENDCASE

IF m.lcSetTalk = "ON"
	SET TALK ON
ENDIF

SELECT PoliInd

*-------------------------------------------------------*
m.llSave	=.F.
m.llESC		=.F.

DO CASE
* Save: [Ctrl+W] was pressed during an Add or Change
CASE m.lnLastKey=23 .AND. m.mMode$"AC"
	m.llSave	=.T.	
	
* Cancel: via [Esc]
CASE m.lnLastKey=27 
	m.llESC	=.T.

	IF m.lnNewRecno >0	;
		.AND. (m.llF8Update .OR. m.mMode="A")
		
		m.llSave =YesNo("Save this record???",0,.T.)
	ENDIF	

ENDCASE
*-------------------------------------------------------*
	
	
IF m.mMode#"V" .OR. LASTKEY()=27
	RELEASE WINDOWS lwPoliScrn
ELSE
	ACTIVATE SCREEN
ENDIF


DO CASE
* Save: [Ctrl+W] was pressed during an Add or Change
CASE m.llSave		&&	m.lnLastKey=23 .AND. m.mMode$"AC"
	m.lCtrlW	=.T.	&&	will indicate that this editing was saved by user keying [Ctrl+W]
		
*	IF m.mMode="C"	;
		.AND. !SEEK(m.cbKey,"PoliInd","cbKey")
*		WAIT WINDOW "Will not be saved because:"+CHR(13)+m.polinumb+m.poliid +" would overwrite"+CHR(13)+PoliInd.PoliNumb+PoliInd.PoliID
*	ELSE
*SET STEP ON 	

	IF m.mMode="A"	;
		.OR. (m.mMode="C" .AND. m.cbKeyOriginalRec=poliind.cbkey) 
*		.OR. (m.mMode="C" .AND. m.cbKey=poliind.cbkey)

*		m.cbKey	=m.cbKeyOriginalRec
			
		DO WHILE .T.
			IF RLOCK()
				* Write to audit file (PoliIndA.DBF)
				=PoliIndAudit("PoliInd") .AND. PoliIndAudit("OFF")
				m.cbKey	=PoliInd.cbKey		&& assures m.cbKey contains the key of the original record, and not somehow overwritten by back end or <?>	
				GATHER MEMVAR MEMO
				UNLOCK
				=BackEndUpdt()
				FLUSH
				EXIT
			ENDIF
		ENDDO
		
	ENDIF
	
*	ENDIF
	
	IF m.llF8Update=.T.
		SET ORDER TO lastname
		SEEK mpolicy
	ENDIF
	
	m.llF8Update=.F.
	

* Cancel: via [Esc]
CASE m.llESC		&&	m.lnLastKey=27 
	m.lCtrlW	=.F.	&&	will indicate that this editing was NOT saved by user keying [Esc]
		
	DO CASE
	* A New policy was cancelled during NewPolicy()
	CASE m.llCancelNewPolicy
	
	* An F8-Updated .OR. newly-(A)dded record
	CASE m.lnNewRecno >0	;
		.AND. (m.llF8Update .OR. m.mMode="A")

		SELECT PoliInd
		GO (m.lnNewRecno)
		
		* Write to audit file (PoliIndA.DBF)
		*
		=PoliIndAudit("PoliInd") .AND. PoliIndAudit("OFF")
		
		SELECT End_Indv
		
		DO WHILE SEEK(PoliInd.PoliNumb +PoliInd.PoliID)
			DELETE
			BLANK
		ENDDO
		
		SELECT PoliLic
		
*		DO WHILE SEEK(PoliInd.PoliNumb +PoliInd.PoliID)
		DO WHILE SEEK(PoliInd.PoliID +PoliInd.PoliNumb)
			DELETE
			BLANK
		ENDDO

		
		SELECT PoliInd
		DELETE
		BLANK	&&	20150127  Blank it so that WebImport9 doesn't potentially see it and mis-assign an imported record as a DUPE
		FLUSH	
		
	OTHERWISE	&&	a (C)hanged record was canceled
		* Write to audit file (PoliIndA.DBF)
		*
		=PoliIndAudit("PoliInd") .AND. PoliIndAudit("OFF")
		SCATTER MEMVAR MEMO	&&	This returns front-end mVars to original values
		=BackEndUpdt()	&&	This returns back-end record(s) to match front-end values
		
	ENDCASE
	
	GO (m.lnOrigRec)
		
ENDCASE

=Chart("CLOSE")

IF m.lcSetEscape	="ON"
	SET ESCAPE ON
ENDIF


* Close WebCSV\Web 'extended data'
USE IN PoliExt

* Reset PoliLic original status --------*
IF m.llUsedPoliLic
	SELECT PoliLic
	SET ORDER TO (m.lcOrdPoliLic)
	GO (m.lnRecPoliLic)
ELSE
	USE IN PoliLic
ENDIF

* Reset PoliEmail original status --------*
IF m.llUsedPoliEmail
	SELECT PoliEmail
	SET ORDER TO (m.lcOrdPoliEmail)
	GO (m.lnRecPoliEmail)
ELSE
	USE IN PoliEmail
ENDIF

* Reset Claims original status ---------*
IF m.llUsedClaims
	SELECT Claims
	USE Claims
	SET ORDER TO (m.lcOrderClaims)
	GO (m.lnRecnoClaims)
	SELECT PoliInd
ELSE
	USE IN Claims
ENDIF

* Reset Back-End original status -------*
IF m.llUsedEnd_Indv
	SELECT End_Indv
	SET ORDER TO (m.lcOrderEnd_Indv)
	GO (m.lnRecnoEnd_Indv)
	
	SELECT PoliInd
ELSE
	USE IN End_Indv
ENDIF

* Reset Tasks original status ----------*
IF m.llPoliTask
	SELECT PoliTask
	SET ORDER TO (m.lcOrderPoliTask)
	GO (m.lnRecnoPoliTask)
	
	SELECT PoliInd
ELSE
	USE IN PoliTask
ENDIF


IF m.mMode="C"
	GO (m.lnOrigRec)
ENDIF

ON KEY LABEL F8
ON ERROR &lcOnError
RETURN .T.

***********************************************************************************************************************************
***********************************************************************************************************************************
***********************************************************************************************************************************
***********************************************************************************************************************************
***********************************************************************************************************************************
***********************************************************************************************************************************
***********************************************************************************************************************************
***********************************************************************************************************************************
*-------------------------------------------------------------------------------------------------------------------*
FUNCTION ReadLevelWhen     && Read Level When

DO CASE
CASE m.mMode="A"
	x=NewPolicy()
CASE m.mMode="V"
	CLEAR GETS
ENDCASE
	
RETURN m.mMode$"AC"


*-------------------------------------------------------------------------------------------------------------------*
FUNCTION NewPolicy
PRIVATE m.lnRecno, m.lwGetPoliBatch, m.lcPoliLicKey, m.lnSelect, m.lcLicense, m.lcTipe, m.lcState, m.cbKey
 
m.lnRecno	=RECNO()
	
IF (LASTKEY() = -7)	.AND. !m.llHasNewPolicy   && This is an F8 update
	m.llF8Update = .T.
	ON KEY LABEL F8
*	m.lcPoliLicKey	=PoliInd.PoliNumb +PoliInd.PoliID
*SET STEP ON 
	m.lcPoliLicKey	=PoliInd.PoliID
	m.lcLicense		=PoliInd.License
	m.lcTipe		=PoliInd.Tipe
	m.lcState		=PoliInd.State
	
*	m.lnSelect	=SELECT()
*	SELECT 0
*	SELECT Polilic.polinumb, Polilic.poliid, Polilic.state, Polilic.tipe,;
	  Polilic.license;
	 FROM polilic;
	 WHERE  polinumb+poliid = m.lcPoliLicKey;
	 INTO CURSOR qPoliLic READWRITE 

*	SELECT Polilic.polinumb, Polilic.poliid, Polilic.state, Polilic.tipe,;
	  Polilic.license;
	 FROM polilic;
	 WHERE  poliid = m.lcPoliLicKey;
	 INTO CURSOR qPoliLic READWRITE 

*	SELECT (m.lnSelect)
ENDIF
*SET STEP ON 
IF m.mMode="C" .AND. m.effective=m.end
	PRIVATE mrenew
	mrenew	=""
	
	DO WHILE !mrenew$"YN"
		WAIT WINDOW "This policy was cancelled."+CHR(13)+"Do you still want to renew? (Y/N)" TO mrenew
		mrenew	=UPPER(mrenew)
		
		IF mrenew="N"
			RETURN
		ENDIF
		
	ENDDO
	
ENDIF
		
DO CASE
* Add
CASE m.mMode="A"
	mpolinumb	=mpolicy
	
* Change	
CASE m.mMode="C"
	mpolinumb	=NextChartNumb(PoliInd.PoliNumb)
	
ENDCASE

*DO PROMPT WITH "Add to Policy Number º ESC to Abandon º CTRL-W to Save"
DEFINE WINDOW lwGetPoliBatch FROM 7,15 TO 13,65 SYSTEM SHADOW COLOR SCHEME 8 TITLE "New Policy/Batch Numbers"
merrmess	="Policy Does Not Exist ... Reenter"

DO WHILE .T.
	* Get the new Policy number
	ACTIVATE WINDOW lwGetPoliBatch
	@01,01 SAY "Enter New Policy#: " GET mpolinumb PICT "## !!!!!!!!!-!!" VALID ChkExist(mpolinumb) ERROR merrmess
	@02,01 SAY "           Batch#: " GET m.lcBatchID PICTURE "999999" DEFAULT SPACE(6)
	@03,18 GET m.lnOkCancel ;
		PICTURE "@*H \<OK;\<Cancel" ;
		SIZE 1.769,8.667,0.667 ;
		DEFAULT 2 ;
		FONT "MS Sans Serif", 8 ;
		STYLE "B" 
		
	READ
	
	DEACTIVATE WINDOW lwGetPoliBatch
		
	DO CASE
	* OK
	CASE m.lnOkCancel =1
		* Check for duplicate policy (i.e. someone may have already F8-Updated this one)
		IF m.llF8Update .AND. DupiPolicy(mpolinumb, m.poliid)
			EXIT
		ENDIF
		
		*---------------------------------------------------------------------------*
		* Append new front/back-end records now
		*
		*	Front-end
		SELECT PoliInd
		
		DO CASE
		* Add
		CASE m.mMode="A"
			m.poliid	=PoliIDNew()
*			m.origdate	=policy.effecdate
			SELECT ChartA
			SET ORDER TO PoliNumb
*			m.OrigDate	=IIF(SEEK(mPoliNumb, "ChartA", "PoliNumb"), ChartA.Effective, {})
			m.OrigDate	={}
			
			SELECT PoliInd
		* Change
		CASE m.mMode="C"
			m.notes	=ALLTRIM(PoliInd.Notes)
			
		ENDCASE
		
		m.polinumb	=LEFT(ALLTRIM(mpolinumb) +SPACE(LEN(PoliInd.PoliNumb)), LEN(PoliInd.PoliNumb))
		@03+m.lnViewRow,83+m.lnViewCol	SAY GetOfferAs(m.PoliNumb)	COLOR RGB(255,0,0)

		WAIT WINDOW NOWAIT "Make any changes for the new policy (" +ALLTRIM(m.PoliNumb) +") on this screen." 	;
			+IIF(m.llTaxes, CHR(13)+"Please check the taxes...", "")
		
		SELECT ChartA
		SET ORDER TO PoliNumb
		m.effective	=IIF(SEEK(m.PoliNumb, "ChartA", "PoliNumb")	.AND. YYYYMM(DATE())<=YYYYMM(ChartA.Effective), ChartA.Effective, {})
		m.end 		=IIF(SEEK(m.PoliNumb, "ChartA", "PoliNumb"), ChartA.End, {})
		
		SELECT ChartB
		SET ORDER TO POLIEND   && POLINUMB+ENDORSE
		
		=SEEK(m.PoliNumb +"PREMIUM", "ChartB", "PoliEnd")
		PRIVATE m.lnPremCount
		COUNT TO m.lnPremCount WHILE (ChartB.PoliNumb +LEFT(ChartB.Endorse, 7)) = (m.PoliNumb +"PREMIUM")
			
		DO CASE
		* None found
		CASE m.lnPremCount	=0
		* Only one found
		CASE m.lnPremCount	=1
			SELECT ChartC
			SET ORDER TO  PoliEnd
			m.prem 		=IIF(SEEK(m.PoliNumb+"PREMIUM", "ChartC", "PoliEnd"), ChartC.Amount, 0.00)	&&	policy.premium
		
		* More than one to choose from
		OTHERWISE
			SELECT 0
			SELECT Chartb.polinumb, Chartb.endorse, Chartc.amount, Chartc.effective;
			 FROM Chartc, Chartb;
			 WHERE Chartb.polinumb = Chartc.polinumb;
			   AND (Chartb.endorse = Chartc.endorse;
			   AND IIF(!EMPTY(m.Effective), m.effective = ChartC.effective, .T.);
			   AND Chartb.polinumb = m.polinumb;
			   AND LEFT( Chartb.endorse, 7) = "PREMIUM");
			 INTO CURSOR qPrems

			
			PRIVATE m.lwPrems
			DEFINE WINDOW lwPrems FROM 7,15 TO 15,65 SYSTEM SHADOW COLOR SCHEME 8 
			
			DO WHILE .T.
				WAIT WINDOW NOWAIT "There are " +ALLTRIM(STR(RECCOUNT())) +" PREMIUMS to choose from."
				BROWSE TITLE "Select a PREMIUM = [Ctrl+W]" WINDOW lwPrems;
					FIELDS amount, effective, endorse

				m.Prem =qPrems.Amount
				m.Endorse	=qPrems.Endorse
				
				IF AreYouSure('Select  "' +ALLTRIM(qPrems.endorse) +'"  @ $' +ALLTRIM(STR(m.Prem, 13, 2)) +'?', 7, .T.)
					EXIT
				ENDIF
			ENDDO
			
			USE IN qPrems
			RELEASE WINDOWS lwPrems
		ENDCASE
				
		m.entrydate	=DATE()
		m.entrytime	=TIME()
		m.entryby 	=mstafcode
		m.disksent 	=.F.
		m.certprntd	=.F.
		
		SELECT ChartA
		SET ORDER TO PoliNumb
		m.compowner	=IIF(SEEK(m.PoliNumb, "ChartA", "PoliNumb"), ChartA.cCompOwner, "")	&&	policy.compowner
		
		SELECT PoliInd		
		APPEND BLANK		&&	PoliInd.dbf
		
		* m.cbKey gets redefined here from it's intialization at the top of poliscr3.prg on
		*	-line 154 SCATTER MEMVAR MEMO BLANK
		*		or
		*	-line 164 SCATTER MEMVAR MEMO
		IF EMPTY(PoliInd.cbKey)
			m.cbKey	=GUID(36)	
		ELSE
			m.cbKey	=PoliInd.cbKey
		ENDIF
		
		m.cbKeyOriginalRec	=m.cbKey
		
		m.lnNewRecno	=RECNO()
		
		* If this function was called by F8
		*	- set as empty to assure prior policy year's value doesn't carry over
		IF m.llF8Update
			m.CancDate	={}
			m.OrigDate	={}
			m.ReinDate	={}	
			m.tPurchase	={}
			m.License	=m.lcLicense
			m.Tipe		=m.lcTipe
			m.State		=m.lcState
			m.pdanum	=""
			m.nTxCtAmt	=0
			m.nTxCoAmt	=0
		ENDIF
		
		m.PDANum		=SPACE(LEN(m.PDANum))
		m.claimalert	=.F.
		
		IF TYPE("m.xcLName")="C"
			m.lastname	=m.xcLName
			
			IF EMPTY(m.xcFName)
				m.company	=LEFT(ALLTRIM(m.xcLName) +SPACE(LEN(PoliInd.Company)), LEN(PoliInd.Company))
			ELSE
				m.firstname	=m.xcFName
			ENDIF
			
			m.cSSN4		=m.xcSSN4
			m.license	=m.xcLicense
			m.tipe		=m.xcTipe
		ENDIF
		
		GATHER MEMVAR MEMO
		
*		*	Multi-License? (If this is a "F8" brought forward, there may be multi-License#s to bring forward)
*		IF USED("qPoliLic") 
*			IF RECCOUNT("qPoliLic")>0
*				SELECT qPoliLic
*				REPLACE Polinumb	WITH PoliInd.Polinumb ALL
*				
*				SCAN
*					SCATTER MEMVAR
*					
*					SELECT PoliLic
*					APPEND BLANK
*					GATHER MEMVAR
*					
*					SELECT qPoliLic
*				ENDSCAN
*				
*			ENDIF
*		
*			USE IN qPoliLic
*			m.License	=m.lcLicense		
*			m.Tipe		=m.lcTipe		
*			m.State		=m.lcState
*		ENDIF
		
		
		*	Back-end ------------------------------------------------------
		SELECT End_Indv
		
		IF !EMPTY(ALLTRIM(PoliInd.FirstName) +" " +ALLTRIM(PoliInd.LastName))
			m.Insured	=ALLTRIM(PoliInd.FirstName) +" " +ALLTRIM(PoliInd.LastName)
		ELSE
*			m.Insured	=ALLTRIM(m.Company)
			m.Insured	=ALLTRIM(m.Lastname)
		ENDIF

		DO CASE
		* EXCESS
		CASE SUBSTR(polinumb, 4, 1)="X"
			m.endorse	="EXCESS"
			
		* TAIL
		CASE SUBSTR(polinumb, 4, 2)="TL"
			m.endorse	="TAIL"
			
		* Otherwise
		OTHERWISE
			SELECT ChartB
			m.lcChartBOrder	=ORDER()
			m.lnChartBRecno	=RECNO()
			SET ORDER TO POLIEND   && POLINUMB+ENDORSE
			
			IF SEEK(m.PoliNumb +IIF(m.lnPremCount>1, m.endorse, "PREMIUM"))
				m.Endorse	=ChartB.Endorse
				m.nDispOrdr	=ChartB.nDispOrdr
				m.cForm		=ChartB.cForm
			ELSE
				m.Endorse	="PREMIUM?"
				m.nDispOrdr	=1
				m.cForm		="UNKNOWN"
			ENDIF
			
			SET ORDER TO (m.lcChartBOrder)
			GO (m.lnChartBRecno)
			
			SELECT End_Indv
			
		ENDCASE
		
		m.Premium	=PoliInd.Prem
		m.Expires	=PoliInd.End
		m.Batch_ID	=m.lcBatchID
		m.tax		=0
		m.tax_auth	=""
		
		SELECT End_Indv
		APPEND BLANK		&&	End_Indv.dbf
			
		IF EMPTY(End_Indv.cbKey)
			m.cbKey	=GUID(36)
		ELSE
			m.cbKey	=End_Indv.cbKey
		ENDIF
		
		m.cbPoliInd	=PoliInd.cbKey
		GATHER MEMVAR
		
		m.lnPremiumRecno	=RECNO()	&&	the mvar name is used by BackEndUpdt() and BackEndTaxes(in Policy.prg)
		
		* Setup Surcharge record
		IF m.llTaxes .AND. SUBSTR(m.PoliNumb,11,2)="KY"	;
			.AND. CalcCityAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes()	;
			.AND. KYSurchgCalc()	&&	calculates m.lnKYSurcharge
			SELECT End_Indv
			APPEND BLANK	&&	End_Indv.dbf
			
			IF EMPTY(End_Indv.cbKey)
				m.cbKey	=GUID(36)
			ELSE
				m.cbKey	=End_Indv.cbKey
			ENDIF
			
			m.cbPoliInd	=PoliInd.cbKey
			GATHER MEMVAR
			
			REPLACE End_Indv.Premium	WITH m.lnKYSurcharge,	;
					End_Indv.Tax		WITH 0.00,	;
					End_Indv.Endorse	WITH "SURCHARGE ON PREMIUM"
		ENDIF
		
		=PoliIndAudit("ON") .AND. AddFreebies()	&&	AddFreebies() is in Policy.PRG
		FLUSH
		*---------------------------------------------------------------------------*
		
		SELECT PoliInd
		=JumpToLists()		&& update these to reflect the newly added/changed policy
		SHOW GETS
		
		IF m.mMode="C"
			m.llF8Update	=.T.
		ENDIF
		
		EXIT
			
	* Cancel
	OTHERWISE
		m.llCancelNewPolicy	=.T.
		KEYBOARD '{ESC}'
		EXIT
		
	ENDCASE
		
ENDDO
	
RELEASE WINDOW lwGetPoliBatch

*IF USED("qPoliLic")
*	USE IN qPoliLic
*ENDIF

SELECT poliind

* If this is an F8Update, provide a new audit record
IF m.llF8Update
	=PoliIndAudit("NEW")
ENDIF

RETURN


*-------------------------------------------------------------------------------------------------------------------*
PROCEDURE xxxDateDown
m.effective	=CTOD(SUBS(DTOC(m.effective), 1, 6) +STR(YEAR(m.effective) -1, 4))
m.end		=CTOD(SUBS(DTOC(m.end), 1, 6) +STR(YEAR(m.end) -1, 4))
@10+m.lnViewRow,27 GET m.effective	VALID trakchk(mpremium,m.effective)
@11+m.lnViewRow,27 GET m.end 		VALID chkexpd(m.effective,m.end)
CLEAR GETS
RETURN


*-------------------------------------------------------------------------------------------------------------------*
PROCEDURE xxxDateUp
m.effective	=CTOD(SUBS(DTOC(m.effective), 1, 6) +STR(YEAR(m.effective) +1, 4))
m.end		=CTOD(SUBS(DTOC(m.end), 1, 6) +STR(YEAR(m.end) +1, 4))
@10+m.lnViewRow,27 GET m.effective	VALID trakchk(mpremium,m.effective)
@11+m.lnViewRow,27 GET m.end 		VALID chkexpd(m.effective,m.end)
CLEAR GETS
RETURN


*-------------------------------------------------------------------------------------------------------------------*
PROCEDURE xxxTrakChk
PARAMETER mpremium, meffecdate
PRIVATE mpremium, meffecdate

IF policy.mpremtype="PREF"
	mpremamt	=(mpremium *mnumoflic)
	
	STORE CTOD("  /  /  ") TO mday1, mday2, mday3, mday4
	STORE 0 TO mpay1, mpay2, mpay3, mpay4
	
	IF mpayments>=1
		mday1	=meffecdate+mpday1
		mpay1	=ROUND(mpremamt*mppay1/100,2)
	ENDIF
	
	IF mpayments>=2
		mday2	=meffecdate+mpday2
		mpay2	=ROUND(mpremamt*mppay2/100,2)
	ENDIF
	
	IF mpayments>=3
		mday3	=meffecdate+mpday3
		mpay3	=ROUND(mpremamt*mppay3/100,2)
	ENDIF
	
	IF mpayments=4
		mday4	=meffecdate+mpday4
		mpay4	=ROUND(mpremamt*mppay4/100,2)
	ENDIF
	
	DO timedisp WITH 20
	CLEAR GETS
ENDIF

RETURN .T.


*-------------------------------------------------------------------------------------------------------------------*
PROCEDURE xxxchkexpd
PARAMETER mcurdate,mexpdate
PRIVATE mcurdate,mexpdate

IF GOMONTH(mcurdate,12)<>mexpdate
	WAIT "Current Expiration Date is not 12 Months from Current Effective Date" WINDOW
ENDIF

RETURN .T.


*-------------------------------------------------------------------------------------------------------------------*
PROCEDURE ChkExist
PARAMETERS m.xcPoliNumb
PRIVATE m.llChkExist, m.lnSelect
m.lnSelect	=SELECT()

SELECT ChartA
SET ORDER TO PoliNumb
m.llChkExist	=SEEK(m.xcPoliNumb, "ChartA", "PoliNumb")
m.AddTl_Key		=IIF(m.llChkExist, ChartA.AddTl_Key, m.AddTl_Key)
*m.Effective		=IIF(m.llChkExist .AND. EMPTY(m.Effective), ChartA.Effective, m.Effective)

SELECT (m.lnSelect)
RETURN m.llChkExist


*-------------------------------------------------------------------------------------------------------------------*
FUNCTION taxamt
m.prem	=ROUND(mamtrcvd / (1+(mtax_pct/100)),2)
m.taxes	=mamtrcvd-m.prem
@07+m.lnViewRow,55 SAY mtax_pct	PICTURE "999.99"
@07+m.lnViewRow,63 SAY m.taxes 	PICTURE "9,999.99"
@06+m.lnViewRow,58 SAY m.prem 	SIZE 1,10
RETURN .T.


*-------------------------------------------------------------------------------------------------------------------*
FUNCTION ChkInsured
PRIVATE m.lnSelect
m.lnSelect	=SELECT()

SELECT 0
USE PoliInd AGAIN ALIAS ChkInsured ORDER ChkInsure	&& UPPER(LEFT(POLINUMB,2)+SUBSTR(POLINUMB,11,2)+LEFT(ALLTRIM(FIRSTNAME)+SPACE(30),30)+LEFT(ALLTRIM(LASTNAME)+SPACE(30),30))

IF EMPTY(m.firstname) AND EMPTY(m.lastname)
	return_value=.T.
ELSE
	* Display a notice if it is found .AND. the EntryDate .OR. EntryTime differs.
	IF SEEK(LEFT(m.polinumb,2)+SUBSTR(m.polinumb,11,2)+LEFT(ALLTRIM(UPPER(m.firstname)),30)+LEFT(ALLTRIM(UPPER(m.lastname)),30),"CHKINSURED")	;
		.AND. (m.entrydate#entrydate .OR. m.entrytime#entrytime)
		WAIT WINDOW "Insured name already exists for policy "+ALLTRIM(chkinsured.polinumb)+"."+CHR(13)+"Possible duplicate entry. Please investigate."
		return_value=.T.
	ELSE
		return_value=.T.
	ENDIF
	
ENDIF

USE IN ChkInsured

SELECT (m.lnSelect)
RETURN return_value


*-------------------------------------------------------------------------------------------------------------------*
FUNCTION ChkLicense
PRIVATE m.llReturn, m.lnSelect
m.llReturn	=.T.	&&	default to TRUE
m.lnSelect	=SELECT()

SELECT 0
USE PoliInd AGAIN ALIAS ChkLicense ORDER ChkLicense
	
DO CASE
CASE EMPTY(m.license) 
	m.llReturn=.T.
	
CASE ALLTRIM(m.License)="12345678"
	m.llReturn=.T.
		
OTHERWISE

	DO CASE
	* Mississippi .OR. Iowa
	CASE ("MS"$m.polinumb) .OR. ("IA"$m.polinumb)
		IF SEEK(LEFT(m.polinumb,2)+SUBSTR(m.polinumb,11,2)+UPPER(m.license),"CHKLICENSE") 
			LOCATE REST WHILE (LEFT(ChkLicense.PoliNumb,2) +SUBSTR(ChkLicense.PoliNumb,11,2) +UPPER(ChkLicense.License))	;
							=(LEFT(m.PoliNumb,2)+SUBSTR(m.PoliNumb,11,2)+UPPER(m.license))	;
							FOR m.entrydate#ChkLicense.EntryDate .AND. m.entrytime#ChkLicense.EntryTime	.AND. ChkLicense.Tipe =m.Tipe
			m.llReturn	=!FOUND()
		ENDIF
	
	OTHERWISE
		IF SEEK(LEFT(m.polinumb,2)+SUBSTR(m.polinumb,11,2)+UPPER(m.license),"CHKLICENSE") AND m.entrydate#entrydate AND m.entrytime#entrytime
			WAIT WINDOW "License number already exists for policy "+ALLTRIM(ChkLicense.PoliNumb)+"."
			m.llReturn=.F.
		ENDIF
		
	ENDCASE
			
ENDCASE
	
USE IN ChkLicense

SELECT (m.lnSelect)
RETURN m.llReturn


*-------------------------------------------------------------------------------------------------------------------*
FUNCTION ChkEIN
PRIVATE m.llReturn, m.lnSelect
m.llReturn	=.T.	&&	default to TRUE
	
IF EMPTY(m.cEIN) 
	m.llReturn=.T.
ELSE	
	m.lnSelect	=SELECT()
	
	IF USED("ChkEIN")		&& 20160815 Not sure why this was erroring as "alread in use"
		USE IN ChkEIN
	ENDIF
	
	SELECT 0
	USE PoliInd AGAIN ALIAS ChkEIN ORDER ChkEIN
			
		
	IF SEEK(LEFT(m.polinumb,2)+SUBSTR(m.polinumb,11,2)+UPPER(m.cEIN),"ChkEIN") 
		SCAN REST WHILE LEFT(m.PoliNumb,2)+SUBSTR(m.PoliNumb,11,2)+UPPER(m.cEIN) = LEFT(ChkEIN.PoliNumb,2)+SUBSTR(ChkEIN.PoliNumb,11,2)+UPPER(ChkEIN.cEIN)	;
			FOR m.EntryDate#ChkEIN.EntryDate .AND. m.EntryTime#ChkEIN.EntryTime
			WAIT WINDOW "License number already exists for policy "+ALLTRIM(ChkLicense.PoliNumb)+"."
			m.llReturn=.F.
			EXIT
		ENDSCAN
	ENDIF
		
	USE IN ChkEIN
	
	SELECT (m.lnSelect)
ENDIF
	
RETURN m.llReturn


*-------------------------------------------------------------------------------------------------------------------*
FUNCTION ChkSSN4
PRIVATE m.lnSelect, m.llChkSSN4
m.lnSelect	=SELECT()

SELECT 0
USE PoliInd AGAIN ALIAS ChkSSN4 ORDER ChkSSN4	&& && UPPER(LEFT(POLINUMB,2)+SUBSTR(POLINUMB,11,2))+CSSN4+CSSN5+LICENSE+TIPE

* If not cSSN4 .or. no License/Tipe, no need to check for duplicate policy.
IF EMPTY(m.cSSN4) .OR. EMPTY(m.License +m.Tipe)
	m.llChkSSN4=.T.
ELSE
*	IF SEEK(LEFT(m.PoliNumb,2) +SUBSTR(m.PoliNumb,11,2) +m.cSSN4 +m.cSSN5 +m.License +m.Tipe,"CHKSSN4")  AND m.entrydate#entrydate AND m.entrytime#entrytime
	IF SEEK(LEFT(m.PoliNumb,2) +SUBSTR(m.PoliNumb,11,2) +m.cSSN4 +m.License +m.Tipe,"CHKSSN4")  AND m.entrydate#entrydate AND m.entrytime#entrytime
		WAIT WINDOW "Social Security Number and Lic#/Type already exist for policy "+ALLTRIM(ChkSSN4.PoliNumb)+"."
		m.llChkSSN4=.F.
	ELSE
		m.llChkSSN4=.T.
	ENDIF
	
ENDIF

USE IN ChkSSN4

SELECT (m.lnSelect)
RETURN m.llChkSSN4


*-------------------------------------------------------------------------------------------------------------------*
FUNCTION ChkTipe
PARAMETERS m.lcPoliNum, m.lcTipe
	
IF EMPTY(m.license)	;
	.AND. (ALLTRIM(m.lcTipe)="T" .OR. ALLTRIM(m.lcTipe)="TT")

	WAIT WINDOW NOWAIT 'License defaults to "12345678" when Type is "T" or "TT".'
	m.License="12345678"
ENDIF	

RETURN .T.
	
			
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION NextChartNumb
PARAMETERS m.xcPoliNumb
PRIVATE m.lnSelect, m.llChartA, m.lcState, m.lcYear, m.lcPoliNumb, m.lcProgram

m.lnSelect	=SELECT()
m.llChartA	=USED("ChartA")
m.lcState	=SUBSTR(m.xcPoliNumb, 11, 2)
m.lcYear	=LEFT(m.xcPoliNumb, 2)
m.lcProgram =SUBSTR(m.xcPoliNumb, 4, 2)
m.lcPoliNumb=""	&&	default

IF !m.llChartA
	USE ChartA
ELSE
	PRIVATE m.lcChartAOrder, m.lnChartARecno
	SELECT ChartA
	m.lcChartAOrder	=ORDER()
	m.lnChartARecno	=RECNO()
ENDIF

*-----------------------------------------------------------*
* Find the most current (active) policy
*-----------------------------------------------------------*
SET ORDER TO STATEYR	DESCENDING   && STATE+LEFT(POLINUMB,2)
SEEK (m.lcState)
*LOCATE REST FOR ChartA.State =m.lcState .AND. ChartA.lActive
LOCATE REST FOR ChartA.State =m.lcState .AND. ChartA.lActive .AND. SUBSTR(ChartA.PoliNumb, 4, 2)=m.lcProgram
m.lcPoliNumb	=ChartA.PoliNumb
*-----------------------------------------------------------*


IF m.llChartA
	SET ORDER TO (m.lcChartAOrder)
	GO (m.lnChartARecno)
ELSE
	USE IN ChartA
ENDIF

SELECT (m.lnSelect)
RETURN m.lcPoliNumb
	

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION NextPoliNumb
PARAMETERS m.xcPoliNumb
PRIVATE m.lnSelect, m.lcPoliNumb, m.llPolEffUsed, m.lcState
m.lnSelect	=SELECT()
m.llPolEffUsed	=USED("PolEff")

IF m.llPolEffUsed
	SELECT PolEff
ELSE
	SELECT 0
	USE Web\PolEff
ENDIF

LOCATE FOR ALLTRIM(m.xcPoliNumb)=ALLTRIM(PolEff.PoliNumb)

IF FOUND()
	m.lcState	=PolEff.State
	LOCATE REST FOR PolEff.State	=m.lcState
	CONTINUE
ENDIF

m.lcPoliNumb	=LEFT(ALLTRIM(PolEff.PoliNumb) +SPACE(LEN(PoliInd.PoliNumb)), LEN(PoliInd.PoliNumb))

IF !m.llPolEffUsed
	USE IN PolEff
ENDIF

SELECT (m.lnSelect)
RETURN m.lcPoliNumb
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION DupiPolicy
PARAMETERS m.xcPoliNumb, m.xcPoliID
PRIVATE m.lnSelect, m.llFound
m.lnSelect	=SELECT()

SELECT 0
USE poliind AGAIN ALIAS chkpoliid ORDER polinum_id 

m.llFound	=SEEK(m.xcPoliNumb + m.xcPoliID)

IF m.llFound
	WAIT WINDOW "POLIID already exists for this policy!!"	
*		+CHR(13) +"Possible duplicate entry.  No changes have been made."	;
*		+CHR(13) +"Press any key to continue..."
		
		m.lnTop	=INT((WROWS()/2) - 10)
		m.lnLeft=ABS(INT((WROWS()/2) -50))
		DEFINE WINDOW lwDupiPolicy	;
		FROM m.lnTop,m.lnLeft TO m.lnTop+12,m.lnLeft+60 		;
		TITLE "Duplicate PoliID#"	;
		NOFLOAT 	;
		NOCLOSE 	;
		SHADOW 		;
		NOMINIMIZE 	;
		COLOR SCHEME 1
		ACTIVATE WINDOW lwDupiPolicy
		@ 1,1 say "PoliID# "+m.xcpoliid +" is already used on Policy# "+ALLTRIM(m.xcpolinumb)
		@ 2,1 say "by policy holder "+ALLTRIM(ChkPoliID.firstname)+" "+ALLTRIM(ChkPoliID.lastname)
		@ 3,1 say "                 "+ALLTRIM(ChkPoliID.address1)
		@ 4,1 say "                 "+ALLTRIM(ChkPoliID.city)+", "+ALLTRIM(ChkPoliID.state)+"  "+ALLTRIM(ChkPoliID.zip)
		@ 5,1 say "  Proceed with "+m.xcpoliid+" anyway?"
		@ 5,1 say "                                                   "
		@ 6,1 say "  Proceed using PoliID# "+m.xcpoliid+" anyway?"

		m.lnYN=2
		@ 8,22 prompt "Yes"
		@ 8,27 prompt "No"
		MENU TO m.lnYN
		m.llFound	=IIF(m.lnYN =1, .F., .T.)
		RELEASE WINDOWS lwDupiPolicy	

ENDIF

USE IN chkpoliid
SELECT (m.lnSelect)
RETURN m.llFound
		
		
*---------------------------------------------------------------------------------------------------------------------*
* Determine if Individual has a policy later than the one currently displayed
FUNCTION HasNewPolicy
PARAMETERS m.xcPoliNumb, m.jcPoliID, m.jcWindowTitle
PRIVATE m.llHasNewPI, m.lnSelect, m.lcSetDeleted, m.lcNextPolicy, m.lcState, m.lcLastName
m.llHasNewPI	=.F.
m.lnSelect		=SELECT()
m.lcSetDeleted	=SET("DELETED")
SET DELETED ON

* Open PoliInd table again for search purposes.  Close upon completion of searching.
SELECT 0
USE poliind AGAIN ALIAS HasNewPI 

* FIRST:  Search by PoliID  ------------------------------------------------------------*
SET ORDER TO PoliID	ASCENDING
GO RECNO("PoliInd")		&&	move record pointer to currently displayed policy

* Search from this record for a policy > the currently displayed policy
LOCATE REST FOR (HasNewPI.PoliNumb > m.xcPoliNumb) 	;
	.AND. (LEFT(HasNewPI.PoliNumb, 2) =ALLTRIM(STR(val(LEFT(poliind.polinumb,2))+1))) 		;
	.AND. SUBSTR(HasNewPI.PoliNumb, 11, 2) = SUBSTR(m.xcPoliNumb, 11, 2)	;
	WHILE HasNewPI.PoliID = m.jcPoliID

* 	.AND. (LEFT(HasNewPI.PoliNumb, 2) < "20") 		;

* If person has a newer Policy 
m.llHasNewPI	=FOUND() 
	
* SECOND:  Search by SS4#---------------------------------------------------------------*
* fix this!
IF !m.llHasNewPI .AND. !EMPTY(PoliInd.cSSN4)
	* Search by SS4#
*	SET ORDER TO SS4LICTIP   ASCENDING && CSSN4+CSSN5+LICENSE+TIPE
	SET ORDER to SSKEY   && CSSN4+LEFT(FIRSTNAME,2)+LEFT(LASTNAME,3)+SUBSTR(POLINUMB,11,2)+STR(10000-IIF(LEFT(POLINUMB,2)>"50",VAL("19"+LEFT(POLINUMB,2)),VAL("20"+LEFT(POLINUMB,2))))
	GO RECNO("PoliInd")	&&	move record pointer to currently displayed policy
	
	* Search from this record for a policy > the currently displayed policy
	LOCATE REST FOR (HasNewPI.PoliNumb > m.xcPoliNumb)	;
		.AND. (LEFT(HasNewPI.PoliNumb, 2) =ALLTRIM(STR(val(LEFT(poliind.polinumb,2))+1))) 		;
		.AND. SUBSTR(HasNewPI.PoliNumb, 11, 2) = SUBSTR(m.xcPoliNumb, 11, 2)	;
		WHILE HasNewPI.cSSN4 = PoliInd.cSSN4+PoliInd.cSSN4
	
	* If person has a newer Policy 
	m.llHasNewPI	=FOUND() 
ENDIF

* THIRD:  Search by Lic# (for State & Tipe)  -------------------------------------------*
IF !m.llHasNewPI .AND. !EMPTY(PoliInd.License) .AND. !EMPTY(PoliInd.Tipe)
	* Lic# for State+Tipe
	SET ORDER TO STATELIC	ASCENDING   && SUBSTR(POLINUMB,11,2)+LICENSE
	GO RECNO("PoliInd")	&&	move record pointer to currently displayed policy
	
	* Search from this record for a policy > the currently displayed policy
	LOCATE REST FOR (HasNewPI.PoliNumb > m.xcPoliNumb)	;
				.AND. (LEFT(HasNewPI.PoliNumb, 2) =ALLTRIM(STR(val(LEFT(poliind.polinumb,2))+1)))	;
				.AND. (HasNewPI.Tipe=PoliInd.Tipe)	;	
				.AND. SUBSTR(HasNewPI.PoliNumb, 11, 2) = SUBSTR(m.xcPoliNumb, 11, 2)	;
				WHILE (SUBSTR(HasNewPI.PoliNumb, 11, 2) +HasNewPI.License) = (SUBSTR(PoliInd.PoliNumb, 11, 2) +PoliInd.License)	
		
	* If person has a newer Policy 
	m.llHasNewPI	=FOUND() 
ENDIF

IF m.llHasNewPI
	* (Note:  "m.jcWindowTitle" is the local name for a mvar that has been passed by reference)
	*	Update the definition of reference-passed mvar
	m.jcWindowTitle	="NOTE! There is an updated policy:  " +ALLTRIM(HasNewPI.PoliNumb)
ENDIF

USE	

IF m.lcSetDeleted	="OFF"
	SET DELETED OFF
ENDIF

SELECT (m.lnSelect)
RETURN m.llHasNewPI


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION BackEndUpdt
PRIVATE m.lnSelect, m.lcOrder
m.lnSelect	=SELECT()
 
DO CASE
* Both City/County tax authority
CASE !EMPTY(m.cTxCtAuth) .AND. !EMPTY(m.cTxCoAuth)
	m.Tax_Auth	=ALLTRIM(m.cTxCtAuth)+"/"+ALLTRIM(m.cTxCoAuth)
* City only
CASE !EMPTY(m.cTxCtAuth)
	m.Tax_Auth	=m.cTxCtAuth
* County only	
CASE !EMPTY(m.cTxCoAuth)
	m.Tax_Auth	=m.cTxCoAuth
* none	
OTHERWISE
	m.Tax_Auth	=""
ENDCASE

SELECT End_Indv
m.lcOrder	=ORDER()

* Update any related back-end records by Insured & Tax_Auth to reflect the front-end Firstname/Lastname/Company & Tax_Auth.
IF SEEK(PoliInd.PoliNumb +PoliInd.PoliID)
	* If this is a KY policy:
	*	- check if it is a SURCHARGE ON PREMIUM
	*		* make Premium value equal to the current value of mvar 'm.lnKYSurcharge'
	*		* If mmode='A', make Effective value equal to the current value of mvar m.Effective
	* If this is NOT a KY policy
	*	- check if Premium is 0.00
	*		* If m.llF8Update .OR. mmode='A', make Effective value equal to the current value of mvar m.Effective
*	REPLACE End_Indv.Insured	WITH IIF(!EMPTY(m.FirstName), ALLTRIM(m.FirstName) +" " +ALLTRIM(m.LastName), m.Company)	
	REPLACE End_Indv.Insured	WITH IIF(!EMPTY(m.FirstName), ALLTRIM(m.FirstName) +" " +ALLTRIM(m.LastName), m.LastName)	;
			WHILE (End_Indv.PoliNumb +End_Indv.PoliID) = (m.PoliNumb +m.PoliID)
	=SEEK(PoliInd.PoliNumb +PoliInd.PoliID)
	REPLACE End_Indv.Tax_Auth	WITH m.Tax_Auth	;
			End_Indv.Premium	WITH IIF( End_Indv.Endorse="SURCHARGE ON PREMIUM" .AND. SUBSTR(End_Indv.PoliNumb,11,2)="KY" .AND. (m.lnKYSurcharge >0), m.lnKYSurcharge, End_Indv.Premium)	;
			End_Indv.Effective	WITH IIF(!("DUPE"$PoliInd.PDAnum) .AND. End_Indv.Premium=0 .AND. SEEK(End_indv.PoliNumb +End_Indv.Endorse,"ChartB","POLIEND"), m.Effective, End_Indv.Effective)	;
			WHILE (End_Indv.PoliNumb +End_Indv.PoliID) = (m.PoliNumb +m.PoliID)	;
			FOR !("BACKDATE"$End_Indv.Endorse)
	
	
	* "Tail End Date" ---------------------------------------------------------------*
	SET ORDER TO ENDORSE DESCENDING    && UPPER(POLINUMB+POLIID)+DTOS(ENTRYDATE)+ENTRYTIME+STR(NDISPORDR)
	=SEEK(PoliInd.PoliNumb +PoliInd.PoliID)
	LOCATE REST WHILE (End_Indv.PoliNumb +End_Indv.PoliID) = (m.PoliNumb +m.PoliID)	;
		FOR "TAIL"$End_Indv.Endorse
	
	DO CASE
	* Found & active (positive End_Indv.Premium)
	CASE FOUND() .AND. End_Indv.Premium>0
		IF PoliInd.Renew # End_Indv.Expires	;
			.OR. EMPTY(Poliind.Renew)
			PRIVATE m.i
				
			* Look for TAIL 1 - 6 in the Endorse
			FOR m.i = 1 TO 6
				IF ALLTRIM(STR(m.i)) $ End_Indv.Endorse
					m.Renew	=CTOD(LEFT(DTOC(End_Indv.effective),6)+STR(YEAR(End_Indv.effective)+m.i,4))
				
					IF EMPTY(m.Renew)
						m.Renew	=(End_Indv.effective+(365 * m.i))
					ENDIF
				
					EXIT
				ENDIF
			NEXT
					
			REPLACE Poliind.Renew	WITH m.Renew
		ENDIF
	
	* Found & not active (negative End_Indv.Premium)
	*	or not found.
	CASE FOUND() .AND. End_Indv.Premium<0;
		.OR. !FOUND()
		IF !EMPTY(PoliInd.Renew)
			m.Renew	= {}
			REPLACE Poliind.Renew	WITH m.Renew
		ENDIF
	
	ENDCASE
	
	SET ORDER TO m.lcOrder
	SHOW GET m.Renew
	*--------------------------------------------------------------------------------*

			
	* If we've F8-Updated or (A)dded a new record in the front-end 
	*	and we're also currently pointing to that front-end record...
	IF ((m.llF8Update .OR. m.mMode="A") .AND. m.lnNewRecno = RECNO("PoliInd"))	
		* ... point to the newly added corresponding record in the back-end...
		GO (m.lnPremiumRecno)
		* ... and update it to reflect the Tax, Effective and Premium in the front-end's mVars
				REPLACE End_Indv.Tax	WITH m.Taxes,	;
					End_Indv.Effective	WITH m.Effective,	;
					End_Indv.Premium	WITH m.Prem	
	ENDIF
	
ENDIF

SELECT (m.lnSelect)
RETURN .T.
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpToLists
PRIVATE m.lnSelect
m.lnSelect	=SELECT()	
SELECT 0
USE PoliInd AGAIN ALIAS JumpTable
*SET STEP ON 
* Create the Arrays
m.lnPoliIDCnt	=JumpByPoliID()
m.lnLicenseCnt	=JumpByLicense()
m.lnSSN4Cnt		=JumpBySSN4()
m.lnEINCnt		=JumpByEIN()
USE IN JumpTable

SELECT Claims
m.lnClaimsCnt	=JumpToClaims()

SELECT (m.lnSelect)
SHOW GETS ONLY



*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpByPoliID
PRIVATE m.i, m.lcSetExact, m.lnRows
*m.lcSetExact	=SET("Exact")
*SET EXACT ON
SET ORDER TO POLIID	DESCENDING	&& POLIID in JumpTable

IF !EMPTY(PoliInd.PoliID) .AND. SEEK(PoliInd.PoliID)
	COUNT TO m.lnRows WHILE JumpTable.PoliID == PoliInd.PoliID
ELSE
	m.lnRows	=0
ENDIF

IF m.lnRows>0
	DIMENSION laPoliID[m.lnRows, 2]
	=SEEK(PoliInd.PoliID)
	
	FOR i=1 TO m.lnRows
		laPoliID[i,1]	=LEFT(ALLTRIM(JumpTable.PoliNumb)+IIF("DUPE"$JumpTable.PDAnum,"D",""), LEN(JumpTable.PoliNumb))
		laPoliID[i,2]	=RECNO()
		SKIP
	NEXT
	
ELSE
	DIMENSION laPoliID[1, 2]
ENDIF

*IF m.lcSetExact	="OFF"
*	SET EXACT OFF
*ENDIF

RETURN m.lnRows


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpByLicense
PRIVATE m.i, m.lcSetExact, m.lnRows
*m.lcSetExact	=SET("Exact")
*SET EXACT ON
SET ORDER TO STATELIC   DESCENDING	&& SUBSTR(POLINUMB,11,2)+LICENSE in JumpTable
IF !EMPTY(m.License) .AND. SEEK(SUBSTR(m.PoliNumb, 11, 2) +m.License)
	COUNT TO m.lnRows WHILE (SUBSTR(JumpTable.PoliNumb, 11, 2) +JumpTable.License) == (SUBSTR(m.PoliNumb, 11, 2) +m.License)
ELSE
	m.lnRows	=0
ENDIF
	
IF m.lnRows>0
	DIMENSION laLicense[m.lnRows, 2]
	=SEEK(SUBSTR(m.PoliNumb, 11, 2) +m.License)
	
	FOR i=1 TO m.lnRows
		laLicense[i,1]	=LEFT(ALLTRIM(JumpTable.PoliNumb)+IIF("DUPE"$JumpTable.PDAnum,"D",""), LEN(JumpTable.PoliNumb))
		laLicense[i,2]	=RECNO()
		SKIP
	NEXT
	
ELSE
	DIMENSION laLicense[1, 2]
ENDIF

*IF m.lcSetExact="OFF"
*	SET EXACT OFF
*ENDIF

RETURN m.lnRows
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpBySSN4
PRIVATE m.i, m.lcSetExact, m.lnRows
*m.lcSetExact	=SET("Exact")
*SET EXACT ON
SET ORDER to SSKEY   && CSSN4+LEFT(FIRSTNAME,2)+LEFT(LASTNAME,3)+SUBSTR(POLINUMB,11,2)+STR(10000-IIF(LEFT(POLINUMB,2)>"50",VAL("19"+LEFT(POLINUMB,2)),VAL("20"+LEFT(POLINUMB,2))))

*IF !EMPTY(m.cSSN4) .AND. SEEK(m.cSSN4 +LEFT(m.FirstName,2) +LEFT(m.LastName,3) +SUBSTR(m.polinumb,11,2))
IF !EMPTY(m.cSSN4) .AND. SEEK(m.cSSN4 +LEFT(m.FirstName,2))
*	COUNT TO m.lnRows WHILE JumpTable.cSSN4 +LEFT(JumpTable.FirstName,2) +LEFT(JumpTable.LastName,3) +SUBSTR(JumpTable.polinumb,11,2) == m.cSSN4 +LEFT(m.FirstName,2) +LEFT(m.LastName,3) +SUBSTR(m.polinumb,11,2)
	COUNT TO m.lnRows WHILE JumpTable.cSSN4 +LEFT(JumpTable.FirstName,2) == m.cSSN4 +LEFT(m.FirstName,2)
ELSE
	m.lnRows	=0
ENDIF

IF m.lnRows >0
	DIMENSION laSSN4[m.lnRows, 2]
*	=SEEK(m.cSSN4 +LEFT(m.FirstName,2) +LEFT(m.LastName,3) +SUBSTR(m.polinumb,11,2))
	=SEEK(m.cSSN4 +LEFT(m.FirstName,2))
	
	FOR i=1 TO m.lnRows
		laSSN4[i,1]	=LEFT(ALLTRIM(JumpTable.PoliNumb)+IIF("DUPE"$JumpTable.PDAnum,"D",""), LEN(JumpTable.PoliNumb))
		laSSN4[i,2]	=RECNO()
		SKIP
	NEXT
	
ELSE
	DIMENSION laSSN4[1, 2]	
ENDIF

*IF m.lcSetExact	="OFF"
*	SET EXACT OFF
*ENDIF

RETURN m.lnRows


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpByEIN
PRIVATE m.i, m.lcSetExact, m.lnRows
*m.lcSetExact	=SET("Exact")
*SET EXACT ON
SET ORDER TO EINkey   && cEIN+LEFT(LastName,3)+LEFT(Company,3)+SUBSTR(PoliNumb,11,2)+PoliNumb
	
*IF !EMPTY(m.cEIN) .AND. SEEK(m.cEIN +LEFT(m.Lastname,3) +LEFT(m.Company,3) +SUBSTR(m.PoliNumb,11,2))
IF !EMPTY(m.cEIN) .AND. SEEK(m.cEIN)
*	COUNT TO m.lnRows WHILE (JumpTable.cEIN +LEFT(JumpTable.Lastname,3) +LEFT(JumpTable.Company,3) +SUBSTR(JumpTable.PoliNumb,11,2)) == (m.cEIN +LEFT(m.Lastname,3) +LEFT(m.Company,3) +SUBSTR(m.PoliNumb,11,2))
	COUNT TO m.lnRows WHILE JumpTable.cEIN == m.cEIN 
ELSE
	m.lnRows	=0
ENDIF

IF m.lnRows >0
	DIMENSION laEIN[m.lnRows, 2]
*	=SEEK(m.cEIN +LEFT(m.Lastname,3) +LEFT(m.Company,3) +SUBSTR(m.PoliNumb,11,2))
	=SEEK(m.cEIN)
	
	FOR i=1 TO m.lnRows
		laEIN[i,1]	=LEFT(ALLTRIM(JumpTable.PoliNumb)+IIF("DUPE"$JumpTable.PDAnum,"D",""), LEN(JumpTable.PoliNumb))
		laEIN[i,2]	=RECNO()
		SKIP
	NEXT
	
ELSE
	DIMENSION laEIN[1, 2]	
ENDIF

*IF m.lcSetExact	="OFF"
*	SET EXACT OFF
*ENDIF

RETURN m.lnRows


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpToClaims
PRIVATE m.i, m.lcSetExact, m.lnRows
*m.lcSetExact=SET("Exact")
*SET EXACT ON
m.lnRows	=0	

IF !EMPTY(m.PoliID)	
	IF SEEK(m.PoliID)
		COUNT TO m.lnRows WHILE Claims.PoliID ==m.PoliID
	ENDIF	
ENDIF

IF m.lnRows >0
	DIMENSION laClaims[m.lnRows, 2]
	=SEEK(m.PoliID)
	
	FOR i=1 TO m.lnRows
		laClaims[i,1]	=Claims.ClaimNumb
		laClaims[i,2]	=RECNO()
		SKIP
	NEXT
	
ELSE
	DIMENSION laClaims[1, 2]	
ENDIF

*IF m.lcSetExact	="OFF"
*	SET EXACT OFF
*ENDIF

RETURN m.lnRows


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpThisPoliID
PARAMETER xnPoliID
*PARAMETER jcPoliID

*O (laPoliID[ASUBSCRIPT(laPoliID, ASCAN(laPoliID, jcPoliID), 1), 2])
*GO (laPoliID[ASUBSCRIPT(laPoliID, ASCAN(laPoliID, xnPoliID), 1), 2])
GO (laPoliID[xnPoliID,2])
SCATTER MEMVAR MEMO
cJumpPoliID	=PoliInd.Polinumb
cJumpLicense=PoliInd.License
cJumpSSN4	=PoliInd.cSSN4+PoliInd.License
cJumpEIN	=PoliInd.cEIN+PoliInd.License
m.cbKeyOriginalRec	=PoliInd.cbKey
SHOW GETS ONLY
@03+m.lnViewRow,83+m.lnViewCol	SAY GetOfferAs(m.PoliNumb)	COLOR RGB(255,0,0)

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpThisLicense
PARAMETER xnLicense
*O (laLicense[ASUBSCRIPT(laLicense, ASCAN(laLicense, jcLicense), 1), 2])
GO (laLicense[xnLicense,2])
SCATTER MEMVAR MEMO
cJumpPoliID	=PoliInd.Polinumb
cJumpLicense=PoliInd.License
cJumpSSN4	=PoliInd.cSSN4+PoliInd.License
cJumpEIN	=PoliInd.cEIN+PoliInd.License
m.cbKeyOriginalRec	=PoliInd.cbKey
SHOW GETS ONLY
@03+m.lnViewRow,83+m.lnViewCol	SAY GetOfferAs(m.PoliNumb)	COLOR RGB(255,0,0)

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpThisSSN4
PARAMETER xnSSN4
PRIVATE m.lnRecno, m.lnOriginRec
m.lnOriginRec	=RECNO()

*GO (laSSN4[ASUBSCRIPT(laSSN4, ASCAN(laSSN4, jcSSN4), 1), 2])
GO (laSSN4[xnSSN4,2])

DO FORM forms\sskey.scx WITH (m.cSSN4 +LEFT(m.Firstname,2) +LEFT(m.Lastname,3) +SUBSTR(m.polinumb,11,2)) TO m.lnRecno

IF m.lnRecno >0
	GO (m.lnRecno)
	SCATTER MEMVAR MEMO
	cJumpPoliID	=PoliInd.Polinumb
	cJumpLicense=PoliInd.License
	cJumpSSN4	=PoliInd.cSSN4+PoliInd.License
	cJumpEIN	=PoliInd.cEIN+PoliInd.License
	SHOW GETS ONLY
	@03+m.lnViewRow,83+m.lnViewCol	SAY GetOfferAs(m.PoliNumb)	COLOR RGB(255,0,0)
ELSE
	GO (m.lnOriginRec)
ENDIF

m.cbKeyOriginalRec	=PoliInd.cbKey

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpThisEIN
PARAMETER jcEIN

*GO (laEIN[ASUBSCRIPT(laEIN, ASCAN(laEIN, jcEIN), 1), 2])

DO FORM forms\einkey.scx WITH (m.cEIN +LEFT(m.Lastname,3)+LEFT(m.Company,3)+SUBSTR(m.polinumb,11,2)) TO m.lnRecno

IF m.lnRecno >0
	GO (m.lnRecno)
	SCATTER MEMVAR MEMO
	cJumpPoliID	=PoliInd.Polinumb
	cJumpLicense=PoliInd.License
	cJumpSSN4	=PoliInd.cSSN4+PoliInd.License
	cJumpEIN	=PoliInd.cEIN+PoliInd.License
	m.cbKeyOriginalRec	=PoliInd.cbKey
	
	SHOW GETS ONLY
	@03+m.lnViewRow,83+m.lnViewCol	SAY GetOfferAs(m.PoliNumb)	COLOR RGB(255,0,0)
ENDIF

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION JumpThisClaim
PARAMETER jcClaim
PRIVATE m.lnSelect

* Only do this if there was actually an item (w/recno) in the Claims list
IF (laClaims[ASUBSCRIPT(laClaims, ASCAN(laClaims, jcClaim), 1), 2]) >0
	m.lnSelect	=SELECT()
	
	SELECT Claims
	GO (laClaims[ASUBSCRIPT(laClaims, ASCAN(laClaims, jcClaim), 1), 2])
	DO Screens\PoliClai.spr
	
	SELECT (m.lnSelect)
ENDIF


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION SetJumpPoliID
cJumpPoliID	=PoliInd.Polinumb

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION SetJumpLicense
cJumpLicense=PoliInd.License

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION SetJumpSSN4
cJumpSSN4	=PoliInd.cSSN4

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION SetJumpEIN
cJumpEIN	=PoliInd.cEIN

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ToEndorsements
PRIVATE m.lnCurObj
m.lnCurObj	=_CUROBJ
		
IF LASTKEY()=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

IF m.mMode$'AC'
	ON KEY LABEL F4
	ON KEY LABEL F12
	ON KEY LABEL F11
ENDIF

*SET STEP ON 

DO programs\endorse2 WITH "POLIIND"
SHOW GET m.Renew
REPLACE PoliInd.Renew	WITH m.Renew
SHOW GET m.PDANum

IF m.mMode$'AC'
	ON KEY LABEL F4	 l=F4ToNotes()	&&	_CUROBJ=OBJNUM(m.notes)
	ON KEY LABEL F12 l=ToEndorsements()
	ON KEY LABEL F11 l=ToPoliTask()
ENDIF

*_CUROBJ=m.lnCurObj
_CUROBJ=1

RETURN -1
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ToPoliExt
PRIVATE m.lnCurObj, m.lnSelect, m.lwPoliExt, m.lcOrder
m.lnSelect	=SELECT()
SELECT PoliExt
m.lnCurObj	=_CUROBJ
m.lcOrder	=ORDER()

IF LASTKEY()=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

IF m.mMode$'AC'
	ON KEY LABEL F4
	ON KEY LABEL F12
	ON KEY LABEL F11
ENDIF
		
*DEFINE WINDOW lwPoliExt FROM 7,15 TO 13,165 SYSTEM SHADOW COLOR SCHEME 8 FLOAT
DEFINE WINDOW lwPoliExt IN SCREEN FROM 7,1 TO 13,WCOLS()-1 SYSTEM SHADOW COLOR SCHEME 8 FLOAT

SET ORDER TO CBPOLIIND   && CBPOLIIND
=SEEK(PoliInd.cbKey)
SET KEY TO PoliInd.cbkey
BROWSE WINDOW lwPoliExt TITLE "Extended Data"	;
FIELDS cUsername:8:H="User",	;
	cPolicyTyp:10:H="PolicyType", ;
	cConfLic:23:H="CONFORMITY License(s)", ;
	cImpfile:33:H="Import file", ;
	cImpfileDt:21:H="Date/Time", ;
	cPayeename:30:H="Payee", ;
	cWebuser:30:H="Web user", ;
	cApprTrain:25:H="Appraiser Trainee(s)", ;
	cApprLic:25:H="Appraiser License(s)", ;
	cPmtID:12:H="PaymentID", ;
	cPBname:H="Primary Broker", ;
	cPBaddress:H="PB address", ;
	cPBcity:H="PB city", ;
	cPBstate:H="PB St", ;
	cPBzip:H="PB zip",	;
	lCommerc:H="Commercial(75%+ sales?)",	;
	lClm5yr:H="ClaimHx(clm <5 yrs?)",	;
	lCurrClm:H="ClaimCurr(have curr clm?)" NOEDIT

SET KEY TO 
RELEASE WINDOWS lwPoliExt
SET ORDER TO (m.lcOrder)

SELECT (m.lnSelect)

IF m.mMode$'AC'
	ON KEY LABEL F4	 l=F4ToNotes()	&&	_CUROBJ=OBJNUM(m.notes)
	ON KEY LABEL F12 l=ToEndorsements()
	ON KEY LABEL F11 l=ToPoliTask()
ENDIF

_CUROBJ=m.lnCurObj
RETURN -1


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ToLicense
PRIVATE m.lnCurObj, m.lcLicenseCnt
m.lnCurObj	=_CUROBJ

IF LASTKEY()=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

IF m.mMode$'AC'
	ON KEY LABEL F4
	ON KEY LABEL F12
	ON KEY LABEL F11
ENDIF

DO programs\polilic.prg
KEYBOARD '{LEFTARROW}'

IF m.mMode$'AC'
	ON KEY LABEL F4	 l=F4ToNotes()	&&	_CUROBJ=OBJNUM(m.notes)
	ON KEY LABEL F12 l=ToEndorsements()
	ON KEY LABEL F11 l=ToPoliTask()
ENDIF

m.lcLicenseCnt	=LicenseCnt(m.polinumb, m.poliid)
SHOW GET m.lnLicense,1 PROMPT 'Licenses' +m.lcLicenseCnt

_CUROBJ=m.lnCurObj
RETURN .T.

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ToEmail
PRIVATE m.lnCurObj, m.lcEmailCnt
m.lnCurObj	=_CUROBJ

IF LASTKEY()=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

IF m.mMode$'AC'
	ON KEY LABEL F4
	ON KEY LABEL F12
	ON KEY LABEL F11
ENDIF

DO programs\poliemail.prg
KEYBOARD '{LEFTARROW}'

IF m.mMode$'AC'
	ON KEY LABEL F4	 l=F4ToNotes()	&&	_CUROBJ=OBJNUM(m.notes)
	ON KEY LABEL F12 l=ToEndorsements()
	ON KEY LABEL F11 l=ToPoliTask()
ENDIF

m.lcEmailCnt	=EmailCnt(m.PoliID)
SHOW GET m.lnEmail,1 PROMPT 'Email+' +m.lcEmailCnt

_CUROBJ=m.lnCurObj
RETURN .T.

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ToPoliTask
PRIVATE m.lnCurObj, m.lcPoliTaskCnt
m.lnCurObj	=_CUROBJ

IF LASTKEY()=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

IF m.mMode$'AC'
	ON KEY LABEL F4
	ON KEY LABEL F12
	ON KEY LABEL F11
ENDIF

DO programs\PoliTk26.spr	WITH "POLISCR3"
KEYBOARD '{LEFTARROW}'

IF m.mMode$'AC'
	ON KEY LABEL F4	 l=F4ToNotes()	&&	_CUROBJ=OBJNUM(m.notes)
	ON KEY LABEL F12 l=ToEndorsements()
	ON KEY LABEL F11 l=ToPoliTask()
ENDIF

SHOW GET m.lnPoliTask,1 

_CUROBJ=m.lnCurObj
RETURN .T.




*---------------------------------------------------------------------------------------------------------------------*
FUNCTION PoliIDNew
PRIVATE m.lnSelect, m.lcPoliID
m.lnSelect	=SELECT()

SELECT 0
USE PoliID

DO WHILE .T.

	IF RLOCK()
		REPLACE PoliID.PoliID WITH PADL(ALLTRIM(STR(VAL(PoliID.PoliID) +1)), 8, "0")
		m.lcPoliID	=PoliID.PoliID
		UNLOCK
		WAIT CLEAR
		EXIT
	ENDIF
	
	WAIT WINDOW NOWAIT "Waiting for a new PoliID#..."
ENDDO

USE IN PoliID

SELECT (m.lnSelect)
RETURN m.lcPoliID	


*---------------------------------------------------------------------------------------------------------------------*
* Type (m.tipe) validations
*	Documented in Y:\SHARED\POLICY\Overview Information\Policy Information_files\REFERENCE FILES\LicenseTypes.htm
FUNCTION TipeChk
PRIVATE m.llChkTipe
m.llChkTipe	=.T.	&&	default to TRUE

DO CASE
* Alabama
CASE SUBSTR(m.polinumb,11,2)="AL"
	&&	anything goes

* Alaska
CASE SUBSTR(m.polinumb,11,2)="AK"
	m.llChkTipe = (EMPTY(m.tipe) .OR.  ALLTRIM(m.tipe)$"RECA,RECB,RECS")

	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Alaska are: RECA, RECB, RECS or [spaces]"
	ENDIF

* Colorado
CASE SUBSTR(m.polinumb,11,2)="CO" 
	IF SUBSTR(m.polinumb,4,2)="EO"
		m.llChkTipe	=(EMPTY(m.Tipe) .OR. (LEFT((ALLTRIM(m.tipe)+"***"), 2)$"EA,EC,EI,EL,ER,EP,FA,IC,IA,IL,II,IP,IR,SP"))
		
		IF !m.llChkTipe
			WAIT WINDOW "Valid types for Colorado EO are: [Individuals]=EA,EI,ER,FA,IA,II,IR,SP  [Firms]=EC,EL,EP,IC,IL,IP  or [spaces]"
		ENDIF
	ENDIF
	
	IF SUBSTR(m.polinumb,4,2)="AP"
		m.llChkTipe	=(EMPTY(m.Tipe) .OR. (LEFT((ALLTRIM(m.tipe)+"***"), 2)$"AL AR CG CR"))
		
		IF !m.llChkTipe
			WAIT WINDOW "Valid types for Colorado AP are: AL,AR,CG,CR  or [spaces]"
		ENDIF
	ENDIF
	
	IF SUBSTR(m.polinumb,4,2)="ML"
		m.llChkTipe	=(EMPTY(m.Tipe) .OR. (LEFT((ALLTRIM(m.tipe)+"***"), 2)$"ML"))
		
		IF !m.llChkTipe
			WAIT WINDOW "Valid type for Colorado ML is: MLO or [spaces]"
		ENDIF
	ENDIF
	
* Idaho
CASE SUBSTR(m.polinumb,11,2)="ID"
	m.llChkTipe	=(EMPTY(m.tipe) .OR. LEFT((ALLTRIM(m.tipe)+"***"), 2)$"DB SP BR AB LB MA MS CO LC LP")
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Idaho are: [Individuals]=DB,SP,BR,AB,LB,MA,MS  [Firms]=CO,LC,LP  or [spaces]"
	ENDIF

* Iowa
CASE SUBSTR(m.polinumb,11,2)="IA"
	m.llChkTipe	=(EMPTY(m.Tipe) .OR. ALLTRIM(m.tipe)$"SBF") 
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Iowa are: [Individuals] S, B  [Firms] F, or [space]"
	ENDIF

* Kentucky
CASE SUBSTR(m.polinumb,11,2)="KY"
	m.llChkTipe	=(EMPTY(m.Tipe) .OR. ALLTRIM(m.tipe)$"SB") 
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Iowa are: S, B, or [space]"
	ENDIF
	
* Louisiana
CASE SUBSTR(m.polinumb,11,2)="LA"
	m.llChkTipe	=(EMPTY(m.Tipe) .OR. ALLTRIM(m.tipe)$"S,B,F,CGA,CRA,TRA") 

	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Louisianna are: [Individuals] S, B, CRA, CGA, TRA  [Firms] F, or [spaces]"
	ENDIF

* Mississippi
CASE SUBSTR(m.polinumb,11,2)="MS"
	m.llChkTipe	=(EMPTY(m.tipe) .OR. ALLTRIM(m.tipe)$"BCSF")
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Mississippi are: [Individuals] B, S  [Firms] C, F, or [space]"
	ENDIF

* Montana
CASE SUBSTR(m.polinumb,11,2)="MT"
	m.llChkTipe	=(EMPTY(m.tipe) .OR. ALLTRIM(m.tipe)$"RRE-BRO-LIC,RRE-RBS-LIC,RRE-RPM-LIC")
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Montana are: RRE-BRO-LIC, RRE-RBS-LIC, RRE-RPM-LIC, or [space]"
	ENDIF
	
* Nebraska
CASE SUBSTR(m.polinumb,11,2)="NE"
	* Anything goes
*	m.llChkTipe	=(EMPTY(m.Tipe))

* New Mexico
CASE SUBSTR(m.polinumb,11,2)="NM"
	m.llChkTipe	=(EMPTY(m.Tipe) .OR. ALLTRIM(m.tipe)$"AB,B,QB,REC-") 
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for New Mexico are: AB, B, QB, REC- or [space]"
	ENDIF
	
* North Dakota
CASE SUBSTR(m.polinumb,11,2)="ND"
	m.llChkTipe	=(EMPTY(m.tipe) .OR. ALLTRIM(m.tipe)$"BS")
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for North Dakota are: B, S, or [space]"
	ENDIF

* Rhode Island
CASE SUBSTR(m.polinumb,11,2)="RI"
	m.llChkTipe	=(EMPTY(m.Tipe) .OR. ALLTRIM(m.tipe)$"B,S") 
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Rhode Island are: S, B, or [space]"
	ENDIF

* South Dakota
CASE SUBSTR(m.polinumb,11,2)="SD"
	m.llChkTipe	=(EMPTY(m.tipe) .OR. ALLTRIM(m.tipe)$"AUC,BA,BRO,PM,RRA,SP")
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for South Dakota are: AUC, BA, BRO, PM, RRA, SP, or [space]"
	ENDIF


* Tennessee
CASE SUBSTR(m.polinumb,11,2)="TN"
	m.llChkTipe	=(EMPTY(m.tipe) .OR. ALLTRIM(m.tipe)$"AF,B,TS")
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Tennessee are: AF, B, TS, or [space]"
	ENDIF
	
	
* Wyoming
CASE SUBSTR(m.polinumb,11,2)="WY"

	m.llChkTipe =(EMPTY(m.tipe) .OR. ALLTRIM(m.Tipe)$"AB,AB RE-,RB,RB RE-,S,S RE-")
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types for Wyoming are: AB, AB RE-, RB, RB RE-, S, S RE-, or [space]"
	ENDIF
	

OTHERWISE
	m.llChkTipe	=(EMPTY(m.Tipe) .OR. ALLTRIM(m.tipe)$"ABCLMPRSTU") 
	
	IF !m.llChkTipe
		WAIT WINDOW "Valid types are: ABCFLMPRSTU or [space]"
	ENDIF
	
ENDCASE

RETURN m.llChkTipe
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION TipeSetVars

DO CASE
* Alabama
CASE SUBSTR(m.polinumb,11,2)="AL"
	m.tipe	=LEFT(m.tipe,1)		

* Alaska
CASE SUBSTR(m.polinumb,11,2)="AK"
	m.tipe	=LEFT(m.tipe,4)

* Colorado 
CASE SUBSTR(m.polinumb, 11, 2)="CO" 
	* (Brokers, Appraisers)
	IF SUBSTR(m.polinumb, 5, 2)$"EO,AP"
		m.tipe	=LEFT(m.tipe, 2)		
	ENDIF
	* (Mortgage Loan Originator)
	IF SUBSTR(m.polinumb, 5, 2)$"ML"
		m.tipe	=LEFT(m.tipe, 3)		
	ENDIF
	
* Idano
CASE SUBSTR(m.polinumb, 11, 2)="ID"
	m.tipe	=LEFT(m.tipe, 2)
	
* Iowa
CASE SUBSTR(m.polinumb, 11, 2)="IA"
	m.tipe	=LEFT(m.tipe, 1)		
			
* Kentucky
CASE SUBSTR(m.polinumb, 11, 2)="KY"
	m.tipe	=LEFT(m.tipe, 1)		

* Louisiana
CASE SUBSTR(m.polinumb, 11, 2)="LA"
	m.tipe	=LEFT(m.tipe, 3)		

* Mississippi
CASE SUBSTR(m.polinumb, 11, 2)="MS"
	m.tipe	=LEFT(m.tipe, 1)

* Montana
CASE SUBSTR(m.polinumb, 11, 2)="MT"
	m.tipe	=LEFT(m.tipe+SPACE(LEN(poliind.Tipe)), LEN(poliind.Tipe))
	
* Nebraska
CASE SUBSTR(m.polinumb, 11, 2)="NE"
	m.tipe	=LEFT(m.tipe, 1)
	SHOW GET m.tipe disabled
	
* New Mexico
CASE SUBSTR(m.polinumb, 11, 2)="NM"
	m.tipe	=LEFT(m.tipe, 4)

* North Dakota
CASE SUBSTR(m.polinumb, 11, 2)="ND"
	m.tipe	=LEFT(m.tipe, 1)

* Rhode Island
CASE SUBSTR(m.polinumb, 11, 2)="RI"
	m.tipe	=LEFT(m.tipe, 1)

* South Dakota
CASE SUBSTR(m.polinumb, 11, 2)="SD"
	m.tipe	=LEFT(m.tipe, 3)

* Idaho
CASE SUBSTR(m.polinumb, 11, 2)="ID"
	m.tipe	=LEFT(m.tipe,2)	

* Tennessee
CASE SUBSTR(m.polinumb, 11, 2)="TN"
	m.tipe	=LEFT(m.tipe, 2)
	
* Wyoming
CASE SUBSTR(m.polinumb, 11, 2)="WY"
	m.tipe	=LEFT(m.tipe, 6)

OTHERWISE
	m.tipe	=LEFT(m.tipe, 1)
		
ENDCASE

SHOW GET m.Tipe

RETURN .T.
	

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION GetPremiumRecno			
PRIVATE m.lnSelect, m.lnPremiumRecno
m.lnSelect			=SELECT()
m.lnPremiumRecno	=0	&&	default to Zero

SELECT End_Indv
=SEEK(PoliInd.PoliNumb +PoliInd.PoliID)

SCAN WHILE (End_Indv.PoliNumb +End_Indv.PoliID) == (PoliInd.PoliNumb +PoliInd.PoliID)
*	IF "PREMIUM"$ALLTRIM(UPPER(End_Indv.Endorse))	
	IF LEFT(ALLTRIM(UPPER(End_Indv.Endorse)),7) ="PREMIUM"	;
		.AND. End_Indv.Effective =PoliInd.Effective	;
		.AND. End_Indv.Premium =PoliInd.Prem	
*		.AND. End_Indv.nDispOrdr=1
		m.lnPremiumRecno	=RECNO()
		EXIT
	ENDIF
ENDSCAN

SELECT (m.lnSelect)
RETURN m.lnPremiumRecno
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ChkEffRange
PARAMETERS m.xcPoliNumb, m.xdEffective
PRIVATE m.lnSelect, m.llReturn

m.lnSelect	=SELECT()

SELECT ChartA
SET ORDER TO POLINUMB   && POLINUMB
m.llFound	=SEEK(m.xcPoliNumb, "ChartA")
m.llDateGood=(m.llFound .AND. BETWEEN(m.xdEffective, ChartA.Effective, ChartA.End))

SELECT (m.lnSelect)

DO CASE
CASE !m.llFound
	m.lcValidMsg	="Policy# " +ALLTRIM(m.xcPoliNumb) +" not found! (ChartA.DBF)"
CASE !m.llDateGood
	m.lcValidMsg	="Invalid Effective Date!" +CHR(13)+"Must be between " +DTOC(ChartA.Effective) +" and "+DTOC(ChartA.End) +"."
OTHERWISE
	m.lcValidMsg	=""
ENDCASE

RETURN (m.llFound .AND. m.llDateGood)
		
		
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ChkEffPrem
PARAMETERS m.xcPoliNumb, m.jlSetPrem
PRIVATE m.lnSelect, m.llChartC, m.lnPremium, m.lnMonth
m.lnSelect	=SELECT()


IF TYPE("m.jlSetPrem")='C'
	m.jlSetPrem	=.T.
ENDIF

m.llChartC	=USED("ChartC")

IF !m.llChartC
	SELECT 0
	USE ChartC 
ELSE
	PRIVATE m.lcChartCOrder, m.lnChartCRecno
	SELECT ChartC
	m.lcChartCOrder	=ORDER()
	m.lnChartCRecno	=RECNO()
ENDIF

IF TYPE("m.Endorse")#'C'
*	m.Endorse	=PADR("PREMIUM", LEN(ChartC.Endorse), " ")
	IF SEEK(m.polinumb +m.poliid,"End_indv","Endorse") .AND. m.lnPremiumRecno>0
		SELECT End_Indv
		GO (m.lnPremiumRecno)
		m.Endorse	=End_Indv.Endorse
	ELSE
		SELECT ChartB
		SET ORDER TO POLIEND   && POLINUMB+ENDORSE	
		m.Endorse	=IIF(SEEK(m.PoliNumb+"PREMIUM"), ChartB.Endorse, "PREMIUM?")
	ENDIF
	
	SELECT ChartC
ENDIF

SET ORDER TO POLIEND   && POLINUMB+ENDORSE+DTOS(EFFECTIVE)

m.llGotIt	=SEEK(m.PoliNumb) 

IF m.llGotIt
	* If this is Rhode Island and the Effective's month is May, and the year of the PoliNumb is the year of the Effective
	IF "RI"$m.PoliNumb .AND. MONTH(m.Effective)=5 .AND. (LEFT(m.polinumb,2)=RIGHT(STR(YEAR(m.effective)), 2))
		m.lnMonth=4
	ELSE
		m.lnMonth=MONTH(m.Effective)
	ENDIF

	LOCATE REST WHILE ChartC.PoliNumb =m.PoliNumb	;
		FOR (ALLTRIM(m.Endorse)$ChartC.Endorse)	;
		.AND. m.lnMonth=MONTH(ChartC.Effective);
		.AND. YEAR(m.Effective)=YEAR(ChartC.effective)	;
		.OR.	;
			((m.Effective=ChartC.Effective) .AND. (ALLTRIM(m.Endorse)=ALLTRIM(ChartC.Endorse)))
	m.llGotIt	=FOUND()
ENDIF

IF m.llGotIt
	m.lnPremium	=ChartC.Amount
ELSE
*	GO RECNO(0)
	SKIP -1
	
*	IF (m.PoliNumb +m.Endorse) =(ChartC.PoliNumb +ChartC.Endorse)
	IF (m.PoliNumb =ChartC.PoliNumb) .AND. (ALLTRIM(m.Endorse)$ChartC.Endorse)
		m.lnPremium	=ChartC.Amount
	ELSE
		m.lnPremium	=0.00
	ENDIF
	
ENDIF

IF m.jlSetPrem .AND. m.lnPremium>0 .AND. EMPTY(m.prem)
	m.Prem	=m.lnPremium
	SHOW GET m.Prem
ELSE

	DO CASE
	* If found Premium is not zero, and entered Premium doesn't match it.
	CASE m.lnPremium >0 .AND. m.Prem # m.lnPremium
		WAIT WINDOW NOWAIT "Premium should be $"+ALLTRIM(STR(m.lnPremium, 7, 2)) +" when Effective Date is " +DTOC(m.Effective)
		
		IF AreYouSure("Change Premium to $"+ALLTRIM(STR(m.lnPremium))+"?", 19)
			m.Prem	=m.lnPremium
			SHOW GET m.Prem
			
			IF (m.llF8Update .OR. m.mMode="A")
				x=KYSurchgCalc() .AND. BackEndUpdt()
			ENDIF
			
			IF (m.llTaxes .AND. (m.mMode="A" .OR. m.llF8Update))
				x=CalcCityAmt() .AND. CalcCountyAmt() .AND. TotalTaxes() .AND. BackEndTaxes()
			ENDIF
			
		ENDIF
	
	* No m.Effective entered
	CASE EMPTY(m.Effective) .AND. SEEK(m.PoliNumb +m.Endorse)
		IF !EMPTY(m.Prem)
			LOCATE REST WHILE (ChartC.PoliNumb +ChartC.Endorse) = (m.PoliNumb +m.Endorse)	;
				FOR ChartC.Amount =m.Prem
				
			IF FOUND()
				m.Effecive	=ChartC.Effective
				SHOW GET m.Effective
			ENDIF
			
			WAIT WINDOW "Please enter a different Premium amount and an Effective Date!"
		ELSE
			WAIT WINDOW "Enter a Premium amount!"
		ENDIF
		
	CASE !SEEK(m.PoliNumb +m.Endorse)
		WAIT WINDOW ALLTRIM(m.PoliNumb)+ALLTRIM(m.Endorse) +" not found in Premium Chart!"
	
	ENDCASE
	
ENDIF

* Restore ChartC status
IF !m.llChartC
	USE IN ChartC
ELSE
	IF ORDER() # m.lcChartCOrder
		SET ORDER TO (m.lcChartCOrder)
		IF m.lnChartCRecno>0
			GO (m.lnChartCRecno)
		ENDIF
	ENDIF
ENDIF

SELECT (m.lnSelect)
RETURN .T.
	


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION F4ToNotes
_CUROBJ=OBJNUM(m.notes)
KEYBOARD '{CTRL+HOME}'
	

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION MsgCurrent
WAIT WINDOW TIMEOUT 5 "No F8 -- this is a current policy."

	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION PoliScrnError
PARAMETERS m.lnERROR, m.lcMESSAGE, m.lnLINENO, m.lcPROGRAM
DO CASE
CASE m.lnError=108
		
	DO CASE
	CASE m.mMode="A"
		WAIT WINDOW TIMEOUT 5 "Attempting to add new record..."
	CASE m.mMode="C"
		WAIT WINDOW TIMEOUT 5 "Attempting to add new record for "+ALLTRIM(PoliInd.FirstName) +" "+ALLTRIM(PoliInd.LastName)
	ENDCASE
	
	RETRY
	
OTHERWISE
	IF "POLISCRP"$m.lcMessage
	ELSE
		WAIT WINDOW ALLTRIM(STR(m.lnERROR)) +": " +m.lcMESSAGE +".  Line:"+ALLTRIM(STR(m.lnLINENO)) +", " +m.lcPROGRAM
	ENDIF
	
ENDCASE
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION SayIt
PARAMETERS m.jcStr, m.jnRow, m.jnCol, m.jcRGBColor
IF !EMPTY(m.jcRGBColor)
	@m.jnRow,m.jnCol SAY m.jcStr	COLOR RGB(&jcRGBColor)
ELSE
	@m.jnRow,m.jnCol SAY m.jcStr
ENDIF
RETURN .T.
	
	
*---------------------------------------------------------------------------------------------------------------------*
* ??? In use no more?
*FUNCTION GetTax_Auth
*PARAMETERS m.jcTax_Auth, m.jcState
*PRIVATE m.lnSelect, m.llGetTax_Auth
*m.lnSelect		=SELECT()
*m.jcTax_Auth	=ALLTRIM(STRTRAN(m.jcTax_Auth, ", "+m.jcState))
*
*SELECT Tax
*m.llGetTax_Auth=SEEK(m.jcState +m.jcTax_Auth)
*m.lnTax_Rate	=Tax.Tax_Rate
*
*SELECT (m.lnSelect)
*
*IF m.llGetTax_Auth 
*	=SayIt(STR(m.lnTax_Rate*100,5,2)+"%", 08+m.lnViewRow, 44+m.lnViewCol, "0,225,0,255,255,255")
*ELSE
*	=SayIt(" ?.??%", 08+m.lnViewRow, 44+m.lnViewCol, "225,0,0,255,255,255")
*ENDIF
*
*RETURN .T.
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION PoliIndAudit
PARAMETERS m.xcCase
PRIVATE m.llReturn, m.lnSelect, m.lcIndent
m.llReturn	=.T.		&&	default to TRUE
m.lcIndent	=CHR(16) +" "

DO CASE
* Open audit table, if not already, then add a record with defaulted input.
*	Record remains available for input by other CASEs.
CASE m.xcCase="ON" .AND. !USED("PoliIndA")
	USE PoliIndA IN 0	&&	Table is the Audit Trail companion of PoliInd.DBF
	m.dIn		=DATE()
	m.cTimeIn	=TIME()
	m.cUser		=mstafcode
	INSERT INTO PoliIndA FROM MEMVAR
	
	USE PoliIndX IN 0	ORDER Field_Name 	&&	Table contains extended structure of PoliInd.DBF with a TAG on Field_Name.
											&&	Usage includes discovering Numeric data length and decimal values.

* There has been an new record bebug via F8Update 
*	Provide a new audit record
CASE m.xcCase="NEW"
	m.dIn		=DATE()
	m.cTimeIn	=TIME()
	m.cUser		=mstafcode
	INSERT INTO PoliIndA FROM MEMVAR
	REPLACE PoliIndA.mEdit	WITH DTOC(DATE())+[ ]+TIME() +[- ] +ALLTRIM(mstafcode) +[ added via F8Update.] +CHR(13) +ALLTRIM(PoliIndA.mEdit)
		
		
* Close audit table after inputting OUT fields
CASE m.xcCase="OFF"
	REPLACE PoliIndA.dOut		WITH DATE()
	REPLACE PoliIndA.cTimeOut	WITH TIME()
	USE IN PoliIndA
	USE IN PoliIndX


* Write PoliInd editing to mEdit
CASE m.xcCase="PoliInd"

	IF m.mMode="A"
		REPLACE PoliIndA.mEdit	WITH DTOC(DATE())+[ ]+TIME() +[- ] +ALLTRIM(mstafcode) +[ added.] +CHR(13) +ALLTRIM(PoliIndA.mEdit)
	ELSE
		* Scan structure, build string of changed fields
		m.lnSelect	=SELECT()
		
		SELECT PoliInd
		m.lnFCount	=FCOUNT()
		m.llChanged	=.F.		&&	default to FALSE
		m.lcHeadString	=DTOC(DATE())+[ @ ]+TIME() +[- ] +ALLTRIM(mstafcode) +[ changed]
		m.lcItemString	=""		&&	default to [blank]
		
		FOR i=1 TO m.lnFCount
		
			IF EVALUATE(FIELD(i)) #EVALUATE([m.]+FIELD(i))
				DO CASE
				* Character
				CASE TYPE(FIELD(i))='C'
					m.lcItemString	=LOWER(FIELD(i)) +[: ] +ALLTRIM(EVALUATE(FIELD(i))) +[ (to) ] +ALLTRIM(EVALUATE([m.]+FIELD(i)))
					
				* Numeric
				CASE TYPE(FIELD(i))='N'
					IF SEEK(FIELD(i), "PoliIndX", "Field_Name")
						m.lcItemString	=LOWER(FIELD(i)) +[: ] +ALLTRIM(STR(EVALUATE(FIELD(i)), PoliIndX.Field_Len, PoliIndX.Field_Dec)) +[ (to) ] +ALLTRIM(STR(EVALUATE([m.]+FIELD(i)), PoliIndX.Field_Len, PoliIndX.Field_Dec))
					ELSE					
						m.lcItemString	=LOWER(FIELD(i)) +[: <unknown field name>]
					ENDIF
					
				* Logical
				CASE TYPE(FIELD(i))='L'
					m.lcItemString	=LOWER(FIELD(i)) +[: ] +IIF(EVALUATE(FIELD(i)), ".T.", ".F.") +[ (to) ] +IIF(EVALUATE([m.]+FIELD(i)), ".T.", ".F.")
					
				* Date
				CASE TYPE(FIELD(i))='D'
					m.lcItemString	=LOWER(FIELD(i)) +[: ] +DTOC(EVALUATE(FIELD(i))) +[ (to) ] +DTOC(EVALUATE([m.]+FIELD(i)))
					
				* Memo
				CASE TYPE(FIELD(i))='M'
					* LENC()
					m.lcItemString	=LOWER(FIELD(i)) +[: ] +ALLTRIM(STR(MEMLINES(EVALUATE(FIELD(i))))) +[ (to) ] +ALLTRIM(STR(MEMLINES(EVALUATE([m.]+FIELD(i))))) +[ lines.]
					
				ENDCASE
				
				IF !m.llChanged
					m.llChanged	=.T.
					REPLACE PoliIndA.mEdit	WITH ALLTRIM(PoliIndA.mEdit) +m.lcHeadString +CHR(13)
				ENDIF
				
				REPLACE PoliIndA.mEdit	WITH ALLTRIM(PoliIndA.mEdit) +m.lcIndent +m.lcItemString +CHR(13)
			ENDIF
			
		NEXT
		
		IF !m.llChanged
			REPLACE PoliIndA.mEdit	WITH ALLTRIM(PoliIndA.mEdit) +m.lcHeadString +[ nothing.] +CHR(13)
		ENDIF
		
	ENDIF
		
		
* Write End_Indv editing to mEdit
CASE m.xcCase="End_Indv"
	m.lnSelect	=SELECT()
	SELECT PoliIndA
		
	REPLACE PoliIndA.mEdit	WITH ALLTRIM(PoliIndA.mEdit) +DTOC(DATE())+[ @ ]+TIME() +[- ] +ALLTRIM(mstafcode) +[ added] +CHR(13) ;
								+m.lcIndent +[insured: ] +ALLTRIM(End_indv.Insured) +CHR(13)	;
								+m.lcIndent +[endorsement: ] +ALLTRIM(End_Indv.Endorse) +CHR(13)	;
								+m.lcIndent +[premium: $] +ALLTRIM(STR(End_indv.Premium, 7, 2)) +CHR(13)	;
								+m.lcIndent +[tax $] +ALLTRIM(STR(End_Indv.Tax, 7, 2)) +CHR(13)	;
								+m.lcIndent +[tax_auth: ] +ALLTRIM(End_Indv.Tax_Auth) +CHR(13)	;
								+m.lcIndent +[state_eff: ] +ALLTRIM(End_Indv.State_Eff) +CHR(13);
								+m.lcIndent +[effective: ] +DTOC(End_Indv.Effective) +CHR(13)	;
								+m.lcIndent +[expires: ] +DTOC(End_Indv.Expires) +CHR(13)	;
								+m.lcIndent +[batch_id: ] +ALLTRIM(End_Indv.Batch_Id) +CHR(13)
	
	SELECT (m.lnSelect)
	
* Write "Unknown" to mEdit
OTHERWISE
	REPLACE PoliIndA.mEdit	WITH ALLTRIM(PoliIndA.mEdit) +[?unknown CASE entry by FUNCTION PoliIndAudit()?] +CHR(13)

ENDCASE


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION BizRuleTaxes
PARAMETERS m.xcTaxAuthority, m.xnTaxRate, m.xnRow, m.xnCol
PRIVATE m.llReturn
m.llReturn	=.T.	&&	default to TRUE

DO CASE
CASE m.xcTaxAuthority="OFF"
	RELEASE WINDOWS lwBizRuleTaxes
*	SET COVERAGE TO 
	
* Change rule:
CASE m.mMode="C" .AND. !m.llF8Update
	DEFINE WINDOW lwBizRuleTaxes	;
		FROM m.xnRow,m.xnCol TO m.xnRow+6,m.xnCol+60 ;
		TITLE "When CHANGING:"	;
		NOFLOAT 	;
		NOCLOSE 	;
		SHADOW 		;
		NOMINIMIZE 	;
		COLOR SCHEME 1
	ACTIVATE WINDOW lwBizRuleTaxes TOP
	@0,1 SAY "     Tax Rate will NOT re-calculate Tax Amount!"
	@1,1 SAY "You may either..."
	@2,1 SAY "* Press [Enter] to accept " +STR(m.xntaxrate, 6,4) +" for " +ALLTRIM(m.xcTaxAuthority)
	@3,1 SAY "  OR"                                                               
	@4,1 SAY "* enter a different tax rate now."
	SHOW WINDOW lwBizRuleTaxes
	
* Add rule:
CASE m.mMode="A" .OR. m.llF8Update
	DEFINE WINDOW lwBizRuleTaxes	;
		FROM m.xnRow,m.xnCol TO m.xnRow+6,m.xnCol+60 ;
		TITLE "When ADDING:"	;
		NOFLOAT 	;
		NOCLOSE 	;
		SHADOW 		;
		NOMINIMIZE 	;
		COLOR SCHEME 1
	ACTIVATE WINDOW lwBizRuleTaxes TOP
	@0,1 SAY "     Tax Rate will be used to calculate Tax Amount."
	@1,1 SAY "You may either..."
	@2,1 SAY "* Press [Enter] to accept " +STR(m.xntaxrate, 6,4) +" for " +ALLTRIM(m.xcTaxAuthority)
	@3,1 SAY "  OR"
	@4,1 SAY "* enter a different tax rate now."
	SHOW WINDOW lwBizRuleTaxes
	
* only View or Notes
OTHERWISE
	m.llReturn	=.F.
ENDCASE

RETURN m.llReturn
		
		
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION LicenseCnt
PARAMETERS m.xcPoliNumb, m.xcPoliID
PRIVATE m.lnSelect, m.lnLicenseCnt
m.lnSelect	=SELECT()

SELECT PoliLic
*=SEEK(m.xcPoliNumb +m.xcPoliID)
=SEEK(m.xcPoliID)
*COUNT TO m.lnLicenseCnt	WHILE PoliNumb+PoliID =m.xcPoliNumb +m.xcPoliID FOR !DELETED()
COUNT TO m.lnLicenseCnt	WHILE PoliID =m.xcPoliID FOR !DELETED()

SELECT (m.lnSelect)
RETURN ALLTRIM(STR(m.lnLicenseCnt))
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION EmailCnt
PARAMETERS m.xcPoliID
PRIVATE m.lnSelect, m.lnEmailCnt
m.lnSelect	=SELECT()

SELECT PoliEmail
=SEEK(m.xcPoliID)
COUNT TO m.lnEmailCnt	WHILE cPoliID =m.xcPoliID FOR !DELETED()

SELECT (m.lnSelect)
RETURN ALLTRIM(STR(m.lnEmailCnt))
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION PoliTaskCnt
PARAMETERS m.xcbPoliIndK, m.xnRow, m.xnCol
PRIVATE m.lnSelect, m.lnDone, m.lnDue
m.lnDone	=0
m.lnDue		=0
m.lnSelect	=SELECT()

SELECT PoliTask
=SEEK(m.xcbPoliIndK)
SCAN WHILE PoliTask.cbPoliIndK = m.xcbPoliIndK FOR !DELETED()
	IF PoliTask.nPercent=100
		m.lnDone	=(m.lnDone +1)
	ELSE
		m.lnDue		=(m.lnDue +1)
	ENDIF
ENDSCAN

IF (m.lnDone +m.lnDue) >0
	=SEEK(m.xcbPoliIndK)
ENDIF

@m.xnRow,m.xnCol SAY SPACE(5)
@m.xnRow,m.xnCol +IIF(LEN(ALLTRIM(STR(m.lnDone)))=2, 0, 1) SAY ALLTRIM(STR(m.lnDone)) +"|" +ALLTRIM(STR(m.lnDue)) STYLE "U"

IF m.lnDue>0
	@m.xnRow+1,m.xnCol +3 SAY "DUE"	COLOR R+/W*	STYLE "B"
ELSE
	@m.xnRow+1,m.xnCol +3 SAY "Due"
ENDIF

SELECT (m.lnSelect)
RETURN .T.
	

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION GetEffective
PRIVATE m.ldEffective
*IIF((End_Indv.Endorse="SURCHARGE ON PREMIUM" .OR. (End_Indv.Premium=0 .AND. SUBSTR(End_Indv.PoliNumb,11,2)="KY" .AND. !("PENDING"$End_Indv.Endorse))), m.Effective, End_Indv.Effective)	;

DO CASE
* SURCHARGE ON PREMIUM
CASE End_Indv.Endorse="SURCHARGE ON PREMIUM"
	m.ldEffective	=m.Effective
	
* PENDING endorsement
CASE (m.llF8Update .OR. mmode='A') .AND. "PENDING"$End_Indv.Endorse
	m.ldEffective	=m.Effective
	
OTHERWISE
	m.ldEffective	=End_Indv.Effective
	
ENDCASE

RETURN m.ldEffective
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION YYYYMM
PARAMETERS m.xD
RETURN ALLTRIM(STR(YEAR(m.xd))) +RIGHT("0"+ALLTRIM(STR(MONTH(m.xD))),2)


*---------------------------------------------------------------------------------------------------------------------*
*FUNCTION BlankTheSSN5
*PARAMETERS m.xnRow, m.xnCol
*
*m.cSSN5=SPACE(LEN(PoliInd.cSSN5)
*@m.xnRow,m.xnCol SAY IIF(!EMPTY(m.cSSN5), REPLICATE(CHR(149),5),"    ")


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION SetIndvOrFirm
PARAMETERS xnPolicyTyp

DO CASE
* Individual
CASE m.xnPolicyTyp <=1 
	@01+m.lnViewRow,02+m.lnViewCol SAY "First Name" SIZE 1,10, 0
	@02+m.lnViewRow,03+m.lnViewCol SAY  "Last Name" SIZE 1,9, 0
	@03+m.lnViewRow,05+m.lnViewCol SAY    "Company" SIZE 1,7, 0
	SHOW GET m.Firstname ENABLE

* Firm
CASE m.xnPolicyTyp =2
	@01+m.lnViewRow,02+m.lnViewCol SAY "          " SIZE 1,10, 0
	@02+m.lnViewRow,03+m.lnViewCol SAY  "  Company" SIZE 1,9, 0
	@03+m.lnViewRow,05+m.lnViewCol SAY    "    DBA" SIZE 1,7, 0
	SHOW GET m.Firstname DISABLE

ENDCASE

RETURN .T.


