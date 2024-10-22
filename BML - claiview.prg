*****************************************************************************
*                                                                           *
* CLAIVIEW.PRG: Handles claims viewing                                      *
*
*
* 1/22/2004 mac - added aggregate loss/expense for the polinumb/poliid 
*		for cna - carrier 1004
*
*	3/20/2004 - mac changed indexes to cdx's 
*	compcode.idx = payeetype+compcode
*	compname.idx = payeetype+substr(upper(compname,1,15))
*	compstci.idx = payeetype+st+city
*
*	changed to tag
*	ptypcode = payeetype+compcode
*	ptypname = payeetype+substr(upper(compname,1,15))
*	ptypstct = payeetype+st+city
*
*
*	3/20/2004 mac changed attorney to use cdx's rather than idx's
*	attocomp.idx = compcode+substr(attorney,1,10)
*
*	changed to tag
*	attocomp = compcode+substr(attorney,1,10)
*
*	3/23/2004 mac - backed out tag change
*****************************************************************************
PARAMETERS m.jcMode
PRIVATE mcomprec, maction, m.llCNAClaimUsed, m.lcOnError, m.llCourtStUsed, m.lnX, m.lnBIrow, m.lnBIcol, m.lnSelect, m.llUsedStaff

IF FILE("F:\SB\BML\STEP.ON")	&&	.AND. "C:"$SYS(5) 
	SET STEP ON 
ENDIF


IF .F. .and. 'Visual FoxPro 09'$VERSION()
    PRIVATE llWasTheme
    llWasTheme = _Screen.Themes
    _Screen.Themes=.T.
    _Screen.Themes = .F.
ENDIF


m.lcOnError	=ON("ERROR")
ON ERROR DO ClaiViewScrnError WITH ERROR(), MESSAGE(), LINENO(), PROGRAM()

m.lnX		=02


*-------------------------------------------------------*
m.lnSelect	=SELECT()
m.llUsedStaff	=USED("Staff")
IF !m.llUsedStaff
	SELECT 0
	USE Staff
ELSE
	SELECT Staff
ENDIF
SET ORDER TO CODE   && UPPER(CODE)
IF SEEK(m.iStafCode) .AND. !EMPTY(Staff.Add1 +Staff.Add2)
	m.lnBIrow	=VAL(ALLTRIM(Staff.Add1)) +m.lnX
	m.lnBIcol	=VAL(ALLTRIM(Staff.Add2))
ELSE
	m.lnBIrow	=14
	m.lnBIcol	=90
ENDIF

IF !m.llUsedStaff
	USE IN Staff
ENDIF

SELECT (m.lnSelect)
*-------------------------------------------------------*


m.llCNAClaimUsed	=USED("CNAClaim")


SET PRINTER TO
SET PRINTER OFF
DEFINE WINDOW DataCode	FROM 03,20 TO 14,69 SYSTEM SHADOW COLOR SCHEME 8
DEFINE WINDOW DataCode2 FROM 17,20 TO 28,69 SYSTEM SHADOW COLOR SCHEME 8
DEFINE WINDOW company	FROM 03,04 TO 14,77 SYSTEM SHADOW COLOR SCHEME 8
DEFINE WINDOW mmessmemo	FROM 11,01 TO 23,78 TITLE "Message" SHADOW COLOR SCHEME 8
DEFINE WINDOW getinfo	FROM 08,10 TO 12,70 SYSTEM COLOR SCHEME 7
DEFINE WINDOW getyn		FROM 08,10 TO 12,70 SYSTEM COLOR SCHEME 7
*muser=TRIM(GETENV("bmluser"))

DO CASE
CASE mentry="Policy"
	SELECT 5
	USE memotemp
	SELECT 6
	USE DataCode ORDER tag codetype NOUPDATE
	SELECT 7
	USE company INDE compcode,compname,compstci	&& 3/20/2004 mac
	SET ORDER TO PTypCode
*	use company order tag ptypcode
	SELECT 8
	USE attorney INDE attoname,attocomp	&& 3/20/2004 mac
*	use attorney order tag attorney
	SET RELA TO "A"+compcode INTO company
	
CASE mentry="Poliind"
	SELECT 5
	USE memotemp
	SELECT 6
	USE DataCode ORDER tag codetype NOUPDATE
	SELECT 7
	USE company INDE compcode,compname,compstci	&& 3/20/2004 mac
	SET ORDER TO PTypCode
*	use company order tag ptypcode
	SELECT 8
	USE attorney INDE attoname,attocomp	&& 3/20/2004 mac
*	use attorney order tag attorney
	SET RELA TO "A"+compcode INTO company
	
CASE mentry="Company Claims"
	SELECT 6
	USE DataCode ORDER tag codetype	NOUPDATE
	SELECT company
	mcomprec=RECN()
	SET ORDER TO PTypCode
	SELECT 8
	USE attorney INDE attoname,attocomp	&& 3/20/2004 mac
*	use attorney order tag attorney
	SET RELA TO "A"+compcode INTO company
	
CASE mentry="Losses"
	SELECT 5
	USE memotemp
	SELECT 6
	USE DataCode ORDER tag codetype	NOUPDATE
	SELECT 7
	USE company INDE compcode,compname,compstci	&& 3/20/2004 mac
	SET ORDER TO PTypCode
*	use company order tag ptypcode
	SELECT 8
	USE attorney INDE attoname,attocomp	&& 3/20/2004 mac
*	use attorney order tag attorney
	SET RELA TO "A"+compcode INTO company
	
ENDCASE


*---------------------------------
* Set up PoliInd for use.
m.llUsedPoliInd	=USED("PoliInd")

IF !m.llUsedPoliInd
	SELECT 0
	USE PoliInd
ELSE
	SELECT PoliInd
	m.lcOrderPoliInd	=ORDER()
ENDIF

SET ORDER TO POLINUM_ID   && POLINUMB+POLIID
*---------------------------------

*---------------------------------
* Set up ClaiSetl for use.
m.llUsedClaiSetl	=USED("ClaiSetl")

IF !m.llUsedClaiSetl
	SELECT 0
	USE ClaiSetl
ELSE
	SELECT ClaiSetl
	m.lcOrderClaiSetl	=ORDER()
ENDIF

SET ORDER TO CBCLAIMS   DESCENDING && CBCLAIMSK+DTOS(DDATE)
*---------------------------------
		
*---------------------------------
* Set up ClaiProp for use.
m.llUsedClaiProp	=USED("ClaiProp")

IF !m.llUsedClaiProp
	SELECT 0
	USE ClaiProp
ELSE
	SELECT ClaiProp
	m.lcOrderClaiProp	=ORDER()
ENDIF

SET ORDER to ClaiProp	DESCENDING	&& CLAIMNUMB
*---------------------------------


*SELECT 3	&&	"should" be Claims.dbf
SELECT Claims
m.lcOrderClaims	=ORDER()
m.lcRelaClaims	=SET("RELATION")

SET RELATION TO PoliNumb + PoliID 	INTO PoliInd ADDITIVE
SET RELATION TO cbKey				INTO ClaiSetl ADDITIVE
SET RELATION TO ClaimNumb			INTO ClaiProp ADDITIVE
*=SEEK(Claims.ClaimNumb, "ClaiProp")

SELECT 0
USE DeduSt
COPY TO ARRAY aDeduSt
USE

SELECT Claims

IF m.jcMode="A"
	SELECT Claims
	APPEND BLANK
*	REPLACE Claims.cbKey	WITH GUID(36)
ENDIF
IF EMPTY(Claims.cbKey)
	REPLACE Claims.cbKey	WITH GUID(36)
ENDIF
	
	morigrec	=RECNO()
	
	IF m.jcMode='A'
		IF mentry="Losses"
			mpolinumb=SPACE(15)
		ELSE
			mpolinumb=policy.polinumb
		ENDIF
	ELSE
		mpolinumb	=polinumb
	ENDIF
	
	mpoliid		=poliid
	mclaimnumb	=claimnumb
	mcasenumb	=casenumb
	mcourt		=court
	mcourtst	=courtst
	mstate		=policy.state
	mplaintiff	=plaintiff
	
	IF m.jcMode='A' .AND. mentry="Poliind"
		mdefendant=ALLTRIM(poliind.lastname) + " " + ALLTRIM(poliind.firstname)
	ELSE
		mdefendant	=defendant
	ENDIF
	
	mcovered	=covered
	mcompcode	=compcode
	
	SELECT company
	SEEK "A"+mcompcode
	mcompname	=compname
		
	SELECT claims
	mattorney	=attorney
	mrealcomp	=realcomp
	mrealdba	=realdba
	mrealtor	=realtor
	
	IF m.jcMode='A'
		mpolieff=policy.effecdate
		mpoliexp=policy.TO
	ELSE
		mpolieff	=polieff
		mpoliexp	=poliexp
	ENDIF
	
	mdefastat	=defastat
	msuitstat	=suitstat
	mcarriernam	=carriernam
	mdateclaim	=dateclaim
	mdaterepo	=daterepo
	mdatenoti	=datenoti
	mdateloss	=dateloss
	mdedlosstat	=dedlosstat
	mdedlosdate	=dedlosdate
	mdedlaestat	=dedlaestat
	mdedlaedate	=dedlaedate
	mtrial		=trial
	mmediation	=mediation
	mdarbitrate	=darbitrate
	mextaint	=extaint
	mdateclosed	=dateclosed
	mstatus		=STATUS
	mre1		=re1
	mre2		=re2
	mre3		=re3
	mrealest	=realest
	mdeal		=deal
	malleg1		=alleg1
	malleg2		=alleg2
	malleg3		=alleg3
	malleg4		=alleg4
	mdenial1	=denial1
	mdenial2	=denial2
	mdenial3	=denial3
	mclreserves	=clreserves
	mexreserves	=exreserves
	mresrtot	=resrtot
	mclaimpaymt	=claimpaymt
	mexpenpaymt	=expenpaymt
	msubroex	=subroex
	msubrocl	=subrocl
	mbroker		=broker
	mlossoutcm	=lossoutcm
	
	IF m.jcMode='A'
		mcompowner	=policy.compowner
	ELSE
		mcompowner	=compowner
	ENDIF
	
	mexaminer	=examiner
	mxspolicy	=xspolicy
	mxscarrier	=xscarrier
	mccnumb		=ccnumb
	mdamages	= 0		&& 1/22/2004 mac
	mexpenses	= 0		&& 1/22/2004 mac
	mseverity	= severity	&& 1/27/2004 mac
	minc_type	= inc_type	&& 1/27/2004 mac
	mlRiskNamed	= lRiskNamed
	mtSeeking	=tSeeking
	mlCNANotify	=lCNANotify
	mdCNARepo	=dCNARepo
	mClmtAdd1	=ClmtAdd1
	mClmtAdd2	=ClmtAdd2
	mClmCity	=ClmCity
	mClmtState	=ClmtState
	mClmtZIP	=ClmtZIP
	mClmtPhn1	=ClmtPhn1
	mClmtPhn2	=ClmtPhn2
	mClmtFAX	=ClmtFAX
	mClmtEMail	=ClmtEMail
	mCCName		=CCName
	mCCFirm		=CCFirm
	mCCAdd1		=CCAdd1
	mCCAdd2		=CCAdd2
	mCCCity		=CCCity
	mCCState	=CCState
	mCCZIP		=CCZIP
	mCCPhn1		=CCPhn1
	mCCPhn2		=CCPhn2
	mCCFAX		=CCFAX
	mCCEmail	=CCEmail
	mlExsNotify	=lExsNotify
	mdExsRepo	=dExsRepo
	m.nDmgDeduSt=(nDmgDeduSt +1)
	m.nExpDeduSt=(nExpDeduSt +1)
	m.lBodyInj	=lBodyInj
	m.dBIsent	=dBIsent
	m.dBIReturn	=dBIreturn
	m.dBINotify	=dBInotify
*	m.lHasMedi	=lHasMedi
	m.nHasMedi	=(nHasMedi+1) &&	offset by 1+ to match with array aHasMedi
	
SET UDFPARMS TO REFERENCE
CLEAR

PRIVATE aHasMedi
DIMENSION aHasMedi[3]
	aHasMedi[1]="Unknown"
	aHasMedi[2]="Yes"
	aHasMedi[3]="No"


m.llCourtStUsed		=USED("CourtSt") 

IF FILE("CourtSt.DBF")
	IF !m.llCourtStUsed
		SELECT 0
		USE CourtSt
	ENDIF
	
	DIMENSION aCourtSt[1], aCourt[1]
	aCourtSt[1]	=SPACE(LEN(CourtSt.CourtSt))
	aCourt[1]	=SPACE(LEN(CourtSt.Court))
	=SetupCourtSt()		&& in Claims.prg
ENDIF

m.lcClaimChkError	=""

DO WHILE .T.
		
	DO WHILE .T.
		CLEAR GETS
		
		SELECT company
*		SET ORDER TO ptypcode		&&	11/28/2005 SAB:  Should I do this or not?  Maggie got "not ordered" error once but couldn't duplicate.
		SEEK "A"+claims.compcode
		
		SELECT claims
		mpolnum=mpolinumb
		
*		=ClaimLabels()
				
*		IF mstatus="P"	&& Party
*			@02,1 SAY "           Claim:" GET mclaimnumb PICT "@!"
*		ELSE
			@02,1 SAY "           Claim:" GET mclaimnumb PICT "@!" WHEN m.jcMode="A" .OR. NoClaimChildren(mclaimnumb) VALID ClaimChk(@mclaimnumb) ERROR m.lcClaimChkError	&&	"Claim #/Incident is in Use"
*			@02,1 SAY "           Claim:" GET mclaimnumb PICT "@!" WHEN m.jcMode="A"	VALID ClaimChk(@mclaimnumb) ERROR m.lcClaimChkError	&&	"Claim #/Incident is in Use"
*		ENDIF
*SET STEP ON 						
		@02,34.25+1 GET mstatus PICT "!" 	;
			VALID StatChk(mstatus) ;
			ERROR "Status must be - Denied / Open / Reopen / Closed / Incident / Party"
		@02,36.5+1 SAY "Examiner:" GET mexaminer
		@03,0 TO 03,80
		@04,1 SAY "     Allegations:" GET malleg1 PICT "@!" VALID datachk("A",malleg1,.F.)
		@04,26.25 GET mre1
		@05,19 GET malleg2 PICT "@!" VALID datachk("A",malleg2,.T.)
		@05,25.25+1 GET mre2
		@06,19 GET malleg3 PICT "@!" VALID datachk("A",malleg3,.T.)
		@06,25.25+1 GET mre3
		
*		=DispBodyInj(@mpolinumb, @mpoliid, 4, 82)

		* A
		@07,1 SAY "Reported to RISC:" GET mdaterepo WHEN dateproc() VALID DTOC(mdaterepo)<>"  /  /  " ERROR "A Date must be Entered"
		@07,29.25+1 SAY " Loss Outcome:" GET mlossoutcm PICTURE "@!" VALID datachk("O", mlossoutcm, .F.)
			
		* B
		@08,1 SAY "Insured Notified:" GET mdatenoti WHEN .F. VALID DTOC(mdatenoti)<>"  /  /  " ERROR "A Date must be Entered"
		@08,29.25+2 SAY "Denial Codes:" GET mdenial1 PICT "@!" VALID datachk("E",mdenial1,mstatus<>"D")
		@08,47.5+1 GET mdenial2 PICT "@!" VALID datachk("E",mdenial1,.T.)
		@08,50.75+1 GET mdenial3 PICT "@!" VALID datachk("E",mdenial1,.T.)
		
		* C
		@09,1 SAY "   Loss Occurred:" GET mdateloss WHEN .F.
		@09,29.25+2 SAY "      Closed:" GET mdateclosed
			
		IF mstatus<>"P"
			DO losstype WITH mdateloss,.T.,mpolinumb
		ENDIF
		
		@09,55.5+1 SAY mdefastat
		@10,0 TO 10,80
		@11,35 SAY GetOfferAs(mpolinumb)	COLOR RGB(255,0,0)
		@11,5 SAY "Group Policy:" GET mpolinumb PICT "@!";
			WHEN mentry="Losses";
			VALID polichk(mpolinumb) .AND. GetCNAClaim(mpolinumb, mdatenoti) .AND. DispOfferAs(mpolinumb, 11, 33)
		@11,44 SAY "Carrier:" GET mcompowner
		@11,59.25+1 SAY policy.addtl_key
		
		IF !EMPTY(mpolinumb)
*			DO programs\chkcnt WITH mpolinumb,.T.,"",12
			DO programs\chkcnt WITH mpolinumb,.T.,"",12
		ENDIF
		
		@12,1 SAY "          Limits:"
*		=CovDedu(ClaimNumb, PoliNumb, PoliID, 09, 90)
		=CovDedu(ClaimNumb, PoliNumb, PoliID, m.lnBIrow-10, 90)
*		@13,1 SAY "        Licensee:" GET mpoliid PICT "@!"
		@13,1 SAY "        Licensee:" 
		@13,19 GET mpoliid ;
			WHEN p_id_whn(mpolinumb);
			VALID poliidchk(mpolinumb,mpoliid)	;
				.AND. CheckBI(mpolinumb, mpoliid, m.lnBIrow, m.lnBIcol)
				
*				.AND. DispBodyInj(@mpolinumb, @mpoliid, 4, 82)	;
*			ERROR "Not a valid licensee for "+mpolinumb
			
			* FONT 'Arial',10	STYLE 'B'
			
		=GetBI(m.lnBIrow, m.lnBICol) .AND. IIF(m.jcMode="V", CheckBI(mpolinumb, mpoliid, m.lnBIrow, m.lnBIcol), .T.)

			
		@13,28.25+2 SAY GetLicensee(mpolinumb,mpoliid)
		
		@14,1 SAY "   Excess Policy:" GET mxspolicy
*		@14,COL()+1 SAY "         Carrier:" GET mxscarrier
		@14,44 SAY "Carrier:" GET mxscarrier
		
		IF !EMPTY(mxspolicy)
			DO programs\chkcnt WITH mxspolicy,.T.,"",15
		ENDIF
		
		@15,01 SAY "   Excess Limits:"
		@16,01 SAY "Covered Insured?:" GET mcovered PICT "@S35"
		@17,01 SAY "RealtyFirm-legal:" GET mrealcomp PICT "@!" WHEN prerealcomp() VALID postrealcomp()
		@17,50 SAY " DBA:" GET mrealdba	PICTURE "@!"
		@18,01 SAY "         Realtor:" GET mrealtor
		@19,01 SAY "          Broker:" GET mbroker
		@18,50 SAY "Type:" GET mrealest PICT "@!" VALID datachk("R",mrealest,.F.)
		@19,50 SAY "Deal:" GET mdeal PICT "@!" VALID datachk("D",mdeal,.F.)
		@20,00 TO 20,80
		@20,19 SAY "Damages"
		@20,32 SAY "Expenses"
		@20,44 SAY "Incurred"
		@20,56 SAY "CC"
*		@21,01 SAY "                  Damages      Expenses    Incurred    CC"
		
		DO incurred
		
		@21,01 SAY "        Reserves:" GET mclreserves PICT "#######.##" VALID Incurred() WHEN mstatus<>"I"
		@21,32 GET mexreserves PICT "#######.##" VALID Incurred() WHEN mstatus<>"I"
		@21,44 GET mresrtot PICT "#######.##" VALID Incurred()				
		@21,56 GET mccnumb FUNCTION "!" PICT "XXXXXXXXXXXXXXX"	VALID GetCNAClaim(mpolinumb, mdatenoti) .AND. ChgCNAClaim()
				
		@22,01 SAY "        Payments:" GET mclaimpaymt PICT "#######.##" WHEN .F.
		@22,32 GET mexpenpaymt PICT "#######.##" WHEN .F.
			
		@23,01 SAY "        Recovery:" GET msubrocl PICT "#######.##" WHEN .F.
		@23,32 GET msubroex PICT "#######.##" WHEN .F.

*		@24,01 SAY " Deductible/Paid:" GET mdedlosstat PICT "Y"
		@24.25,01 SAY "  Deductible-Dmg:" 
		@22.50,20 GET m.nDmgDeduSt	;
			FUNCTION "^"	;
			FROM aDeduSt	;
			WHEN (m.nDmgDeduSt#2 .OR. UPPER(ALLTRIM(istafcode))$"MKNAUSS, SBBML")	;
			VALIDATE IIF(m.nDmgDeduSt#2, .T., IIF(m.nDmgDeduSt=2 .AND. UPPER(ALLTRIM(istafcode))$"MKNAUSS, SBBML", .T., .F.))
			
*		@24,20 GET m.nDmgDeduSt	FUNCTION "^"	FROM aDeduSt
		@24.25,35 SAY "on" 
		@24.25,38 GET mDedLosDate
		@25.75,13 SAY "-Exp:" 
		@24.00,20 GET m.nExpDeduSt ;
			FUNCTION "^"	;
			FROM aDeduSt	;
			WHEN (m.nExpDeduSt#2 .OR.  UPPER(ALLTRIM(istafcode))$"MKNAUSS, SBBML")	;
			VALIDATE IIF(m.nExpDeduSt#2, .T., IIF(m.nExpDeduSt=2 .AND. UPPER(ALLTRIM(istafcode))$"MKNAUSS, SBBML", .T., .F.))
			
*		@25.50,20 GET m.nExpDeduSt FUNCTION "^"	FROM aDeduSt
		@25.75,35 SAY "on"
		@25.75,38 GET mDedLaeDate

*		@24,20.25+1 GET mdedlosdate
*		@24,31.50+1 GET mdedlaestat PICT "Y"
*		@24,33.75+1 GET mdedlaedate

		if (mcompowner = '1004' .OR. mcompowner = '1005') then 	&& 1/22/2004 mac
					
			IF !EMPTY(Claims.PoliID)
				Do aggregate 
			ENDIF
			
			@23,57 say "Polinumb/Poliid Aggregate"	STYLE "U"	&& 1/22/2004 mac
			@24,57 say "Damages           Expense"	&& 1/22/2004 mac
			@25,56 say mDamages -0	pict "#######.##"	&& 1/22/2004 mac		
			@25,72 say mExpenses -0	pict "#######.##"	&& 1/22/2004 mac		
		endif
		
*		@25,0 TO 25,80
		@27.5,0 TO 27.5,80
*		m.lnX	=2
*		@27,1 SAY "     Suit Status:" GET msuitstat PICTURE "@M NOT A SUIT,LAWSUIT,NOT AVAILABLE,LEGAL,ARBITRATION,NON-LEGAL,MEDIATION,NOC-NOT OTHERWISE CLASSIFIED" && 2/1/2004 MAC - ADDED ADDITIONAL OPTIONS
*		@26+m.lnX,1 SAY "     Suit Status:" GET msuitstat PICTURE "@M ARBITRATION,LAWSUIT,LEGAL,MEDIATION,NOC-NOT OTHERWISE CLASSIFIED,NON-LEGAL,NOT A SUIT,NOT AVAILABLE,REGULATORY COMPLAINT,REGULATORY COMPLAINT AND LEGAL,REGULATORY COMPLAINT AND NON-LEGAL,SUBPOENA,SUBPOENA AND NON-LEGAL"
*		@26+m.lnX,1 SAY "     Suit Status:" GET msuitstat PICTURE "@M ARBITRATION,ASSOCIATION/BOARD COMPLAINT,ATTORNEY GENERAL COMPLAINT,LAWSUIT,LEGAL,LEGAL AND ASSOC/BD COMPL,MEDIATION,NOC-NOT OTHERWISE CLASSIFIED,NON-LEGAL,NON-LEGAL AND ASSOC/BD COMPL,NOT A SUIT,NOT AVAILABLE,REGULATORY COMPLAINT,REGULATORY COMPLAINT AND LEGAL,REGULATORY COMPLAINT AND NON-LEGAL,SUBPOENA,SUBPOENA AND NON-LEGAL"
		@26+m.lnX,1 SAY "     Suit Status:" GET msuitstat PICTURE "@!" VALID DataChk("S",msuitstat,.F.)

		@26+m.lnX,52 SAY "Trial Date:" GET mtrial
*		@27,52 SAY "   Med/Arb:" GET mmediation
		@27+m.lnX,1 SAY "            Case:" GET mcasenumb WHEN mstatus<>"I"
		@27+m.lnX,40 SAY "Mediation:" 	GET mmediation
		@27+m.lnX,64 SAY "Arbitration:" 	GET mdarbitrate


*		m.lnX	=0
		IF !USED("CourtSt")	
			@28+m.lnX,1  SAY "           Court:" GET mcourt PICTURE "@S30" WHEN mstatus<>"I"
			@28+m.lnX,52 SAY "  Court St:" GET mcourtst	PICTURE "!!"
		ELSE
			@28.50+m.lnX,09 SAY "Court St:"
			@28.50+m.lnX,28 SAY "Court:"
			@27.25+m.lnX,19 GET m.mcourtst	PICTURE "@^"	FROM aCourtSt	VALID ShowCourtSt() SIZE 1,LEN(Claims.CourtSt)+4,0		&&	MESSAGE 'Select a "Court State" or key {Ctrl+0} to enter [blank].'
			@27.25+m.lnX,35 GET m.mcourt 	PICTURE "@^"	FROM aCourt		WHEN mstatus<>"I" .AND. SetUpCourt(m.mCourtSt) 	VALID ShowCourt(m.mCourtSt)	SIZE 1,LEN(Claims.Court)+4,0	&&	MESSAGE 'Select a ' +IIF(EMPTY(m.mCourtSt), '"Court State - Court"', '"Court" in ' +m.mCourtSt)
*			@28.25,19 GET m.mcourtst	PICTURE "@^"	FROM aCourtSt	VALID ShowCourtSt() 		&&	MESSAGE 'Select a "Court State" or key {Ctrl+0} to enter [blank].'
*			@28.25,35 GET m.mcourt 		PICTURE "@^"	FROM aCourt		WHEN mstatus<>"I" .AND. SetUpCourt(m.mCourtSt) 	VALID ShowCourt(m.mCourtSt)	SIZE 1,LEN(Claims.Court)+4,0	&&	MESSAGE 'Select a ' +IIF(EMPTY(m.mCourtSt), '"Court State - Court"', '"Court" in ' +m.mCourtSt)
			m.lnX	=(m.lnX+1)
		ENDIF
		
		@29+m.lnX,1 SAY "        Attorney:" GET mattorney PICTURE "@S30!" VALID compchk(mattorney)
		@29+m.lnX,49.25+1 SAY compphone(mcompcode)
		@30+m.lnX,1 SAY "        Law Firm:" GET mcompname PICTURE "@S40!" WHEN .F.
*		@32,1 SAY "       Plaintiff:" GET mplaintiff PICTURE "@S35"
		@31+m.lnX,17 SAY CHR(45)+CHR(16)
*		@32,02 GET m.lnPlaintiff	DEFAULT 1	WHEN m.jcMode#"V"	FUNCTION '*N ' +IIF(GotClmt(), "Plaintiff+", "-plaintiff-")	SIZE 2,15	VALID IIF(EditClmt()=1, 1, 1)
		@31+m.lnX,19 GET mPlaintiff	PICTURE "@S35"
			
		IF (mcompowner = '1004' .OR. mcompowner = '1005') then
			@30+m.lnX,60 SAY "Insured Id:"
			@31+m.lnX,55 GET mseverity PICTURE "@!" VALID DataChk("I",mseverity,.T.)
		ENDIF

		@33+m.lnX,1 SAY "       Defendant:" GET mdefendant PICTURE "@S35"
		@34+m.lnX,1 SAY "   Carrier Named:" GET mcarriernam PICTURE "@M ,COVERAGE DISPUTE,DIRECT ACTION,DOI COMPLAINT,SUBPOENA"
				
		
		@34+m.lnX,41 GET mlRiskNamed FUNCTION "*C RISC named?" COLOR ,,,,,,,,R+/W* VALID NoChangeByPressingEnter(OBJVAR())

		IF (mcompowner = '1004' .OR. mcompowner = '1005') then		&& 1/24/2004 mac 
*			@ 34,42 say "Claimant Id:"
*			@ 33,55 GET minc_type ;
			PICTURE "@^ ;NOT AVAILABLE;SELLER;BUYER;INSPECTOR/INSPECTION CO;TITLE INSURANCE CO;LENDER;LESSOR;LESSEE;BUILDER/DEVELOPER;GOVERNMENT;INSURANCE;NOT CLASSIFIED" ;
			SIZE 1,25 ;
			DEFAULT " " 
			@33+m.lnX,55 SAY "Claimant Id:"
			@34+m.lnX,55 GET minc_type PICTURE "@!" VALID DataChk("C",minc_type,.T.)
		ENDIF

		@35+m.lnX,17 SAY CHR(45)+CHR(16)
		@35+m.lnX,19 GET mtSeeking FUNCTION "@K$ 99,999,999.99"	WHEN m.jcMode#"V"	VALID SetCNA()
		@35+m.lnX,42 SAY "Last Demand:"
		
*		IF mlCNANotify
*			@37,19 GET mlCNANotify 	FUNCTION "*C CNA Notified?"	COLOR ,,,,,,,,R+/W*	VALID SetCNA()	ENABLED
*			@37,36 GET mdCNARepo	WHEN mlCNANotify	COLOR R+/W*					VALID SetCNA()	ENABLED
*		ELSE
*			@37,19 GET mlCNANotify 	FUNCTION "*C CNA Notified?"	COLOR ,,,,,,,,R+/W*	DISABLED
*			@37,36 GET mdCNARepo	WHEN mlCNANotify	COLOR R+/W*					DISABLED
*		ENDIF		

		@36+m.lnX,19 GET mlCNANotify 	FUNCTION "*C CNA Notified?"	COLOR ,,,,,,,,R+/W*	VALID NoChangeByPressingEnter(OBJVAR())
		@36+m.lnX,40 GET mdCNARepo	WHEN mlCNANotify	COLOR R+/W*					
		@37+m.lnX,19 GET mlExsNotify 	FUNCTION "*C Excess Notified?"	COLOR ,,,,,,,,R+/W*	VALID NoChangeByPressingEnter(OBJVAR())
		@37+m.lnX,40 GET mdExsRepo	WHEN mlExsNotify	COLOR R+/W*					
		
		=ShowNoteExcerpt(37+m.lnX, 55, 32.5)
		@35+m.lnX,55 GET m.lnSettlements 	DEFAULT 1 	WHEN m.jcMode#"V"	;
											FUNCTION '*N '+IIF(EOF("ClaiSetl"), "-none-", ALLTRIM(TRANSFORM(claiSetl.tsettleamt, '$$$,$$$,$$$.99')) +" on " +DTOC(ClaiSetl.dDate)+IIF(!EMPTY(ClaiSetl.mNote),"+",""))	;
											SIZE 2,30	VALID IIF(ToSettlements()=-1 .AND. SetCNA(), 1, 1) 
*		@37+m.lnX,60 SAY "{"+IIF(!EMPTY(ClaiSetl.mNote),LEFT(MLINE(ClaiSetl.mNote,MEMLINES(ClaiSetl.mNote)),15)+"...}",SPACE(20))
		@31+m.lnX,02 GET m.lnPlaintiff		DEFAULT 1	WHEN m.jcMode#"V"	FUNCTION '*N ' +IIF(GotClmt(), "Plaintiff+", "-plaintiff-")	SIZE 2,15	VALID IIF(EditClmt()=1, 1, 1)
		@35+m.lnX,02 GET m.lnSeeking		DEFAULT 1	WHEN m.jcMode#"V"	FUNCTION '*N ' +IIF(EMPTY(Claims.mSeeking), "-seeking-", LEFT(MLINE(mseeking,1,0), 13) +"+")	SIZE 2,15	VALID EditSeeking()
		@38+m.lnX,02 SAY "Propert-y/ies:"
		@38+m.lnX,19 GET m.lnProperty		DEFAULT 1	WHEN m.jcMode#"V"	FUNCTION '*N '+IIF(EOF("ClaiProp"), "-none-", ALLTRIM(ClaiProp.cAdd1) +" (1/" +ALLTRIM(STR(PropCount(Claims.ClaimNumb)))+")") 	SIZE 2,30	VALID IIF(ToProperty()=-1 .AND. SetCNA(), 1, 1)
		@38+m.lnX,55 GET m.lnClaiEnd 		DEFAULT 1	WHEN m.jcMode#"V"	FUNCTION '*N Endorsements'	SIZE 2,30	VALID ChkEndorsements(mclaimnumb, mpolinumb, mpoliid)
		
		IF m.jcMode$"AC"
			DO PROMPT WITH "Enter Information / CTRL-W to Save / ESC to Abandon"
			READ CYCLE WHEN CheckBI(mpolinumb, mpoliid, m.lnBIrow, m.lnBIcol)
			m.llSave	=(LASTKEY()=23)
*			m.llSave	=YesNo("Save Claim?",0)

			IF mpolinumb<>mpolnum .AND. m.jcMode="C"
				ACTIVATE WINDOW getinfo
				myorn=.F.
				@1,2 SAY "Are you sure you want to change Policy Number: " GET myorn PICT "Y"
					
				READ
				
				IF myorn=.F.
					CLEAR TYPEAHEAD
					KEYBOARD CHR(27)
				ELSE
					mpolnum=mpolinumb
					mpoliid=SPACE(8)
				ENDIF
				
				DEACTIVATE WINDOW getinfo
			ENDIF
			
			DO CASE
			*        [Ctrl+W]
			CASE m.llSave
*				SEEK Policy.PoliNumb+mclaimnumb
				
*				IF EOF() .OR. RECNO()=morigrec
					
					IF m.jcMode="A"
						mclaims=1
						mopen=IIF(mstatus$"OR",1,0)
*						APPEND BLANK
*						morigrec=RECNO()
					ELSE
*						GO morigrec
						mclaims=0
							
						DO CASE
						CASE STATUS$"OR" .AND. mstatus$"ICD"
							mopen=(-1)
						CASE STATUS$"ICD" .AND. mstatus$"OR"
							mopen=1
						OTHERWISE
							mopen=0
						ENDCASE
					ENDIF
					
					DO WHILE .T.
									
						IF RLOCK()
							WAIT CLEAR
								
							IF clreserves<>mclreserves .OR. exreserves<>mexreserves
								SELECT 0
								USE reserves INDE reserves
								
								IF claims.clreserves<>mclreserves
									APPEND BLANK
									
									DO WHILE .T.
											
										IF RLOCK()
											REPL TYPE WITH "C",claimnumb WITH mclaimnumb,;
												DATE WITH isysdate,TIME WITH TIME(),;
												amount WITH mclreserves
											UNLOCK
											EXIT
										ENDIF
										
									ENDDO
									
								ENDIF
								
								IF claims.exreserves<>mexreserves
									APPEND BLANK
										
									DO WHILE .T.
										
										IF RLOCK()
											REPL TYPE WITH "E",claimnumb WITH mclaimnumb,;
												DATE WITH isysdate,TIME WITH TIME(),;
												amount WITH mexreserves
											UNLOCK
											EXIT
										ENDIF
										
									ENDDO
									
								ENDIF
								
								USE		&&	Reserves.dbf
								SELECT claims
							ENDIF
							
							* 08/28/2008:
							* Determine is "Checks" (Fees.DBF) and "Audit Trail" (ClaimsAu.DBF) 
							*	requires an update.
							*	If we are in "C"hange mode and the ClaimNumb had changed,
							*	change the Fees.ClaimNumb accordingly.
							m.llUpdateFeesAu=(m.jcMode="C" .AND. ALLTRIM(Claims.ClaimNumb)#ALLTRIM(mclaimnumb))
							m.lcOldClaimNumb=Claims.ClaimNumb
							
							REPLACE polinumb WITH mpolinumb,	;
								claimnumb 	WITH mclaimnumb,	;
								casenumb 	WITH mcasenumb,;
								insured 	WITH policy.insured,;
								court 		WITH mcourt,;
								courtst		WITH mcourtst,;
								state 		WITH mstate,	;
								plaintiff 	WITH mplaintiff,	;
								defendant 	WITH mdefendant,;
								compcode 	WITH mcompcode,	;
								attorney 	WITH mattorney,;
								realcomp 	WITH mrealcomp,;
								realtor 	WITH mrealtor,	;
								defastat 	WITH mdefastat;
								suitstat 	WITH msuitstat,;
								covered 	WITH mcovered
								
							REPLACE polieff WITH mpolieff,	;
								poliexp 	WITH mpoliexp,;
								datenoti 	WITH mdatenoti,	;
								dateloss 	WITH mdateloss,;
								daterepo 	WITH mdaterepo,	;
								dateclosed 	WITH mdateclosed,;
								dateclaim 	WITH mdateclaim,;
								extaint 	WITH mextaint,	;
								STATUS 		WITH mstatus,;
								re1 		WITH mre1,	;
								re2 		WITH mre2,	;
								re3 		WITH mre3
								
							REPLACE dedlosstat WITH mdedlosstat,	;
								dedlosdate 	WITH mdedlosdate,	;
								dedlaestat 	WITH mdedlaestat, 	;
								dedlaedate 	WITH mdedlaedate
								
							REPLACE trial 	WITH mtrial, 	;
								mediation 	WITH mmediation	;
								dArbitrate	WITH mdArbitrate
								
							REPLACE realest WITH mrealest,	;
								deal 		WITH mdeal,	;
								alleg1 		WITH malleg1,	;
								alleg2 		WITH malleg2,	;
								alleg3 		WITH malleg3,	;
								alleg4 		WITH malleg4,	;
								denial1 	WITH mdenial1,	;
								denial2 	WITH mdenial2,	;
								denial3 	WITH mdenial3,	;
								clreserves 	WITH mclreserves,	;
								exreserves 	WITH mexreserves,	;
								claimpaymt 	WITH mclaimpaymt,	;
								expenpaymt 	WITH mexpenpaymt,	;
								subroex 	WITH msubroex,	;
								subrocl 	WITH msubrocl,	;
								broker 		WITH mbroker,	;
								lossoutcm 	WITH mlossoutcm,	;
								compowner 	WITH mcompowner,	;
								examiner 	WITH mexaminer,	;
								resrtot 	WITH IIF(STATUS$"OR",mclreserves+mexreserves,mclaimpaymt+mexpenpaymt-(msubroex+msubrocl)),	;
								COUNT 		WITH 1
								
							REPLACE xspolicy 	WITH  mxspolicy
							REPLACE xscarrier 	WITH mxscarrier
							REPLACE poliid 		WITH mpoliid
							REPLACE ccnumb 		WITH mccnumb,;
								carriernam 		WITH mcarriernam
								
							REPLACE severity WITH mseverity, ;
								inc_type 	with minc_type  	&& 1/27/2004 mac
								
							REPLACE lRiskNamed	WITH mlRiskNamed
							REPLACE tSeeking	WITH mtSeeking
							
							REPLACE lCNANotify	WITH mlCNANotify
							REPLACE dCNARepo	WITH mdCNARepo
							REPLACE ClmtAdd1	WITH mClmtAdd1,;
									ClmtAdd2	WITH mClmtAdd2,;
									ClmCity		WITH mClmCity,;
									ClmtState	WITH mClmtState,;
									ClmtZIP		WITH mClmtZIP,;
									ClmtPhn1	WITH mClmtPhn1,;
									ClmtPhn2	WITH mClmtPhn2,;
									ClmtFAX		WITH mClmtFAX,;
									ClmtEMail	WITH mClmtEMail
							REPLACE CCName		WITH mCCName,;
									CCFirm		WITH mCCFirm,;
									CCAdd1		WITH mCCAdd1,;
									CCAdd2		WITH mCCAdd2,;
									CCCity		WITH mCCCity,;
									CCState		WITH mCCState,;
									CCZIP		WITH mCCZIP,;
									CCPhn1		WITH mCCPhn1,;
									CCPhn2		WITH mCCPhn2,;
									CCFAX		WITH mCCFAX,;
									CCEmail		WITH mCCEmail
							REPLACE realdba	WITH mrealdba
							REPLACE lExsNotify	WITH mlExsNotify
							REPLACE dExsRepo	WITH mdExsRepo
							REPLACE nDmgDeduSt	WITH MAX((m.nDmgDeduSt -1), 0)
							REPLACE nExpDeduSt	WITH MAX((m.nExpDeduSt -1), 0)
							REPLACE lBodyInj	WITH m.lBodyInj	;
									dBIsent		WITH m.dBIsent	;
									dBIreturn	WITH m.dBIreturn;
									dBInotify	WITH m.dBInotify;
									nHasMedi	WITH MAX(m.nHasMedi-1, 0)
							
							* 08/28/2008
							IF m.llUpdateFeesAu
								=UpdateFees(m.lcOldClaimNumb, Claims.ClaimNumb)
								=UpdateAudit(m.lcOldClaimNumb, Claims.ClaimNumb)
							ENDIF
							
							* 05/17/2006 Steven Bennett
							*	Populate audit trial:  date, time, StaffCode 
							=ClaimAudit(mpolinumb, mclaimnumb, DATE(), TIME(), m.mStafCode)
							
							m.jcMode="C"
							SELECT policy
							SET ORDER TO POLINUMB   && POLINUMB
							
							PRIVATE m.llFound
							m.llFound=SEEK(mpolinumb)

							DO WHILE m.llFound
								
								IF RLOCK()
									
									IF !m.test_mode
										REPLACE policy.claims WITH policy.claims+mclaims,;
											policy.OPEN WITH policy.OPEN+mopen
									ENDIF
									
									UNLOCK
									EXIT
								ENDIF
								
							ENDDO
							
							UNLOCK
							EXIT
						ENDIF
						
						WAIT WINDOW NOWAIT "Attempting to lock Claim record and save updates..."
					ENDDO
					
					EXIT
				
			OTHERWISE
			
				IF m.jcMode='A'
					DELETE
				ENDIF
				
				EXIT
			
			ENDCASE
			
		ELSE
			EXIT
		ENDIF
	
	ENDDO
		
	IF m.jcMode="V"
		CLEAR GETS
		EXIT
	ELSE
	
		DO CASE
*		CASE .NOT. (READKEY()=12 .OR. READKEY()=268) .AND. m.bml
*		CASE LASTKEY()#27	&&	m.llSave
		* If ChgNotes or "m.llSave	=(LASTKEY()=23)" followed the above READ
		CASE m.jcMode='N' .OR. m.llSave
			DO PROMPT WITH "[Ctrl-W]=Save   [Esc]=Cancel"
			SELECT claims
			
			IF (mentry="Policy" AND policy.claimalert) OR (mentry="Poliind" AND poliind.claimalert)
				REPLACE NOTE WITH "CLAIMS ALERT ON POLICY RECORD! PLEASE REVIEW POLICY NOTES."
					
				* Write this memo to ClmNote\<claimnumb+".txt">
				IF FILE("ClmNote\" +ALLTRIM(Claims.ClaimNumb)+".txt")
					COPY MEMO Note TO ("ClmNote\" +ALLTRIM(Claims.ClaimNumb))	ADDITIVE
				ELSE
					COPY MEMO Note TO ("ClmNote\" +ALLTRIM(Claims.ClaimNumb))	
				ENDIF			
				
			ENDIF
			
			m.lcNoteSave=ALLTRIM(Claims.Note)
				
			DO WHILE .T.
				=INKEY()
				MODIFY MEMO NOTE WINDOW lwClaimMemo
				m.lnLastKey	=LASTKEY()
				
				DO CASE
				* [Esc]ape
				CASE m.lnLastKey =27
					REPLACE Claims.Note	WITH m.lcNoteSave
					EXIT
				
				* New Note is smaller than prior to editing
				CASE LEN(ALLTRIM(Claims.Note)) < LEN(m.lcNoteSave)
				
					IF YesNo("Reduce Notes???",0)
						EXIT
					ELSE
						REPLACE Claims.Note	WITH m.lcNoteSave
						m.lcNoteSave=ALLTRIM(Claims.Note)
					ENDIF
				
				* Otherwise: For example, [Ctrl+W]=23.
				OTHERWISE
					EXIT
					
				ENDCASE
					
			ENDDO
			
			CLOSE MEMO NOTE
			
			* Write this memo to ClmNote\<claimnumb+".txt">
			IF FILE("ClmNote\" +ALLTRIM(Claims.ClaimNumb)+".txt")
				COPY MEMO Note TO ("ClmNote\" +ALLTRIM(Claims.ClaimNumb))	ADDITIVE
			ELSE
				COPY MEMO Note TO ("ClmNote\" +ALLTRIM(Claims.ClaimNumb))	
			ENDIF			
			
				
			* If [Esc] was pressed, Exit.
			IF m.lnLastKey=27
				EXIT
			ENDIF
				
			IF m.lnLastKey=23
				DO closatty
				
				SELECT Claims
				* 05/17/2006 Steven Bennett
				*	Populate audit trial:  date, time, StaffCode 
				=ClaimAudit(mpolinumb, mclaimnumb, DATE(), TIME(), m.mStafCode)
				
				SELECT 0
				USE Message		&&	INDE messre
				SET ORDER TO Re
				
				SEEK Claims.ClaimNumb
				
				IF EOF()
					USE
						
					IF YesNo("Leave Reminder Note (Y/N) ?",20)
						SELECT claims
						mremind=calendar(CTOD("  /  /  "))
						mstaff=getstaff(istafcode)
						RELEASE WINDOW stafbrow
						
						SELECT 0
						USE MESSAGE		&&	INDE messto,messfrom,messtore,messre,messatty
						SET ORDER TO ToNoti
						APPEND BLANK
						
						DO WHILE .T.
						
							IF RLOCK()
								REPLACE TO 		WITH mstaff,	;
										FROM 	WITH "Claims    ",;
										re 		WITH TRIM(mclaimnumb),;
										claim 	WITH .T.,;
										senddate WITH DATE(),	;
										notidate WITH mremind,	;
										compcode WITH mcompcode
								REPLACE note 	WITH "Review notes for Claim " +ALLTRIM(mclaimnumb) +CHR(13) ;
													+"         Insured: " +Policy.Insured +CHR(13)	;
													+"Plaint v. Defend: " +ALLTRIM(mplaintiff) +" v. "+ALLTRIM(mdefendant) +CHR(13) +CHR(13)	;
													+msuitstat +CHR(13) +CHR(13)
								memofldnam="note"
								MODI MEMO NOTE WINDOW mmessmemo
								UNLOCK
								EXIT
							ENDIF
							
						ENDDO
						
						USE
						SELECT Claims
					ENDIF
					
				ELSE
					SELECT Message
					USE
					SELECT Claims
					DO programs\messrev WITH "CLAIMS", Claims.ClaimNumb, istafcode
					SELECT Claims
				ENDIF
					
				DO openatty
				
				SELECT claims
				myorn	=YesNo("Do you want to add a related party? (Y/N)",20)
					
				IF myorn=.T.
					m.jcMode="A"
					SELECT Claims
					APPEND BLANK
					morigrec	=RECNO()
											
*					IF AT(".",mclaimnumb)>0
*						mclaimnumb=LEFT(LEFT(mclaimnumb,AT(".",mclaimnumb))+SPACE(LEN(claims.claimnumb)), LEN(claims.claimnumb))
*					ELSE
*						mclaimnumb=LEFT(ALLTRIM(mclaimnumb)+"."+SPACE(LEN(claims.claimnumb)), LEN(claims.claimnumb))
*					ENDIF
					
					* Defaults
					mstatus="P"
					mclreserves=0
					mexreserves=0
					mclaimpaymt=0
					mexpenpaymt=0
					msubroex=0
					msubrocl=0
					* Ticket #431------------------------------------*
					* Reported to RISC
						mdaterepo={}
					* Insured Notified
						mdatenoti={}
					* Date of Loss
					    mdateloss={}
					* Licensee
						mPoliID=SPACE(LEN(Claims.PoliID))
					* Covered Insured?
						mcovered=SPACE(LEN(Claims.Covered))
					* Excess Notified?
						mlExsNotify=.F.
					* Insured ID
						mseverity=SPACE(LEN(Claims.Severity))
					* Claimant ID
						mInc_Type=SPACE(LEN(Claims.Inc_Type))
					* Last Demand
						mtSeeking=0.00
					* Loss Outcome
						mlossoutcm=SPACE(LEN(Claims.LossOutcm))
					* Status
						mstatus=SPACE(LEN(claims.status))
					* "CC" number
						mccnumb=SPACE(LEN(claims.ccnumb))
					* Attorney/Law Firm
						mattorney=SPACE(LEN(claims.attorney))
						mcompcode=SPACE(LEN(claims.compcode))
						mcompname=SPACE(LEN(company.compname))
				ELSE
					EXIT
				ENDIF
					
			ENDIF
				
		OTHERWISE
			EXIT
		ENDCASE
		
	ENDIF

ENDDO

SET UDFPARMS TO VALUE

DO CASE
CASE mentry="Policy"
	SELECT DataCode
	USE
	SELECT memotemp
	USE
	SELECT attorney
	USE
	SELECT company
	USE
	
CASE mentry="Poliind"
	SELECT DataCode
	USE
	SELECT memotemp
	USE
	SELECT attorney
	USE
	SELECT company
	USE
	
CASE mentry="Losses"
	SELECT DataCode
	USE
	SELECT attorney
	USE
	SELECT company
	USE
	
CASE mentry="Company Claims"
	SELECT DataCode
	USE
	SELECT attorney
	USE
	SELECT company
	GO mcomprec
	
ENDCASE


DO CASE
CASE m.llUsedPoliInd .AND. USED("PoliInd")
	SET ORDER TO (m.lcOrderPoliInd)

CASE !m.llUsedPoliInd .AND. USED("PoliInd")
	USE IN PoliInd
	
ENDCASE

*-----------------------------------------------*
* Claims Settlements
DO CASE
CASE m.llUsedClaiSetl .AND. USED("ClaiSetl")
	SET ORDER TO (m.lcOrderClaiSetl)

CASE !m.llUsedClaiSetl .AND. USED("ClaiSetl")
	USE IN ClaiSetl
	
ENDCASE

*-----------------------------------------------*
* Claims Property
DO CASE
CASE m.llUsedClaiProp .AND. USED("ClaiProp")
	SET ORDER TO (m.lcOrderClaiProp)

CASE !m.llUsedClaiProp .AND. USED("ClaiProp")
	USE IN ClaiProp
	
ENDCASE

*-----------------------------------------------*
IF USED("CNAClaim")
	USE IN CNAClaim
ENDIF

IF !m.llCourtStUsed
	USE IN CourtSt
ENDIF

SELECT Claims
SET ORDER TO (m.lcOrderClaims)
SET RELATION TO &lcRelaClaims
*SET RELA TO polinumb INTO policy

IF morigrec<>0
	mstart=morigrec
ENDIF

ON ERROR &lcOnError
CLEAR GETS
RETURN

*************************************************************************************************
*************************************************************************************************
*************************************************************************************************
*************************************************************************************************
*************************************************************************************************
FUNCTION Calc
******************************************
* Calc Percentages
******************************************
mtotal=mperres+mpercom+mperland+mperprop+mperappr+mperothr
mtitle="    % Total: "+LTRIM(STR(mtotal,10,2))+"    "
@19,40-LEN(mtitle)/2 SAY mtitle
RETURN .T.

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION p_id_whn
******************************************
*	Determines if policy is a group policy
*	and thus needs to have a poliid entered
******************************************
PARAMETERS mpolinumb
PRIVATE mpolinumb

mreturn=.F.
USE poliind IN 0 ALIAS temp ORDER tag polinum_id AGAIN

IF SEEK(mpolinumb,"TEMP")
	mreturn=.T.
ENDIF

USE IN temp
RETURN mreturn


*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE PoliID_Lst	&&	3/27/2001 mac
******************************************
*	creates a browse window of possible
*	poliids
******************************************
PARAMETERS mvalue,mvalue2
oalias = ALIAS()

IF !USED("POLIIND")
	SELECT 0
	USE poliind
	opn_file = .F.
ELSE
	SELECT poliind
	opn_file = .T.
ENDIF

oorder = ORDER()
SET ORDER TO polinum_id
SEEK mvalue+mvalue2

IF !FOUND()
	SEEK mvalue
ENDIF

IF FOUND()
	DEFINE WINDOW poliidwin FROM 5,5 TO 18,74 SYSTEM SHADOW COLOR SCHEME 8
	ON KEY
	ON KEY LABEL enter KEYBOARD CHR(23)
	BROWSE FIELDS polinumb:H="Policy #", poliid, firstname:H="First Name", ;
		lastname:H="Last Name",company:H="Company",;
		address1:H="Address1", ;
		address2:H="Address2", ;
		city, state, zip, origdate, effective, END, cancdate, ;
		reindate, renew, retroactiv:H="Retroactive", ;
		prem:H="Premium",license:H="License" ;
		KEY mvalue ;
		WINDOW poliidwin TITLE "Poliids" NOMODIFY
	ON KEY
	m.lnLastKey	=LASTKEY()
	
	IF m.lnLastKey=27
		*				IF mpolrec<>0
		*					GO mpolrec
		*				ENDIF
	ELSE
		mpoliid=poliind.poliid
	ENDIF
	*			mreturn = .t.
ELSE
	*			mreturn = .F.
ENDIF

USE
SELECT &oalias
RETURN



*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION PoliChk
******************************************
* Policy Number Check
******************************************
PARAMETERS mvalue
PRIVATE mfile,mvalue,mvalue2
mfile=ALIAS()
SELECT policy
SEEK mvalue

IF EOF()
	DEFINE WINDOW poliwin FROM 5,5 TO 18,74 SYSTEM SHADOW COLOR SCHEME 8
	SEEK TRIM(mvalue)
	
	IF EOF()
		SEEK SUBS(mvalue,1,2)
		
		IF EOF()
			SEEK SUBS(mvalue,1,1)
			
			IF EOF()
				GO TOP
			ENDIF
			
		ENDIF
		
	ENDIF
	
	ON KEY
	ON KEY LABEL enter KEYBOARD CHR(23)
	BROWSE FIELDS polinumb:H="Policy #", insured:H="Insured", compname :H="Company Name" WINDOW poliwin TITLE "Policies" NOMODIFY
	ON KEY
	m.lnLastKey	=LASTKEY()
*	=ClaimLabels()
	
	IF m.lnLastKey=27
		mreturn=.F.
	ELSE
		mreturn=.T.
		mstate=state
		mvalue=polinumb
		@13,28 SAY insured
	ENDIF
	
ELSE
	mstate=state
	mreturn=.T.
ENDIF

IF m.jcMode="A"
	DO getcomp
ENDIF

SELECT &mfile

IF morigrec<>0
	GO morigrec
ENDIF

RETURN mreturn



*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION PoliIDChk
******************************************
* Poliid Check
******************************************
PARAMETERS mvalue,mvalue2
PRIVATE mfile,mvalue,mvalue2
mfile=ALIAS()

IF !USED("POLIIND")
	SELECT 0
	USE poliind
	opn_file = .F.
ELSE
	SELECT poliind
	opn_file = .T.
ENDIF

oorder = ORDER()
SET ORDER TO polinum_id

IF !EMPTY(mvalue2)
	SEEK mvalue+mvalue2
	
	IF !FOUND()
		mreturn=.F.
	ELSE
		mpoliid = mvalue2
		@13,28.25+2 SAY getlicensee(mvalue,mvalue2)
		mreturn=.T.
	ENDIF
	
ELSE
	@13,28.25+2 SAY getlicensee(mvalue,mvalue2)
	mreturn = .T.
ENDIF

USE
SELECT &mfile
RETURN mreturn



*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION DataChk
******************************************
* Data chk
******************************************
PARAMETERS chktype,mvalue,blankok
PRIVATE mfile,chktype,mvalue,blankok

IF chktype="O" AND !(mstatus $ "C,D")
	blankok = .T.
ENDIF

*** jbl 11/1/2000 Added Case to handle Loss Outcome field ("O")
DO CASE   && case used because there is no index to seek for DESC; therefore used a LOCATE (small table)
* Loss Outcome -------------------------------------*
CASE chktype="O"	
	IF blankok .AND. LEN(TRIM(mvalue))=0
		mreturn=.T.
	ELSE
		mfile=ALIAS()
		SELECT DataCode
		LOCATE FOR TRIM(mvalue) = ALLTRIM(DESC)
		
		IF EOF() .OR. ALLTRIM(mvalue) <> ALLTRIM(DESC)
			LOCATE FOR TRIM(mvalue) = ALLTRIM(DESC)
			
			IF EOF()
				LOCATE FOR TRIM(mvalue) = ALLTRIM(DESC)
				
				IF EOF()
					GO TOP
				ENDIF
				
			ENDIF
			
			ON KEY
			ON KEY LABEL enter KEYBOARD CHR(23)
			BROWSE KEY chktype FIELDS code:H="Code", DESC:H="Description" WINDOW DataCode TITLE "Valid Codes" NOMODIFY
			ON KEY
			m.lnLastKey	=LASTKEY()
			
			IF m.lnLastKey=27
				mreturn=.F.
			ELSE
				mreturn=.T.
				mvalue = DESC
			ENDIF
			
		ELSE
			mreturn=.T.
		ENDIF
		
		SELECT &mfile
	ENDIF
			

* (I)nsured ID or (C)laimant ID Descriptions
CASE m.chktype="I" .OR. m.chktype="C"
		
		IF blankok .AND. LEN(TRIM(mvalue))=0
			mreturn=.T.
		ELSE
			mfile=ALIAS()
			SELECT DataCode
*			SEEK chktype+TRIM(mvalue)
			LOCATE FOR ALLTRIM(DataCode.CodeType)=ALLTRIM(m.ChkType) .AND. ALLTRIM(DataCode.Desc)=ALLTRIM(m.mValue)
				
			IF EOF() .OR. ALLTRIM(chktype)+ALLTRIM(mvalue)<>ALLTRIM(DataCode.CodeType)+ALLTRIM(DataCode.Desc)
*				SEEK chktype+TRIM(mvalue)
				LOCATE FOR ALLTRIM(DataCode.CodeType)=ALLTRIM(m.ChkType) AND ALLTRIM(DataCode.Desc)=ALLTRIM(m.mValue)
					
				IF EOF()
*					SEEK chktype+SUBS(mvalue,1,1)
					LOCATE FOR ALLTRIM(DataCode.CodeType)=ALLTRIM(m.ChkType)
						
					IF EOF()
						GO TOP
					ENDIF
						
				ENDIF
					
				ON KEY
				ON KEY LABEL enter KEYBOARD CHR(23)
				BROWSE KEY m.ChkType FIELDS DESC:H="Description" WINDOW DataCode2 TITLE "Valid Descriptions" NOMODIFY
				ON KEY
				m.lnLastKey	=LASTKEY()
					
				IF m.lnLastKey=27
					mreturn=.F.
				ELSE
					mreturn=.T.
					mvalue=DataCode.Desc
				ENDIF
					
			ELSE
				mreturn=.T.
			ENDIF
				
			SELECT &mfile
		ENDIF
			
* Suit Status ----------------------------------------------* 
CASE m.ChkType = "S"
*SET STEP ON 
		IF (blankok .AND. LEN(TRIM(mvalue))=0)	;
			.OR. LASTKEY()=9 					;
			.OR. LASTKEY()=15	&&	[Tab] .or. [Ctrl+Tab]
			mreturn=.T.
		ELSE
			m.lnSelect	=SELECT()
			SELECT DataCode
			m.lcOrder	=ORDER()
			SET ORDER TO CODEDESC   && UPPER(CODETYPE+DESC)
			=SEEK(m.ChkType)
			
			IF !EMPTY(mvalue) .AND. !SEEK(m.ChkType + m.mValue)
				=SEEK(m.ChkType)
			ENDIF
				
			DO WHILE .T.		
				ON KEY
				ON KEY LABEL enter KEYBOARD CHR(23)
				BROWSE KEY m.ChkType FIELDS x=ALLTRIM(code)+"  " + DataCode.Desc:H="Suit Status" WINDOW DataCode2 TITLE "Suit Status" NOMODIFY
				ON KEY
				m.lnLastKey	=LASTKEY()
						
				IF m.lnLastKey=27
					mreturn=.T.
					EXIT
				ELSE
					IF DataCode.Code="X"
						WAIT WINDOW NOWAIT "Do not select "+ALLTRIM(DataCode.Code)+" "+ALLTRIM(DataCode.Desc)+"!"
						LOOP
					ENDIF
				
					mreturn=.T.
					mvalue=UPPER(DataCode.Desc)
					EXIT
				ENDIF
			ENDDO
	
			SET ORDER TO (m.lcOrder)
			SELECT (m.lnSelect)
		ENDIF
			

* Otherwise ------------------------------------------------*
OTHERWISE
		
		IF blankok .AND. LEN(TRIM(mvalue))=0
			mreturn=.T.
		ELSE
			mfile=ALIAS()
			SELECT DataCode
			SEEK chktype+TRIM(mvalue)
			
			IF EOF() .OR. chktype+mvalue<>codetype+code
				SEEK chktype+TRIM(mvalue)
				
				IF EOF()
					SEEK chktype+SUBS(mvalue,1,1)
					
					IF EOF()
						GO TOP
					ENDIF
					
				ENDIF
				
				ON KEY
				ON KEY LABEL enter KEYBOARD CHR(23)
				BROWSE KEY chktype FIELDS code:H="Code", DESC:H="Description" WINDOW DataCode TITLE "Valid Codes" NOMODIFY
				ON KEY
				m.lnLastKey	=LASTKEY()
				
				IF m.lnLastKey=27
					mreturn=.F.
				ELSE
					mreturn=.T.
					mvalue=code
				ENDIF
				
			ELSE
				mreturn=.T.
			ENDIF
			
			SELECT &mfile
		ENDIF
		
ENDCASE
*=ClaimLabels()
RETURN mreturn



*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION CompChk
**********************************************
*   Company Check
**********************************************
PARAMETERS mvalue
PRIVATE mfile
mfile=ALIAS()
SELECT attorney
SEEK TRIM(mvalue)

IF EOF() .OR. mvalue<>attorney
	SEEK TRIM(mvalue)
	
	IF EOF()
		SEEK SUBS(mvalue,1,3)
		
		IF EOF()
			SEEK SUBS(mvalue,1,1)
			
			IF EOF()
				GO TOP
			ENDIF
			
		ENDIF
		
	ENDIF
	
	ON KEY
	
	DO WHILE .T.
		maction=""
		DO PROMPT WITH "Select Attorney or Contact / F10 to Add / ESC to Abandon"
		mprefix=""
		DO programs\alphseek
		ON KEY LABEL f10 DO programs\action WITH "ADD"
		ON KEY LABEL enter DO programs\action WITH "ENTER"
		
*		IF mcompowner="1004"
*			BROWSE FIELDS;
				attorney:H="Attorney",;
				company.compname:H="Company Name":33,;
				company.useagain:H="Use":p="Y";
				FOR company.cna;
				WINDOW company;
				TITLE "Valid Attorney's";
				NOMODIFY
*			BROWSE FIELDS;
				attorney:H="Attorney",;
				company.compname:H="Company Name":33,;
				company.useagain:H="Use":p="Y";
				FOR SEEK(compcode, "Company", "CompCode") .and. Company.cna=.T.;
				WINDOW company;
				TITLE "Valid Attorney's";
				NOMODIFY
*		ELSE
			BROWSE FIELDS;
				attorney:H="Attorney",;
				company.compname:H="Company Name":33,;
				company.useagain:H="Use":p="Y";
				WINDOW company;
				TITLE "Valid Attorneys";
				FOR Company.cna;
				NOMODIFY
*		ENDIF
		
		ON KEY
		m.lnLastKey	=LASTKEY()
*		=ClaimLabels()
		
		DO CASE
		CASE maction="ENTER"
			mreturn		=.T.
*			mcompcode	=compcode
			mcompcode	=Attorney.CompCode
			mattorney	=PADR(attorney.attorney,LEN(attorney.attorney))
			mcompname	=PADR(company.compname,LEN(company.compname))
			@30,50 SAY compphone(mcompcode)
			SHOW GETS
			mvalue=attorney
			EXIT
			
		CASE maction="ADD"
			DO programs\addcomp WITH mvalue
			
		CASE m.lnLastKey=27
			mreturn=.F.
			EXIT
			
		ENDCASE
	ENDDO
ELSE
	mreturn=.T.
ENDIF
SELECT &mfile
RETURN mreturn


*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION LossType
****************************
*   Losstype
****************************
PARAMETER mdateloss,mdisplay,mypolinumb
PRIVATE mdateloss,mresult,malias

DO CASE
CASE DTOC(mdateloss)="  /  /  "
	mresult="* Loss Date *"
	mdefastat=IIF(LEN(TRIM(mdefastat))=0,"Researching",mdefastat)
	
CASE mdateloss>=mpolieff .AND. mdateloss<=mpoliexp
*	mresult="*  Covered  *"
	mResult="* Dates w/in Effective *"
*	mdefastat=IIF(LEN(TRIM(mdefastat))=0,"Covered",mdefastat)
	mDefaStat=IIF(LEN(TRIM(mDefaStat))=0,"Dates w/in Effective",mDefaStat)
	
CASE mdateloss<GOMONTH(mpolieff,-120)
*	mresult="* Uncovered *"
	mResult="* Dates NOT w/in Effective *"
*	mdefastat=IIF(LEN(TRIM(mdefastat))=0,"Not Covered",mdefastat)
	mDefaStat=IIF(LEN(TRIM(mDefaStat))=0,"Dates NOT w/in Effective",mDefaStat)
	
CASE mdateloss>=policy.retrodate .AND. mdateloss<=mpolieff
	mresult="* Prior Act/DOL *"
	mdefastat=IIF(LEN(TRIM(mdefastat))=0,"Prior Act/DOL",mdefastat)
	
CASE mdateloss>mpoliexp
	malias=ALIAS()
	SELECT 0
	USE statecnt order policy
	SEEK SUBS(mypolinumb,11,2)+SUBS(mypolinumb,6,5)+SUBS(mypolinumb,14,2)+SUBS(mypolinumb,1,2)
	
	IF EOF()
*		mresult="* Uncovered *"
		mResult="* Dates NOT w/in Effective *"
*		mdefastat=IIF(LEN(TRIM(mdefastat))=0,"Not Covered",mdefastat)
		mDefaStat=IIF(LEN(TRIM(mDefaStat))=0,"Dates NOT w/in Effective",mDefaStat)
	ELSE
		
		IF mdateloss>mpoliexp .AND. mdateloss<=mpoliexp+mextenper
*			mresult="*    Tail   *"
			mResult="* Tail Endorsement *"
			mdefastat=IIF(LEN(TRIM(mdefastat))=0,"Covered Tail",mdefastat)
			mDefaStat=IIF(LEN(TRIM(mDefaStat))=0,"Tail Endorsement",mDefaStat)
		ELSE
*			mresult="* Uncovered *"
			mResult="* Dates NOT w/in Effective *"
*			mdefastat=IIF(LEN(TRIM(mdefastat))=0,"Not Covered",mdefastat)
			mDefaStat=IIF(LEN(TRIM(mDefaStat))=0,"Dates NOT w/in Effective",mDefaStat)
		ENDIF
		
	ENDIF
	
	USE
	SELECT &malias
	
ENDCASE

*mdefastat=PADR(mdefastat,15)
mdefastat=PADR(mdefastat,30)
RETURN .T.



*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION ClaimChk
******************************************
* Check for Unique Claim Number
******************************************
PARAMETERS mvalue
PRIVATE mvalue, m.lcOrder, mReturn
m.lcOrder	=ORDER()

	IF mstatus="I"
	
		IF SUBS(mvalue,1,4)="INC-"
			mreturn=.T.
		ELSE
			WAIT "Incidents must begin with INC- and may be followed with four digits" WINDOW
			mreturn=.F.
		ENDIF
		
*		SET ORDER TO 3
		SET ORDER TO CLAIMNUMB   && UPPER(CLAIMNUMB)
		SEEK mvalue
		
		IF EOF() .OR. morigrec=RECNO()
			mreturn=.T.
		ELSE
*			WAIT "Incident Number "+mvalue+" is in use" WINDOW
			m.lcClaimChkError="Incident# "+ALLTRIM(mValue)+" is being used by"+CHR(13)+"Policy# "+ALLTRIM(polinumb)+"/"+ALLTRIM(poliid)+" "+ALLTRIM(Insured)+"."
			mreturn=.F.
		ENDIF
	
	*	SET ORDER TO morder
		SET ORDER TO (m.lcOrder)
	
		IF morigrec<>0
			GO morigrec
		ENDIF
		
	ELSE
*		SET ORDER TO 3
		SET ORDER TO CLAIMNUMB   && UPPER(CLAIMNUMB)

		SEEK mvalue
	
		IF EOF() .OR. morigrec=RECN()
			mreturn=.T.
		ELSE
*			WAIT "Claim Number "+mvalue+" is being used for Policy # "+polinumb WINDOW
			m.lcClaimChkError="Claim# "+ALLTRIM(mValue)+" is being used by"+CHR(13)+"Policy# "+ALLTRIM(polinumb)+"/"+ALLTRIM(poliid)+" "+ALLTRIM(insured)+"."
			mreturn=.F.
		ENDIF
		
	*	SET ORDER TO morder
		SET ORDER TO (m.lcOrder)
		
		IF morigrec<>0
			GO morigrec
		ENDIF
		
	ENDIF


RETURN mreturn



*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION Incurred
**************************
* Incurred
**************************
*mresrtot=IIF(STATUS$"OR",mclreserves+mexreserves,(mclaimpaymt+mexpenpaymt)-(msubrocl+msubroex))
mresrtot=IIF(mSTATUS$"OR",mclreserves+mexreserves,(mclaimpaymt+mexpenpaymt)-(msubrocl+msubroex))

IF mstatus="C"
	
	IF (mclreserves +mexreserves) <>(mclaimpaymt +mexpenpaymt) -(msubrocl+msubroex)
		@22,54 SAY "*"
	ENDIF
	
ELSE
	@22,54 SAY " "
ENDIF

* Display a notice if Damages Reserves are >= 35k
*	.OR.
*	if Total Reserves are >= 50k
IF (mresrtot >=50000) .OR. (mclreserves >=35000)
	PRIVATE m.lcNotifyDateTime, m.lcTime
	m.lcNotifyDateTime	=DTOC(DATE()+1)		&& get tomorrow's date
	m.lcTime	=LEFT(TIME(), 5)			&& get current hrs:mins
	
	* Convert 24hr time to 12hr time and append am/pm
	DO CASE
	CASE BETWEEN(m.lcTime, "00:00", "11:59")
		m.lcNotifyDateTime	=m.lcNotifyDateTime +" "+m.lcTime+"am"
		
	CASE BETWEEN(m.lcTime, "12:00", "23:59")
	
		IF BETWEEN(LEFT(m.lcTime,2), "13", "23")
			m.lcTime	=ALLTRIM(STR(VAL(LEFT(m.lcTime, 2))-12)) +SUBSTR(m.lcTime, 3, 3)
		ENDIF
		
		m.lcNotifyDateTime	=m.lcNotifyDateTime +" "+m.lcTime+"pm"
	ENDCASE
	
	DO CASE
	* Reserve Total is >= 50k
	CASE mresrtot >= 50000
		WAIT WINDOW "Total Reserve exceeds 50K!!" + CHR(13) +"Please notify CNA by " +m.lcNotifyDateTime +"." +CHR(13) +"Press any key to continue..."
	* Reserve Damages is >= 35k
	CASE mclreserves >= 35000
		WAIT WINDOW "Damage Reserve exceeds 35K!!" + CHR(13) +"Please notify CNA by " +m.lcNotifyDateTime +"." +CHR(13) +"Press any key to continue..."
	ENDCASE
	
ENDIF


*-------------------------------------------*	
* Reserves (Damages .or. Expenses) must be
*	greater than or equal to
*	Payments - Recovery
DO CASE
* Damages
CASE !(mClReserves >=(mClaimPaymt - mSubroCl))
	WAIT WINDOW "Minimum Damage Reserve allowed is $"+ ALLTRIM(STR((mClaimPaymt - mSubroCl), 11, 2))
* Expenses
CASE !(mExReserves >=(mExpenPaymt - mSubroEx))
	WAIT WINDOW "Minimum Expense Reserve allowed is $"+ ALLTRIM(STR((mExpenPaymt - mSubroEx), 11, 2))
ENDCASE

RETURN .T.


*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE Aggregate		
	*************************
	* Aggregate		&& 1/22/2004 mac
	*************************
	oalias = alias()
	oorder = order()
	usedclm = used('claims')
	oordclm = 0
	usedfees = used('fees')
	oordfees = 0
	mDamages = 0
	mExpenses = 0
	if usedclm then
		select claims
		oordclm = order()
		use claims order polinum_id in 0 again alias tmpclaims
	else
		use claims order polinum_id in 0 alias tmpclaims
	endif
	if usedfees then
		select fees
		oordfees = order()
		USE fees IN 0 ORDER claimnumb AGAIN ALIAS tmpfees
	else
		USE fees IN 0 ORDER claimnumb ALIAS tmpfees
	endif
*	IF EMPTY(claims.poliid) AND SUBSTR(claims.polinumb,11,2)#"MS"
*		use in tmpclaims
*		use in tmpfees
*		if usedclm
*			SELECT claims
*			set order to oordclm
*		endif
*		if usedfees
*			select fees
*			set order to oordfees
*		endif
*		select &oalias
*		set order to oorder
*		RETURN
*	ENDIF

	SELECT tmpclaims
	SET RELATION TO claimnumb INTO tmpfees
	SET SKIP TO tmpfees
	SEEK claims.polinumb+claims.poliid
		
	SCAN WHILE tmpclaims.polinumb=claims.polinumb AND tmpclaims.poliid=claims.poliid
	
		IF !TmpFees.lRemoved
			IF tmpfees.acctnum="60199000001000"	;
			.OR. TmpFees.AcctNum="Damages"
				mDamages=mDamages+tmpfees.amount
			ENDIF
			
			IF tmpfees.acctnum="80130000000050"	;
			.OR. TmpFees.AcctNum="Claims Expense"
				mExpenses=mExpenses+tmpfees.amount
			ENDIF
		ENDIF
				
	ENDSCAN
	
	USE IN tmpclaims
	USE IN tmpfees
	SELECT &oalias
	set order to oorder
	
	mDamages	=(mDamages - mSubroCl)
	mExpenses	=(mExpenses - mSubroEx)
	return


*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION SeekIt
	**************************
	* Seekit
	**************************
	PARAMETER mprefix
	PRIVATE mprefix,mtest
	ON KEY
	DEFINE WINDOW seekit FROM 10,20 TO 14,60
	ACTIVATE WINDOW seekit
	mtest=SPAC(15)
	m.lnLastKey	=LASTKEY()
	KEYBOARD CHR(m.lnLastKey)
	@1,5 SAY "Find:" GET mtest PICT "@!"
	READ
	SEEK mprefix+TRIM(mtest)
	DEACTIVATE WINDOW seekit
	DO programs\alphseek
	ON KEY LABEL f10 DO programs\action WITH "ADD"
	ON KEY LABEL enter DO programs\action WITH "ENTER"
	RETURN

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE GetStaff
	**************************
	* Getstaff
	**************************
	PARAMETER mstaff
	PRIVATE mstaff, m.lnSelect
*	malias=ALIAS()
	m.lnSelect	=SELECT()
	
	SELECT 0
	USE staff AGAIN ALIAS GetStaff 	&& INDE stafcode
	SET ORDER TO CODE   && UPPER(CODE)
	SEEK mstaff
	DEFINE WINDOW stafbrow FROM 4,30 TO IIF(RECC()>10,RECC()+3,14),44 SYSTEM SHADOW COLOR SCHEME 8 TITLE "To Who?"
	ON KEY LABEL enter KEYBOARD CHR(23)
	BROWSE FIELDS code:H="Staff" WINDOW stafbrow NOEDIT	NODELETE
	ON KEY
	m.lnLastKey	=LASTKEY()
	
	IF m.lnLastKey=23
		mstaff=code
	ELSE
		mstaff=istafcode
	ENDIF
	
	USE IN GetStaff
*	SELECT &malias
	SELECT (m.lnSelect)
*	=ClaimLabels()
	RETURN mstaff

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE OpenAtty
	******************
	* Openatty
	******************
	SELECT 7
	USE company INDE compcode,compname,compstci	&& 3/20/2004 mac
	SET ORDER TO PTypCode
*	use company order tag ptypcode
	SELECT 8
	USE attorney INDE attoname,attocomp	&& 3/20/2004
*	use attorney order tag attorney
	SET RELA TO "A"+compcode INTO company
	RETURN

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE ClosAtty
	******************
	* Closatty
	******************
	SELECT company
	USE
	SELECT attorney
	USE
	RETURN

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE DateProc
	********************
	* Dateproc
	********************
	PRIVATE m
	DEFINE WINDOW datechk FROM 4,10 TO 22,70 SYSTEM SHADOW COLOR SCHEME 8
	ACTIVATE WINDOW datechk
	DO WHILE .T.
		@0,2 SAY "Policy / Claim Date Information"
		@1,0 SAY REPL("Ä",WCOL())
*		IF m.test_mode
*			@2,1 SAY "         Original Effective Date: " GET policy.effective WHEN .F.
*			@3,1 SAY "          Current Effective Date: " GET policy.start WHEN .F.
*			@4,1 SAY "         Current Expiration Date: " GET policy.END WHEN .F.
*			@5,1 SAY "                Retroactive Date: " GET policy.retro WHEN .F.
*			@6,1 SAY "               Cancellation Date: " GET policy.canceled WHEN .F.
*			@7,1 SAY "                  Reinstate Date: "
*		ELSE
			* If this Claim's PoliNumb + PoliID is found in PoliInd, display
			*		PoliInd data because it is more specific.
*			IF USED("PoliInd") .AND. FOUND("PoliInd")
*				@1,10 SAY "[Individual's Policy Dates]"
*				@2,1 SAY "         Original Effective Date: " GET PoliInd.OrigDate 	WHEN .F.
*				@3,1 SAY "          Current Effective Date: " GET PoliInd.effective WHEN .F.
*				@4,1 SAY "         Current Expiration Date: " GET PoliInd.End 		WHEN .F.
*				@5,1 SAY "                Retroactive Date: " GET PoliInd.RetroActiv WHEN .F.
*				@6,1 SAY "               Cancellation Date: " GET PoliInd.cancdate 	WHEN .F.
*				@7,1 SAY "                  Reinstate Date: " GET PoliInd.reindate 	WHEN .F.
*			ELSE
*				@1,10 SAY "[General Policy Dates]"
*				@2,1 SAY "         Original Effective Date: " GET policy.origdate WHEN .F.
*				@3,1 SAY "          Current Effective Date: " GET policy.effecdate WHEN .F.
*				@4,1 SAY "         Current Expiration Date: " GET policy.TO WHEN .F.
*				@5,1 SAY "                Retroactive Date: " GET policy.retrodate WHEN .F.
*				@6,1 SAY "               Cancellation Date: " GET policy.cancdate WHEN .F.
*				@7,1 SAY "                  Reinstate Date: " GET policy.reindate WHEN .F.
*			ENDIF
			IF USED("PoliInd") .AND. FOUND("PoliInd")
				@1,10 SAY "[Individual's Policy Dates]"
				@2,1 SAY "         Original Effective Date: " +DTOC(PoliInd.OrigDate)
				@3,1 SAY "          Current Effective Date: " +DTOC(PoliInd.effective)
				@4,1 SAY "         Current Expiration Date: " +DTOC(PoliInd.End) 
				@5,1 SAY "                Retroactive Date: " +DTOC(PoliInd.RetroActiv)
				@6,1 SAY "               Cancellation Date: " +DTOC(PoliInd.cancdate)
				@7,1 SAY "                  Reinstate Date: " +DTOC(PoliInd.reindate)
			ELSE
				@1,10 SAY "[General Policy Dates]"
				@2,1 SAY "         Original Effective Date: " +DTOC(policy.origdate) 
				@3,1 SAY "          Current Effective Date: " +DTOC(policy.effecdate)
				@4,1 SAY "         Current Expiration Date: " +DTOC(policy.TO)
				@5,1 SAY "                Retroactive Date: " +DTOC(policy.retrodate)
				@6,1 SAY "               Cancellation Date: " +DTOC(policy.cancdate) 
				@7,1 SAY "                  Reinstate Date: " +DTOC(policy.reindate) 
			ENDIF
*		ENDIF
		@8,0 SAY REPL("Ä",WCOL())
		@09,1 SAY "Date Incident Converted to Claim: " GET mdateclaim
		@10,1 SAY " Effective Date at Time of Claim: " GET mpolieff
		@11,1 SAY "Expiration Date at Time of Claim: " GET mpoliexp
		@12,1 SAY " Report Date (RISC told of loss): " GET mdaterepo	
		@13,1 SAY "       Ph Notified (date served): " GET mdatenoti	VALID GetCNAClaim(mpolinumb, mdatenoti)
		@14,1 SAY "       Date of Loss (trans date): " GET mdateloss
		@15,0 SAY REPL("Ä",WCOL())
*		@16,1 SAY "            Default Claim Status: " GET mdefastat PICT "@M Covered,Prior Act,Not Covered,Covered Ext,Covered Tail,Researching" WHEN losstype(mdateloss,.F.,mpolinumb)
		@16,1 SAY "     Default Claim Status: " GET mdefastat PICT "@M Auto ERP,Dates w/in Effective,Dates NOT w/in Effective,Prior Act/DOL,Researching,Tail Endorsement" WHEN losstype(mdateloss,.F.,mpolinumb)
* Run C:\temp\claiupdt.prg to covert Claims.DefaStat data as following:
*Covered Ext	Auto ERP
*Covered		Dates w/in Effective
*Not Covered	Dates NOT w/in Effective
*Prior Act		Prior Act/DOL
*Researching	Researching
*Covered Tail	Tail Endorsement
		
		READ
		m.lnLastKey	=LASTKEY()
		
		* [Esc] .or. [Ctrl+W]
*		IF (LASTKEY()=27 .OR. LASTKEY()=23)
		IF (m.lnLastKey=27 .OR. m.lnLastKey=23)
			
			DO CASE
			CASE mDateLoss>mDateNoti .AND. !EMPTY(mDateNoti)
				WAIT WINDOW NOWAIT '"Date of Loss" must be <= "Ph Notified"' +CHR(13)+'Try again...'
			
			CASE mDateNoti>mDateRepo .AND. !EMPTY(mDateRepo)
				WAIT WINDOW NOWAIT '"Ph Notified" must be <= "Report Date"' +CHR(13)+'Try again...'
			
			OTHERWISE
				EXIT
				
			ENDCASE
			
		ENDIF
		
	ENDDO
	
	DEACTIVATE WINDOW datechk
	RELEASE WINDOW datechk
	SHOW GETS
	@9,56 SAY mdefastat
	RETURN .F.

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE StatChk
	PARAMETER m.xcStatus
	PRIVATE m.llReturn
			
	IF m.xcStatus$"DORCIP"	&& Denied / Open / Reopen / Closed / Incident / Party
		m.llReturn	=.T.
	ELSE
		m.llReturn	=.F.
	ENDIF
	
	IF Claims.Status="C" .AND. m.xcStatus="R"
		REPLACE Claims.Note WITH Claims.Note+CHR(10)+CHR(10)+DTOC(DATE())+"  Case Originally Closed "+DTOC(mdateclosed)+"... Reopened "+DTOC(DATE())
		mdateclosed	=CTOD("  /  /  ")
		SHOW GET mdateclosed
	ENDIF
	
	IF Claims.Status$"DOI" .AND. m.xcStatus="R"
		WAIT WINDOW "You Can Only REOPEN a Closed Case" 
		=MESSAGEBOX("Note:  you may only REOPEN a Closed case!",48,"Not Closed!")
		m.llReturn	=.F.
	ENDIF
	
	IF m.xcStatus="P"
		REPLACE Claims.Note WITH Claims.Note +CHR(10)+CHR(10) +DTOC(DATE()) + " - Party to other suit"
	ENDIF
	
	IF Claims.Status="C" .AND. m.xcStatus="R"	;
		.OR. m.xcStatus="P"
		
		* Write this memo to ClmNote\<claimnumb+".txt">
		IF FILE("ClmNote\" +ALLTRIM(Claims.ClaimNumb)+".txt")
			COPY MEMO Claims.Note TO ("ClmNote\" +ALLTRIM(Claims.ClaimNumb))	ADDITIVE
		ELSE
			COPY MEMO Claims.Note TO ("ClmNote\" +ALLTRIM(Claims.ClaimNumb))	
		ENDIF			
		
	ENDIF
	
	* Closing the claim
	IF m.xcStatus = "C"
		IF EMPTY(mdateclosed)
			mdateclosed	=DATE()
		ENDIF
		SHOW GET mdateclosed
		
		=MESSAGEBOX("Note:  confirm Closed date.",48,"Closed date-->"+DTOC(mdateclosed))
	ENDIF
	
	RETURN m.llReturn

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION GetLicensee
	PARAMETERS mpolinumb,mpoliid
	PRIVATE mreturn
	mreturn	=""
	malias	=ALIAS()
	IF EMPTY(mpoliid)
		USE policy IN 0 ALIAS temp ORDER polinumb AGAIN
		IF SEEK(mpolinumb,"temp")
			mreturn=ALLTRIM(temp.insured)
		ENDIF
		USE IN temp
	ELSE
		USE poliind IN 0 ALIAS temp ORDER polinum_id AGAIN
		IF SEEK(mpolinumb+mpoliid,"temp")
			mreturn=ALLTRIM(temp.firstname)+" "+ALLTRIM(temp.lastname)
		ENDIF
		USE IN temp
		USE claims IN 0 ALIAS temp ORDER polinum_id AGAIN
		SELECT temp
		SEEK mpolinumb+mpoliid
		IF FOUND()
			SKIP
			IF polinumb+poliid=mpolinumb+mpoliid
				mreturn=mreturn+" (Multiple Claims)"
			ENDIF
		ENDIF
		USE IN temp
	ENDIF
	SELECT (malias)
*	RETURN PADR(mreturn,50)
	RETURN PADR(mreturn,40)

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE getcomp
	***************
	* Getcomp
	***************
	PRIVATE malias
	IF m.test_mode
		mrealcomp=policy.company
	ELSE
		malias=ALIAS()
		SELECT 0
		USE statecnt
*		LOCATE FOR state=SUBS(policy.polinumb,11,2) .AND. policode=SUBS(policy.polinumb,4,2) .AND. polisuf=SUBS(policy.polinumb,15,2)
		LOCATE FOR state=SUBS(policy.polinumb,11,2) .AND. policode=SUBS(policy.polinumb,4,2) .AND. polisuf=SUBS(policy.polinumb,14,2)
		IF .NOT. EOF()
			IF insured
				mrealcomp=policy.compname
			ELSE
				mrealcomp=policy.insured
			ENDIF
		ELSE
			mrealcomp=SPAC(30)
		ENDIF
		USE
		SELECT (malias)
	ENDIF
	RETURN

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE prerealcomp
	**************
	* Prerealcomp
	**************
	ON KEY LABEL f10 DO getrealcomp
	DO PROMPT WITH "Press F10 to Select Realty Company"
	RETURN

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE getrealcomp
	**************
	* Getrealcomp
	**************
	PRIVATE malias,morder,minsured,mrelation,mtarget
	ON KEY
	mrelation=RELATION(1)
	mtarget=TARGET(1)
	malias=ALIAS()
	SELECT 0
	USE statecnt
	LOCATE FOR state=SUBS(mpolinumb,11,2) .AND. policode=SUBS(mpolinumb,4,2) .AND. polisuf=SUBS(mpolinumb,15,2)
	minsured=insured
	USE
	SELECT policy
	morder=ORDER()
	IF minsured
		SET ORDER TO polistco
	ELSE
		SET ORDER TO polistin
	ENDIF
	mstate=SUBS(polinumb,11,2)
	DO WHILE .T.
		maction=" "
		ON KEY LABEL F DO programs\action WITH "SRCHIT"
		ON KEY LABEL enter KEYBOARD CHR(23)

		IF minsured
			BROW KEY mstate FIELDS insured,compname WINDOW company NOEDIT NOMENU
		ELSE
			BROW KEY mstate FIELDS compname,insured WINDOW company NOEDIT NOMENU
		ENDIF
		
		m.lnLastKey	=LASTKEY()
*		=ClaimLabels()
		
		DO CASE
			CASE maction="SRCHIT"
				DO srchit
			CASE m.lnLastKey=23 .OR. m.lnLastKey=27
				EXIT
		ENDCASE
	ENDDO

	IF m.lnLastKey=23
		IF minsured
			mrealcomp=compname
		ELSE
			mrealcomp=insured
		ENDIF
		SHOW GET mrealcomp
	ENDIF
	ON KEY
	SET ORDER TO morder
	SELECT (malias)
	IF LEN(TRIM(mrelation))<>0
		SET RELATION TO &mrelation INTO (mtarget)
	ENDIF
	DO prerealcomp
	RETURN

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE postrealcomp
	**************
	* Postrealcomp
	**************
	ON KEY
	DO PROMPT WITH "Enter Information / CTRL-W to Save / ESC to Abandon"
	RETURN

*--------------------------------------------------------------------------------------------------------------------------------------
PROCEDURE srchit
	ON KEY
	ACTIVATE WINDOW getinfo
	SET CONFIRM ON
	morigstate=mstate
	mname=SPAC(30)
	@1,1 SAY "St:" GET mstate PICT "@!" VALID chkstate(@mstate)
	@1,COL()+2 SAY "Name:" GET mname PICT "@S10!"
	READ
	SET CONFIRM OFF
	DEACTIVATE WINDOW getinfo
	IF morigstate<>mstate
		malias=ALIAS()
		SELECT 0
		USE statecnt
		LOCATE FOR state=mstate
		minsured=insured
		USE
		SELECT (malias)
	ENDIF
	IF minsured
		SET ORDER TO polistco
	ELSE
		SET ORDER TO polistin
	ENDIF
	SEEK mstate+mname
	IF EOF()
		SEEK mstate
		IF EOF()
			GO TOP
		ENDIF
	ENDIF
	RETURN

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION bmlspell
	PARAMETER local_text
	IF fs_spell(" ", @local_text, .F.)
		RETURN local_text
	ENDIF
	RETURN local_text

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION compphone
	PARAMETERS mcompcode
	PRIVATE mreturn, m.lnSelect
	m.lnSelect	=SELECT()
	mreturn=""
	SELECT 0
	USE company AGAIN ALIAS temp 
	IF SEEK("A"+mcompcode,"temp","PTypCode")
		mreturn=temp.phone
	ENDIF
	USE IN temp
	SELECT (m.lnSelect)
	RETURN mreturn

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION GetCNAClaim
PARAMETERS m.jcPoliNumb, m.jdDateNoti

IF !EMPTY(m.jcPoliNumb) .AND. !EMPTY(m.jdDateNoti)
	PRIVATE m.lnSelect
	m.lnSelect	=SELECT()
	m.llCNAClaimUsed	=USED("CNAClaim")
	
	IF m.llCNAClaimUsed
		SELECT CNAClaim
	ELSE
		SELECT 0
		USE CNAClaim
	ENDIF
	
*	LOCATE FOR ALLTRIM(m.jcPoliNumb)$CNAClaim.PoliNumb .AND. BETWEEN(m.jdDateNoti, CNAClaim.dEffective, CNAClaim.dEnd-1)
	* Steven Bennett:  We started having the actual last date of coverage in the CNAClaim.dEnd field rather than the start of the next year.
	*		For example:
	*			Instead of this:   01/01/2014  01/01/2015
	*			 we now do this:   01/01/2014  12/31/2014
	LOCATE FOR ALLTRIM(m.jcPoliNumb)$CNAClaim.PoliNumb .AND. BETWEEN(m.jdDateNoti, CNAClaim.dEffective, CNAClaim.dEnd)
	
	IF FOUND()
		PRIVATE m.llInput
		m.llInput	=.F.
		
		DO CASE
		* Inputing CNA Claim# into blank field
		CASE EMPTY(m.mccnumb)
			m.llInput	=.T.

		* Pre-existing CNA Claim# differs from CNAClaim.cClaimNum
		CASE!EMPTY(m.mccnumb) .AND. ALLTRIM(m.mccnumb) # ALLTRIM(CNAClaim.cClaimNum)
			WAIT WINDOW "'" +ALLTRIM(CNAClaim.cClaimNum) +"' is the correct CNA Claim# for this Notification Date/Group Policy!"	;
				+CHR(13)+"PLEASE NOTIFY Deb Lanham IF YOU ELECT TO CHANGE IT!"	;
				+CHR(13)+"(Press any key...)"
		ENDCASE
			
		IF m.llInput
			m.mccnumb	=CNAClaim.cClaimNum
			SHOW GET m.mccnumb
		ENDIF
		
	ELSE
		WAIT WINDOW NOWAIT "No CNA Claim number for 'Insured Notified Date' of " +DTOC(m.jdDateNoti) +" and 'Group Policy' of " +ALLTRIM(m.jcPoliNumb) +CHR(13)+"found in CNAClaim.DBF."
	ENDIF

	SELECT (m.lnSelect)
ENDIF

RETURN .T.


*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION ChgCNAClaim
IF !EMPTY(m.mccnumb) .AND. (ALLTRIM(m.mccnumb) # ALLTRIM(Claims.ccnumb))
	PRIVATE m.lcLine1, m.lcLine2, m.lcLine3, m.lcLine4, m.lcLine5
	m.lcLine2	=" you have changed this claim's CNA Claim# ('RE8 number')"	
	m.lcLine3	="        from " +ALLTRIM(Claims.ccnumb) +" to " +ALLTRIM(m.mccnumb) +"."	
	m.lcLine4	=""
	m.lcLine5	="Thank you. (Press any key...)"
	
	FOR i=1 TO 3
		m.lcLine1	=" ATTENTION ("+ALLTRIM(STR(i))+" of 3):  Please notify Deb Lanham that"
		WAIT WINDOW  m.lcLine1	;
			+CHR(13)+m.lcLine2	;
			+CHR(13)+m.lcLine3	;
			+CHR(13)+m.lcLine4	;
			+CHR(13)+m.lcLine5
	NEXT	
ENDIF

RETURN .T.

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION ClaimAudit
PARAMETERS m.tcPoliNumb, m.tcClaimNumb, m.tdDate, m.tcTime, m.tcStaffCode
PRIVATE m.lnSelect, m.llClaimsAu

m.lnSelect		=SELECT()
m.llClaimsAu	=USED("ClaimsAu")

IF m.llClaimsAu
	SELECT ClaimsAu
ELSE
	SELECT 0
	USE ClaimsAu
ENDIF

APPEND BLANK
REPLACE ClaimsAu.PoliNumb	WITH m.tcPoliNumb,	;
		ClaimsAu.ClaimNumb	WITH m.tcClaimNumb,;
		ClaimsAu.dDateEdit	WITH m.tdDate,		;
		ClaimsAu.cTimeEdit	WITH m.tcTime,		;
		ClaimsAu.cCodeEdit	WITH m.tcStaffCode

IF !m.llClaimsAu
	USE
ENDIF

SELECT (m.lnSelect)
RETURN .T.

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION YesNo
parameter mtitle,mrowno,myes
mcurwin=wontop()
mcol1=40-len(mtitle)/2-5
mcol2=40+len(mtitle)/2+5
mcol1=iif(mcol1>30,30,mcol1)
mcol2=iif(mcol2<50,50,mcol2)
mcent=(mcol2-mcol1)/2
if mrowno=0
    mrowno=10
endif
define window yesno from mrowno,mcol1 to mrowno+2,mcol2 double title mtitle shadow color scheme 2

activate window yesno

option=2
if myes
    option=1
endif
@0,mcent-5 prompt "YES"
@0,mcent+2 prompt "NO"
menu to option
deactivate window yesno
do case
case mcurwin="Stub Information"
    mcurwin=""
endcase
release window yesno
if len(mcurwin)=0
    activate screen
else
    activate window (mcurwin)
endif

return iif(option=1,.t.,.f.)

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION ToSettlements
PRIVATE m.lnLastKey
m.lnLastKey	=LASTKEY()

IF m.lnLastKey=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

*IF "2.6"$VERSION()
*	SET STEP ON 
*	DO Screens\ClaiSetl.spr
*PUBLIC m.xcbClaimKey, m.xcClaimNumb
*m.xcbClaimKey	=Claims.cbKey
*m.xcClaimNumb	=mclaimnumb
PUSH KEY
DO Form FORMS\CLAISETL.SCX 	WITH Claims.cbkey, mclaimnumb
POP KEY

*ELSE
*	DO FORM ClaiSetl
*ENDIF

SHOW GET m.lnSettlements,1	PROMPT IIF(EOF("ClaiSetl"), "-none-", "$"+ALLTRIM(STR(ClaiSetl.tSettleAmt,11,2)) +" on "+DTOC(ClaiSetl.dDate)+IIF(!EMPTY(ClaiSetl.mNote),"+",""))
GO RECNO()
=ShowNoteExcerpt(37+m.lnX, 55, 32.5)

RETURN -1

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION ToProperty
PRIVATE m.lnLastKey
m.lnLastKey	=LASTKEY()

IF m.lnLastKey=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

*IF "2.6"$VERSION()
	DO Screens\ClaiProp.spr
*ELSE
*	DO FORM ClaiProp
*ENDIF

*SHOW GET m.lnProperty,1	PROMPT IIF(EOF("ClaiProp"), "-none-", ALLTRIM(ClaiProp.cAdd1))
SHOW GET m.lnProperty,1	PROMPT IIF(EOF("ClaiProp"), "-none-", ALLTRIM(ClaiProp.cAdd1) +" (1/" +ALLTRIM(STR(PropCount(Claims.ClaimNumb)))+")")
RETURN -1

*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION SetCNA
PRIVATE m.lnLastKey
m.lnLastKey	=LASTKEY()

IF m.lnLastKey=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

IF (mtSeeking>=50000 .OR. ClaiSetl.tSettleAmt>=50000)
	PRIVATE m.llMsg
	m.llMsg	=.F.	&&	default to FALSE
		
*	IF EMPTY(mdCNARepo)
*		mdCNARepo	=DATE()
*	ENDIF
		
	IF !mlCNANotify
*?		mlCNANotify	=.T.
		m.llMsg		=.T.
		PRIVATE m.lcNotice
		
		IF ClaiSetl.tSettleAmt>=50000
			m.lcNotice	="demanding $" +ALLTRIM(STR(ClaiSetl.tSettleAmt, 12, 2))
		ELSE
			m.lcNotice	="seeking $" +ALLTRIM(STR(mtSeeking, 12, 2))
		ENDIF
		
	ENDIF
	
	SHOW GET mlCNANotify	ENABLED
	SHOW GET mdCNARepo		ENABLED
	
	IF m.llMsg
		WAIT WINDOW "Please notify CNA that Insured is "+m.lcNotice +CHR(13) +"(Press any key to continue...)"
	ENDIF
	
ELSE
	mlCNANotify	=.F.
	mdCNARepo	={}
	SHOW GET mlCNANotify	DISABLED
	SHOW GET mdCNARepo		DISABLED
ENDIF

RETURN .T.
	
	
*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION EditSeeking
PRIVATE m.lnLastKey
m.lnLastKey=LASTKEY()

IF m.lnLastKey=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

PRIVATE m.lmSeeking, m.lwEditSeeking
m.lmSeeking	=Claims.mSeeking
DEFINE WINDOW lwEditSeeking FROM 13,1 TO 30,78 TITLE "Seeking text:  [Ctrl+W]=Save, [Esc]=Cancel" SHADOW COLOR SCHEME 10 FLOAT

DO WHILE .T.
	MODIFY MEMO Claims.mSeeking	WINDOW lwEditSeeking
	m.lnLastKey	=LASTKEY()
				
	DO CASE
	* [Esc]ape
	CASE m.lnLastKey =27 .AND. !(m.jcMode='A')
		REPLACE Claims.mSeeking	WITH m.lmSeeking
		EXIT
				
	* New Note is smaller than prior to editing
	CASE LEN(ALLTRIM(Claims.mSeeking)) < LEN(m.lmSeeking) .AND. !(m.jcMode='A')
				
		IF YesNo("Reduce this text???",32)
			EXIT
		ELSE
			REPLACE Claims.mSeeking	WITH m.lmSeeking
		ENDIF
				
	* Otherwise: For example, [Ctrl+W]=23.
	OTHERWISE
		KEYBOARD '{ENTER}'
		=INKEY(0)
		EXIT
					
	ENDCASE
					
ENDDO
			
RELEASE WINDOW lwEditSeeking
CLOSE MEMO mSeeking

SHOW GET m.lnSeeking,1	PROMPT IIF(EMPTY(Claims.mSeeking), "-seeking-", LEFT(MLINE(mseeking,1,0), 13)+"+")
RETURN .T.


*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION GotClmt
PRIVATE m.llGotClmt
m.llGotClmt	=!EMPTY(mClmtAdd1 +mClmtAdd2 +mClmCity +mClmtState +mClmtZIP +mClmtPhn1 +mClmtPhn2 +mClmtFAX +mClmtEMail 	;
				+mCCName +mCCFirm +mCCAdd1 +mCCAdd2 +mCCCity +mCCState +mCCZIP +mCCPhn1 +mCCPhn2 +mCCFAX +mCCEmail)
RETURN m.llGotClmt
	
	
*--------------------------------------------------------------------------------------------------------------------------------------
FUNCTION EditClmt
PRIVATE m.lnLastKey
m.lnLastKey	=LASTKEY()

IF m.lnLastKey=23
	_CUROBJ=1
	KEYBOARD '{CTRL+W}'
	RETURN .T.
ENDIF

PRIVATE lwEditClmt, m.lnLastKey, m.lcAdd1, m.lcAdd2, m.lcCity, m.lcState, m.lcZIP, m.lcPhn1, m.lcPhn2, m.lcFAX, m.lcEMail,	;
        m.lcCCName, m.lcCCFirm, m.lcCCAdd1, m.lcCCAdd2, m.lcCCCity, m.lcCCState, m.lcCCZIP, m.lcCCPhn1, m.lcCCPhn2, m.lcCCFAX, m.lcCCEmail

* Capture values to restore from if User [Esc]s
m.lcAdd1	=mClmtAdd1
m.lcAdd2	=mClmtAdd2
m.lcCity	=mClmCity
m.lcState	=mClmtState
m.lcZIP		=mClmtZIP
m.lcPhn1	=mClmtPhn1
m.lcPhn2	=mClmtPhn2
m.lcFAX		=mClmtFAX
m.lcEMail	=mClmtEMail
m.lcCCName	=mCCName
m.lcCCFirm	=mCCFirm
m.lcCCAdd1	=mCCAdd1
m.lcCCAdd2	=mCCAdd2
m.lcCCCity	=mCCCity
m.lcCCState	=mCCState
m.lcCCZIP	=mCCZIP
m.lcCCPhn1	=mCCPhn1
m.lcCCPhn2	=mCCPhn2
m.lcCCFAX	=mCCFAX
m.lcCCEmail	=mCCEmail

DEFINE WINDOW lwEditClmt	FROM 04,5 TO 24,80	;
	SYSTEM SHADOW FLOAT 	;
	TITLE IIF(EMPTY(mPlaintiff), "Plaintiff", ALLTRIM(mPlaintiff)) +" info"		
ACTIVATE WINDOW lwEditClmt	

DO WHILE .T.
	@00,01 SAY " Plaintiff"
	@01,01 SAY "Address 1:" GET	mClmtAdd1	PICTURE "@!"
	@02,01 SAY "        2:" GET	mClmtAdd2	PICTURE "@!"
	@03,01 SAY "     City:" GET	mClmCity	PICTURE "@!"
*	@03,12+LEN(Claims.ClmCity)+1 SAY "State:" GET mClmtState PICTURE "!!"
	@03,48 SAY "State:" 	GET mClmtState 	PICTURE "!!"
*	@03,12+LEN(Claims.ClmCity+"State:"+Claims.ClmtState)+3 SAY "ZIP:" GET	mClmtZIP	PICTURE "@R 99999-9999"
	@03,58 SAY "ZIP:" 		GET mClmtZIP 	PICTURE "@R 99999-9999"
	@04,01 SAY "  Phone 1:" GET	mClmtPhn1	PICTURE "@R 999/999-9999"
	@05,01 SAY "        2:" GET mClmtPhn2	PICTURE "@R 999/999-9999"	
	@06,01 SAY "      FAX:" GET	mClmtFAX 	PICTURE "@R 999/999-9999"	
	@07,01 SAY "    eMail:" GET	mClmtEMail	
	@09,01 SAY " Plaintiff's Counsel"	
	@10,01 SAY "     Name:" GET	mCCName	PICTURE "@!"
	@10,42 SAY "Firm:" 		GET mCCFirm PICTURE "@S25!"
	@11,01 SAY "Address 1:" GET	mCCAdd1	PICTURE "@!"
	@12,01 SAY "        2:" GET	mCCAdd2	PICTURE "@!"
	@13,01 SAY "     City:" GET	mCCCity	PICTURE "@!"
*	@13,12+LEN(Claims.CCCity)+1 SAY "State:" GET mCCState PICTURE "!!"
	@13,48 SAY "State:" 	GET mCCState PICTURE "!!"
*	@13,12+LEN(Claims.CCCity+"State:"+Claims.CCState)+3 SAY "ZIP:" GET	mCCZIP	PICTURE "@R 99999-9999"
    @13,58 SAY "ZIP:" 		GET	mCCZIP	PICTURE "@R 99999-9999"
	@14,01 SAY "  Phone 1:" GET	mCCPhn1	PICTURE "@R 999/999-9999"
	@15,01 SAY "        2:" GET mCCPhn2	PICTURE "@R 999/999-9999"	
	@16,01 SAY "      FAX:" GET	mCCFAX 	PICTURE "@R 999/999-9999"	
	@17,01 SAY "    eMail:" GET	mCCEMail	
	READ
	m.lnLastKey	=LASTKEY()
	
	DO CASE
	*          [Ctrl+W]        
	CASE m.lnLastKey=23 
		KEYBOARD '{ENTER}'
		=INKEY(0)
		EXIT
	*			  [Esc]
	CASE m.lnLastKey=27
		* Restore prior values
		mClmtAdd1	=m.lcAdd1
		mClmtAdd2	=m.lcAdd2
		mClmCity	=m.lcCity
		mClmtState	=m.lcState
		mClmtZIP	=m.lcZIP
		mClmtPhn1	=m.lcPhn1
		mClmtPhn2	=m.lcPhn2
		mClmtFAX	=m.lcFAX
		mClmtEMail	=m.lcEMail	
		mCCName		=m.lcCCName
		mCCFirm		=m.lcCCFirm
		mCCAdd1		=m.lcCCAdd1
		mCCAdd2		=m.lcCCAdd2
		mCCCity		=m.lcCCCity
		mCCState	=m.lcCCState
		mCCState	=m.lcCCZIP
		mCCPhn1		=m.lcCCPhn1
		mCCPhn2		=m.lcCCPhn2
		mCCFAX		=m.lcCCFAX
		mCCEmail	=m.lcCCEmail
		EXIT
	
	OTHERWISE
	ENDCASE
	
ENDDO

RELEASE WINDOW lwEditClmt	
SHOW GET m.lnPlaintiff,1	PROMPT IIF(GotClmt(), "Plaintiff+", "-plaintiff-")

RETURN 1

*-----------------------------------------------------------------------------------*
FUNCTION PropCount
PARAMETERS m.jcClaimNumb
PRIVATE m.lnSelect, m.lnPropCount
m.lnSelect	=SELECT()

SELECT ClaiProp
=SEEK(m.jcClaimNumb)
COUNT TO m.lnPropCount WHILE ClaiProp.cClaimNumb=m.jcClaimNumb
=SEEK(m.jcClaimNumb)

SELECT (m.lnSelect)
RETURN m.lnPropCount
	
	
*---------------------------------------------------------------------------------------------------------------------*
FUNCTION UpdateFees
PARAMETERS m.jcOldClaimNumb, m.jcNewClaimNumb
PRIVATE m.lnSelect
m.lnSelect	=SELECT()

SELECT 0
USE Fees	AGAIN ALIAS UpdateFees
SET ORDER TO CLAIMNUMB   && CLAIMNUMB

=SEEK(m.jcOldClaimNumb)
REPLACE UpdateFees.ClaimNumb	WITH m.jcNewClaimNumb	;
	UpdateFees.Note WITH DTOC(DATE()) +":  Claim# changed from " +ALLTRIM(m.jcOldClaimNumb) +" to " +ALLTRIM(m.jcNewClaimNumb) +". (" +ALLTRIM(mstafcode) +")" +CHR(13) +ALLTRIM(UpdateFees.note)	;
	WHILE ALLTRIM(UpdateFees.ClaimNumb) = ALLTRIM(m.jcOldClaimNumb)

USE IN UpdateFees

SELECT (m.lnSelect)
RETURN .T.


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION UpdateAudit
PARAMETERS m.jcOldClaimNumb, m.jcNewClaimNumb
PRIVATE m.lnSelect, m.llClaimsAu

m.lnSelect		=SELECT()
m.llClaimsAu	=USED("ClaimsAu")

IF m.llClaimsAu
	SELECT ClaimsAu
ELSE
	SELECT 0
	USE ClaimsAu
ENDIF

REPLACE ClaimsAu.ClaimNumb	WITH m.jcNewClaimNumb	;
		FOR ALLTRIM(ClaimsAu.ClaimNumb) = ALLTRIM(m.jcOldClaimNumb)

IF !m.llClaimsAu
	USE
ENDIF

SELECT (m.lnSelect)
RETURN .T.


*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ChkEndorsements
PARAMETERS m.xcClaimNumb, m.xcPoliNumb, m.xcPoliID
PRIVATE m.llReturn, m.lcMessage
m.llReturn	=.F.		&&	default to FALSE
DO CASE
CASE EMPTY(m.xcClaimNumb)
	m.lcMessage	="Please provide a Claim Number!"
CASE EMPTY(m.xcPoliNumb)
	m.lcMessage	="Please provide a PoliNumb!"
CASE EMPTY(m.xcPoliID)
	m.lcMessage	="Please provide a PoliID!"
	
OTHERWISE
 
	DO programs\ClaiEnd	WITH m.xcClaimNumb, m.xcPoliNumb, m.xcPoliID
*	=ClaimLabels()
	m.llReturn	=.T.
ENDCASE

IF !m.llReturn
	WAIT WINDOW NOWAIT m.lcMessage
ENDIF

RETURN m.llReturn

*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ClaiViewScrnError
PARAMETERS m.lnERROR, m.lcMESSAGE, m.lnLINENO, m.lcPROGRAM
DO CASE
CASE m.lnError=108
		
	DO CASE
	CASE m.jcMode="A"
		WAIT WINDOW TIMEOUT 5 "Attempting to add new record..."
	CASE m.jcMode="C"
		WAIT WINDOW TIMEOUT 5 "Attempting to add new record for "+ALLTRIM(PoliInd.FirstName) +" "+ALLTRIM(PoliInd.LastName)
	ENDCASE
	
	RETRY
	
OTHERWISE
	WAIT WINDOW ALLTRIM(STR(m.lnERROR)) +": " +m.lcMESSAGE +".  Line:"+ALLTRIM(STR(m.lnLINENO)) +", " +m.lcPROGRAM
	
ENDCASE



*---------------------------------------------------------------------------------------------------------------------*
FUNCTION ClaimLabels
		@02,1 SAY "           Claim:" && GET mclaimnumb PICT "@!"
		@02,1 SAY "           Claim:" && GET mclaimnumb PICT "@!" VALID claimchk(mclaimnumb) ERROR "Claim #/Incident is in Use"
						
*		@02,34.25+1 GET mstatus PICT "!" VALID statchk(mstatus) ERROR "Status must be - Denied / Open / Reopen / Closed / Incident / Party"
		@02,36.5+1 SAY "Examiner:" && GET mexaminer
		@03,0 TO 03,80
		@04,1 SAY "     Allegations:" && GET malleg1 PICT "@!" VALID datachk("A",malleg1,.F.)
*		@04,26.25+1 GET mre1
*		@05,19 GET malleg2 PICT "@!" VALID datachk("A",malleg2,.T.)
*		@05,25.25+1 GET mre2
*		@06,19 GET malleg3 PICT "@!" VALID datachk("A",malleg3,.T.)
*		@06,25.25+1 GET mre3
			
		* A
		@07,1 SAY "Reported to RISC:" && GET mdaterepo WHEN dateproc() VALID DTOC(mdaterepo)<>"  /  /  " ERROR "A Date must be Entered"
		@07,29.25+1 SAY " Loss Outcome:" && GET mlossoutcm PICTURE "@!" VALID datachk("O", mlossoutcm, .F.)
			
		* B
		@08,1 SAY "Insured Notified:" && GET mdatenoti WHEN .F. VALID DTOC(mdatenoti)<>"  /  /  " ERROR "A Date must be Entered"
		@08,29.25+2 SAY "Denial Codes:" && GET mdenial1 PICT "@!" VALID datachk("E",mdenial1,mstatus<>"D")
*		@08,47.5+1 GET mdenial2 PICT "@!" && VALID datachk("E",mdenial1,.T.)
*		@08,50.75+1 GET mdenial3 PICT "@!" && VALID datachk("E",mdenial1,.T.)
		
		* C
		@09,1 SAY "   Loss Occurred:" && GET mdateloss WHEN .F.
		@09,29.25+2 SAY "      Closed:" && GET mdateclosed
			
*		IF mstatus<>"P"
*			DO losstype WITH mdateloss,.T.,mpolinumb
*		ENDIF
		
		@09,55.5+1 SAY mdefastat
		@10,0 TO 10,80
		@11,35 SAY GetOfferAs(mpolinumb)	COLOR RGB(255,0,0)
		@11,5 SAY "Group Policy:" && GET mpolinumb PICT "@!";
*			WHEN mentry="Losses";
			VALID polichk(mpolinumb) .AND. GetCNAClaim(mpolinumb, mdatenoti) .AND. DispOfferAs(mpolinumb, 11, 33)
		@11,44 SAY "Carrier:" && GET mcompowner
		@11,59.25+1 SAY policy.addtl_key
		
		IF !EMPTY(mpolinumb)
**			DO programs\chkcnt WITH mpolinumb,.T.,"",12
			DO programs\chkcnt WITH mpolinumb,.T.,"",12
		ENDIF
		
		@12,1 SAY "          Limits:"
		@13,1 SAY "        Licensee:" && GET mpoliid PICT "@!";
*			WHEN p_id_whn(mpolinumb);
			VALID poliidchk(mpolinumb,mpoliid);
			ERROR "Not a valid licensee for "+mpolinumb
			
		@13,28.25+2 SAY GetLicensee(mpolinumb,mpoliid)
		@14,1 SAY "   Excess Policy:" && GET mxspolicy
**		@14,COL()+1 SAY "         Carrier:" GET mxscarrier
		@14,44 SAY "Carrier:" && GET mxscarrier
		
*		IF !EMPTY(mxspolicy)
*			DO programs\chkcnt WITH mxspolicy,.T.,"",15
*		ENDIF
		
		@15,01 SAY "   Excess Limits:"
		@16,01 SAY "Covered Insured?:" && GET mcovered PICT "@S35"
		@17,01 SAY "RealtyFirm-legal:" && GET mrealcomp PICT "@!" WHEN prerealcomp() VALID postrealcomp()
		@17,50 SAY " DBA:" && GET mrealdba	PICTURE "@!"
		@18,01 SAY "         Realtor:" && GET mrealtor
		@19,01 SAY "          Broker:" && GET mbroker
		@18,50 SAY "Type:" && GET mrealest PICT "@!" VALID datachk("R",mrealest,.F.)
		@19,50 SAY "Deal:" && GET mdeal PICT "@!" VALID datachk("D",mdeal,.F.)
		@20,00 TO 20,80
		@20,19 SAY "Damages"
		@20,32 SAY "Expenses"
		@20,44 SAY "Incurred"
		@20,56 SAY "CC"
*		@21,01 SAY "                  Damages      Expenses    Incurred    CC"
		
		DO incurred
		
		@21,01 SAY "        Reserves:" && GET mclreserves PICT "#######.##" VALID Incurred() WHEN mstatus<>"I"
*		@21,32 GET mexreserves PICT "#######.##" VALID Incurred() WHEN mstatus<>"I"
*		@21,44 GET mresrtot PICT "#######.##" VALID Incurred()
				
*		@21,56 GET mccnumb FUNCTION "!" PICT "XXXXXXXXXXXXXXX"	VALID GetCNAClaim(mpolinumb, mdatenoti) .AND. ChgCNAClaim()
				
		@22,01 SAY "        Payments:" && GET mclaimpaymt PICT "#######.##" WHEN .F.
*		@22,32 GET mexpenpaymt PICT "#######.##" WHEN .F.
			
		@23,01 SAY "        Recovery:" && GET msubrocl PICT "#######.##" WHEN .F.
*		@23,32 GET msubroex PICT "#######.##" WHEN .F.

		@24,01 SAY " Deductible/Paid:" && GET mdedlosstat PICT "Y"

*		@24,20.25+1 GET mdedlosdate
*		@24,31.50+1 GET mdedlaestat PICT "Y"
*		@24,33.75+1 GET mdedlaedate

		if (mcompowner = '1004' .OR. mcompowner = '1005') then 	&& 1/22/2004 mac
			
*			IF !EMPTY(Claims.PoliID)
*				Do aggregate 
*			ENDIF
			
			@22,45 say "Polinumb/Poliid Aggregate"	&& 1/22/2004 mac
			@23,45 say "Damages           Expense"	&& 1/22/2004 mac
			@24,45 say mDamages -0	pict "#######.##"	&& 1/22/2004 mac		
			@24,61 say mExpenses -0	pict "#######.##"	&& 1/22/2004 mac		
		endif

		@25,0 TO 25,80
*		@27,1 SAY "     Suit Status:" GET msuitstat PICTURE "@M NOT A SUIT,LAWSUIT,NOT AVAILABLE,LEGAL,ARBITRATION,NON-LEGAL,MEDIATION,NOC-NOT OTHERWISE CLASSIFIED" && 2/1/2004 MAC - ADDED ADDITIONAL OPTIONS
		@26,1 SAY "     Suit Status:" && GET msuitstat PICTURE "@M ARBITRATION,LAWSUIT,LEGAL,MEDIATION,NOC-NOT OTHERWISE CLASSIFIED,NON-LEGAL,NOT A SUIT,NOT AVAILABLE,REGULATORY COMPLAINT,REGULATORY COMPLAINT AND LEGAL,REGULATORY COMPLAINT AND NON-LEGAL,SUBPOENA"
		@26,52 SAY "Trial Date:" && GET mtrial
*		@27,52 SAY "   Med/Arb:" && GET mmediation
		@27,1 SAY "            Case:" && GET mcasenumb WHEN mstatus<>"I"
		@27,40 SAY "Mediation:" 	&& GET mmediation
		@27,64 SAY "Arbitration:" 	&& GET mdarbitrate
		
		
*		m.lnX	=0
*		IF !USED("CourtSt")
*			@28,1 SAY "           Court:" && GET mcourt PICTURE "@S30" WHEN mstatus<>"I"
*			@28,52 SAY "  Court St:" && GET mcourtst	PICTURE "!!"
*		ELSE
*			m.lnX	=1
*			@28.50+m.lnX,09 SAY "Court St:"
*			@28.50+m.lnX,28 SAY "Court:"
*		ENDIF
		
		@29+m.lnX,1 SAY "        Attorney:" && GET mattorney PICTURE "@S30!" VALID compchk(mattorney)
		@29+m.lnX,49.25+1 SAY compphone(mcompcode)
		@30+m.lnX,1 SAY "        Law Firm:" && GET mcompname PICTURE "@S40!" WHEN .F.
*		@32,1 SAY "       Plaintiff:" GET mplaintiff PICTURE "@S35"
		@31+m.lnX,17 SAY CHR(45)+CHR(16)
*		@32,02 GET m.lnPlaintiff	DEFAULT 1	WHEN m.jcMode#"V"	FUNCTION '*N ' +IIF(GotClmt(), "Plaintiff+", "-plaintiff-")	SIZE 2,15	VALID IIF(EditClmt()=1, 1, 1)
*		@31,19 GET mPlaintiff	PICTURE "@S35"
			
		IF (mcompowner = '1004' .OR. mcompowner = '1005') then
			@30+m.lnX,60 SAY "Insured Id:"
*			@31,55 GET mseverity PICTURE "@!" VALID DataChk("I",mseverity,.T.)
		ENDIF

		@33+m.lnX,1 SAY "       Defendant:" && GET mdefendant PICTURE "@S35"
		@34+m.lnX,1 SAY "   Carrier Named:" && GET mcarriernam PICTURE "@M ,COVERAGE DISPUTE,DIRECT ACTION"
				
		
*		@34,41 GET mlRiskNamed FUNCTION "*C RISC named?" COLOR ,,,,,,,,R+/W*

		IF (mcompowner = '1004' .OR. mcompowner = '1005') then		&& 1/24/2004 mac 
*			@ 34,42 say "Claimant Id:"
*			@ 33,55 GET minc_type ;
			PICTURE "@^ ;NOT AVAILABLE;SELLER;BUYER;INSPECTOR/INSPECTION CO;TITLE INSURANCE CO;LENDER;LESSOR;LESSEE;BUILDER/DEVELOPER;GOVERNMENT;INSURANCE;NOT CLASSIFIED" ;
			SIZE 1,25 ;
			DEFAULT " " 
			@33+m.lnX,55 SAY "Claimant Id:"
*			@34,55 GET minc_type PICTURE "@!" VALID DataChk("C",minc_type,.T.)
		ENDIF

		@35+m.lnX,17 SAY CHR(45)+CHR(16)
*		@35,19 GET mtSeeking FUNCTION "@K$ 99,999,999.99"	WHEN m.jcMode#"V"	VALID SetCNA()
		@35+m.lnX,42 SAY "Last Demand:"
		
*		IF mlCNANotify
*			@37,19 GET mlCNANotify 	FUNCTION "*C CNA Notified?"	COLOR ,,,,,,,,R+/W*	VALID SetCNA()	ENABLED
*			@37,36 GET mdCNARepo	WHEN mlCNANotify	COLOR R+/W*					VALID SetCNA()	ENABLED
*		ELSE
*			@37,19 GET mlCNANotify 	FUNCTION "*C CNA Notified?"	COLOR ,,,,,,,,R+/W*	DISABLED
*			@37,36 GET mdCNARepo	WHEN mlCNANotify	COLOR R+/W*					DISABLED
*		ENDIF		

*		@36,19 GET mlCNANotify 	FUNCTION "*C CNA Notified?"	COLOR ,,,,,,,,R+/W*	
*		@36,40 GET mdCNARepo	WHEN mlCNANotify	COLOR R+/W*					
*		@37,19 GET mlExsNotify 	FUNCTION "*C Excess Notified?"	COLOR ,,,,,,,,R+/W*	
*		@37,40 GET mdExsRepo	WHEN mlExsNotify	COLOR R+/W*					
		
*		@35,55 GET m.lnSettlements 	DEFAULT 1 	WHEN m.jcMode#"V"	FUNCTION '*N '+IIF(EOF("ClaiSetl"), "-none-", ALLTRIM(TRANSFORM(claiSetl.tsettleamt, '$$$,$$$,$$$.99')) +" on " +DTOC(ClaiSetl.dDate)) 	SIZE 2,30	VALID IIF(ToSettlements()=-1 .AND. SetCNA(), 1, 1)
*		@31,02 GET m.lnPlaintiff	DEFAULT 1	WHEN m.jcMode#"V"	FUNCTION '*N ' +IIF(GotClmt(), "Plaintiff+", "-plaintiff-")	SIZE 2,15	VALID IIF(EditClmt()=1, 1, 1)
*		@35,02 GET m.lnSeeking		DEFAULT 1	WHEN m.jcMode#"V"	FUNCTION '*N ' +IIF(EMPTY(Claims.mSeeking), "-seeking-", LEFT(MLINE(mseeking,1,0), 13) +"+")	SIZE 2,15	VALID EditSeeking()
		@38+m.lnX,02 SAY "Propert-y/ies:"
RETURN .T.


FUNCTION NoChangeByPressingEnter
PARAMETERS m.xcVarName
PRIVATE m.lnLastKey
m.lnLastKey=LASTKEY()

IF m.lnLastKey=13
	DO CASE
	CASE m.xcVarName="M.MLRISKNAMED"
		m.mlRiskNamed 	=!m.mlRiskNamed
		SHOW GET m.mlRiskNamed
	CASE m.xcVarName="M.MLCNANOTIY"
		m.mlCNANotify	=!m.mlCNANotify
		SHOW GET m.mlCNANotify
	CASE m.xcVarName="M.MLEXSNOTIFY"
		m.mlExsNotify	=!m.mlExsNotify
		SHOW GET m.mlExsNotify
	ENDCASE
ENDIF
	
RETURN .T.


**************************************************************************************
FUNCTION CovDedu
PARAMETERS m.xcClaimNumb, m.xcPoliNumb, m.xcPoliID, m.xnRow, m.xnCol
PRIVATE m.lnSelect, m.ClaimNumb, m.PoliNumb, m.PoliID, m.Insured, m.lChk, m.lnLine, m.lcDedu, m.llFound
m.lnSelect	=SELECT()
m.lnLine	=-1

@ m.xnRow,m.xnCol CLEAR TO m.xnRow+3,m.xnCol+80

SELECT 0
USE ChartB
SET ORDER TO POLIEND   && POLINUMB+ENDORSE

SELECT 0
USE ClaiEnd 
*SET ORDER TO ClaiEnd   && UPPER(CLAINUMB+POLINUMB+POLIID+ENDORSEMENT)+DTOS(ENTRYDATE)+ENTRYTIME+STR(NDISPORDR)
SET ORDER TO CLAIORDR   && CLAIMNUMB+POLINUMB+POLIID+STR(NDISPORDR)
SET RELATION TO polinumb+endorse INTO chartb

=SEEK(m.xcClaimNumb +m.xcPoliNumb +m.xcPoliID)
LOCATE REST WHILE (m.xcClaimNumb +m.xcPoliNumb +m.xcPoliID)=(CLAIMNUMB+POLINUMB+POLIID)	;
	FOR lChk 	;
	.AND. (ChartB.nDmgDedu+ChartB.nExpDedu+ChartB.nCombiDedu)>0	;
	.AND. LEFT(Endorse,7)#"PREMIUM"	;
	.AND. !("HIGHER LIMIT"$Endorse)
	
m.llFound	=FOUND()

DO WHILE FOUND() .AND. m.lnLine<4
	DO CASE
	CASE ChartB.nCombiDedu>0
		m.lcDedu=ALLTRIM(STR(ChartB.nCombiDedu))+"(Combi)"
	CASE ChartB.nDmgDedu>0 .AND. ChartB.nExpDedu>0
		m.lcDedu=ALLTRIM(STR(ChartB.nDmgDedu))+"(D), "+ALLTRIM(STR(ChartB.nExpDedu))+"(E)"
	CASE ChartB.nDmgDedu>0
		m.lcDedu=ALLTRIM(STR(ChartB.nDmgDedu))+"(D)"
	CASE ChartB.nExpDedu>0
		m.lcDedu=ALLTRIM(STR(ChartB.nExpDedu))+"(E)"
	OTHERWISE
		m.lcDedu=""
	ENDCASE

	m.lnLine	=(m.lnLine+1)
*	@m.xnRow+m.lnLine,m.xnCol SAY ALLTRIM(Endorse)+", "+m.lcDedu
	@m.xnRow+2-m.lnLine,m.xnCol SAY ALLTRIM(Endorse)+", "+m.lcDedu
	CONTINUE
ENDDO

IF m.llFound
	m.lnLine	=(m.lnLine+1)
	@m.xnRow+2-m.lnLine,m.xnCol SAY "Endorsement Deductibles"	STYLE "U"
ENDIF

USE IN ChartB
USE IN ClaiEnd
SELECT (m.lnSelect)

*---------------------------------------------------------------------------*
FUNCTION NoClaimChildren
PARAMETERS m.xcClaimnumb, m.lnSelect
m.lnSelect	=SELECT()

* Tables with potential children
*	ClaiSetl - already open & related
*	ClaiProp - already open & related
*	Reserves - not currently open

* Temporarily open/relate Reserves.dbf in order to determine in any child records, including ClaiSetl and ClaiProp, are present.
SELECT 0
USE Reserves
SET ORDER TO CLAIDATE   DESCENDING && UPPER(CLAIMNUMB+DTOS(DATE))   

SELECT Claims
*IF m.jcMode="A"
*	SET RELATION TO mValue 			INTO Reserves ADDITIVE
*ELSE
*	SET RELATION TO Claims.Claimnumb 			INTO Reserves ADDITIVE
*ENDIF
SET RELATION TO m.xcClaimNumb	INTO Reserves	ADDITIVE 

GO RECNO()
m.llOk2Edit	=EOF("ClaiSetl") .AND. EOF("ClaiProp") .AND. EOF("Reserves")

IF !m.llOk2Edit
	m.lcNoEdit="This Claim# "+IIF(m.jcMode="A", ALLTRIM(m.xcClaimnumb), ALLTRIM(claims.claimnumb)) +" must be changed manually due to having" +CHR(13)	;
				+IIF(!EOF("ClaiSetl"), "Settlement ", "")+IIF(!EOF("ClaiProp"),"Property ","")+IIF(!EOF("Reserves"),"Reserves ","")+"records."+CHR(13)	;
				+"Press any key to continue..."
ENDIF

USE IN Reserves
SELECT (m.lnSelect)
RETURN m.llOk2Edit


*-----------------------------------------------------*
FUNCTION DispBodyInj
PARAMETERS m.xcPolinumb, m.xcPoliID, m.xnRow, m.xnCol
PRIVATE m.lcBodyInjDesc	&&, m.lcIsSetInClaiEnd
m.lcBodyInjDesc=""
*m.lcIsSetInClaiEnd=""
*@m.xnRow+0, m.xnCol SAY m.xcPolinumb+" "+m.xcPoliID
*@m.xnRow+1, m.xnCol SAY "Has Bodily Injury Coverage:" +IIF(HasBodyInjCov(m.xcPolinumb, m.xcPoliID, @lcBodyInjDesc), "Yes = "+m.lcBodyInjDesc, "No")
*@m.xnRow+2, m.xnCol SAY "Set .T. in ClaiEnd?" +IIF(IsSetInClaiend(mclaimnumb, m.xcPolinumb, m.xcPoliID, @lcIsSetInClaiEnd), "Yes", "No")+" = "+m.lcIsSetInClaiEnd

* Has BODILY INJURY
*SET STEP ON 
*IF HasBodyInjCov(m.xcPolinumb, m.xcPoliID, @lcBodyInjDesc)
IF .T.
*	@m.xnRow+0, m.xnCol	GET m.lBodyInj	VALID SetInClaiend(m.lBodyInj, mclaimnumb, @lcIsSetInClaiEnd) .OR. .T.
	@m.xnRow+0, m.xnCol	GET m.lBodyInj	VALID SetInClaiend(m.lBodyInj, mclaimnumb) .OR. .T.
		IF m.lBodyInj
			@m.xnRow+0, m.xnCol+2 SAY "Applied: "	COLOR RGB(0,255,0)
		ELSE
			@m.xnRow+0, m.xnCol+2 SAY "Apply?   "	COLOR RGB(255,0,0)
		ENDIF		
		@m.xnRow+0, m.xnCol+12 SAY ALLTRIM(m.lcBodyInjDesc)
	@m.xnRow+1, m.xnCol GET m.dBIsent
		@m.xnRow+1, m.xnCol+12 SAY "- Medicare Info Sent to Claimant"
	@m.xnRow+2, m.xnCol GET m.dBIReturn
		@m.xnRow+2, m.xnCol+12 SAY "- Claimant Returned Medicare Form"
*	@m.xnRow+3, m.xnCol GET m.lHasMedi
*		@m.xnRow+3, m.xnCol+12 SAY "- Has Medicare?"
	@m.xnRow+1.75, m.xnCol GET m.nHasMedi	FUNCTION "^"	FROM aHasMedi	DEFAULT m.nHasMedi
		@m.xnRow+3.5, m.xnCol+15 SAY "Has Medicare?"
	@m.xnRow+5, m.xnCol GET m.dBINotify
		@m.xnRow+5, m.xnCol+12 SAY "- Notified Carrier to Add Medicare Claim"
ELSE
* No BODILY INJURY
	@m.xnRow+0,m.xnCol+0 CLEAR TO m.xnRow+5,WCOLS(WONTOP())
ENDIF

RETURN .T.

*-----------------------------------------------------*
FUNCTION HasBodyInjCov
PARAMETERS m.xcPolinumb, m.xcPoliID, m.xcBodyInjDesc
PRIVATE m.lnSelect, m.llHasBodyInjCov
m.lnSelect	=SELECT()
m.llHasBodyInjCov	=.F.	&& default

SELECT 0
USE End_Indv	AGAIN ALIAS EndBodyInj
SET ORDER TO ENDORSE   && UPPER(POLINUMB+POLIID)+DTOS(ENTRYDATE)+ENTRYTIME+STR(NDISPORDR)

=SEEK(m.xcPoliNumb +m.xcPoliID)
SCAN WHILE (polinumb + poliid) = (m.xcPoliNumb + m.xcPoliID)	;
	FOR "BODILY INJURY"$Endorse .AND. effective # expires  && .AND. expires<DATE()
	m.xcBodyInjDesc	=ALLTRIM(EndBodyInj.Endorse)
	m.llHasBodyInjCov	=.T.
ENDSCAN

USE IN EndBodyInj
SELECT (m.lnSelect)
RETURN m.llHasBodyInjCov
	
*----------------------------------------------------------
FUNCTION SetInClaiend
PARAMETERS m.xlBodyInj, m.xcClaimNumb	&&, m.xcIsSetInClaiEnd
PRIVATE m.lnSelect, m.llIsSetInClaiend
m.lnSelect	=SELECT()
m.llIsSetInClaiend	=.F.	&& default

SELECT 0
USE ClaiEnd	AGAIN ALIAS ClaiEndBodyInj
SET ORDER TO CLAIEND   && UPPER(CLAIMNUMB+POLINUMB+POLIID+ENDORSE)+DTOS(ENTRYDATE)+ENTRYTIME+STR(NDISPORDR)

=SEEK(m.xcClaimNumb +mPoliNumb +mPoliID)
SCAN WHILE (claimnumb + polinumb + poliid) = (m.xcClaimNumb + mPoliNumb + mPoliID)	;
	FOR "BODILY INJURY"$Endorse .AND. expires<DATE()
*	m.xcIsSetInClaiEnd	=ALLTRIM(ClaiEndBodyInj.Endorse)
	REPLACE ClaiEndBodyInj.lChk	WITH m.xlBodyInj
	m.llIsSetInClaiend	=ClaiEndBodyInj.lChk
ENDSCAN

USE IN ClaiEndBodyInj
SELECT (m.lnSelect)
RETURN m.llIsSetInClaiend


*----------------------------------------------------------
FUNCTION CheckBI
PARAMETERS m.xcPolinumb, m.xcPoliID, m.xnRow, m.xnCol
PRIVATE m.lcBodyInjDesc	&&, m.lcIsSetInClaiEnd
m.lcBodyInjDesc=""
*m.lcIsSetInClaiEnd=""
*@m.xnRow+0, m.xnCol SAY m.xcPolinumb+" "+m.xcPoliID
*@m.xnRow+1, m.xnCol SAY "Has Bodily Injury Coverage:" +IIF(HasBodyInjCov(m.xcPolinumb, m.xcPoliID, @lcBodyInjDesc), "Yes = "+m.lcBodyInjDesc, "No")
*@m.xnRow+2, m.xnCol SAY "Set .T. in ClaiEnd?" +IIF(IsSetInClaiend(mclaimnumb, m.xcPolinumb, m.xcPoliID, @lcIsSetInClaiEnd), "Yes", "No")+" = "+m.lcIsSetInClaiEnd

* Has BODILY INJURY
*SET STEP ON 
IF HasBodyInjCov(m.xcPolinumb, m.xcPoliID, @lcBodyInjDesc)
	* Labels
	IF m.lBodyInj
		@m.xnRow+0, m.xnCol+2 SAY "Applied: "	COLOR RGB(0,255,0)
	ELSE
		@m.xnRow+0, m.xnCol+2 SAY "Apply?   "	COLOR RGB(255,0,0)
	ENDIF		
	@m.xnRow+0, m.xnCol+12 SAY ALLTRIM(m.lcBodyInjDesc)
	@m.xnRow+1, m.xnCol+12 SAY "- Medicare Info Sent to Claimant"
	@m.xnRow+2, m.xnCol+12 SAY "- Claimant Returned Medicare Form"
	@m.xnRow+3.5, m.xnCol+15 SAY "Has Medicare?"
	@m.xnRow+5, m.xnCol+12 SAY "- Notified Carrier to Add Medicare Claim"
	
	* SHOW GETs
	SHOW GET m.lBodyInj		ENABLED 
	SHOW GET m.dBIsent		ENABLED	
	SHOW GET m.dBIReturn	ENABLED
	SHOW GET m.nHasMedi		ENABLED
	SHOW GET m.dBINotify	ENABLED

ELSE
	* No BODILY INJURY
	SHOW GET m.lBodyInj		DISABLED 
	SHOW GET m.dBIsent		DISABLED	
	SHOW GET m.dBIReturn	DISABLED
	SHOW GET m.nHasMedi		DISABLED
	SHOW GET m.dBINotify	DISABLED

	@m.xnRow,m.xnCol CLEAR TO m.xnRow+6,WCOLS(WONTOP())
ENDIF

RETURN .T.


*----------------------------------------------------------
FUNCTION GetBI
PARAMETERS m.xnRow, m.xnCol
	@m.xnRow+0, m.xnCol	GET m.lBodyInj	VALID SetInClaiend(m.lBodyInj, mclaimnumb) .OR. .T.		DISABLED
	@m.xnRow+1, m.xnCol GET m.dBIsent															DISABLED
	@m.xnRow+2, m.xnCol GET m.dBIReturn															DISABLED
	@m.xnRow+1.75, m.xnCol GET m.nHasMedi	FUNCTION "^"	FROM aHasMedi	DEFAULT m.nHasMedi	DISABLED
	@m.xnRow+5, m.xnCol GET m.dBINotify															DISABLED
RETURN .T.



*----------------------------------------------------------
FUNCTION ShowNoteExcerpt
PARAMETERS m.xnRow, m.xnCol, m.xnLength
PRIVATE m.lnMemoWidth
m.lnMemoWidth	=SET("Memowidth")
@m.xnRow, m.xnCol	SAY SPACE(m.xnLength)
IF !EMPTY(ClaiSetl.mNote)
	SET MEMOWIDTH TO 60
	@m.xnRow, m.xnCol	SAY "{..."+RIGHT(MLINE(ClaiSetl.mNote,MEMLINES(ClaiSetl.mNote)), m.xnLength-5)+"}"
	SET MEMOWIDTH TO (m.lnMemoWidth)
ENDIF

RETURN .T.

