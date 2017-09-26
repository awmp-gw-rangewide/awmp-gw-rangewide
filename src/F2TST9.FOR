C     Last change:  CA   13 Jul 2004    8:14 pm
C **********************************************************************

C     WARNING HARD WIRED ABUNDANCE ESTIMATES


      MODULE ARRAY_PAR

      INTEGER MXSIM, MXAGE, I1YR, I1CYR, I1SYR, ICEND, IENDYR, IENDY2

C : extended gray whale trial values
c      PARAMETER (MXSIM=5000, MXAGE=40, I1YR=-414, I1CYR=-414, I1SYR=-39,
c     +           ICEND=299,IENDYR=300,IENDY2=1100)

C : standard gray whale trial values
      PARAMETER (MXSIM=5000, MXAGE=40, I1YR=-450, I1CYR=-450, I1SYR=-39,
     +           ICEND=99,IENDYR=100,IENDY2=500)

C     MXSIM  Maximum number of simulations (=max value of NTRIAL)
C     MXAGE  Maximum number of age classes (=max value of MAXAGE)
C     I1YR   first year of population arrays
C     I1CYR  first year of catch arrays given to SLA
C     I1SYR  first year of survey arrays given to SLA
C     ICEND  last year of catch & survey arrays given to SLA
C     IENDYR last year of management (=max value of NYEAR)
C     IENDY2 last year of extra population simulations run when conditioning
C            For most runs IENDYR=IENDY2=100 and ICEND=IENDYR-1.

      END MODULE ARRAY_PAR

C --------------------------------------------------------------------

      MODULE DECL_STKVRS

      USE ARRAY_PAR
      COMMON /STKVRS/ PMATF(I1YR:IENDY2),N1PLUS(I1YR:IENDY2),
     +       FMATUR(0:MXAGE),RECF(0:MXAGE),SUR(0:MXAGE),UNRM(0:MXAGE),
     +       UNRF(0:MXAGE),RM(0:MXAGE),RF(0:MXAGE),Z,A1,FEC,RY,F0,
     +       SUMSUR,PROBE(-3:IENDY2),MAXAGE,OPTDD,IYRPRJ,INITYR,NY,
     +       ISTOCH,ILAG,OPTM,DOCON,OPTEE,PSURV(I1YR:IENDY2),
     +       SELSUVF,SELSUVM,FECEPS(0:IENDY2),SIGFECEPS,ProwEPS
      DOUBLE PRECISION  PMATF,N1PLUS,RM,RF,UNRM,UNRF,FMATUR,RECF,SUR,
     +       Z,A1,FEC,RY,F0,SUMSUR,PSURV,SELSUVF,SELSUVM,FECEPS,
     +       SIGFECEPS,PROWEPS
      INTEGER PROBE,MAXAGE,OPTDD,IYRPRJ,INITYR,NY,ISTOCH,ILAG,OPTM,
     +       DOCON,OPTEE

      COMMON /STKVR2/ CATCHM,CATCHF,PCALF,PMATA,KDD,A,PREGI,BA,AR,
     +       GAMMAL,RYS,NEXP
      DOUBLE PRECISION CATCHM(I1YR:IENDYR),CATCHF(I1YR:IENDYR),
     +       CATCHMBY(I1YR:IENDYR),CATCHFBY(I1YR:IENDYR),
     +       CATCHMCAN(I1YR:IENDYR),CATCHFCAN(I1YR:IENDYR),
     +       PCALF(I1YR:IENDY2),PMATA(I1YR:IENDY2),KDD(I1YR:IENDY2),
     +       A(I1YR:IENDY2),PREGI(0:MXAGE),BA(0:MXAGE),AR,GAMMAL,
     +       RYS(I1YR:IENDY2),NEXP(I1YR:IENDY2)
C      
      END MODULE DECL_STKVRS

C --------------------------------------------------------------------

      MODULE DECL_SETUP

      USE ARRAY_PAR
      COMMON /SETUP/   MSYL,MSYR,RREC,MAT50,AREC,PREGTG,OPTF,OPMSYL,
     +                 OPTK,OPTA,OPTI,ITENT,IYRTAR,R1PLUS
      DOUBLE PRECISION MSYL,MSYR,RREC,MAT50,AREC,PREGTG,R1PLUS
      INTEGER OPTF,OPMSYL,OPTK,OPTA,OPTI,ITENT,IYRTAR

      COMMON /INPUTS/ BIAS(I1SYR:IENDYR),NEED(0:IENDYR+10),
     +        CRATIO,CRATIO2,CRATIO3
     +        H1,H2,H3,BIASC,ISIGHT(-1:IENDYR),NTRIAL,OPTB,IFREQ,
     +        STRATG,ICAT1,ICATB, NEVENT,EVENTS,EVENT1,ISUR1,OPTHC,
     +        BIASDEF(I1SYR:IENDYR),
     +        CANCATCH(0:IENDYR+10),CANCATCHF
      DOUBLE PRECISION BIAS,NEED,CRATIO,CRATIO2,CRATIO3,
     +       BIASC,H1,H2,H3,BIASDEF,CANCATCH,CANCATCHF
      INTEGER ISIGHT,NTRIAL,OPTB,IFREQ,STRATG,ICAT1,ICATB,NEVENT,EVENTS,
     +        EVENT1,ISUR1,OPTHC
C
      END MODULE DECL_SETUP

C --------------------------------------------------------------------

      MODULE DECL_ENVCOM

      COMMON /ENVCOM / RHO,SIGEPS,SIGCON,BEXP,MUI,BMIN,BMAX
      DOUBLE PRECISION RHO,SIGEPS,SIGCON,BEXP(-800:800),MUI(-800:800),
     +                 BMIN,BMAX

C     BEXP is set in SETENV for range of values of MuI to solve integral A2.8

      END MODULE DECL_ENVCOM

C --------------------------------------------------------------------

      MODULE DECL_CONVRS

      COMMON /CONVRS/ PTARGT,ADULTS,JUVS,INVFEC,K1,TRANSA,ICAOPT
      DOUBLE PRECISION PTARGT,ADULTS,JUVS,INVFEC,K1
      INTEGER TRANSA,ICAOPT

      COMMON /PRIORS/ MSYLP,MSYRP,APARP,JMORTP,AMORTP,PREGNP,TPARP,
     +                TARGP,CVADP,CARRYP,BIASCP,RHOP,ILAGP,ARP,GAMMALP,
     +                CVCADP,ISEEDC,PBIASP
      DOUBLE PRECISION MSYLP(4),MSYRP(4),APARP(4),JMORTP(4),AMORTP(4),
     +       PREGNP(4),TPARP(4),TARGP(4),CVADP(4),CARRYP(4),BIASCP(4),
     +       RHOP(4),ILAGP(4),ARP(4),GAMMALP(4),CVCADP(4),PBIASP(4)
      INTEGER ISEEDC
      COMMON /PRIORC/ MSYLT,MSYRT,APART,JMORTT,AMORTT,PREGNT,TPART,
     +            TARGT,CVADT,CARRYT,BIASCT,RHOT,ILAGT,ART,GAMMALT,
     +            CVCADT,PBIAST
      CHARACTER*1 MSYLT,MSYRT,APART,JMORTT,AMORTT,PREGNT,TPART,
     +            TARGT,CVADT,CARRYT,BIASCT,RHOT,ILAGT,ART,GAMMALT,
     +            CVCADT,PBIAST

C     ICAOPT Option to include calf data in calculating the likelihood (gray)
C            using CALFINTE  1: Estimate bias or 2:Fixed Bias=1  or
C            using CALFLIKE  3: Estimate bias or 4:Fixed Bias=1

      END MODULE DECL_CONVRS

C --------------------------------------------------------------------

      MODULE DECL_DATVRS

      COMMON /DATVRS/ CV1EST,DOF,ETA,CVTRUE,OPTDET,OPTCV,CVADD2,
     +                DOFS,ISEED1,ISEED2,ISEED3,QVAR1,QVAR2,QVAR3,
     +                RELSIGMA1,RELSIGMA2,RELSIGMA3,QPASS,SIGMAPASS,
     +                PBIAS,BETA1,BETA2
      DOUBLE PRECISION CV1EST,DOF,ETA,CVTRUE,CVADD2,DOFS,
     +                QVAR1,QVAR2,QVAR3,QPASS,
     +                RELSIGMA1,RELSIGMA2,RELSIGMA3,SIGMAPASS,
     +                PBIAS,BETA1,BETA2
      INTEGER OPTDET,OPTCV,ISEED1,ISEED2,ISEED3
C
      END MODULE DECL_DATVRS

C --------------------------------------------------------------------

      MODULE DECL_STOCHM

      COMMON / STOCHM / RandNo,NormNO,LASTQ,Ivar,Qvar,ISTOC2
      DOUBLE PRECISION RandNo(1300000),NormNO(8500),LASTQ
      INTEGER Ivar,Qvar,ISTOC2
      
      END MODULE DECL_STOCHM

C --------------------------------------------------------------------
C --------------------------------------------------------------------
      
      PROGRAM F2TST
C                                                     28 APRIL 2004
C   F2-TST Version 7: Control program for testing potential aboriginal
C                     subsistence management procedures, Fishery type 2.
C
C     Recent updates:                                        date
C     Automated conditioning            Jun 2000; completed 5Jan 01
C     Stochastic model corrected                            1Nov 00
C     Stochastic RY calculation rewritten                    Feb 01
C     Time lag added                                        5Jan 01
C     Print pregnancy data if DOCON=-1                       Feb 01
C     Random number 0.0 disallowed + SolveB corrected        Feb 01
C     Inertial model added                                   Feb 01
C     1st year of population projection, epidemics,
C       time varying & integrated parameters                 Mar 01
C     Correct integer selection from uniform distribution   5Jun 01
C     Additional stochastic parameters                       Jun 01
C     Gray whale conditioning                                Oct 01
C     Correction setting SUR(0) when M changes [BR10d&11]    Oct 01
C     Gray whale CVs                                         Jan 02
C     Alternative episodic event settings                    Sep 03
C     Correction to inertia trial conditioning               Apr 04
C
C     The program generates the 'true' population using a series of
C     catches set by the SLC.  'Measurements' or estimates of the stock
C     size are generated, incorporating random noise, for input to the SLC.
C     
C     The management procedure (SLC) or Subsistence Limit Calculation is
C     a self-contained module called to set catch limits using abundance
C     estimates, past catches, need level and calf and mature ratios.
C
C     Notes:
C     The equation numbers refer to SC/53/4 Annex E Appendix 3B & C.
C     Define year 0 as the first year of management.  
C     Maturity is taken to mean age of first parturition throughout.
C     Time dependence in K must start AFTER 1st year of pop projection IYRPRJ
C     Cannot calculate H with strategic surveys
C     Strategic survey options assumes IFREQ=5
C
C Settings:        OPTCV   NY   REF[1:1]
C          Bowhead   1      1      B
C          Gray     2or3    0      G
C
C
C PARAMETERS -----------------------------------------------------------
C
C  Population parameters
C     PMATF(IYR)  True mature population size at start of year IYR
C                 (prior to CATCHF(IYR) being removed).  Set in STKUPA
C     N1PLUS(IYR)  1+ population (males+females).  Set in STKUPA
C     PSURV(IYR)   Size of surveyed population in IYR Set in STKUPA
C     KDD(IYR)    Carrying capacity for density dependent component of
C                 population.  Set in SETKA (up to year 0) & SETVRS
C     CATCHM(IYR) & CATCHF(IYR)  True catch of males/females in year IYR
C     CRATIO     Sex ratio of future catches
C     CATM(IYR) & CATF Catch of males/females in IYR, as passed to SLC.
C            Setup in INIT.  Historic values in error if BIASC.ne.1
C     BIASC  If BIASC.NE.1 the SLC is not given the true premanagement catch
C     NYEAR  Number of years of management.  Read in. 1-100
C     IYRPRJ Start of population projection on scale when management begins
C            in year 0.  May be before 1st catch (stochastic model)
C            or after 1st catch (Factor P)
C     INITYR Year when population pristine. May be earlier (stocastic model)
C     OPTDD  Density dependence on:   0: 1+ population  1: mature population.
C     OPTF   Fishing pattern defining MSYR is uniform selectivity on: 0:1+ 1:mat.pop
C     OPMSYL MSYL is defined on   0: 1+ population   1: mature population.
C     MSYL   MSY Level. Read in. See OPMSYL for definition.
C     MSYR   MSY rate.  Read in. See OPTF for definition.
C     Z      Density dependent exponent, calculated from MSYL (in MSYPAR).
C     A1     Resilience parameter (set in MSYPAR).
C     A(IYR) Resilience parameter in IYR. Set in SETKA (up to year 0) & SETVRS
C     MAT50  Age of maturity (knife edge). Set in RESET or conditioning
C            (Actually age of 1st parturition = age of maturity+1)
C     SUR(A) Survival rate of animals of age A. Sex independent. Set in RESET
C     FEC    Fecundity. See full definition in MSYPAR
C     RREC   Ratio of pristine recruited population to number of age 0
C     NY     minimum interval between calvings (1:bowhead 0:gray)
C     PROBE(IYR) % reduction in population if an epidemic occurs in IYR. Set in RESET.
C     EVENTS % reduction in population if an epidemic occurs in future. Read in
C     EVENT1 % reduction in population if an historic epidemic occurs. Read in
C     NEVENT No. of events (i.e. epidemics) occurring between yrs 1-50
C            If NEVENT=-1 population is reduced to 2000 in year 1.
C            If NEVENT=3 3 events occur in yrs 1-75 with at least 2 in yrs 1-50.
C            If NEVENT=5 10% reduction every 5 years
C     OPTEE  Defines epidemic type. 0:Uniform thru all ages  1:Youngest 1st. Read in
C     OPTHC  If OPTHC=1 change historic catch ratio 1600-1964 to be 50:50.
C                    =2 apply catch bias to total catch from 1940 to CATB GR08a
C     ILAG   Time lag in density dependence. Read in / set in GENP2 if OPTI>0
C     OPTK,OPTA,OPTM: options defining time dependence in K, A &/or M
C            over manangement period only (except tent model)
C            0: none  1: doubles  2: halves  3: sinusoidal  4: steps  5: tent
C     OPTI   Set for integrated trial: hist.catch bias, stochastic parameter
C            and (if OPTI=2) DD time lag.
C     AR     Inertia model A ratio = A/AI. Used to set intrinsic pregnancy rates
C     PREGTG Target maximum pregnancy rate (for conditioning) & in inertia model
C     GAMMAL Inertia model gamma parameter (used if ISTOCH=-1 or -2)
C
C Survey estimate parameters:
C     SIGHT(IYR) Absolute abundance estimate in IYR  eg sightings.
C                Set to -1 if no data available that year. Passed to SLC.
C     CVX(IYR)   Estimated CV of the sightings estimate
C                Set to -1 if no data available.  Passed to SLC.
C     BIAS(IYR)  Bias of absolute abundance estimates in IYR. Set in INIT
C     OPTB   BIAS changes linearly (or is constant) unless OPTB = 1:sinusoidal
C     ISIGHT(IYR) =1 in years when sightings surveys to be run. Set in INIT
C            1st survey made in year -1. If IFREQ=5 the 2nd is in year 4
C     CV1EST = CV(est), the expectation value of CVX when
C            P=ref.level (=0.6K for bowhead, 1968 value for gray). Read in.
C     ETA    Process error parameter.  If ETA = 0 there is no process error.
C            If OPTCV=0 ETA is read in; otherwise ETA is calculated from CVTRUE 
C     OPTDET Set on deterministic run  0: stochastic   1:deterministic
C     OPTCV  Defines how CVs are set. 0: ETA read in   1: bowhead
C                                     2: gray basecase 3: gray + add .1to CVtrue
C     ISUR1  Year of first sightings survey.  Passed to SLC
C
C Stochastic parameters
C     ISTOCH Population model: >0 = stochastic with   1: SIGQ=0  or  2: SIGQ>0)
C            0 = standard deterministic Pella Tomlinson model
C           -1 = inertia dynamics (intrinsic preg.rate) + gamma density dep.
C           -2 = gamma density dependence
C           -3 = inertia dynamics (intrinsic preg.rate) + PT density dependence
C     RHO = CORRELATION IN RECRUITMENT (=0.0 for no auto correlation) read in
C     SIGCON = SIGEPS / SQRT(1-RHO**2) (see adjunct 1: stochasticity in births)
C     BEXP is set in SETENV for range of values of MuI to solve integral A2.8
C     NRY  No. of Monte Carlo replicates.
C     RandNo,NormNO,Ivar,Qvar =Random nos.for binomial distributions if ISTOCH>0
C
C Need Parameters (in terms of number of whales)
C     NEED(IYR) Total need for years IYR to IYR+IQUOTA-1 where a new strike
C               limit is due in IYR. Set = -1 in other years.  Set in INIT.
C     IQUOTA No of years of quota = period for which strike limit is set
C            Strike limit is split equally to give annual value
C
C General
C     IYR    Current year
C     N      Current trial number  (1,NTRIAL)
C     NTRIAL Number of trials.  Read in.  1-400
C     IOUT   Output file
C     IN,IN4 Input files
C     REF    Reference number of run
C     DOCON  =0 if parameters read in; =-1 checking/printing nos. pregnant etc
C            =1 on conditioning run;   =2 on conditioning with extra printing;
C     ISEED1-4 Seeds for random number generators:
C            1: Set SIGHT   2: Set CVX   3 & 4: Set calf/mature ratios   5 Stochastic Probs
C
C DEFINITIONS ----------------------------------------------------------
C
      USE ARRAY_PAR
      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_ENVCOM
      USE DECL_CONVRS
      USE DECL_DATVRS
      USE DECL_STOCHM
C
      COMMON / AEP / AEPON
      INTEGER AEPON

      COMMON /SIMVRS/MSYLN(MXSIM),MSYRN(MXSIM),MAT50N(MXSIM),
     +               SURJN(MXSIM),SURN(MXSIM),CVADD(MXSIM),
     +               ARN(MXSIM),PREGTR(MXSIM),GAMLN(MXSIM),
     +               AGEJN(MXSIM),ISEEDS(MXSIM,5),ISEEN(MXSIM),
     +               BIASSCALE(MXSIM),PBIASV(MXSIM)
      DOUBLE PRECISION MSYLN,MSYRN,MAT50N,SURJN,SURN,CVADD,ARN,
     +                PREGTR,GAMLN,BIASSCALE,PBIASV
      INTEGER AGEJN,ISEEDS,ISEEN

      DOUBLE PRECISION CATCHQ,CATM(I1CYR:ICEND),CATF(I1CYR:ICEND),
     +       CATMBY(I1CYR:ICEND),CATFBY(I1CYR:ICEND),
     +       CATMCAN(I1CYR:ICEND),CATFCAN(I1CYR:ICEND),
     +       SIGHT(I1SYR:ICEND),CVX(I1SYR:ICEND),
     +       HIDEAL(0:ICEND),NEEDI,NVAL,QYR,PHIT,H,NEWQ,
     +       MSYL1,MSYLM,MSYR1,MSYRM,MSYLE,MSYRE,
     +       KN(MXSIM),PN(MXSIM),PTARGN(MXSIM),P2002N(MXSIM),CCONST,
     +       CM,CF,FBYCATCH,QPAR1,QPAR2
      INTEGER NYEAR,IYR,I,N,IOUT,IN,IN2,IN3,IN4,IYRQ,ISGHT,
     +        IYRMAN,NSIM,OK,IQUOTA,NRY,SLAOPT,SINDX
      CHARACTER REF*10,DESC*50,PROG*9,PARFIL*12,MATFIL*12,MANAGE*1
      LOGICAL INGRACE
      INTEGER GETLASTSURVEY
      REAL*8 DOQUOTA
      EXTERNAL GETLASTSURVEY,DOQUOTA

      DATA IOUT/8/,IN/7/,IN2/10/,IN3/11/,IN4/12/
      OPEN (IN,FILE='COPY.DAT',STATUS='OLD')
      OPEN (IN2,FILE='RANDOM.NUM',STATUS='OLD')
C
C
C INITIALISATION -------------------------------------------------------
C
      PROG = 'F2TST: V9'
      READ (IN,'(T37,A /A/2(/45X,A))') REF,DESC,PARFIL,MATFIL
C     Read management type & whether conditioning. Allocate output file.
C     If MANAGE=Z a zero strike limit is set & results output to AW-RESZ
C     If MANAGE=H the strike limit is set =Hideal & results output to AW-RESH
C     If MANAGE=C constant strike limit is set = CCONST
      OPEN (16,FILE='MANAGE.DAT',STATUS='OLD')
      READ (16,'(A1/I1/I4/F4.0/I4)') MANAGE, DOCON, NRY, CCONST, SLAOPT
      CLOSE(16) 
      OPEN (IOUT,FILE='AW-RES'// MANAGE,STATUS='UNKNOWN')
      WRITE (IOUT,'(/4A/1X,A/)')' CASE: ',REF,'     PROGRAM: ',PROG,DESC
C     Open Catch file (CATCHB.DAT or CATCHG.DAT or CATCHF.DAT)
      OPEN (IN3,FILE='CATCH'//REF(1:1)//'.DAT',STATUS='OLD')
      
      OPEN(UNIT=999,FILE="STOCHMODEL.OUT")
      
C     Call INIT to read in / set up parameters. Call PNTOUT to copy to output
      CALL INIT (CATM,CATF,CATMBY,CATFBY,CATMCAN,CATFCAN,
     +     SIGHT,CVX,NYEAR,IYRMAN,IQUOTA,SINDX)
     
C     Reset NRY=no. of Monte Carlo replicates used to set stochastic RY (see G)
      IF (ISTOCH.LE.0 .OR. STRATG.NE.0) NRY=0
      IF (ISTOCH.GT.0 .AND.NRY.EQ.0 .AND. MANAGE.EQ.'H')
     +  STOP 'ERROR: To use QUOTA=H IDEAL, use NRY>0 so RY can be set'
      CALL PNTOUT (IOUT,NYEAR,IQUOTA,PARFIL,NRY,IYRMAN)

C     Set up extra output files if printing extra info
      IF (DOCON.EQ.-1) THEN
        OPEN (30,FILE='PREG.OUT',STATUS='UNKNOWN')
        WRITE (30,*) REF,MANAGE
        OPEN (31,FILE='TIMEPAR.OUT',STATUS='UNKNOWN')
        WRITE (31,*) REF,MANAGE
      ENDIF
C     Read in random number seeds for all trials : store in ISEEDS
      READ (IN2,'((5I8))') ((ISEEDS(N,I),I=1,5),N=1,NTRIAL)
      CLOSE (IN2)
C
C     Call SETENV to set up relation between mu and b (for integral A2.8)
C     and BINSET sets log[N!/n!(N-n)!]
      IF (ISTOCH.GE.1) THEN
        IF (ISTOCH.EQ.2) CALL SETENV()
        CALL BINSET()
      ENDIF
C
C
C CONDITIONING -------------------------------------------------------
C
C     Do Conditioning - it is run separately from trials.
C                       MATFIL=Conditioning data file
      AEPON = 0
      IF (DOCON.GE.1) THEN
        OPEN(UNIT=14,FILE=MATFIL,STATUS='OLD')
        OPEN(UNIT=13,FILE='CONDIT.TMP',STATUS='UNKNOWN')
        CALL CONDG(IYRMAN,REF)
        STOP
      ENDIF  
C
C READ CONDITIONING RESULTS --------------------------------------------
C
C     Otherwise not a conditioning run: read parameter file name & parameters
      OPEN (IN4,FILE=PARFIL,STATUS='OLD')
      READ (IN4,*)
      DO 5 N=1,NTRIAL
        READ(IN4,*) AGEJN(N),MAT50N(N),MSYLN(N),MSYRN(N),SURJN(N),
     +   SURN(N),KN(N),PN(N),ISEEN(N),PTARGN(N),CVADD(N),
     +   DD,ARN(N),PREGTR(N),P2002N(N),QPAR1,DD,QPAR2,DD,DD,PBIASV(N)
     
      IF (SINDX.EQ.1) BIASSCALE(N) = 1.0
      IF (SINDX.EQ.2) BIASSCALE(N) = QPAR1
      IF (SINDX.EQ.3) BIASSCALE(N) = QPAR2
    5 CONTINUE
      CLOSE (IN4)
C
      QYR = DBLE(IQUOTA)
C
C     Call ISLC to initialise management routine
      CALL ISLC
C
C
C TRIALS BEGIN ---------------------------------------------------------
C
      DO 200 N = 1,NTRIAL
C
C       Reset parameters and reseed the random number generators
        CALL RESET(N,NYEAR,SIGHT,CVX,HIDEAL,NRY,CATM,CATF)
C
C       If wanted, print out info on pregnancy rates and calves
        IF (DOCON.EQ.-1) THEN
          CLOSE (30)
          OPEN (30,FILE='PREG.OUT',STATUS='OLD',ACCESS='APPEND')
          WRITE (30,'(A4,5A9,A4,I4)')
     +      'Yr','Calves','MatFem','AvailF','DetPreg', 'StocPrg','N=',N
          CLOSE (31)
          OPEN (31,FILE='TIMEPAR.OUT',STATUS='OLD',ACCESS='APPEND')
          WRITE (31,'(A4,5A9,I4)')'Yr','K','A','M(Ad)','Bias','PR  N=',N
        ENDIF
C
C       Age structured model.  
C       MSYPAR sets A1 & Z, SETKA sets PMATF, N1PLUS, and PSURV for IYR=IYRPRJ,-1
C       MSYP2 sets MSYL and MSYR in terms of other population components
        CALL MSYPAR(OK)
        CALL MSYP2 (MSYL1,MSYLM,MSYLE,MSYR1,MSYRM,MSYRE)
        CALL SETKA (KN(N),PN(N),PHIT,OK)
C       Set up time dependent parameters
        CALL SETVRS(NYEAR,ISEED3)
C
C       Special feature if there are future episodic events
        IF (NEVENT.NE.0) CALL FINDEXTRAS(NRY)

        IYR=-1
C       Call DATGEN to reset parameters and, if ISIGHT(-1)=1 to generate
C       the last premanagement survey estimate, SIGHT(-1), & its CV
        CALL DATGEN (SIGHT(IYR),CVX(IYR),1,PSURV(IYR),KN(N),
     +               BIAS(IYR),ISIGHT(IYR),PHIT)
C
C       Advance population to IYR=0
        CM = CATCHM(IYR) + CATCHMBY(IYR) + CATCHMCAN(IYR)
        CF = CATCHF(IYR) + CATCHFBY(IYR) + CATCHFCAN(IYR)
        CALL STKUPA (IYR,CM,CF,OK,NRY,NEED(0))

C       Check that target was achieved and print out new parameters
        PRINT '(I4,1X,A,A41,4F6.0)',N,REF,DESC,PHIT,PHIT-PTARGN(N),
     +                                PSURV(-1),PSURV(-1)-P2002N(N)
        IF (ABS(PHIT-PTARGN(N)).GT.10.D0)  STOP ' Target popln not hit'
        IF (ABS(PSURV(-1)-P2002N(N)).GT.10.D0) 
     +      STOP' Yr -1 popln not hit'
        WRITE (IOUT,'(/''Trial:'',I4,A10,4I8 / 12X,2(''MSY'',3(A,F8.5,4X
     +      )))') N,'Seeds:',(ISEEDS(N,I),I=1,4),'L 1+:',MSYL1,'Mat:',
     +      MSYLM,'Exp:',MSYLE,'R 1+:',MSYR1,'Mat:',MSYRM,'Exp:',MSYRE
        WRITE (IOUT,'(2(A,I2),11(1X,A,F6.4))')  ' AM:',NINT(MAT50N(N)),
     +        ' AJ:',AGEJN(N),  'SJ:',SURJN(N),'SA:',SURN(N),
     +        'FMax:',PREGTR(N),'AR:',ARN(N),  'Gamma:',GAMLN(N),
     +        'CVAdd:',CVADD(N),'FEC:',FEC,'F0:',F0
C        WRITE(IOUT,*) FEC,F0,FEC*(1+A(I1YR)*(1.0-MSYLM**Z)),
C     +       FEC*(1+A(I1YR))
        WRITE(999,9991) N,AGEJN(N),SURJN(N),SURN(N),SUR(0),SUR(1),
     +   SUR(MAXAGE),MAT50N(N),A1,FEC,Z,AREC
9991    FORMAT(1x,I4,1x,I4,1x,5(F7.5,1x),F6.1,1x,4(F10.7,1x))     

C   Notes: Regular surveys are run every IFREQ years, when ISIGHT(IYR)=1.
C   Strategic surveys: see section Dii
C   If STRATG.ne.0 => run extra strategic surveys  if abundance is half last survey

C       Reset IYRQ to set quota in year 0
        IYRQ = 0

C       Reinitialise the management routine (if required)
        NSIM = N
        CALL RSLC (ICAT1,ISUR1,NSIM)
C
C       Find the F for the bycatch
        TOP = 0
        BOT = 0
        DO 99 IYR = -6,-2
         TOP = TOP + CATMBY(IYR)+CATFBY(IYR)
         BOT = BOT + N1PLUS(IYR)
99      CONTINUE
        FBYCATCH = TOP/BOT
C
        DO 83 IYR = 0,NYEAR-1
         BIAS(IYR) = BIASDEF(IYR)*BIASSCALE(N)
83      CONTINUE         
C
        INGRACE = .FALSE.
        DO 100 IYR = 0,NYEAR-1
C
C         Set ISGHT=1 to run a regular survey. May be reset 'strategically'
          ISGHT = ISIGHT(IYR)


C         Test if a new quota, NEWQ, is to be set this year for use in IYRQ
C         (if NEED(IYR+1)>0, IYR=0
          IF (NEED(IYR+1).GT.0.D0 .OR. IYR.EQ.0) THEN
           IF (NEED(IYR+1).GT.0.D0) IYRQ = IYR+1
           NEEDI = NEED(IYRQ)
C
C          Find H-ideal using mean need over quota period (unless RY unset)
C          HIDEAL was initialised = -1 in RESET
           NVAL = NEEDI / QYR
           IF (N1PLUS(IYR).LT.H1) THEN
            H = 0.D0
           ELSE IF (N1PLUS(IYR).LE.MSYL1*KN(N)) THEN
            H = H2 * RY
           ELSE
            H = H3 * MSYR1 * MSYL1 * KN(N)
           ENDIF
           IF (RY.NE.-1.D0) HIDEAL(IYRQ) = MAX(0.D0, MIN(H, NVAL))
           NEWQ = DOQUOTA(MANAGE,
     +               CATM,CATF,CATMBY,CATFBY,CATMCAN,CATFCAN,
     +               SIGHT,CVX,IYRQ,ICAT1,ISUR1,NEEDI,NSIM,IQUOTA,
     +               SLAOPT,CCONST,HIDEAL(IYRQ),NVAL,QYR,IYRMAN)

          ENDIF
C
C         Just had a survey
          ISURVY = GETLASTSURVEY(IYR,CVX)
          IF (ISURVY.EQ.IYR-1) THEN
           IF (INGRACE) THEN
            NEWQ = DOQUOTA(MANAGE,
     +               CATM,CATF,CATMBY,CATFBY,CATMCAN,CATFCAN,
     +               SIGHT,CVX,IYRQ,ICAT1,ISUR1,NEEDI,
     +               NSIM,IQUOTA,SLAOPT,CCONST,HIDEAL(IYRQ),NVAL,
     +               QYR,IYRMAN)
            INGRACE = .FALSE.
            TOTALQUOTA = NEWQ*QYR
            TOTALCATCH = 0
            DO 61 I=IYRLSTQUOTA,IYR-1
             TOTALCATCH = TOTALCATCH + CATCHM(I)+CATCHF(I)          
   61       CONTINUE  
            YrsLeft = IYRLSTQUOTA+IQUOTA-1 - IYR+1
            CATCHQ = (TOTALQUOTA-TOTALCATCH)/YrsLeft
C           Store values for next IQUOTA years
            DO 62 I=IYR,MIN(IYRLSTQUOTA+IQUOTA-1,NYEAR-1)
              CATM(I) = CATCHQ * CRATIO
              CATF(I) = CATCHQ - CATM(I)
              CATCHM(I) = CATM(I)
              CATCHF(I) = CATF(I)
   62       CONTINUE
           ENDIF
C
C          Strategic surveys
           IF (STRATG.EQ.1) THEN
            ISURVY2 = GETLASTSURVEY(ISURVY-1,CVX)
            IF (SIGHT(ISURVY).LT.0.5*SIGHT(ISURVY2)) THEN
             WRITE(*,*) "Doing Strategic survey",ISURVY,ISURVY2
             CALL DATGEN (SIGHT(IYR),CVX(IYR),0,PSURV(IYR),KN(N),
     +                 BIAS(IYR),1,PHIT)
            
            ENDIF
           ENDIF 
          ENDIF
C
C         DATGEN sets abundance estimate and CV in IYR if ISGHT=1: Section B1
          CALL DATGEN (SIGHT(IYR),CVX(IYR),0,PSURV(IYR),KN(N),
     +                 BIAS(IYR),ISGHT,PHIT)

C         If NEED>0 the new quota will be applied this year for IQUOTA years
          IF (NEED(IYR).GT.0D0) THEN
            CATCHQ = NEWQ
C           Store values for next IQUOTA years
            DO 50 I=IYR,MIN(IYR+IQUOTA-1,NYEAR-1)
              CATM(I) = CATCHQ * CRATIO
              CATF(I) = CATCHQ - CATM(I)
              CATCHM(I) = CATM(I)
              CATCHF(I) = CATF(I)
   50       CONTINUE
C
C           When was the last survey undertaken
            ISURVY = GETLASTSURVEY(IYR,CVX)
C
C           Halve the quota
            IF (IYR.GT.(ISURVY+10)) THEN
             INGRACE = .TRUE.
             DO 51 I=IYR+IQUOTA/2,MIN(IYR+IQUOTA-1,NYEAR-1)
              CATM(I) = 0
              CATF(I) = 0
              CATCHM(I) = CATM(I)
              CATCHF(I) = CATF(I)
   51        CONTINUE
            ENDIF
C
C           No quota
            IF (IYR.GT.(ISURVY+20)) THEN
             INGRACE = .TRUE.
             DO 52 I=IYR,MIN(IYR+IQUOTA-1,NYEAR-1)
              CATM(I) = 0
              CATF(I) = 0
              CATCHM(I) = CATM(I)
              CATCHF(I) = CATF(I)
   52        CONTINUE
            ENDIF
C
C           Set Year of last
            IYRLSTQUOTA = IYR
C
          ENDIF
C
C         Generate future bycatch data (UPDATE)
          IF (MANAGE.EQ.'Z') THEN
           CATCHMBY(IYR) = 0
           CATCHMBY(IYR) = 0
           CATCHMCAN(IYR) = 0
           CATCHFCAN(IYR) = 0
          ELSE
           CATCHMBY(IYR) = FBYCATCH*N1PLUS(IYR)*CRATIO2
           CATCHFBY(IYR) = FBYCATCH*N1PLUS(IYR)*(1-CRATIO2)
           CATCHMCAN(IYR) = CANCATCH(IYR)*CRATIO3
           CATCHFCAN(IYR) = CANCATCH(IYR)*(1-CRATIO3)
          ENDIF 

C         Advance stock to next year, ie remove catch & set PMATF(IYR+1).
C         If quota CATCHQ > recruited population then it is reset in STKUPA
          CM = CATCHM(IYR) + CATCHMBY(IYR) + CATCHMCAN(IYR)
          CF = CATCHF(IYR) + CATCHFBY(IYR) + CATCHFCAN(IYR)
          CALL STKUPA (IYR,CM,CF,OK,NRY,NEED(IYR+4))

C         Set CATM & CATF to actual catch taken
          CATM(IYR) = CATCHM(IYR)
          CATF(IYR) = CATCHF(IYR)
          CATMBY(IYR) = CATCHMBY(IYR)
          CATFBY(IYR) = CATCHFBY(IYR)
          CATMCAN(IYR) = CATCHMCAN(IYR)
          CATFCAN(IYR) = CATCHFCAN(IYR)

  100   CONTINUE

C       Call REPORT to print out PMATF & CATCHM in year IYR
        CALL REPORT (IOUT,NYEAR,HIDEAL,SIGHT,CVX,ISUR1,IYRMAN)

  200 CONTINUE

      CLOSE (IOUT)
      CLOSE (1)
      CLOSE (30)
      STOP
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      INTEGER FUNCTION GETLASTSURVEY(IYR,CVX)

C     Find when the last survey was conducted
      
      USE DECL_STKVRS
      REAL*8 CVX(I1SYR:ICEND)
      INTEGER IYR
      
      DO 10000 IY = IYR,INITYR,-1
       IF (CVX(IY).GT.0) GOTO 10100
10000 CONTINUE
10100 CONTINUE
      GETLASTSURVEY = IY

      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      REAL*8 FUNCTION DOQUOTA(MANAGE,CATM,CATF,
     +               CATMBY,CATFBY,CATMCAN,CATFCAN,
     +               SIGHT,CVX,IYRQ,
     +               ICAT1,ISUR1,NEEDI,
     +               NSIM,IQUOTA,SLAOPT,CCONST,HIDEAL,NVAL,
     +               QYR,IYRMAN)
C
      USE ARRAY_PAR
      CHARACTER*1 MANAGE
      REAL*8 CATM(I1CYR:ICEND),CATF(I1CYR:ICEND)
      REAL*8 CATMBY(I1CYR:ICEND),CATFBY(I1CYR:ICEND)
      REAL*8 CATMCAN(I1CYR:ICEND),CATFCAN(I1CYR:ICEND)
      REAL*8 SIGHT(I1SYR:ICEND),CVX(I1SYR:ICEND)
      REAL*8 NEEDI,NEWQ,CCONST,HIDEAL,NVAL,QYR
      INTEGER ICAT1,ISUR1,NSIM,IQUOTA,SLAOPT,IYRQ,IYRMAN


C     Call SLC (subsistence limit calculation) to set the block quota.
      IF (MANAGE.EQ.' ') THEN
       CALL SLCG (CATM,CATF,CATMBY,CATFBY,CATMCAN,CATFCAN,
     +               SIGHT,CVX,IYRQ,ICAT1,ISUR1,
     +               NEEDI, NEWQ,NSIM,IQUOTA,SLAOPT,IYRMAN)
       NEWQ = NEWQ / QYR
C      Ensure 0 <= NEWQ <= NEED
       NEWQ = MAX(0.D0, MIN(NEWQ,NVAL))
C     
      ELSE IF (MANAGE.EQ.'Z') THEN
       NEWQ = 0.D0
      ELSE IF (MANAGE.EQ.'Y') THEN
       NEWQ = 0.D0
      ELSE IF (MANAGE.EQ.'C') THEN
       NEWQ = CCONST
      ELSE IF (MANAGE.EQ.'H') THEN
       NEWQ = HIDEAL
      ELSE
       STOP 'Unknown management option - not blank, Z or H'
      ENDIF
      DOQUOTA = NEWQ
C      
      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      SUBROUTINE INIT (CATM,CATF,CATMBY,CATFBY,CATMCAN,CATFCAN,
     +   SIGHT,CVX,NYEAR,IYRMAN,IQUOTA,SINDX)

C     INIT reads parameter file and initialises variables
C
      USE DECL_STKVRS
      USE DECL_ENVCOM
      USE DECL_SETUP
      USE DECL_DATVRS
      USE DECL_CONVRS
C
      DOUBLE PRECISION CATM(I1CYR:ICEND),CATF(I1CYR:ICEND),
     +        CATMBY(I1CYR:ICEND),CATFBY(I1CYR:ICEND),
     +        CATMCAN(I1CYR:ICEND),CATFCAN(I1CYR:ICEND),
     +        SIGHT(I1SYR:ICEND),CVX(I1SYR:ICEND),
     +        BINC,BIASH1,BIASH2,BIASF,CM,CF,CINC,
     +        NEEDS(10)
      INTEGER NYEAR,IYR,I,IN,IN3,ISTART,IYRMAN,IQUOTA,IGSURV,NSURV
      INTEGER SINDX,II

      DATA IN/7/,IN3/11/
C
C     Read in model parameters/options, checking they are within allowed range
      READ (IN,'((44X,I4))') NTRIAL,NYEAR,ISTART,IYRPRJ,IYRMAN,IQUOTA,
     +   MAXAGE,ISTOCH,OPTF,OPMSYL,OPTDD,OPTK,OPTA,OPTM,ILAG,NEVENT,
     +   EVENTS,EVENT1,OPTEE,OPTHC,OPTI,NY,IYRTAR
C
C     The model begins in actual year ISTART. IYRMAN = 1st year of management.
C     Re-scale to define year 0 as first year of management. 
C     IYRPRJ 1st year of population projection. May be before 1st catch
C         (stochastic model) or after 1st catch (Section Dx + Gray basecase)
      IYRPRJ = IYRPRJ - IYRMAN
      INITYR = ISTART - IYRMAN
      IYRTAR = IYRTAR - IYRMAN
      IF (MAXAGE.GE.MXAGE .OR. MAXAGE.LE.2) STOP 'MAXAGE out of range'
      IF (NYEAR .LT.0 .OR. NYEAR.GT.IENDYR) STOP 'NYEAR out of range'
      IF (INITYR.GT.IYRPRJ) STOP 'INITYR out of range'
      IF (-ILAG.LT.IYRPRJ)  STOP 'ILAG out of range'
      ITENT = -2
C     For tent K or A set ITENT = -51 = year tent shape change begins
      IF (OPTK.EQ.5 .OR. OPTA.EQ.5) ITENT = -51
      IF (IYRPRJ.LT.I1YR.OR.IYRPRJ.GE.ITENT)STOP 'IYRPRJ out of range'
C
      READ (IN,'((45X,F12.8))') RHO,SIGEPS,CRATIO,CRATIO2,CRATIO3,BIASC
      READ (IN,'((45X,F12.8))') SIGFECEPS,ProwEps
      READ (IN,'((45X,F12.8))') SELSUVM,SELSUVF
      READ (IN,'((45X,F9.2))') CANCATCH(0),CANCATCHF
      READ (IN,'((44X,I4))') ICAT1,ICATB
      ICAT1 = ICAT1 - IYRMAN
      ICATB = ICATB - IYRMAN
      IF (ICAT1.LT.I1CYR .OR. ICAT1.GE.0) STOP 'ICAT1 out of range'
      READ (IN,'((45X,F9.2))') AREC
      READ (IN,'((45X,I4))') NEEDINDEX

C     Set stochastic parameter SIGCON = SIGEPS/SQRT(1-RHO**2) =const in eqnA2.7
      SIGEPS = SQRT(SIGEPS)
      SIGCON = SIGEPS / SQRT(1.D0 - RHO**2)
      IF (SIGCON.GT.0.D0.AND.ISTOCH.EQ.1) STOP 'SIGCON>0 but ISTOCH=1'

C     Read in abundance data parameters
      READ(IN,'(/(44X,I4))') ISUR1,IGSURV,STRATG,OPTDET,OPTCV,IFREQ,OPTB
      ISUR1 = ISUR1-IYRMAN
      IGSURV = IGSURV-IYRMAN
      IF (ISUR1.GE.0.OR.ISUR1.LT.I1SYR) STOP 'ERROR IN SURVEY YEAR'
      IF (IGSURV.LT.-1) STOP 'ERROR IN DATE OF 1ST GENERATED SURVEY'
      READ (IN,'((45X,F9.2))') BIASH1,BIASH2,BIASDEF(25),BIASF,DOF,
     + CV1EST,BETA1,BETA2
      READ(IN,'((44X,I4))') SINDX
      READ(IN,'((44X,I6))') ISEEDC
C
C *** PRIORS ***
C     Read in the formal priors (used for checking when not conditioning)
C     (Some may not be used)
      READ (IN,*)
      CALL RPRIOR (MSYLT,MSYLP,IN)
      CALL RPRIOR (MSYRT,MSYRP,IN)
      CALL RPRIOR (APART,APARP,IN)
      CALL RPRIOR (JMORTT,JMORTP,IN)
      CALL RPRIOR (AMORTT,AMORTP,IN)
      CALL RPRIOR (PREGNT,PREGNP,IN)
      CALL RPRIOR (TPART,TPARP,IN)
      CALL RPRIOR (TARGT,TARGP,IN)
      CALL RPRIOR (CVADT,CVADP,IN)
      CALL RPRIOR (PBIAST,PBIASP,IN)
      CALL RPRIOR (CARRYT,CARRYP,IN)
      CALL RPRIOR (GAMMALT,GAMMALP,IN)
      CALL RPRIOR (ART,ARP,IN)
      IF (OPTI.NE.0) THEN
       CALL RPRIOR (BIASCT,BIASCP,IN)
       CALL RPRIOR (RHOT,RHOP,IN)
       CALL RPRIOR (ILAGT,ILAGP,IN)
      ENDIF
      CLOSE (IN)
C
C *** CATCHES ***
      DO 20 IYR = I1CYR,ICAT1-1
        CATM(IYR) = 0.D0
        CATF(IYR) = 0.D0
   20 CONTINUE
      DO 21 IYR = I1YR,ICAT1-1
        CATCHM(IYR) = 0.D0
        CATCHF(IYR) = 0.D0
   21 CONTINUE
C     Read catches by sex into arrays given to the SLC
      READ (IN3,*)
      DO 22 IYR = ICAT1,-1
        READ (IN3,*) I,CM,CF,CMBY,CFBY,CMCAN,CFCAN
        CATM(IYR) = CM 
        CATF(IYR) = CF
        CATMBY(IYR) = CMBY
        CATFBY(IYR) = CFBY
        CATMCAN(IYR) = CMCAN 
        CATFCAN(IYR) = CFCAN
        IF (I-IYRMAN.NE.IYR) STOP 'ERROR ON READING CATCH FILE'

C       Historic catches up to year ICATB may be biased (unknown to SLC)
C       This will be reset in GENP2 if OPTI>0 (integrated trials)      (see Dv)
        IF (BIASC.NE.1.D0 .AND. IYR.LE.ICATB .AND. OPTHC.LE.1) THEN
          CM = CM * BIASC
          CF = CF * BIASC
          CMBY = CMBY * BIASC
          CFBY = CFBY * BIASC
          CMCAN = CMCAN * BIASC
          CFCAN = CFCAN * BIASC
        ENDIF

C       Copy to catch array for population projection
C       Any catches before IYRPRJ (=start of projection) are ignored
        CATCHM(IYR) = CM 
        CATCHF(IYR) = CF 
        CATCHMBY(IYR) = CMBY 
        CATCHFBY(IYR) = CFBY 
        CATCHMCAN(IYR) = CMCAN
        CATCHFCAN(IYR) = CFCAN
   22 CONTINUE
C
C     If OPTHC=1 change historic catch ratio 1600-1964 to 50:50 (unknown to SLC)
      IF (OPTHC.EQ.1)  THEN
        DO 24 IYR = ICAT1,(1964-IYRMAN)
          CATCHM(IYR) = (CATCHM(IYR)+CATCHF(IYR)) * 0.5D0
          CATCHF(IYR) = CATCHM(IYR)
          CATCHMBY(IYR) = (CATCHMBY(IYR)+CATCHFBY(IYR)) * 0.5D0
          CATCHFBY(IYR) = CATCHMBY(IYR)
          CATCHMCAN(IYR) = (CATCHMCAN(IYR)+CATCHFCAN(IYR)) * 0.5D0
          CATCHFCAN(IYR) = CATCHMCAN(IYR)
   24   CONTINUE
      ELSE IF (OPTHC.EQ.2) THEN
        DO 25 IYR = (1940-IYRMAN),ICATB
          CATCHM(IYR) = CATCHM(IYR) * BIASC
          CATCHF(IYR) = CATCHF(IYR) * BIASC
          CATCHMBY(IYR) = CATCHMBY(IYR) * BIASC
          CATCHFBY(IYR) = CATCHFBY(IYR) * BIASC
          CATCHMCAN(IYR) = CATCHMCAN(IYR) * BIASC
          CATCHFCAN(IYR) = CATCHFCAN(IYR) * BIASC
   25   CONTINUE
      ENDIF
C
C     Set historic bias array (BIAS): change linearly from BIASH1 to BIASH2
      BINC = (BIASH2-BIASH1) / DBLE(-ISUR1-1)
      BIAS(ISUR1) = BIASH1
      DO 35 IYR = ISUR1+1,-1
        BIAS(IYR) = BIAS(IYR-1) + BINC
   35 CONTINUE

C     If EVENT1 > 0 there was an epidemic in yr -3 with EVENT1% reduction. Set PROBE
      IF (EVENT1.GT.0) PROBE(-3) = EVENT1
C
C     The rest is not needed for conditioning run
      IF (DOCON.GE.1) RETURN
C
C *** ABUNDANCE ESTIMATES ***
C     Initialise SIGHT & CVX. 
      DO 40 IYR = I1SYR,-1
        SIGHT(IYR) = -1.D0
        CVX(IYR) = -1.D0
   40 CONTINUE

      READ (IN3,'(// I2 //)') NSURV
      DO 45 II = 1,NSURV
        READ (IN3,*) I,SIGHTV,CVXV
        IYR = I - IYRMAN
        SIGHT(IYR) = SIGHTV
        CVX(IYR) = CVXV
        WRITE(*,*) SIGHT(IYR),CVX(IYR)
   45 CONTINUE
      CLOSE(IN3)
      IF (IYR.GE.IGSURV) STOP 'ERROR(4) IN SURVEY YEAR'
C
C
C *** Initialise other variables:   ------------------------------------
C
C     Set ISIGHT=1 in survey years. IGSURV=1st generated survey
      DO 50 IYR=-1,IENDYR
        ISIGHT(IYR) = 0
   50 CONTINUE
      DO 52 IYR=IGSURV,NYEAR-2,IFREQ
        ISIGHT(IYR) = 1
   52 CONTINUE

C     Set annual NEED array: Linear
      OPEN(UNIT=788,FILE="NEED.CSV")
      READ(788,*)
      READ(788,*)
      READ(788,*)
      DO 60 IYR = 0,NYEAR+10
       READ(788,*) I,(NEEDS(II),II=1,4)
       NEED(IYR) = NEEDS(NEEDINDEX)
60    CONTINUE       
C     Set Canadian array: Linear
      CINC = (CANCATCHF - CANCATCH(0)) / DBLE(NYEAR)
      DO 61 IYR = 1,NYEAR+10
        CANCATCH(IYR)  = CANCATCH(IYR-1) + CINC
   61 CONTINUE
C
C     Now reset need array to total value for each IQUOTA year period.
C     NEED(IYR)=0 in other years (when there is no new strike limit)
      DO 70 IYR = 0,NYEAR-1,IQUOTA
      DO 70 I=IYR+1,IYR+IQUOTA-1
         NEED(IYR) = NEED(IYR)+NEED(I)
         NEED(I) = 0.D0
   70 CONTINUE
C     Extend array so program doesn't fall over when testing NEED(IYR+4) in main
      DO 71 I=0,3
        NEED(NYEAR+I) = 0.D0
   71 CONTINUE

C     Set bias array (BIAS): changes linearly unless OPTB = 1:sinusoidal
C     BIAS in year 25 was read in
      BIASDEF(0) = BIASH2
      IF (OPTB.EQ.0 .AND. BIASH2.NE.BIASDEF(25)) THEN
C       Avoid discontinuity at year 0: change linearly from BIASH2 to
C       BIASDEF(25) in yr 25.  Constant from year 25. See Div
        BINC = (BIASDEF(25)-BIASH2) / DBLE(25)
        DO 80 IYR = 1,25
          BIASDEF(IYR) = BIASDEF(IYR-1) + BINC
   80   CONTINUE
        BINC = (BIASF-BIASDEF(25)) / DBLE(NYEAR-26)
        DO 81 IYR = 26,NYEAR-1
          BIASDEF(IYR) = BIASDEF(IYR-1) + BINC
   81   CONTINUE
      ELSE IF (OPTB.EQ.0) THEN
        BINC = (BIASF-BIASDEF(0)) / DBLE(NYEAR-1)
        DO 82 IYR = 1,NYEAR-1
          BIASDEF(IYR) = BIASDEF(IYR-1) + BINC
   82   CONTINUE
      ELSE
C       Cyclic bias.
        CALL TIMDEP(BIASDEF,BIASDEF(0),0.d0,I1SYR,ICEND,0,NYEAR-1,3)
      ENDIF
      PRINT '('' Bias: '',F7.3 / 10(10F7.3 /) )',(BIAS(I),I=-1,100)

      RETURN
      END


C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE RESET (N,NYEAR,SIGHT,CVX,HIDEAL,NRY,CATM,CATF)
C
C     RESET values for new trial

      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_DATVRS

      COMMON /SIMVRS/MSYLN(MXSIM),MSYRN(MXSIM),MAT50N(MXSIM),
     +               SURJN(MXSIM),SURN(MXSIM),CVADD(MXSIM),
     +               ARN(MXSIM),PREGTR(MXSIM),GAMLN(MXSIM),
     +               AGEJN(MXSIM),ISEEDS(MXSIM,5),ISEEN(MXSIM),
     +               BIASSCALE(MXSIM),PBIASV(MXSIM)
      DOUBLE PRECISION MSYLN,MSYRN,MAT50N,SURJN,SURN,CVADD,ARN,
     +                PREGTR,GAMLN,BIASSCALE,PBIASV
      INTEGER AGEJN,ISEEDS,ISEEN,ISEED5

      EXTERNAL XNORM
      DOUBLE PRECISION SIGHT(I1SYR:ICEND),CVX(I1SYR:ICEND),
     +       HIDEAL(0:ICEND),SN,SJN,CATM(I1CYR:ICEND),CATF(I1CYR:ICEND)
      DOUBLE PRECISION MA5(55),EPS,XNORM
      INTEGER N,NYEAR,IYR,AGEJ,J,NRY,NYRS,ISEEDZ,INEXT5,INXTP5
C
C     Set new negative values for the random number generator seeds -
C     the generator is reset whenever a negative seed is used.
      ISEED1 = ISEEDS(N,1)
      ISEED2 = ISEEDS(N,2)
      ISEED3 = ISEEDS(N,3)
      ISEED4 = ISEEDS(N,4)
      ISEED5 = ISEEDS(N,5)

      MSYL = MSYLN(N)
      MSYR = MSYRN(N)
      MAT50 = MAT50N(N)
      SJN = SURJN(N)
      SN  = SURN(N)
      AGEJ  = AGEJN(N)
      ISEEDZ = ISEEN(N)
      CVADD2 = CVADD(N)*CVADD(N)
      AR = ARN(N)
      GAMMAL = GAMLN(N)
      PREGTG = PREGTR(N)
      IF (AGEJ.GT.MAXAGE .OR. MAT50.GT.MAXAGE) STOP 'ERROR 1 in RESET'
      IF (MSYL.LT.MSYLP(1).OR.MSYL.GT.MSYLP(2)) STOP 'ERROR 2 in RESET'
      IF (MSYR.LT.MSYRP(1).OR.MSYR.GT.MSYRP(2)) STOP 'ERROR 3 in RESET'
      PBIAS = PBIASV(N)
C
C     Set survival rates {Eqn A1.2}
      DO 5 J = 0,AGEJ
       SUR(J) = SJN
   5  CONTINUE
      DO 6 J = AGEJ+1,MAXAGE
       SUR(J) = SN
   6  CONTINUE
      IF (AGEJ.GT.0) SUR(0) = SJN*SN
C
C     If OPTI>0 set integrated parameters - trial BR13
      IF (OPTI.GT.0) CALL GENP2(ISEEDZ)

C     Generate random variables (more are needed to set RY)
      NYRS = MAX((NYEAR-IYRPRJ+1),NRY)
      IF (ISTOCH.GE.1) CALL SETRAN(ISEEDZ,MAXAGE,ISTOCH,NYRS)

C     If OPTM>0 set up increment for future changes to SUR array - see Dviii
      IF (OPTM.GT.0) CALL SETSUR(0,AGEJ,0)
C
      EPS = SIGFECEPS * XNORM (1.D0,0.D0,ISEED5,MA5,INEXT5,INXTP5)
      FecEps(0) = EPS
      DO 20 IYR = 1,NYEAR
       EPS = SIGFECEPS * XNORM (1.D0,0.D0,ISEED5,MA5,INEXT5,INXTP5)
       FecEps(IYR) = ProwEps*FecEps(IYR-1)+ SQRT(1-ProwEps**2.0)*EPS
20    CONTINUE       
C
C     Reinitialise HIDEAL & arrays which are passed to the SLC.
      DO 10 IYR=0,NYEAR-1
        CATM(IYR) = 0.D0
        CATF(IYR) = 0.D0
        HIDEAL(IYR)= -1.D0
        SIGHT(IYR) = -1.D0
        CVX(IYR)   = -1.D0
   10 CONTINUE
C
      RETURN
      END

C **********************************************************************
C
      SUBROUTINE GENP2(ISEED)
C
C     Generate parameters from prior distributions for integrated trials (BR13)
C
      USE DECL_STKVRS
      USE DECL_ENVCOM
      USE DECL_SETUP
      USE DECL_CONVRS

      DOUBLE PRECISION OBIAS,MA(55),RAN1
      INTEGER ISEED,ISEEDZ,IYR,INEXT,INEXTP
      EXTERNAL RAN1
      SAVE INEXT,INEXTP,MA
C
C     Seed generation - don't change initial value
C     RAN1 now called directly. Old version used PRIOR function, but lead to
C          reproducibility problems (due to PRIOR's stored parms).
      ISEEDZ = ISEED
      ISEEDZ  = INT(-RAN1(ISEEDZ,MA,INEXT,INEXTP)*1000000.D0)
C
C     Generate and apply the historic catch bias U[0.5,1.5]
      OBIAS = BIASC
      BIASC = RAN1(ISEEDZ,MA,INEXT,INEXTP)*(BIASCP(2)-BIASCP(1))
     +                                    +BIASCP(1)
      DO 30 IYR=ICAT1,ICATB
        CATCHM(IYR) = CATCHM(IYR) * BIASC / OBIAS
        CATCHF(IYR) = CATCHF(IYR) * BIASC / OBIAS
   30 CONTINUE
C
      IF (ISTOCH.EQ.2) THEN
C       Generate the serial correlation U[0.47,0.95]
        RHO = RAN1(ISEEDZ,MA,INEXT,INEXTP)*(RHOP(2)-RHOP(1))+RHOP(1)
C       Reset stochastic parameter SIGCON & relation between mu & b
C ****  Recheck this later - Andre has taken out sqrt? **************** !!!!!!
        SIGEPS = SQRT((1.D0 - RHO**2)*0.05845467d0)
c aep   SIGEPS = (1.D0 - RHO**2)*0.05845467d0
        SIGCON = SIGEPS / SQRT(1.D0 - RHO**2)
        CALL SETENV()
      ENDIF
C
C     Generate the density dependent time lag U[0,30] if OPTI=2
      IF (OPTI.EQ.2) ILAG = NINT(RAN1(ISEEDZ,MA,INEXT,INEXTP) *
     +                           (ILAGP(2)-ILAGP(1)) + ILAGP(1))

      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
      SUBROUTINE SETVRS (NYEAR,ISEED3)
C
C     Reset parameters which are time dependent from year 0  (Dvi-ix)
C     (except the varying mortality rate which is done in STKUPA)

      USE DECL_STKVRS
      USE DECL_SETUP

      DOUBLE PRECISION MA(55),RAN1,A2
      INTEGER NYEAR,IYR,INEXT,INXTP,ISEED3,ISEED,I
      EXTERNAL RAN1

      DO 15 IYR = 0,NYEAR
        PROBE(IYR) = 0
        KDD(IYR) = KDD(IYRPRJ)
        A(IYR)   = A1
   15 CONTINUE

      A2 = A1/2.5d0
C     Update time dependent arrays
      IF(OPTK.NE.0)CALL TIMDEP(KDD,KDD(0),0.d0,I1YR,IENDY2,0,NYEAR,OPTK)
      IF(OPTA.NE.0)CALL TIMDEP(  A,  A(0),  A2,I1YR,IENDY2,0,NYEAR,OPTA)

C     Set PROBE: NEVENT years are generated in which an event occurs that reduces
C     the population by EVENTS%. Ensure different years are generated.
C     Events occur in years 1-50 except if NEVENT=3  2 occur from 1-50 & 1 from 1-75.
      ISEED = ISEED3
      IF (NEVENT.EQ.3) THEN
       NMORE = NEVENT
       IF (NEVENT.EQ.3) THEN
        IYR = MAX(1, NINT(RAN1(ISEED,MA,INEXT,INXTP)*75.D0+0.49999D0))
        PROBE(IYR) = MAX(0,EVENTS)
        NMORE = 2
       END IF
       DO 21 I=1,NMORE
   20   IYR = MAX(1, NINT(RAN1(ISEED,MA,INEXT,INXTP)*50.D0+0.49999D0))
        IF (PROBE(IYR).GT.0) GO TO 20
        PROBE(IYR) = MAX(0,EVENTS)
   21  CONTINUE
C
C      Future episodic events - selected to allow the mortality correction factor to be calculated.
       DO 22 IYR = NYEAR+1,IENDY2,25
        PROBE(IYR) = MAX(0,EVENTS)
22     CONTINUE        
   18 CONTINUE      
      ENDIF
      IF (NEVENT.EQ.5) THEN
       DO 25 IYR = 5,IENDY2,5
        PROBE(IYR) = EVENTS
25     CONTINUE        
      ENDIF

      RETURN
      END

C ----------------------------------------------------------------------

      SUBROUTINE TIMDEP (X,X1,X2,ID1,ID2,I1,I2,OPT)

      INTEGER ID1,ID2,I1,I2,OPT,IEND,IYR
      DOUBLE PRECISION X(ID1:ID2),X1,X2,XINC,PIFACT

C     Set time dependent array X as defined by OPT
C       1: doubles  2: halves   3: sinusoidal  4: steps  5: tent (2nd part)
C
C     X1=starting value; X2=step value if OPT=4; I1 & I2=start & end years

      IF (OPT.EQ.3) THEN
C       Cyclic parameter: Fig 1a. Mean in year 0, max (=*1.5) in year 40
        PIFACT = 3.141593D0 / 80.D0
        DO 10 IYR = I1+1,I2
          X(IYR) = X1 * (1.0D0 + 0.5D0*SIN(PIFACT * IYR))
   10   CONTINUE

      ELSE IF (OPT.EQ.4) THEN
C       Steps
        DO 20 IYR = 33,65
          X(IYR) = X2
   20   CONTINUE

      ELSE
C       OPT=1 (X doubles) or 2 (X halves) or 5 (tent: Fig1b)
        IEND = I2
        IF (OPT.EQ.5) THEN
C        Tent part 2: Fig 1b. X returns from *2 to original value over 50yrs
         IEND = I2/2
         X(I1) = 2*X1
        ENDIF
        XINC = X1 / DBLE(IEND-I1)
        IF (OPT.NE.1) XINC = -XINC*0.5d0
        DO 30 IYR = I1+1,IEND
          X(IYR) = X(IYR-1) + XINC
   30   CONTINUE
      END IF

      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE DATGEN(SIGHT,CVX,SETDAT,PSUR,KSUR,BIASI,ISGHT,REFP)

C     DATGEN sets absolute abundance estimate SIGHT(IYR) adding 
C     random noise to the surveyed (1+) population PSUR (set in STKUPA)
C     REFP = reference population level = Population in target year (PHIT).

      USE DECL_DATVRS

      EXTERNAL RAN1,GAMMAZ,XNORM,POISSN,GENBET
      DOUBLE PRECISION SIGHT,CVX,PSUR,KSUR,BIASI,RAN1,GAMMAZ,XNORM,
     +       POISSN,RANNO,EPS,W,CHISQ,CVESQ,BKCON,SIG,SIG2,ACON,BCON,
     +       THTASQ,ALPHSQ,BETASQ,ASQ,BSQ,MA1(55),MA2(55),ECV2,CV2,REFP,
     +       RANNO1,GENBET,BIASJ
      INTEGER SETDAT,ISEED,INEXT1,INXTP1,INEXT2,INXTP2,ISGHT,JSEED

      SAVE BKCON,SIG,ACON,BCON,MA1,INEXT1,INXTP1,MA2,INEXT2,INXTP2
      PARAMETER (ASQ=0.02D0,BSQ=0.012D0)

C     On 1st call to routine or if CV or K change, set the constants
      IF (SETDAT.EQ.1) THEN   
        CVESQ = CV1EST*CV1EST

C       If OPTCV=1 set ETA =additional variance factor. (ETA read in if OPTCV=0)
C       ETA = {CV(true)**2 - CV(est)**2}/(.1+.013/.6) {Eqn B1.8}
C       Use CV(est)**2 = THTASQ (ASQ + BSQ K/P) to give THTASQ   {Eqn B1.7}
        IF (OPTCV.LE.1) THEN
         IF (OPTCV.EQ.1) ETA = (CVTRUE*CVTRUE-CVESQ) * 6.D0 / 0.73D0
         IF (OPTCV.EQ.0) CVTRUE = SQRT(ETA*0.73D0/6.D0 + CVESQ)
         THTASQ = CVESQ / (ASQ + BSQ/0.6D0)
        ELSE IF (OPTCV.GE.2) THEN
         CVTRUE = SQRT(CVADD2 + CVESQ)
         ETA = CVADD2 / (0.1d0 + 0.013d0*KSUR/(MAX(REFP,1.0d0)))        {B1.8b}
         THTASQ = CVESQ / (ASQ + BSQ*KSUR/MAX(REFP,1.0d0))              {B1.7a}
        END IF

C       CV(true)**2= ALPHSQ +BETASQ K/P                                 {B1.5}
C       where ALPHSQ =THTASQ*ASQ +.1*ETA & BETASQ =THTASQ*BSQ +.013ETA  {B1.9}
        ALPHSQ = THTASQ * ASQ + ETA*0.1D0
        IF (OPTCV.EQ.3) ALPHSQ = ALPHSQ + 0.1D0**2
        BETASQ = THTASQ * BSQ + ETA*0.013D0

        BKCON = BETASQ * KSUR
        ACON = THTASQ * ASQ
        BCON = THTASQ * BSQ / BETASQ
C       SIG = sd of lognormal component, used to set EPS {Eqn B1.3}
        SIG = SQRT(LOG(1.D0 + ALPHSQ))

      ENDIF

C     Set EPS, W & SEEDS to update random generators even if no survey

C     XNORM(SIG,MEAN,ISEEDi,...) produces a random number from a normal
C     distribution N [MEAN,SIG**2] using the ith random generator.
C          (if MEAN = 0,  XNORM (SIG,0) = SIG * XNORM (1,0) )
      EPS = SIG * XNORM (1.D0,0.D0,ISEED1,MA1,INEXT1,INXTP1)
      RANNO = RAN1(ISEED1,MA1,INEXT1,INXTP1)
      ISEED  = INT(-RAN1(ISEED2,MA2,INEXT2,INXTP2)*100000.D0)
      RANNO1 = RAN1(ISEED1,MA1,INEXT1,INXTP1)
      JSEED  = INT(-RAN1(ISEED2,MA2,INEXT2,INXTP2)*100000.D0)
C
C     Set abundance estimate SIGHT & CVX if a survey is done this year.
C     Note: if ISGHT=0 then SIGHT & CVX were set =-1 in RESET
C           except for historic values which were read in.

      IF (ISGHT.EQ.0) RETURN

C     1st set W to its expectation value
      W = PSUR/BKCON

      BETAV = GENBET(BETA1,BETA2,JSEED)
      IF (RANNO1.LT.PBIAS) THEN
       BIASJ = BIASI*BETAV
      ELSE
       BIASJ = BIASI
      ENDIF 
C
      IF (OPTDET.EQ.0) THEN
C       Stochastic trial: Set SIGHT = Bias * K * BETASQ * EXP(EPS) * W
C           where W is the Poisson component.  {Eqn B1.1}
        IF (W .LT. 70.D0) W = POISSN (W, RANNO)

        SIGHT = BIASJ * BKCON * EXP(EPS) * W

C       Set CVX, the estimate of the CV  {Eqn B1.6}:
        IF (SIGHT.GT.0.D0) THEN
C         CHISQ= random no from chi square distribution (DOF=deg.of freedom)
C         CVEST**2 = THTASQ(ASQ + BSQ/(W*BETASQ))  {B1.7}
   10     CHISQ = GAMMAZ (DOF, SQRT(DOF+DOF), ISEED)
          ECV2 = ACON + BCON/W                                          {B1.7 / 7a}
          SIG2 = LOG(1.D0 + ECV2)                                       {B1.6 / 6a}
          CV2  = SIG2 * CHISQ/DOF                                       {B1.6}
          CVX = MIN(SQRT(CV2),10.d0)
        ELSE
C         Zero estimate. Store Z(i), the Poisson multiplier, in CVX
          CVX = MIN(BIASJ * BKCON * EXP(EPS),10.0d0)
        ENDIF

      ELSE
C       Deterministic trial {Eqns B1.1, .6 & .7}
        SIGHT = BIASJ * PSUR
        CVX = SQRT( LOG(1.D0 + ACON + BCON/W))
      ENDIF
C      
      RETURN
      END
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE PNTOUT (IOUT,NYEAR,IQUOTA,PARFIL,NRY,IYRMAN)

C     Print parameter list & check values are within range.

      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_DATVRS
      USE DECL_ENVCOM

      INTEGER IOUT,NYEAR,I,IQUOTA,NRY,IYRMAN
      CHARACTER OPT(-3:3)*50,PARFIL*12
      LOGICAL ERR

      ERR = .FALSE.
      OPT(-1) = '**** ERROR: UNKNOWN VALUE'

      OPT(0) = '1+ population'
      OPT(1) = 'Mature population'
      I = OPTF
      IF (I.LT.0 .OR. I.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95)'Option specifying MSYR',OPTF,OPT(I)
      I = OPMSYL
      IF (I.LT.0 .OR. I.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95)'Option specifying MSYL',OPMSYL,OPT(I)
      I = OPTDD
      IF (I.LT.0 .OR. I.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95)'Option specifying density dependence',OPTDD,OPT(I)

      OPT(0) = 'Linear bias'
      OPT(1) = 'Sinusoidal bias'
      I = OPTB
      IF (OPTB.LT.0 .OR. OPTB.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95) 'Variable bias option',OPTB,OPT(I)

      OPT(1) = 'Read in CVtrue'
      OPT(2) = 'CVtr2=CVest2+CVadd2'
      OPT(3) = 'CVtr2=CVest2+CVadd2+.1**2'
      I = OPTCV
      IF (OPTCV.LT.1 .OR. OPTCV.GT.3) CALL SETERR(ERR,I)
      WRITE (IOUT,95) 'Option for specifying CVtrue',OPTCV,OPT(I)

      OPT(0) = 'Stochastic'
      OPT(1) = 'Deterministic'
      I = OPTDET
      IF (OPTDET.LT.0 .OR. OPTDET.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95) 'Data generation (abundance & age)',OPTDET,OPT(I)
      OPT(-3) = 'Inertial model + PT density dependence'
      OPT(-2) = 'Gamma density dependence'
      OPT(-1) = 'Inertial model + gamma density dependence'
      OPT(0) = 'Deterministic PellaTomlinson model'
      OPT(1) = 'Stochastic, no correlation'
      OPT(2) = 'Stochastic, + correlation'
      I = ISTOCH
      IF (ISTOCH.LT.-3 .OR. ISTOCH.GT.2) CALL SETERR(ERR,I)
      WRITE (IOUT,95) 'Population model',ISTOCH,OPT(I)

      WRITE (IOUT,95) 'Number of trials',NTRIAL
      WRITE (IOUT,95) 'Number of years in simulation',NYEAR
      WRITE (IOUT,95) 'Year projection begins', IYRPRJ
      WRITE (IOUT,95) 'Year 0 for calculations',IYRMAN
      WRITE (IOUT,96) 'MSYL',' ',MSYL
      IF (MSYRP(1).EQ.MSYRP(2)) THEN
        WRITE (IOUT,96) 'MSY rate', ' ',MSYRP(1)
      ELSE
        WRITE (IOUT,96) 'MSY rate', ' ',MSYRP(1),' to',MSYRP(2)
      ENDIF
      WRITE (IOUT,95) 'Maximum age class',MAXAGE
      WRITE (IOUT,96) 'Future catch ratio','CRATIO',CRATIO
      WRITE (IOUT,96) 'Historic catch bias','BIASC',BIASC
      WRITE (IOUT,95) 'Density dependent time lag',ILAG
      WRITE (IOUT,95) 'Number of epidemics',NEVENT
      WRITE (IOUT,96) 'Level of need','Final',NEED(NYEAR)
      WRITE (IOUT,96) 'Bias in abundance estimates','Initial:',BIAS(-1),
     +                'Final',BIAS(NYEAR-1)
      WRITE (IOUT,96) 'CV of abundance estimates','CV(true)',CVTRUE
      WRITE (IOUT,96)'Expectation value of CV estimate','CV(est)',CV1EST
      WRITE (IOUT,98) 'Birth stochasticity parameters','RHO',RHO,
     +                'SIGEPS**2:',SIGEPS**2
      WRITE (IOUT,95) 'Frequency of abundance estimates',IFREQ
      WRITE (IOUT,95) 'Length of quota period',IQUOTA
      WRITE (IOUT,95) 'Minimum calving interval',NY
      WRITE (IOUT,95) 'Target year',IYRTAR
      WRITE (IOUT,95) 'No.of Monte Carlo replicates to set RY',NRY
      WRITE (IOUT,97) 'Parameter file',PARFIL
      WRITE (IOUT,97) 'Fishery 2: Bowheads;  format',
     +  '(I4,1x,I4,6F8.0,F9.1,5F8.0)'
      WRITE (IOUT,'(/A,(T8,10F7.2))')' Need:',(NEED(I),I=0,NYEAR-1,
     +                                         IQUOTA)
      IF (ERR) STOP

   95 FORMAT (1X,A,T41,I6,4X,A)
   96 FORMAT (1X,A,T35,A8,2(F8.3,2X,A,:': '),I5)
   97 FORMAT (1X,A,T41,A)
   98 FORMAT (1X,A,T35,A8,F8.3,2X,A,F12.9)

      RETURN
      END

      SUBROUTINE SETERR(ERR,I)
      INTEGER I
      LOGICAL ERR
      ERR = .TRUE.
      I = -1
      RETURN
      END


C     ------------------------------------------------------------------
C     ------------------------------------------------------------------

      SUBROUTINE MSYPAR (OK)

C    MSYPAR finds the resilience A1 & density dependent exponent Z
C    giving the required MSY at the specified MSYL.  It equates the
C    slope in the balancing per capita birthrates at two levels of
C    fishing mortality just above and below MSY, with the derivative of
C    per capita birth rate at MSYL. A root finding routine using Brent's
C    method is used to solve for the density dependent exponent.

      USE DECL_STKVRS
      USE DECL_SETUP

      EXTERNAL ZCALC
      DOUBLE PRECISION P,UF(2),PARS(4),ZCALC,FMSY2,PA,RMAT,DF,TOL,
     +       ZLO,ZHI,F1,F2,U1PLUS(2),UMAT(2),PADJST,UM,U1,
     +       MSYLD,TERM,TERM1,TERM2,TERM3
      INTEGER I,IERR,L,OK
      LOGICAL GOOD
C
C     Set up tolerances
      DF  = 0.00001D0
      TOL = 0.00001D0

C     Note SUR array set in RESET or in conditioning (in GENPAR & HITPEG)
C     Set up maturity ogive: FMATUR =proportion of age A which are mature {A5}
C     Knife edge at age MAT50 in this version
      CALL SETO (FMATUR,0.d0,MAT50,MAXAGE,0)
C     Set up recruitment ogive RECF: knife edge at age AREC (=1bowheads) {Eqn A4.1}
      CALL SETO (RECF,0.d0,AREC,MAXAGE,0)
C
C     Set SUMSUR = SUR + SUR**2 + ..... + SUR**NY. Note SUMSUR=0 if NY=0
C     SUMSUR is used to set birth rate where NY =the minimum calving interval
      SUMSUR = 0.D0
      DO 7 I = 1,NY
        SUMSUR = SUMSUR + SUR(MAXAGE)**I
   7  CONTINUE

C *** Add the relative mature & recruited pristine population sizes starting
C     with unity in the zero age class (L=0)  (PA=relative no.in Lth age class)
      RMAT = FMATUR(0)
      RREC = RECF(0)
      R1PLUS = 0.D0
      PA   = SUR(0)
      DO 9 L = 1,MAXAGE-1
        RMAT = RMAT + PA*FMATUR(L)
        RREC = RREC + PA*RECF(L)
        R1PLUS = R1PLUS + PA
        PA   = PA*SUR(L)
    9 CONTINUE

C     Adjust for last age class being pooled (and fully recruited / mature)
      PA   = PA/(1.D0 - SUR(MAXAGE))
      RMAT = RMAT + PA
      RREC = RREC + PA
      R1PLUS = R1PLUS + PA

C     Set the birth rates FEC & F0 so as to give balance at equilibrium
C     F0  = # of age 0 / # mature (M+F) in pristine population 
C     FEC = # of age 0 / # of available mature females (pristine) {Eqn A2.1}
C     Note: FEC = 2F0 if NY=0 = Standard Hitter-Fitter
C           SUMSUR = SUR + SUR**2 + ... + SUR**NY. SUMSUR=0 if NY=0
      FEC = 1.d0 / (RMAT/2.d0 - SUMSUR)
      F0  = 1.d0 / RMAT
 
C     Set up the recruitment ogive in transition form:
C     RECF(A+1) = fraction of unrecruited animals of age A which recruit
C     at age A+1, except RECF(0) = fraction recruited of age 0  {Eqn A4.2}
      CALL TRFORM(RECF,MAXAGE)

C *** Estimate birth rate at levels of survival which bound MSYR
C     Set two levels of survival after fishing to closely straddle MSY
      UF(1) = 1.D0 - MSYR + DF*.5D0
      UF(2) = UF(1) - DF

      DO 100 I = 1,2

C       Add numbers of mature & 1+, relative to # of age 0
C       P = number of age A relative to # of age 0, starting with age 1
        P = SUR(0)
        UMAT(I) = P*FMATUR(1)
        U1PLUS(I) = P

        IF (OPTF.EQ.0) THEN
C         Fishing pattern is uniform on 1+ population.
          DO 10 L = 2,MAXAGE
            P = P * SUR(L-1) * UF(I)
            UMAT(I) = UMAT(I) + P*FMATUR(L)
            U1PLUS(I) = U1PLUS(I) + P
   10     CONTINUE
        ELSE
C         Fishing pattern is uniform on mature population
          DO 20 L = 2,MAXAGE
            P = SUR(L-1) * P * (1.D0 - (1.D0-UF(I))*FMATUR(L-1))
            UMAT(I) = UMAT(I) + P*FMATUR(L)
            U1PLUS(I) = U1PLUS(I) + P
   20     CONTINUE
        ENDIF
C       Adjust for last age class for pooling
        PADJST = 1.D0/(1.D0 - SUR(MAXAGE)*UF(I))
        UMAT(I) = UMAT(I) - P*FMATUR(MAXAGE) * (1.D0 - PADJST)
        U1PLUS(I) = U1PLUS(I) - P*(1.D0 - PADJST)
C
C       Save birth rate (=births/mature female)
        UF(I) = 2.D0/UMAT(I)
  100 CONTINUE

      UM   =  (UMAT(1) + UMAT(2)) * 0.5D0
      U1   =  (U1PLUS(1) + U1PLUS(2)) * 0.5D0

C     Setup MSYLD (JCRM 1:270 eqn 20)
      IF (OPTDD.EQ.OPMSYL) THEN
C       MSYL defined on density dependent component of population
        MSYLD = MSYL
      ELSE IF (OPMSYL.EQ.0 .AND. OPTDD.EQ.1) THEN
C       MSYL on 1+;  DD on mature
        MSYLD = MSYL * R1PLUS * UM * F0 / U1
      ELSE IF (OPMSYL.EQ.1 .AND. OPTDD.EQ.0) THEN
        MSYLD = MSYL * U1 / (R1PLUS * UM * F0)
      ENDIF

C     TERM (1st 2 terms in {} in eqn A7.3)
      IF (OPTDD.EQ.OPTF) THEN
C       MSYR defined on density dependent component of population
        TERM = 0.D0
      ELSE
        TERM = ((U1PLUS(2)-U1PLUS(1))/U1 - (UMAT(2)-UMAT(1))/UM) / DF
        IF (OPTF.EQ.1 .AND. OPTDD.EQ.0) TERM = -TERM
      ENDIF

C     Place the values to be passed into PARS to solve eqn A7.3
C     Note FMSY2 = 2*f(FMSY) = births / mature female  &  term1 = 2 * df/dF
      PARS(1) = 1.D0 + TERM*MSYR
      FMSY2 = (UF(1) + UF(2)) * .5D0
      TERM1 = (UF(2) - UF(1)) / DF
      TERM2 = 1.d0 - FMSY2*SUMSUR
      TERM3 = FMSY2 * (1.d0+FEC*SUMSUR) - FEC
      PARS(2) = MSYR * TERM1 / (TERM2 * TERM3)
      PARS(3) = MSYLD

C     Find density dependent exponent Z
      ZLO =  -5.D0
      ZHI =   5.D0
      CALL ZBRAC (ZCALC, PARS, 4, ZLO, ZHI, F1, F2, GOOD)
      IF (.NOT.GOOD) GO TO 1000
      CALL ZBRENT (ZCALC, PARS, 4, ZLO, ZHI, F1, F2, TOL, IERR)
      Z = PARS(4)
      IF (Z*LOG(MSYLD).GT.20.D0) GO TO 1000

C     Compute the resilience parameter A {Eqn A7.2}
      A1 = (FMSY2*(1.D0+FEC*SUMSUR)-FEC) /
     +     (FEC*(1.D0-FMSY2*SUMSUR)*(1.D0 - MSYLD**Z))
      OK = 1
      RETURN

C *** Error exit
 1000 OK = 0
      RETURN
      END

C     -----------------------------------------------------------------

      FUNCTION ZCALC (PARS,IERR)

C     The root of this function = density dependent exponent z {Eqn A7.3}
C     PARS(1) = 1 + F (dP/dF/P - dPD/dF/PD)         
C     PARS(2) = F * df(Fmsy)/dF / (1-2Fmax*SUMSUR) * (2Fmax(1+FEC*SUMSUR)-FEC)
C     PARS(3) = MSYLD
C     PARS(4) = z

      DOUBLE PRECISION ZCALC,PARS(4),P4
      INTEGER IERR

      P4 = PARS(4)
      IF (P4.GT.1000.D0) P4 = 1000.D0
      IERR = 0
      IF (ABS(P4) .LE. 0.001D0) THEN
        ZCALC = PARS(1) - PARS(2)*LOG(PARS(3))
      ELSE
        ZCALC = PARS(1) - PARS(2)*(PARS(3)**(-P4)-1.D0) / P4
      ENDIF
      END

C     -----------------------------------------------------------------
C     -----------------------------------------------------------------

      SUBROUTINE MSYP2 (MSYL1,MSYLM,MSYLE,MSYR1,MSYRM,MSYRE)

C     Set MSYL & MSYR in terms of the 1+ and mature components of population
C     (For information and use by RES program)

      USE DECL_SETUP

      EXTERNAL MSYM,MSY1,MSYE
      DOUBLE PRECISION MSYL1,MSYLM,MSYR1,MSYRM,MSYRE,MSYLE,MSYM,
     +       MSY1,MSYE,PARS(4),TOL,ZLO,ZHI,F1,F2
      INTEGER IERR
      LOGICAL GOOD
      TOL = 0.00001D0
C
C     1st set PARS(1) = pristine R1+; PARS(4) = pristine RREC
      PARS(1) = R1PLUS
      PARS(4) = RREC
      ZLO =  MSYR * 0.5D0
      ZHI =  MSYR * 1.5D0

C     Find mature values
      IF (OPTF.EQ.1.AND.OPMSYL.EQ.1) THEN
        MSYRM = MSYR
        MSYLM = MSYL
      ELSE
        CALL ZBRAC (MSYM, PARS, 3, ZLO, ZHI, F1, F2, GOOD)
        IF (.NOT.GOOD) STOP '*****  ERROR:  FAILURE TO SOLVE FOR MSYR'
        CALL ZBRENT (MSYM, PARS, 3, ZLO, ZHI, F1, F2, TOL, IERR)
        MSYRM = PARS(3)
        MSYLM = PARS(2)
      ENDIF

C     Find 1+ values
      IF (OPTF.EQ.0.AND.OPMSYL.EQ.0) THEN
        MSYR1 = MSYR
        MSYL1 = MSYL
      ELSE
        CALL ZBRAC (MSY1, PARS, 3, ZLO, ZHI, F1, F2, GOOD)
        IF (.NOT.GOOD) STOP '*****  ERROR:  FAILURE TO SOLVE FOR MSYR'
        CALL ZBRENT (MSY1, PARS, 3, ZLO, ZHI, F1, F2, TOL, IERR)
        MSYR1 = PARS(3)
        MSYL1 = PARS(2)
      ENDIF
C
C     Find Exploitable values
      CALL ZBRAC (MSYE, PARS, 3, ZLO, ZHI, F1, F2, GOOD)
      IF (.NOT.GOOD) STOP '*****  ERROR:  FAILURE TO SOLVE FOR MSYRe'
      CALL ZBRENT (MSYE, PARS, 3, ZLO, ZHI, F1, F2, TOL, IERR)
      MSYRE = PARS(3)
      MSYLE = PARS(2)
      

      RETURN
      END

C     -----------------------------------------------------------------

      FUNCTION MSY1 (PARS,IERR)

C     Subroutine used to find MSYR and MSYL for the 1+ population
C     using Eqn A7.3

      USE DECL_STKVRS

      DOUBLE PRECISION MSY1,PARS(4),U1PLUS(2),UMAT(2),FMSY2,P,PADJST,
     +       MSYLDZ,F,UF(2),U1,UM,TERM,DF,TEMP,TERM1,TERM2,TERM3,DfDF
      INTEGER IERR,L,I

C     The function is 0 when PARS(3) = MSYR for the 1+ population
C     R1PLUS=PARS(1),  MSYR(1+) = PARS(3),  MSYL(1+) = PARS(2)

      IERR = 0
C *** Estimate birth rate at levels of survival which bound MSYR
      F = PARS(3)
      DF  = 0.00001D0
      UF(1) = 1.D0 - F + DF*.5D0
      UF(2) = UF(1) - DF

      DO 100 I = 1,2

C       Add numbers of mature & 1+, relative to # of age 0
C       P = number of age A relative to # of age 0, starting with age 1
        P = SUR(0)
        UMAT(I) = P*FMATUR(1)
        U1PLUS(I) = P

C       Fishing pattern is uniform on 1+ population
        DO 10 L = 2,MAXAGE
          P = P * SUR(L-1) * UF(I)
          UMAT(I) = UMAT(I) + P*FMATUR(L)
          U1PLUS(I) = U1PLUS(I) + P
   10   CONTINUE
C       Adjust for last age class for pooling
        PADJST = 1.D0/(1.D0 - SUR(MAXAGE)*UF(I))
        UMAT(I) = UMAT(I) - P*FMATUR(MAXAGE) * (1.D0 - PADJST)
        U1PLUS(I) = U1PLUS(I) - P*(1.D0 - PADJST)
C
C       Save birth rate
        UF(I) = 2.D0/UMAT(I)
  100 CONTINUE

C     Set MSYLD**Z using {Eqn A7.1} with 2f=FMSY2 and b(eq)=FEC
      FMSY2 = (UF(1) + UF(2)) * 0.5D0
      TEMP  = FMSY2 / (FEC*(1.d0-FMSY2*SUMSUR))
      MSYLDZ = MAX (0.0001D0, 1.D0 - (TEMP-1.D0)/A1)
C     Now set MSYL(1+) {JCRM 1:270 eqn 20} and solve eqn A7.3 to find MSYR1
      IF (OPTDD.EQ.0) THEN
C       Density dependence on 1+
        TERM = 0.D0
        PARS(2) = MSYLDZ**(1.D0/Z)
      ELSE 
C       Density dependence on mature
        UM   = (UMAT(1) + UMAT(2)) * 0.5D0
        U1   = (U1PLUS(1) + U1PLUS(2)) * 0.5D0
        TERM = ((U1PLUS(2)-U1PLUS(1))/U1 - (UMAT(2)-UMAT(1))/UM) *F / DF
        PARS(2) = MSYLDZ**(1.D0/Z) * U1 / (UM * F0 * PARS(1)) 
      ENDIF
      DfDF = (UF(2)-UF(1))/DF
      TERM1 = (1.D0/MSYLDZ)-1.D0
      TERM2 = (1.d0 - FMSY2*SUMSUR)
      TERM3 = FMSY2*(1.d0+FEC*SUMSUR) - FEC

      MSY1 = 1.D0 + TERM - F/Z * DfDF*TERM1/(TERM2*TERM3)

      RETURN
      END

C     -----------------------------------------------------------------

      FUNCTION MSYE (PARS,IERR)

C     Subroutine used to find MSYR and MSYL for the exploitable population
C     using Eqn A7.3

      USE DECL_STKVRS

      DOUBLE PRECISION MSYE,PARS(4),U1PLUS(2),UMAT(2),FMSY2,PADJST,
     +       MSYLDZ,F,UF(2),U1,UM,TERM,DF,TEMP,TERM1,TERM2,TERM3,DfDF,
     +       UE,UEXP(2),UP,RP
      INTEGER IERR,L,I

C     The function is 0 when PARS(3) = MSYR for the exploitable population
C     R1PLUS=PARS(1),  MSYR(1+) = PARS(3),  MSYL(1+) = PARS(2)

      IERR = 0
C *** Estimate birth rate at levels of survival which bound MSYR
      F = PARS(3)
      DF  = 0.00001D0
      UF(1) = 1.D0 - F + DF*.5D0
      UF(2) = UF(1) - DF

      DO 100 I = 1,2

C       Add numbers of mature & 1+, relative to # of age 0
C       P = number of age A relative to # of age 0, starting with age 1
        RP = 0.D0
        UP = 1.D0
        UMAT(I) = 0.D0
        U1PLUS(I) = 0.D0
        UEXP(I) = 0.D0

C       Fishing pattern is uniform on 1+ population
        DO 10 L = 1,MAXAGE
          RP = RP * SUR(L-1) * UF(I) + UP*SUR(L-1)*RECF(L)
          UP = UP * SUR(L-1) * (1.0d0 - RECF(L))
          UMAT(I) = UMAT(I) + (UP+RP)*FMATUR(L)
          U1PLUS(I) = U1PLUS(I) + (UP+RP)
          UEXP(I) = UEXP(I) + RP
   10   CONTINUE
C       Adjust for last age class for pooling
        PADJST = 1.D0/(1.D0 - SUR(MAXAGE)*UF(I))
        UMAT(I) = UMAT(I) - RP*FMATUR(MAXAGE) * (1.D0 - PADJST)
        U1PLUS(I) = U1PLUS(I) - RP*(1.D0 - PADJST)
        UEXP(I) = UEXP(I) - RP*(1.D0 - PADJST)
C
C       Save birth rate
        UF(I) = 2.D0/UMAT(I)
  100 CONTINUE

C     Set MSYLD**Z using {Eqn A7.1} with 2f=FMSY2 and b(eq)=FEC
      FMSY2 = (UF(1) + UF(2)) * 0.5D0
      TEMP  = FMSY2 / (FEC*(1.d0-FMSY2*SUMSUR))
      MSYLDZ = MAX (0.0001D0, 1.D0 - (TEMP-1.D0)/A1)

C     Now set MSYL(1+) {JCRM 1:270 eqn 20} and solve eqn A7.3 to find MSYR1
      UM   = (UMAT(1) + UMAT(2)) * 0.5D0
      U1   = (U1PLUS(1) + U1PLUS(2)) * 0.5D0
      UE   = (UEXP(1) + UEXP(2)) * 0.5d0
      IF (OPTDD.EQ.0) THEN
C       Density dependence on 1+
        TERM = 0.D0
        TERM = ((UEXP(2)-UEXP(1))/UE - (U1PLUS(2)-U1PLUS(1))/U1) *F / DF
        PARS(2) = MSYLDZ**(1.D0/Z) * UE * PARS(1) / (U1 * PARS(4))
      ELSE 
C       Density dependence on mature
        UM   = (UMAT(1) + UMAT(2)) * 0.5D0
        U1   = (U1PLUS(1) + U1PLUS(2)) * 0.5D0
        TERM = ((UEXP(2)-UEXP(1))/UE - (UMAT(2)-UMAT(1))/UM) *F / DF
        PARS(2) = MSYLDZ**(1.D0/Z) * UE / (UM * F0 * PARS(4)) 
      ENDIF
      DfDF = (UF(2)-UF(1))/DF
      TERM1 = (1.D0/MSYLDZ)-1.D0
      TERM2 = (1.d0 - FMSY2*SUMSUR)
      TERM3 = FMSY2*(1.d0+FEC*SUMSUR) - FEC

      MSYE = 1.D0 + TERM - F/Z * DfDF*TERM1/(TERM2*TERM3)

      RETURN
      END

C     -----------------------------------------------------------------
C     -----------------------------------------------------------------

      FUNCTION MSYM (PARS,IERR)

      USE DECL_STKVRS

      DOUBLE PRECISION MSYM,PARS(4),U1PLUS(2),UMAT(2),FMSY2,P,PADJST,
     +       MSYLDZ,F,UF(2),U1,UM,TERM,DF,TEMP,TERM1,TERM2,TERM3,DfDF
      INTEGER IERR,L,I

C     The root of this function is MSYR for the mature population
C     R1PLUS=PARS(1),  MSYR(mat) = PARS(3),  MSYL(mat) = PARS(2)

      IERR = 0
C *** Estimate birth rate at levels of survival which bound MSYR
      F = PARS(3)
      DF  = 0.00001D0
      UF(1) = 1.D0 - F + DF*.5D0
      UF(2) = UF(1) - DF

      DO 100 I = 1,2

C       Add numbers of mature & 1+, relative to # of age 0
C       P = number of age A relative to # of age 0, starting with age 1
        P = SUR(0)
        UMAT(I) = P*FMATUR(1)
        U1PLUS(I) = P

C       Fishing pattern is uniform on mature population 
        DO 20 L = 2,MAXAGE
          P = SUR(L-1) * P * (1.D0 - (1.D0-UF(I))*FMATUR(L-1))
          UMAT(I) = UMAT(I) + P*FMATUR(L)
          U1PLUS(I) = U1PLUS(I) + P
   20   CONTINUE
C       Adjust for last age class for pooling
        PADJST = 1.D0/(1.D0 - SUR(MAXAGE)*UF(I))
        UMAT(I) = UMAT(I) - P*FMATUR(MAXAGE) * (1.D0 - PADJST)
        U1PLUS(I) = U1PLUS(I) - P*(1.D0 - PADJST)
C
C       Save birth rate
        UF(I) = 2.D0/UMAT(I)
  100 CONTINUE

      FMSY2 = (UF(1) + UF(2)) * 0.5D0
      Temp = FMSY2/(FEC*(1.d0-FMSY2*SUMSUR))
      MSYLDZ = MAX (0.0001d0, 1.d0 - (Temp-1.d0)/A1)
      IF (OPTDD.EQ.0) THEN
C       Density dependence on 1+
        UM   = (UMAT(1) + UMAT(2)) * 0.5D0
        U1   = (U1PLUS(1) + U1PLUS(2)) * 0.5D0
        TERM = ((UMAT(2)-UMAT(1))/UM - (U1PLUS(2)-U1PLUS(1))/U1) *F / DF
        PARS(2) = MSYLDZ**(1.D0/Z) * UM * F0 * PARS(1) / U1
      ELSE 
C       Density dependence on mature
        TERM = 0.D0
        PARS(2) = MSYLDZ**(1.D0/Z)
      ENDIF
      DfDF = (UF(2)-UF(1))/DF
      TERM1 = (1.D0/MSYLDZ)-1.d0
      TERM2 = (1.d0 - FMSY2*SUMSUR)
      TERM3 = FMSY2*(1.D0+FEC*SUMSUR) - FEC

      MSYM = 1.D0 + TERM - F/Z * DfDF*TERM1/(TERM2*TERM3)

      RETURN
      END


C *********************************************************************
C *********************************************************************

      SUBROUTINE SETKA (K,PPRJ,PHIT,OK)

C  Set up the age-structure for this K & PPRJ
C  K=pristine 1+ population; PPRJ=1+ population size at start of projection

      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_STOCHM

      DOUBLE PRECISION K,PPRJ,KREC,RINIT(0:MXAGE),UINIT(0:MXAGE),PA,
     +       R1PLS,CM,CF,RATIO,AINC,KINC,PHIT,CALF,MATF,PSURV1
      INTEGER OK,L,IYR

C     Set up recruitment ogive RECF: knife edge at age 1  {Eqn A4.1}
      CALL SETO (RECF,0.d0,AREC,MAXAGE,0)

C     Scale the zero age class so the relative recruited population is unity.
      PA  = 1.D0/RREC
C     Set up relative recruited & unrecruited age vectors and 1+ population
      UINIT(0) = PA*(1.D0-RECF(0))
      RINIT(0) = PA*RECF(0)
      R1PLS = 1.D0 - RINIT(0)
      DO 20 L = 1,MAXAGE
        PA = PA*SUR(L-1)
        UINIT(L) = PA*(1.D0 - RECF(L))
        RINIT(L) = PA*RECF(L)
        R1PLS = R1PLS + UINIT(L)
   20 CONTINUE
      RINIT(MAXAGE) = RINIT(MAXAGE)/(1.D0 - SUR(MAXAGE))

C     Reset recruitment ogive to transition form for use by STKUPA {Eqn A4.2}
      CALL TRFORM (RECF,MAXAGE)

C     Set up the age-structure for this K (K=pristine 1+ population)
C     Note: in this model the maturity, recruitment and survival ogives
C          are the same for males as for females (hence the 0.5 factor).
      KREC = 0.5D0 * K / R1PLS
      MATF = 0.d0
      PSURV1 = 0.d0
      DO 105 L = 0,MAXAGE
        UNRM(L) = KREC*UINIT(L)
        RM(L)   = KREC*RINIT(L)
        UNRF(L) = UNRM(L)
        RF(L)   = RM(L)
        MATF    = MATF +(RF(L)+UNRF(L))*FMATUR(L)                       {A2.2}
        IF (L.GT.0) PSURV1 = PSURV1 + 
     +              SELSUVM*(UNRM(L)+RM(L)) + SELSUVF*(UNRF(L)+RF(L))
  105 CONTINUE
C     NB K is constant from INITYR to IYRPRJ
      CALF = RM(0)+RF(0)+UNRM(0)+UNRF(0)
      KDD(IYRPRJ) = K
      IF (OPTDD.EQ.1) KDD(IYRPRJ) = MATF
      A(IYRPRJ)   = A1

C     Inertia model: Initialise intrinsic pregnancy rate PREGI & births at age {see Diii}
      IF (ISTOCH.EQ.-1.OR.ISTOCH.EQ.-3) THEN
        RATIO = CALF/MATF
        DO 108 L=0,MAXAGE
          PREGI(L) = FEC
          BA(L) = 0.D0
          IF (NY.GT.0) BA(L) = (RF(L)+UNRF(L))*FMATUR(L)*RATIO
  108   CONTINUE
      ENDIF

      OK = 1

C     Store (use for future reference if IYRPRJ.NE.INITYR)
      PMATF(INITYR) = MATF
      PMATA(INITYR) = MATF * 2.D0
      PCALF(INITYR) = CALF
      N1PLUS(INITYR) = K
      PSURV(INITYR) = PSURV1
      NEXP(INITYR) = KREC * 2.0d0

      IF (IYRPRJ.NE.INITYR) THEN
C       Population not pristine when projection begins
C       Set up age structure for 1+ population=PPRJ in IYRPRJ
        CALL INITP(PPRJ,OK)
C
        IF (OK.EQ.0) RETURN
C
C       Set maturity & 1+ population sizes {A2.2,A2.6}
        N1PLUS(IYRPRJ) = 0.d0
        PSURV(IYRPRJ) = 0.d0
        PMATF(IYRPRJ) = 0.d0
        DO 106 L = 1,MAXAGE
          PMATF(IYRPRJ) = PMATF(IYRPRJ) + (RF(L)+UNRF(L))*FMATUR(L)
          N1PLUS(IYRPRJ) = N1PLUS(IYRPRJ) + RM(L)+UNRM(L)+RF(L)+UNRF(L)
          IF (L.GT.0) PSURV(IYRPRJ) = PSURV(IYRPRJ) + 
     +              SELSUVM*(UNRM(L)+RM(L)) + SELSUVF*(UNRF(L)+RF(L))
  106   CONTINUE
        PMATA(IYRPRJ) = PMATF(IYRPRJ) * 2.D0
      END IF
C
C     Reset IVAR and QVAR, the pointers for the set of random numbers
C     used in the stochastic model (ensures the same values are used in
C     trials as are used during conditioning)
      IVAR = 0
      QVAR = 0
      LASTQ=0.d0      

C     Forward project the population, reseting CM & CF in case they
C             were reduced in STKUPA
C     ITENT=-2 unless OPTK or OPTA=5 when ITENT+1=year of first increase
      DO 110 IYR = IYRPRJ,ITENT
        CM = CATCHM(IYR) + CATCHMBY(IYR) + CATCHMCAN(IYR)
        CF = CATCHF(IYR) + CATCHFBY(IYR) + CATCHFCAN(IYR)
        KDD(IYR+1) = KDD(IYRPRJ)
        A(IYR+1)   = A1
        CALL STKUPA (IYR,CM,CF,OK,0,0.D0)
  110 CONTINUE

      IF (OPTK.EQ.5 .OR. OPTA.EQ.5) THEN
C       K &/or A vary as Tent from yr -50 {see Dvi & Dvii}
        KINC = 0.d0
        IF (OPTK.EQ.5) KINC = KDD(IYRPRJ)/50.d0
        AINC = 0.d0
        IF (OPTA.EQ.5) AINC = A1/50.d0
        DO 120 IYR = ITENT+1,-2
          CM = CATCHM(IYR) + CATCHMBY(IYR) + CATCHMCAN(IYR)
          CF = CATCHF(IYR) + CATCHFBY(IYR) + CATCHFCAN(IYR)
          KDD(IYR+1) = KDD(IYR)+KINC
          A(IYR+1)   = A(IYR)+AINC
          CALL STKUPA (IYR,CM,CF,OK,0,0.D0)
  120   CONTINUE
      END IF

C     Store population in target year
      PHIT = PSURV(IYRTAR)

      RETURN
      END

C *********************************************************************
C
      SUBROUTINE INITP(PPRJ,OK)
C
C    Called from SETKA if IYRPRJ.NE.INITYR (ie projection does not start @ K)
C    Base case for gray whales (BR trial for bowheads - see section Dx)
C    INITP sets up the initial age-distribution in IYRPRJ by
C    solving the population equations to "hit" PPRJ = 1+ pop.size
C
      USE DECL_STKVRS
C
      DOUBLE PRECISION PPRJ,P1PL,ROI,FMIN,FMAX,B
      INTEGER OK,II,J
C
      ROI = 0.d0
      CALL FUN2(ROI,P1PL,B)
      IF (ABS(P1PL-PPRJ).LT.0.0000001d0*PPRJ) GOTO 1001

C     Use a Bisection approach to find the equilibrium age-structure
      FMIN =  0.d0
      FMAX =  1.d0
      DO 1000 II = 1,50
C
C      Bisect F    
       ROI = (FMIN + FMAX) * 0.5d0
       Call FUN2(ROI,P1PL,B)
C
C      Check for convergence
       IF (ABS(P1PL-PPRJ).LT.0.0000001d0*PPRJ) GOTO 1001
       IF (P1PL.GT.PPRJ) THEN
         FMIN = ROI
       ELSE
         FMAX = ROI
       ENDIF
C
1000  CONTINUE
c     GOTO 1001  =  AEP trick !!
      IF (DOCON.NE.1) GOTO 1001
      OK = 0
      RETURN

1001  CONTINUE 

C     Rescale the numbers at age to actual values:
C     (FUN2 set RF & UNRF to sizes scaled by B)
      DO 2000 J = 0,MAXAGE
        UNRM(J) = UNRF(J) * B
        RM(J) =   RF(J) * B
        UNRF(J) = UNRM(J)
        RF(J) =   RM(J)
2000  CONTINUE      
C
      RETURN
      END
C
C *********************************************************************
C
      SUBROUTINE FUN2(ROI,P1PL,B)
C
C     FUN2 sets the 1+ population for the given value of ROI (rate of inc.)
C     Called by INITP (only called if IYRPRJ.NE.INITYR)
C
      USE DECL_STKVRS
C
      DOUBLE PRECISION ROI,P1PL,PMAT,B,B2
      INTEGER J
C
C     Calculate the numbers at age relative to the number of 0 yr olds {A6.3}
C     NB Population is in equilibrium so males=females as SUR is sex independent
      UNRF(0) = 0.5d0
      RF(0) = 0.d0
      DO 70 J = 0,MAXAGE-1
       UNRF(J+1) = SUR(J)*UNRF(J)*(1.d0-RECF(J+1))
       RF(J+1) = SUR(J)*(RF(J)*(1.d0-ROI)+UNRF(J)*RECF(J+1))
 70   CONTINUE
C   
C     Adjust for last age class being pooled (and fully recruited)
      RF(MAXAGE) = RF(MAXAGE) / (1.d0-SUR(MAXAGE)*(1.d0-ROI))

C     Add 1+ & mature female totals
      P1PL = 0.d0
      PMAT = 0.d0
      DO 90 J = 1,MAXAGE
       P1PL = P1PL + (UNRF(J) + RF(J))
       PMAT = PMAT + (UNRF(J) + RF(J))*FMATUR(J)
 90   CONTINUE
      P1PL = P1PL*2.d0
C
C     Find no. of births B {A6.4} and hence 1+ population (P1PL)
C     NB in AW trials ISTOCH>=0 at this point (as the Inertia model always begins @ K)
      B = 0.d0
      IF (PMAT.GT.SUMSUR.AND.PMAT.GT.1.0e-20) THEN
        IF (ISTOCH.EQ.-1.OR.ISTOCH.EQ.-2) THEN
         B2 = 1.d0 - log(1.d0/(FEC*(PMAT-SUMSUR)))/GammaL
        ELSE
         B2 = 1.d0 - (1.d0/(FEC*(PMAT-SUMSUR))-1.d0)/A1
        ENDIF
        IF (B2.GT. 0.d0) THEN                                           ?? CHECK -1 case ?
          IF (ISTOCH.EQ.-1 .OR. ISTOCH.EQ.-2) THEN
            B2 = KDD(IYRPRJ) * B2
          ELSE
            B2 = KDD(IYRPRJ) * B2**(1.d0/Z)
          ENDIF
          IF (OPTDD.EQ.1) THEN
C           Density dependence on mature FEMALES
            B = B2 / PMAT
          ELSE
            B = B2 / P1PL
          ENDIF
        ENDIF
      ENDIF
      P1PL = P1PL*B
C
      RETURN
      END

C *********************************************************************

      SUBROUTINE REPORT (IOUT,NYEAR,HIDEAL,SIGHT,CVX,ISUR1,IYRMAN)

C     REPORT prints out population size & catch arrays from year 0

      USE DECL_STKVRS
      INTEGER IYRMAN

      DOUBLE PRECISION HIDEAL(0:ICEND),SIGHT(I1SYR:ICEND),
     +                 CVX(I1SYR:ICEND)
      INTEGER IOUT,NYEAR,IYR,ISUR1

      WRITE (IOUT,'(A,10A8)') 'Year','MatFem','1+Pop','CM','CF','H',
     +                       'Sight','CV','CVsch','Calfs','RY'
      IYR= INITYR
      WRITE(IOUT,99) IYRMAN+IYR,
     +               IYR,NINT(PMATF(IYR)),NINT(N1PLUS(IYR)),
     +               NINT(PSURV(IYR)),
     +               -1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,
     +               -1.,-1.,-1.,-1,-1.0,NINT(NEXP(IYR))
      WRITE(IOUT,99) (IYRMAN+IYR,
     +               IYR,NINT(PMATF(IYR)),NINT(N1PLUS(IYR)),
     +               NINT(PSURV(IYR)),
     +               CATCHM(IYR),CATCHF(IYR),
     +               CATCHMBY(IYR),CATCHFBY(IYR),
     +               CATCHMCAN(IYR),CATCHFCAN(IYR),
     +               -1.,-1.,-1.,-1.,NINT(PCALF(IYR)),
     +               RYS(IYR),NINT(NEXP(IYR)),IYR=IYRPRJ,ISUR1-1)
      WRITE(IOUT,99) (IYRMAN+IYR,IYR,
     +       NINT(PMATF(IYR)),NINT(N1PLUS(IYR)),
     +       NINT(PSURV(IYR)),
     +       CATCHM(IYR),CATCHF(IYR),
     +       CATCHMBY(IYR),CATCHFBY(IYR),CATCHMCAN(IYR),CATCHFCAN(IYR),
     +       -1.,SIGHT(IYR),CVX(IYR),-1.0d0,
     +       NINT(PCALF(IYR)),RYS(IYR),NINT(NEXP(IYR)),IYR=ISUR1,-1)
      WRITE(IOUT,99) (IYRMAN+IYR,IYR,
     +       NINT(PMATF(IYR)),NINT(N1PLUS(IYR)),
     +       NINT(PSURV(IYR)),
     +       CATCHM(IYR),CATCHF(IYR),
     +       CATCHMBY(IYR),CATCHFBY(IYR),CATCHMCAN(IYR),CATCHFCAN(IYR),
     +       HIDEAL(IYR),SIGHT(IYR),CVX(IYR),-1.0d0,
     +       NINT(PCALF(IYR)),RYS(IYR),NINT(NEXP(IYR)),IYR=0,NYEAR-1)
      WRITE (IOUT,99) IYRMAN+NYEAR,NYEAR, 
     +       NINT(PMATF(NYEAR)),NINT(N1PLUS(NYEAR)),
     +       NINT(PSURV(NYEAR))
   99 FORMAT (I4,1x,I4,3I8,7F8.2,F9.1,2F8.4,I8,F8.1,I8)
      RETURN
      END


C     ------------------------------------------------------------------

      SUBROUTINE SETO (V,SIG,A50,MAXAGE,AMIN)

C     SETO calculates the proportion of each age class mature or
C     recruited, returning the ogive in the vector V
C  NB this routine gives the same results as BALEEN II if 
C     T95 = SIG * 2.94444  - T50  and  AMIN(recruited) = 0
C
      DOUBLE PRECISION V(0:40),SIG,A50,X
      INTEGER MAXAGE,AMIN,L

C     If SIG = 0 ogive is knife edge at age A50.
      IF (SIG.EQ.0.D0) THEN
        DO 10 L = 0,NINT(A50)-1
          V(L) = 0.D0
   10   CONTINUE
        DO 15 L = NINT(A50),MAXAGE-2
          V(L) = 1.D0
   15   CONTINUE

      ELSE

C       Loop over all ages
C       AMIN  = first age at which animals  are recruited or mature
        DO 20 L = 0, AMIN-1
          V(L) = 0.D0
   20   CONTINUE

C       A50   = age at which 50% of animals are recruited or mature
C       SIG   = width of ogive
        DO 25 L = AMIN, MAXAGE-2
          X = (A50 - DBLE(L)) / SIG
          IF (X.LT.-50.D0) THEN
            V(L) = 1.D0
          ELSE IF (X.GT.50.D0) THEN
           V(L) = 0.D0
          ELSE
            V(L) = 1.D0/(1.D0 + EXP(X))
          ENDIF
   25   CONTINUE
C
      ENDIF
C
C  Set the terminal age classes (forced to be fully recruited or mature)
      V(MAXAGE-1) = 1.D0
      V(MAXAGE) = 1.D0
      RETURN
      END

C     ------------------------------------------------------------------

      SUBROUTINE TRFORM (V,MAXAGE)

C     Adjust an ogive to transition form, that is so that 
C     V(L) = the proportion of animals in a given class at age A-1 
C            which make the transition to a different class age A

      DOUBLE PRECISION V(0:40),RM,D
      INTEGER MAXAGE,L

      RM   = V(0)
      DO 10 L = 1,MAXAGE
        IF (RM .LT. 1.D0) THEN
          D = RM
          RM = V(L)
          V(L) = (RM - D)/(1.D0 - D)
        ELSE
          RM = V(L)
          V(L) = 1.D0
        ENDIF
   10 CONTINUE

      RETURN
      END

C     ------------------------------------------------------------------

      SUBROUTINE RPRIOR(DISTRI,PARS,IN)

C     Read in a prior distribution

      INTEGER IN,II
      CHARACTER*1 DISTRI
      DOUBLE PRECISION PARS(4)
C
C     Initial the output values
      DO 10 II = 1,4
       PARS(II) = 0.D0
 10   CONTINUE
      READ(IN,'(45X,A1)') DISTRI 
C
C     Select a distribution
      IF    (DISTRI.EQ.'U'.OR.DISTRI.EQ.'u'
     +  .OR. DISTRI.EQ.'L'.OR.DISTRI.EQ.'l'
     +  .OR. DISTRI.EQ.'D'.OR.DISTRI.EQ.'d'
     +  .OR. DISTRI.EQ.'B'.OR.DISTRI.EQ.'b') THEN
        READ(IN,'(45X,F10.0)') PARS(1)
        READ(IN,'(45X,F10.0)') PARS(2)

      ELSE IF (DISTRI.EQ.'N'.OR.DISTRI.EQ.'n'
     +  .OR.   DISTRI.EQ.'I'.OR.DISTRI.EQ.'i') THEN
        READ(IN,'(45X,F10.0)') PARS(1)
        READ(IN,'(45X,F10.0)') PARS(2)
        READ(IN,'(45X,F10.0)') PARS(3)
        READ(IN,'(45X,F10.0)') PARS(4)

      ELSE IF  (DISTRI.EQ.'G'.OR.DISTRI.EQ.'g') THEN
        READ(IN,'(45X,F10.0)') PARS(1)
        READ(IN,'(45X,F10.0)') PARS(2)
        READ(IN,'(45X,F10.0)') PARS(3)
      ELSE
        PRINT *,DISTRI,PARS
        STOP 'Error: Unknown distribution'
      ENDIF

      RETURN
      END

C *********************************************************************

      SUBROUTINE STKUPA (IYR,CM,CF,OK,NRY,NEEDI)

C     STKUPA updates the stock size at the start of IYR+1, i.e. it
C     updates the RM & UNRM arrays and sets PMATF(IYR+1),PSURV(IYR+1) & 
C     N1PLUS(IYR+1)

      USE DECL_STKVRS
      USE DECL_STOCHM
      USE DECL_SETUP

      COMMON / AEP / AEPON
      INTEGER AEPON

      DOUBLE PRECISION CM,CF,PEXPM,PEXPF,URM,URF,BIN1,BIN2,DDFUN,
     +     BIRTHS,PMAT,P1PLUS,PDD,PMATM,PROBB,NAVMAT,FMALES,MALES,PR,
     +     NEEDI,P1EXP,RX,PREGI0,DDFUN2,OLASTQ,TOTMAT,RATIO,PDD2,
     +     REMOVE,AGEL,PSURV1,FecMaxUse,KparVal,EpsFec,FecUse 
      INTEGER IYR,L,OK,NRY,I,IX,IL
      EXTERNAL BIN1,BIN2
      CHARACTER LINE*49
      
C     Sum the numbers of exploitable whales
      PEXPM = RM(0)
      PEXPF = RF(0)
      DO 10 L = 0,MAXAGE
        PEXPM  = PEXPM + RM(L)
        PEXPF  = PEXPF + RF(L)
   10 CONTINUE
      NEXP(IYR) = PEXPM + PEXPF
      
C     Set URM and URF = average survivorship after catching.  {Eqn A3.1}
      IF (PEXPM.GT.CM) THEN
        URM = 1.D0 - CM/PEXPM
      ELSE
        URM = 0.D0
        CM = PEXPM
        OK = 0
      ENDIF
      IF (PEXPF.GT.CF) THEN
        URF = 1.D0 - CF/PEXPF
      ELSE
        URF = 0.D0
        CF = PEXPF
        OK = 0
      ENDIF

C *** Population projection
C     First advance the pooled age class (gets different treatment) {Eqn A1.1}
C     If ISTOCH=0=Deterministic so BIN2 returns a product of the 2 parameters
C     Do in 2 steps to force the random numbers to be used in the same order
      RX = Bin2(URM*RM(MAXAGE),SUR(MAXAGE))
      RM(MAXAGE) = Bin2(URM*RM(MAXAGE-1),SUR(MAXAGE-1)) + RX
      RX = Bin2(URF*RF(MAXAGE),SUR(MAXAGE))
      RF(MAXAGE) = Bin2(URF*RF(MAXAGE-1),SUR(MAXAGE-1)) + RX

C     Loop advances remaining age classes, working from oldest down.{Eqn A1.1}
C     Add nos. of mature females (PMATF) and 1+ (N1PLUS) {Eqns A2.2 & 6}
      PMAT  =  RF(MAXAGE)
      PMATM =  RM(MAXAGE)
      P1PLUS = RM(MAXAGE) + RF(MAXAGE)
      P1EXP = P1PLUS
      PSURV1 = P1PLUS
      DO 40 L = MAXAGE-1,1,-1
        UNRM(L) = Bin2(UNRM(L-1)*(1.D0 - RECF(L)),SUR(L-1))
        UNRF(L) = Bin2(UNRF(L-1)*(1.D0 - RECF(L)),SUR(L-1))
        RM(L)   = Bin2(RM(L-1)*URM + UNRM(L-1)*RECF(L),SUR(L-1))
        RF(L)   = Bin2(RF(L-1)*URF + UNRF(L-1)*RECF(L),SUR(L-1))
        PMAT = PMAT + (RF(L)+UNRF(L))*FMATUR(L)
        PMATM= PMATM+ (RM(L)+UNRM(L))*FMATUR(L)
        P1PLUS = P1PLUS + RM(L) + UNRM(L) + RF(L) + UNRF(L)
        P1EXP = P1EXP + RM(L) + RF(L)
        IF (L.GT.0) PSURV1 = PSURV1+ 
     +              SELSUVM*(UNRM(L)+RM(L)) + SELSUVF*(UNRF(L)+RF(L))
   40 CONTINUE
      IX = IYR+1
      PMATF(IX) = PMAT
      PMATA(IX) = PMATM + PMAT
      N1PLUS(IX) = P1PLUS
      PSURV(IX) = PSURV1

C     Set PDD = ratio in Eqn A2.3. Reset if Z < 0 (can occur in search for K)
C     Incorporate time lag ILAG (if any)
      IL = MAX(IX-ILAG,IYRPRJ)
      IF (OPTDD.EQ.0) THEN
        PDD = N1PLUS(IL)/(KDD(IL)+0.00001)
      ELSE 
        PDD = PMATF(IL)/(KDD(IL)+0.00001)
      ENDIF

C     Set PDD2 = PDD**Z, checking log value to ensure no overflow error
      IF (PDD.GT.0.D0) THEN
        PDD2 = Z*LOG(PDD)
        IF (PDD2.GT.600.D0) THEN
          IF (DOCON.LT.1) PRINT '(/A,2F15.6/)',' CHECK STKUPA',Z,PDD2
          PDD2=600.D0
        ENDIF
        PDD2 = EXP(PDD2)
      ELSE
        PDD2 = 0.D0
      ENDIF

C     Add new births to zero age-class
C     1st set b(t+1) = probability of births/available female {Eqn A2.3}
      IF (ISTOCH.EQ.-1 .OR. ISTOCH.EQ.-2) THEN
        DDFUN = EXP(GammaL*(1.d0-PDD))
      ELSE
        DDFUN = MAX(0.d0,1.D0 + A(IX)*(1.D0 - PDD2))
      ENDIF

      IF (ISTOCH.EQ.-1.OR.ISTOCH.EQ.-3) THEN

C       Inertia model (see Diii): add births from each age class (BA)
C       Set rate for pooled age class  {Eqn Diii.6}
C       using RX = survivors in MAXAGE class & RF(MAXAGE)-RX = new entrants
C       (Values are * by URF but it cancels out)
        PREGI(MAXAGE-1) =(PREGI(MAXAGE-1)*(RF(MAXAGE)-RX) +
     +                    PREGI(MAXAGE)*RX) / MAX(0.000001D0,RF(MAXAGE))
C       Ensure last years births from pooled age class are removed
        BA(MAXAGE-1) = BA(MAXAGE-1) + BA(MAXAGE)
C
C       Advance pregnancy rates (PREGI) and set births (BA) {Diii.5,3,2,1}
        BIRTHS = 0.D0
        PREGI0 = 0.D0
        TOTMAT = 0.D0
        DO 45 L=MAXAGE,1,-1
          PREGI(L) = PREGI(L-1)
          NAVMAT = MAX(0.D0,(RF(L)+UNRF(L))*FMATUR(L) -BA(L-1)*SUR(L-1))
          PR     = MAX(0.d0, MIN(PREGI(L)*DDFUN,PREGTG))
          BA(L)  = PR*NAVMAT
          BIRTHS = BIRTHS + BA(L)
          PREGI0 = PREGI0 + PREGI(L) * BA(L)
          TOTMAT = TOTMAT + NAVMAT
   45   CONTINUE
        IF (NY.EQ.0) THEN
          DO 46 L=0,MAXAGE
            BA(L)=0.D0
   46     CONTINUE
        ENDIF
C       Set intrinsic pregnancy rate PREGI for new borns {Diii.8 & 7}
C       AR = A ratio = A/AI
        IF (ISTOCH.EQ.-1) DDFUN2 = EXP(GammaL*AR*(1.d0-PDD))
        IF (ISTOCH.EQ.-3) DDFUN2 = 1.D0 + A(IX)*AR*(1.D0-PDD2)
        DDFUN2 = MIN(5.0d0,MAX(0.0d0,DDFUN2))
C
        PREGI(0) = DDFUN2 * PREGI0 / MAX(0.000001D0,BIRTHS)
        PREGI(0) = MAX(0.0d0,MIN(PREGI(0),PREGTG))

      ELSE
        ProbB = MAX(0.000001D0, FEC*DDFUN)
        FecMaxUse = Fec*(1+A(IX))
        FecUse = ProbB
        IF (IX.LT.0) THEN
         EpsFec = 0
        ELSE
         EpsFec = FecEps(IX)
        ENDIF 
        IF (ProbB.LT.0.999*FecMaxUse) THEN
         KparVal = log(FecUse*(FecMaxUse-Fec)/
     +                                ((FecMaxUse-FecUse)*Fec))+EpsFec
        ELSE
         KparVal = 200
        ENDIF
        IF (KparVal.GT.200) KparVal = 200
        ProbB = FecMaxUse*Fec*Exp(KparVal)/
     +                         (Fec*EXP(KparVal)+(FecMaxUse-Fec))

        PR = PROBB
C       Add environmental stochasticity (1st store LASTQ)  {Eqns A2.7-8}
        OLASTQ = LASTQ
        IF (ISTOCH.EQ.2) CALL GBirth(ProbB)
C       Set no.of available females & hence births {A2.1,gen.} NB SUMSUR=0 if NY=0
C       (This assumes that all mature have survival rate=SUR(MAXAGE))
        NAvMat = PMATF(IX)
        DO 50 L=1,NY
          I = MAX(IX-L,IYRPRJ)
          NAvMat = NAVMAT - PCALF(I)*SUR(MAXAGE)**L
   50   CONTINUE
        BIRTHS = Bin1(MAX(NAvMat,0.d0),ProbB)

C       Write out info on birth rates if required
        IF (DOCON.EQ.-1) THEN
         IF (IYR.EQ.IYRPRJ) WRITE (30,999) IYR,PCALF(IYR),PMATF(IYR)
         WRITE (30,999) IX,BIRTHS,PMATF(IX),NAVMAT,PR*100.0,PROBB*100.0
         WRITE (LINE(1:31),'(I4,3F9.4)') I,KDD(IYR)/KDD(IYRPRJ),
     +          A(IYR)/A(IYRPRJ),-LOG(SUR(MAXAGE))
         IF (IYR.GE.ISUR1)WRITE (LINE(32:40),'(F9.4)') BIAS(IYR)
         IF (IYR.GE.0)    WRITE (LINE(41:46),'(I5)')  PROBE(IYR)
         WRITE (31,'(A)') LINE
        END IF
C
      END IF
  999 FORMAT (I4,3F9.0,6F9.3)

      IF (AEPON.EQ.1) THEN
        IF (ISTOCH.GE.0) THEN
         DDFUN2 = 1
         PREGI(0) = FEC
         PREGI(MAXAGE) = FEC
         TOTMAT = NAVMAT
        END IF
        WRITE(*,666) IX,A(IX)*AR,DDFUN2,DDFUN,PDD,
     +     BIRTHS,N1PLUS(IL),pREGTG,PREGI(0),TOTMAT,URF,URM,
     +     PREGI(MAXAGE),CM+CF
      ENDIF
666   FORMAT(I5,F9.4,2F7.4,F7.3,2F10.1,2F9.6,F10.1,3F8.5,F6.0)

C     Allocate births to sex {A2.4 & 5}
      MALES = Bin1(BIRTHS,0.5d0)
      FMALES = BIRTHS - MALES
      RM(0) = Males * RECF(0)
      UNRM(0) = Males - RM(0)
      RF(0) = FMALES * RECF(0)
      UNRF(0) = FMALES - RF(0)
      PCALF(IX) = BIRTHS
C
      IF (DOCON.LT.1) THEN

C       Set RY (if needed - not for conditioning)
        IF (IYR.GE.-20) THEN
          CALL SETRY(P1EXP,NRY,NEEDI,PR,NAvMat,OLASTQ)
          RYS(IX) = RY
        ENDIF

C       Update the survival array if OPTM>0 (See Dviii)
C       (It only changes from year 0 so not used in conditioning)
        IF (OPTM.GT.0 .AND. IX.GE.0)CALL SETSUR(1,0,IX+1)
      ENDIF
C
      IF (IX.LT.-3) RETURN

C     Check PROBE to see if an epidemic occurs - only from IYR=-3
C     (See Dix)
C     NB this is done after RY is set
      IF (PROBE(IX).NE.0 .AND. OPTEE.EQ.0) THEN
C       Deaths occur uniformly thru all age classes
        IF (PROBE(IX).EQ.-1) THEN
C         Reduce population to 2000 in year 1 only.
          RATIO = 2000.D0/N1PLUS(IX)
        ELSE
C         Reduce population by PROBE(IX)%
          RATIO = 1.D0 - (PROBE(IX)/100.D0)
        END IF
        PMATF(IX) = PMATF(IX) * RATIO
        PMATA(IX) = PMATA(IX) * RATIO
        N1PLUS(IX) = N1PLUS(IX) * RATIO
        PSURV(IX) = PSURV(IX) * RATIO
        PCALF(IX) = PCALF(IX) * RATIO
        DO 90 L=0,MAXAGE
          UNRM(L) = UNRM(L) * RATIO
          UNRF(L) = UNRF(L) * RATIO
          RM(L) = RM(L) * RATIO
          RF(L) = RF(L) * RATIO
   90   CONTINUE

      ELSE IF (PROBE(IX).GT.0) THEN
C       Reduce population by PROBE(IX)%, youngest first.
        REMOVE = (N1PLUS(IX)+PCALF(IX))*(PROBE(IX)/100.D0)
        DO 100 L=0,MAXAGE
          AGEL = UNRM(L)+UNRF(L)+RM(L)+RF(L)
          IF (AGEL.LE.REMOVE) THEN
            REMOVE = REMOVE - AGEL
            UNRM(L) = 0.D0
            UNRF(L) = 0.D0
            RM(L) = 0.D0
            RF(L) = 0.D0
          ELSE
C           Only part of this age group die
            RATIO   = REMOVE/AGEL
            UNRM(L) = UNRM(L) * RATIO
            UNRF(L) = UNRF(L) * RATIO
            RM(L) = RM(L) * RATIO
            RF(L) = RF(L) * RATIO
            GO TO 101
          END IF
  100   CONTINUE
C       Now recalculate numbers of mature etc
  101   PCALF(IX) = UNRM(0) + UNRF(0) + RM(0) + RF(0)
        PMATF(IX) = 0.d0
        PMATM = 0.d0
        N1PLUS(IX) = 0.d0
        PSURV(IX) = 0.d0
        DO 105 L2 = L,MAXAGE
          PMATF(IX) = PMATF(IX) + (RF(L2)+UNRF(L2))*FMATUR(L2)
          PMATM     = PMATM     + (RM(L2)+UNRM(L2))*FMATUR(L2)
          N1PLUS(IX) = N1PLUS(IX) +  RM(L2)+UNRM(L2) +RF(L2)+UNRF(L2)
          IF (L.GT.0) PSURV(IX) = PSURV(IX) + 
     +              SELSUVM*(UNRM(L)+RM(L)) + SELSUVF*(UNRF(L)+RF(L))
  105   CONTINUE
        PMATA(IX) = PMATF(IX) + PMATM
      ENDIF

c      PRINT '(I4,3F8.4,4F8.1,3f8.4)',IX+1,SUR(0),SUR(1),SUR(MAXAGE),
c     +      PCALF(IX),PMATF(IX),N1PLUS(IX),navmat,ProbB,pdd

      RETURN
      END

C *********************************************************************

      SUBROUTINE SETRY(P1EXP,NRY,NEEDI,PR,NAvMat,OLASTQ)

C     SETRY sets RY for the new year

      USE DECL_STKVRS
      USE DECL_STOCHM

      DOUBLE PRECISION BIN1,BIN2,BIRTHS,PROBB,NAVMAT,PR,
     +     SUMRY,NEEDI,SR,SRU,RECNEW,P1EXP,SRM,SRF,OLASTQ,OQVAR,SLASTQ
      INTEGER L,ISIM,NRY,OIVAR
      EXTERNAL BIN1,BIN2

C *** Calculation of RY
      IF (ISTOCH.LE.0) THEN

C       Set deterministic RY : Eqn G.2a
C       Sum new recruits and number of survivors (w/o fishing)
        RECNEW = 0.D0
        SR = SUR(MAXAGE)*(RM(MAXAGE) + RF(MAXAGE))
        DO 60 L = 0,MAXAGE-1
          RECNEW = RECNEW + SUR(L)*RECF(L+1)*(UNRM(L)+UNRF(L))
          SR = SR + SUR(L)*(RM(L) + RF(L))
 60     CONTINUE
C
C       RY = (# of recruits - # of deaths) / weighted mean survival: Eqn G.2a
        RY = 0.D0
        IF (SR .GT. 0.D0) RY = (RECNEW+SR-P1EXP) * P1EXP/SR

      ELSE IF (NRY.GT.0 .AND. NEEDI.GT.0.D0) THEN
C       Set stochastic RY (project forward to year IX+1 w/o fishing)
C       NEEDI=0 in years between setting of 5yr quotas
C       IVAR, QVAR & LASTQ are changed so store (here or above) & reset after
        OIVAR = IVAR
        IVAR = 0
        OQVAR = QVAR
        QVAR = 0
        SLASTQ = LASTQ
        SUMRY = 0.d0

        DO 80 ISIM=1,NRY

C         Stochasticity in births.
          LASTQ = OLASTQ
          PROBB = PR
          IF (ISTOCH.EQ.2) CALL GBirth(ProbB)
          BIRTHS = Bin1(MAX(NAvMat,0.d0),ProbB)

C         1st advance the pooled age class. Add surviving births
          SR  =      Bin2(RM(MAXAGE),SUR(MAXAGE))
          SR  = SR + Bin2(RF(MAXAGE),SUR(MAXAGE))
          SRU = SR + Bin2(BIRTHS*RECF(1),SUR(0))

C         Advance remaining recruited age classes & accumulate totals
          DO 70 L = 1,MAXAGE-1
            SRM = Bin2(RM(L),SUR(L))
            SRF = Bin2(RF(L),SUR(L))
            SR  = SR + SRM + SRF
C           Add on new recruits:
            SRU = SRU + SRM + Bin2(UNRM(L)*RECF(L+1),SUR(L))
            SRU = SRU + SRF + Bin2(UNRF(L)*RECF(L+1),SUR(L))
   70     CONTINUE

C         RY = (# of recruits - # of deaths) / weighted mean survival: Eqn G.2b
C         Add to SUM
          IF (SR.GT.0.D0) SUMRY = SUMRY + ((SRU-P1EXP)*P1EXP/SR)

   80   CONTINUE
        RY = SUMRY / DBLE(NRY)
        IVAR = OIVAR
        QVAR = OQVAR
        LASTQ = SLASTQ

      ELSE
C       RY not set
        RY = -1.D0
      ENDIF
C
      RETURN
      END
C
C
C *********************************************************************

      SUBROUTINE SETSUR(I1,IA,IYR)

C     Update survival array SUR if it is time dependent (if OPTM > 0, see Dviii)
C     OPTM= 0: none  1: doubles  2: halves  4: steps (halve then double)
C     I1 = 0 for initialisation.  IA is used to pass value of AGEJ.

      USE DECL_STKVRS

      DOUBLE PRECISION MORTJ,MORT,MJINC,MINC
      INTEGER I1,IA,AGEJ,J,I,IYR
      SAVE MORTJ,MORT,MJINC,MINC,AGEJ

      IF (I1.EQ.0) THEN
C       Initialise variables used to update SUR array (called by RESET)
        AGEJ = IA
        MORTJ = -LOG(SUR(AGEJ))
        MORT  = -LOG(SUR(MAXAGE))
C       Set increments for M doubling or halving
        IF (OPTM.EQ.1) THEN
          MJINC = MORTJ / 100.d0
          MINC  = MORT / 100.d0
        ELSE IF (OPTM.EQ.2) THEN
          MJINC = -MORTJ / 200.d0
          MINC  = -MORT / 200.d0
        ELSE IF (OPTM.EQ.5) THEN
          MJINC = MORTJ / 200.d0
          MINC  = MORT / 200.d0
        ELSE IF (OPTM.EQ.6) THEN
          MJINC = -MORTJ / 300.d0
          MINC  = -MORT / 300.d0
        ENDIF

        RETURN

C     Update survival rate {Eqn A1.2}   (called by STKUPA)
      ELSE IF (OPTM.NE.4) THEN
        MORTJ = MORTJ + MJINC
        MORT = MORT + MINC
      ELSE IF (IYR.EQ.33) THEN
C       33 yr step change in M
        MORTJ = MORTJ * 0.5d0
        MORT = MORT * 0.5d0
      ELSE IF (IYR.EQ.66) THEN
        MORTJ = MORTJ+MORTJ
        MORT  = MORT +MORT
      ELSE
C       SUR doesn't change this year
        RETURN
      ENDIF

C     Corrected Oct01: previously used 1,AGEJ
      DO 5 J = 0,AGEJ
       SUR(J) = EXP(-MORTJ)
   5  CONTINUE
      DO 6 J = AGEJ+1,MAXAGE
       SUR(J) = EXP(-MORT)
   6  CONTINUE
      IF (AGEJ.GT.0) SUR(0) = SUR(0)*SUR(MAXAGE)

C     Reset SUMSUR = SUR + SUR**2 + ..... + SUR**NY.
      SUMSUR = 0.D0
      DO 10 I = 1,NY
        SUMSUR = SUMSUR + SUR(MAXAGE)**I
  10  CONTINUE

      RETURN
      END

C ======================================================================
C
      SUBROUTINE SETENV()
C
C     SETENV only called if ISTOCH=2 (Stochastic model)
C     SETENV sets up relationship between mu and b, ready for integral A2.8

C     Note: SIGCON = SIGEPS / SQRT(1-RHO**2) in integral A2.8
C
      USE DECL_ENVCOM
C
C     Local variables
      DOUBLE PRECISION Mu,q,Term1,term2,Qinc,Total
      INTEGER IM,II
C
C     Work through each value of Mu
C     Set b for 1600 values of Mu from -8 to +8
      DO 10 IM = -800,800
       Mu = IM*0.01d0
       MuI(IM) = Mu
       qinc = 0.01d0
       Total = 0.d0
       DO 5 II = -1000,1000
        q = qinc*II
        Term1 = EXP(q)/(1.D0+EXP(q))
        Term2 = EXP(-(q-Mu)**2 / (2.D0*SIGCON*SIGCON))
        Total = Total + Term1*Term2
    5  CONTINUE
       Total = Total / SQRT(2.D0*3.141592654D0)/SIGCON*qinc
       Bexp(IM) = Total
   10 CONTINUE
      BMIN=BEXP(-800)
      BMAX=BEXP(800)
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE SolveB(Birth,Mu)
C
C     SOLVEB only called if ISTOCH=2 (Stochastic model)
C     SOLVEB solves for Mu given an expected Birth in eqn A2.8
C     using a binary search of array BEXP which was set in SETENV
C
      USE DECL_ENVCOM
C
C     Global variables
      DOUBLE PRECISION Birth,Mu,Alpha,Beta
C
C     Local variables
      INTEGER Imax,Imin,Itst,II
C
C     Do a bisection: find ITST such that BEXP(ITST) > Birth >= BEXP(ITST-1)
      IMin = -800
      Imax = 800
      DO 1000 II = 1,50
       Itst = (Imin+Imax)/2
       IF (Bexp(Itst).GT.Birth.AND.Bexp(Itst-1).LE.Birth) GOTO 1100
       IF (Bexp(Itst).GT.Birth) THEN
        Imax = Itst
       ELSE
        Imin = Itst
       ENDIF
1000  CONTINUE
      IF (Bexp(800).GT.Birth.AND.Bexp(799).LE.Birth) THEN
C       Above loop does not reach ITST=800
        Itst = 800
        GOTO 1100
      ENDIF
      WRITE(*,*) 'Error in SOLVEB',Birth,BMIN,BMAX
      STOP
1100  CONTINUE 
      Beta = (MuI(Itst)-MuI(Itst-1))/(Bexp(Itst)-Bexp(Itst-1))
      Alpha = MuI(Itst)-Beta*Bexp(Itst)
      Mu = Alpha+Beta*Birth
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE GBirth(Birth)
C
C     Routine only called if ISTOCH=2 : stochastic births
C     GBIRTH generates a birth rate as specified in A2.7
C     On entry BIRTH = expectation value
C
      USE DECL_ENVCOM
      USE DECL_STOCHM
C
      DOUBLE PRECISION BIRTH,QT,MU
C
C     Increment Qvar, the pointer for the set of normal random numbers
      QVAR = QVAR + 1
C     Use SolveB to solve eqn A2.8 for Mu
      IF (BIRTH.GT.BMIN. AND. BIRTH.LT.BMAX) THEN
       CALL SOLVEB(BIRTH,MU)
C      Find Qt and BIRTH using eqns A2.7
       QT = MU + RHO*LASTQ + NORMNO(QVAR)*SIGEPS
       LASTQ = QT-MU
       BIRTH = EXP(QT)/(1.D0+EXP(QT))
      ELSE
C      Ensure it can't fall over in solveb
       IF (BIRTH.LT.BMIN) BIRTH=BMIN
       IF (BIRTH.GT.BMAX) BIRTH=BMAX
      ENDIF  
C
      RETURN
      END

C ======================================================================

      REAL*8 FUNCTION RELLIKE(RELYR,RELOBS,CVRELOBS2,NRELABUND)
C
C Compute the likelihood for relative abundance indices (integrate over the
C additional variance
C
      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_DATVRS
C
C     Global variabkes
      DOUBLE PRECISION  RELOBS(22),CVRELOBS2(22)
      INTEGER RELYR(22),NRELABUND
C
C     Local variables
      DOUBLE PRECISION LIKEL,NUMER,DENOM,QQ,LIKEL2
      DOUBLE PRECISION SIGMAADD
      INTEGER I
C
C     Find "Q"
      NUMER = 0
      DENOM = 0
      DO 981 I = 1,NRELABUND
       IYR = RELYR(I)
       NUMER = NUMER+(RELOBS(I)-LOG(PSURV(IYR)))/CVRELOBS2(I)
       DENOM = DENOM+1.0/CVRELOBS2(I)
981   CONTINUE
      QQ = EXP(NUMER/DENOM)
      QPASS = QQ
      
      LIKEL = 0
      BESTL = 0
      DO 1000 II = 0,49
       SIGMAADD = (II+0.5)*0.35/50.0
    
       LIKEL2 = 0
       DO 1100 I = 1,NRELABUND
        IYR = RELYR(I)
        YY1 = RELOBS(I) - LOG(PSURV(IYR)*QQ)
        YY2 = CVRELOBS2(I)+SIGMAADD**2.0
        LIKEL2 = LIKEL2 + (YY1*YY1/YY2 + LOG(YY2)) * 0.5d0                 {A8.1a}
1100   CONTINUE       
       LIKEL = LIKEL + EXP(-LIKEL2)
       IF (LIKEL2.LT.BESTL) THEN
        BESTL = LIKEL2
        SIGMAPASS = SIGMAADD
       ENDIF
C
1000  CONTINUE
      LIKEL = -1*LOG(LIKEL)
C
      RELLIKE = LIKEL
C
      RETURN
      END
    

C ======================================================================

      SUBROUTINE CONDG (IYRMAN,REF)

C     GRAY WHALES: Generate from the prior distributions,
C     calculate the likelihood function and save the results to disk.

      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_DATVRS
C
      INTEGER REPLI,IERR,IYRMAN,I,NALL,ISEEDX,
     +        ISEEDZ,NDRAW,MAXDUP,NABUND,ABSYR(22),IYR,Iunique,ITest,
     +        NRELABUND,NCOUNT,RELYR(22),CNTYR(22),
     +        NREL2ABUND,REl2YR(22)
      INTEGER NABUND2,PSURV2YR(10)
      LOGICAL GOOD,LPRJ
      DOUBLE PRECISION DIF,P1,P2,F1,F2,PARS(10),TOL,
     +       LIKEL,PINIT,ABSOBS(22),CVOBS2(22),
     +       HITPYR,HITPEG,MXLIKE,TOTLKE,C1,YY1,YY2,DOFS1,ECVA2,
     +       RELOBS(22),CVRELOBS2(22),PCOUNT(22),NUMER,DENOM,QQ,
     +       REL2OBS(22),CVREL2OBS2(22),EFFORTP(22),Pred,
     +       LIKEL2,LIKEL3
      DOUBLE PRECISION PSURV2(10),PCV2(10),LIKEA,INCVAL,LIKEV
      CHARACTER REF*10
      DOUBLE PRECISION RELLIKE
      EXTERNAL HITPYR,HITPEG
C
C     Seed assignment 
      LPRJ = IYRPRJ.NE.INITYR
      TOL = 0.00001d0
C
C     Read in the absolute abundance estimates + CVs.
C     Store log of abundance in ABSOBS
      READ(14,*) NABUND
      DO 800 I = 1,NABUND
        READ(14,*) ABSYR(I),ABSOBS(I),CVOBS2(I)
        ABSOBS(I) = log(ABSOBS(I))
        ABSYR(I) = ABSYR(I) - IYRMAN
        CVOBS2(I) = CVOBS2(I)* CVOBS2(I)
800   CONTINUE
      DOFS1 = DOFS *0.5D0 - 1.D0
      READ(14,*)
C
C     Read in the relative abundance estimates + CVs.
C     Store log of abundance in ABSOBS
      READ(14,*) NRELABUND
      DO 810 I = 1,NRELABUND
        READ(14,*) RELYR(I),RELOBS(I),CVRELOBS2(I)
        RELOBS(I) = log(RELOBS(I))
        RELYR(I) = RELYR(I) - IYRMAN
        CVRELOBS2(I) = CVRELOBS2(I)* CVRELOBS2(I)
810   CONTINUE
      READ(14,*)
C
C     Read in the relative abundance estimates + CVs.
C     Store log of abundance in ABSOBS
      READ(14,*) NREL2ABUND
      DO 811 I = 1,NREL2ABUND
        READ(14,*) REL2YR(I),REL2OBS(I),CVREL2OBS2(I)
        REL2OBS(I) = log(REL2OBS(I))
        REL2YR(I) = REL2YR(I) - IYRMAN
        CVREL2OBS2(I) = CVREL2OBS2(I)* CVREL2OBS2(I)
811   CONTINUE
      READ(14,*)
C
C     Read in the count data (Poisson)
C     Store log of abundance in ABSOBS
      READ(14,*) NCOUNT
      WRITE(*,*) NCOUNT
      DO 820 I = 1,NCOUNT
        READ(14,*) CNTYR(I),PCOUNT(I),EFFORTP(I)
        CNTYR(I) = CNTYR(I) - IYRMAN
820   CONTINUE
      READ(14,*)
C
C     Read in the count data (variable biass)
      READ(14,*) NABUND2
      WRITE(*,*) NABUND2
      DO 830 I = 1,NABUND2
       READ(14,*) PSURV2YR(I),PSURV2(I),PCV2(I)
       WRITE(*,*) PSURV2(I)
       PSURV2YR(I) = PSURV2YR(I) - IYRMAN
830   CONTINUE       
C
      NALL = 0
      TOTLKE = 0.D0
      MXLIKE = 0.D0
      NDRAW = 5000
      IF (NABUND.EQ.0) NDRAW = 2500
      ITEST = 5000
      WRITE(*,*) ISEEDC

C     Move through all the replicates
  950 CONTINUE
      DO 1000 REPLI = 1,NDRAW
C
C      Generate ISEEDZ=seed for this replicate + 
C      parameters from the priors using ISEEDC (modify for new cases). Step A8a
       CALL GENPAR(SUR,MAXAGE,ISEEDC,CVADD2,BIAS(IYRTAR),ISEEDZ,LPRJ,
     +             GAMMAL,AR,PBIAS)
C
C      Find the juvenile S corresponding to adult M, pregnancy rate. Step A8b
       P1 = 5.d0
       P2 = 100.d0
       CALL ZBRAC2(HITPEG,PARS,1,P1,P2,F1,F2,GOOD)
       IF (.NOT.GOOD) GO TO 1000
       CALL ZBRNT2(HITPEG,PARS,1,P1,P2,F1,F2,0.0001d0,IERR)
       DIF = HITPEG(PARS,IERR)
       IF (ADULTS.LT.JUVS .OR. ABS(DIF).GT.0.001d0) GO TO 1000
       IF ((ISTOCH.EQ.-3 .OR. ISTOCH.GE.0)  .AND.
     +     (A1*Z.LT.0.D0 .OR. Z.LT.-100.D0  .OR.
     +      Z.GT.1000.D0 .OR. ABS(A1).GT.100.D0)) GO TO 1000

C      Store ISEEDZ and use it to set random no for stochastic population model
       ISEEDX = ISEEDZ
       IF (ISTOCH.GE.1) CALL SETRAN(ISEEDZ,MAXAGE,ISTOCH,1-IYRPRJ)
C
C      If OPTI>0 set integrated parameters: hist.cat.bias, stoch.parms (& lag)
       IF (OPTI.GT.0) CALL GENP2(ISEEDX)
C
C      Find 1+ population in IYRPRJ giving target (PTARGT) in IYRTAR (1968). Step A8c
       IF (LPRJ)  THEN
         P1 = 1.d0
         P2 = K1
       ELSE
         P1 =  5000.D0
         P2 = 10000.D0
       ENDIF
       CALL ZBRAC2(HITPYR,PARS,1,P1,P2,F1,F2,GOOD)
       IF (F1*F2.GT.0) GOTO 1000
       DO 10100 II = 1,20
        PARS(1) = (P1+P2)/2.0
        DIF = HITPYR(PARS,IERR)
        IF (DIF.LT.0) THEN
         P1 = PARS(1)
        ELSE
         P2 = PARS(1)
        ENDIF
10100  CONTINUE        
C        
       IF (.NOT.GOOD) GO TO 1000
       CALL ZBRNT2(HITPYR,PARS,1,P1,P2,F1,F2,TOL,IERR)
       DIF = HITPYR(PARS,IERR)
       IF (ABS(DIF).GT.10.D0) GOTO 1000
       PINIT = PARS(1)
       IF (.NOT.LPRJ) K1 = PINIT
C      Inertia model. Throw out projections with high populations 1880-1920
       IF (ISTOCH.EQ.-3 .AND. N1PLUS(1900-IYRMAN).GT.5000.D0) GO TO 1000   ## Not used in Gray paper
C
C      Calculate likelihood (absolute abundance estimates)
       LIKEL = 0.D0
       C1 = CVADD2 / (0.1D0 + (0.013D0*K1)/PSURV(IYRTAR))
       DO 970 I = 1,NABUND
         IYR = ABSYR(I)
         IF (IYR.NE.IYRTAR) THEN
           WRITE(*,*) IYR,EXP(ABSOBS(I))
           ECVA2 = C1 * (0.1D0 + (0.013D0*K1)/PSURV(IYR))               {A8.2}
           YY1 = ABSOBS(I) - LOG(PSURV(IYR)*BIAS(IYR))
           YY2 = CVOBS2(I) + ECVA2
           LIKEL = LIKEL + (YY1*YY1/YY2 + LOG(YY2)) * 0.5d0             {A8.1a}
         ENDIF
 970   CONTINUE
C
C      Relative abundance index
       IF (NRELABUND.GT.0) THEN
        C1 = CVADD2 / (0.1D0 + (0.013D0*K1)/PSURV(IYRTAR))
        NUMER = 0
        DENOM = 0
        DO 981 I = 1,NRELABUND
         IYR = RELYR(I)
         ECVA2 = C1 * (0.1D0 + (0.013D0*K1)/PSURV(IYR))                 {A8.2}
         YY2 = CVRELOBS2(I)+ECVA2
         NUMER = NUMER+(RELOBS(I)-LOG(PSURV(IYR)))/YY2
         DENOM = DENOM+1.0/YY2
981     CONTINUE
        QQ = EXP(NUMER/DENOM)
        QPASS = QQ
        LIKEL2 = 0
        DO 982 I = 1,NRELABUND
         IYR = RELYR(I)
         ECVA2 = C1 * (0.1D0 + (0.013D0*K1)/PSURV(IYR))                 {A8.2}
         YY1 = RELOBS(I) - LOG(PSURV(IYR)*QQ)
         YY2 = CVRELOBS2(I)+ECVA2
         LIKEL2 = LIKEL2 + (YY1*YY1/YY2 + LOG(YY2)) * 0.5d0             {A8.1a}
982     CONTINUE       
        QVAR1 = QQ
        RELSIGMA1 = SQRT(CVADD2)
       ELSE
        LIKEL2 = 0
        QVAR1 = 1
        RELSIGMA1 = 0
       ENDIF
       IF (NREL2ABUND.GT.0) THEN
        LIKEL3 = RELLIKE(REL2YR,REL2OBS,CVREL2OBS2,NREL2ABUND)
        QVAR2 = QPASS
        RELSIGMA2 = SIGMAPASS
       ELSE
        LIKEL3 = 0
        QVAR2 = 1
        RELSIGMA2 = 0
       ENDIF
       LIKEL = LIKEL + LIKEL2 + LIKEL3
C
C      Find "Q" for the counts
       NUMER = 0
       DENOM = 0
       DO 991 I = 1,NCOUNT
        IYR = CNTYR(I)
        NUMER = NUMER + PCOUNT(I)/(PSURV(IYR)*EFFORTP(I))
        DENOM = DENOM + 1.0
991    CONTINUE
       IF (DENOM.GT.0) QQ = NUMER/DENOM
       QVAR3 = QQ
       DO 992 I = 1,NCOUNT
        IYR = CNTYR(I)
        Pred = PSURV(IYR)*EFFORTP(I)
        YY1 = PCOUNT(I)*LOG(QQ*Pred) - QQ*Pred
        LIKEL = LIKEL - YY1
992    CONTINUE  

       WRITE(*,*) NABUND2
       DO 20000 I = 1,NABUND2
        LIKEA = 0
        INCVAL = 1.0/50.0
        WRITE(*,*) PSURV2(I)
        DO 21000 J = 1,50
         BETAV =  INCVAL*(FLOAT(J)-0.5)
         ERROR = LOG(PSURV2(I)) - LOG(BETAV*PSURV(PSURV2YR(I)))
         ERROR = ERROR / PCV2(I)
         LIKEV = EXP(-ERROR*ERROR)
         DENSITY = BETAV**(BETA1-1.0)*(1.0-BETAV)**(BETA2-1.0)
         LIKEA = LIKEA + 1.0/50.0*LIKEV*DENSITY
21000   CONTINUE        
        LIKEL = LIKEL - LOG(LIKEA)
20000  CONTINUE       
C        
C      If likelihood v. large don't store
       IF (LIKEL.GT.99999.d0) GO TO 1000
C
       OPEN(UNIT=13,FILE='CONDIT.TMP',ACCESS='APPEND')
       WRITE(13,600) LikeL,TRANSA,NINT(MAT50),MSYL,MSYR,JUVS,ADULTS,K1,
     +    PINIT,ISEEDX,PTARGT,SQRT(CVADD2),AR,PregTG,
     +    GAMMAL,PSURV(-1),QVAR1,RELSIGMA1,QVAR2,RELSIGMA2,QVAR3,PBIAS
       CLOSE(13)

C      Find MXLIKE = maximum value of likelihood and test probability of
C      having sufficient replicates
       LIKEL = EXP(-LIKEL+40.D0)
       TOTLKE = TOTLKE + LIKEL
       IF (LIKEL.GT.MXLIKE) MXLIKE = LIKEL
       NALL = NALL + 1
       IF (MOD(NALL,200).EQ.0) PRINT '(2A,I10,F10.5,I5)',' CONDG  ',
     +             REF,NALL,MXLIKE/TOTLKE,ITEST

1000  CONTINUE
C
1001  IF (NALL.GT.0 .AND. MXSIM.GT.100) THEN
C      AEP gray whale runs & extended inertia trials
       CALL COND2(NTRIAL,NALL,MXLIKE,TOTLKE,MAXDUP,IUnique,'P100.PAR ')
       PRINT '(A,I6,I8,I6)',' CONDGA  ',MAXDUP,ITEST,Iunique
C       CALL COND2(MXSIM,NALL,MXLIKE,TOTLKE,MAXDUP,IUnique,'PARMS.PAR')
C       PRINT '(A,I6,I8,I6)',' CONDGB  ',MAXDUP,ITEST,Iunique
       IF (MAXDUP.LE.1.OR.Iunique.GT.0.975*NTRIAL.OR.ITEST.LE.0) RETURN
      ELSE IF (NALL.GT.0) THEN
C      AW trials
       CALL COND2(NTRIAL,NALL,MXLIKE,TOTLKE,MAXDUP,IUnique,'P100.PAR ')
       PRINT '(A,I6,I8,I6/)',' CONDG ',MAXDUP,ITEST,Iunique
       IF (MAXDUP.EQ.1 .OR. ITEST.LE.0) RETURN
      ENDIF
      ITEST = ITEST - 1
      GO TO 950
 600  FORMAT(F11.5,2I3,4F16.13,2F16.9,I11,F10.3,4F16.12,F10.3,
     +       2(F15.12,F10.7),F15.13,F16.12)
      END

C **********************************************************************

      SUBROUTINE COND2(NREPL,NALL,MXLIKE,TOTLKE,MaxDup,IUnique,OFILE)

C     COND2 reads in CONDIT.TMP and selects NREPL sets of parameters
C     with replacement (computes the major weight and the number of
C     duplicate replicates (subroutine based on SUMMARY.FOR)

C     Global variables
      INTEGER NREPL,NALL
      DOUBLE PRECISION TOTLKE,MXLIKE
      CHARACTER*9 OFILE
C
C     Local variables
      DOUBLE PRECISION LikeL,MA(55),CUMDO(10000),Cumtot,RAN1,RNLogL
      INTEGER ISEED,I,II,INEXT,INEXTP,IQQ,IDupl,MaxDup,Iunique
      CHARACTER*278 STRING
      EXTERNAL RAN1
C
C     Generate a wad of uniform random numbers
      ISEED = -8989
      DO 2000 I = 1,NREPL
       CUMDO(I) = RAN1(ISEED,MA,INEXT,INEXTP)
2000  CONTINUE
      CALL SORT(CUMDO,NREPL)

      OPEN(UNIT=13,FILE='CONDIT.TMP',STATUS='OLD')
      OPEN(UNIT=15,FILE=OFILE,STATUS='UNKNOWN')
      WRITE(15,'(1X,2A3,6A16,A11,A10,4A16,A10,2(A15,A10),A15,A15,A12)') 
     +   'Aa','Am','MSYL',
     +   'MSYR','MORTJ','MORTA','K1+','PInit','ISEED','PTarget','CVADD',
     +   'AR','PREG-MAX','GAMMA','P0',"Q1","SIG1","Q2","SIG2",
     +   "Q3","PBIAS",'LIKEL'
      CumTot = 0.d0
      IQQ = 1
      Iunique = 0
      MaxDup = 0
      DO 3000 II = 1,NALL
       READ(13,'(F11.5,A)') LikeL,STRING
       RNLogL = Likel
       LikeL = EXP(-LikeL+40.d0)
       CumTot = CumTot + LikeL
       Idupl = 0
3001   IF (IQQ.LE.NREPL .AND. CumTot/TOTLKE.GT.CumDO(IQQ)) THEN
         WRITE(15,'(1X,A,F11.5)') STRING,RNLogL
         IF (Idupl.EQ.0) Iunique = Iunique + 1
         Idupl = Idupl + 1
         IF (Idupl.GT.MaxDup) MaxDup = IDupl
         IQQ = IQQ + 1
         GOTO 3001
       ENDIF
3000  CONTINUE
      CLOSE(13)
      WRITE (15,*)
      WRITE (15,*) ' COND2',MaxDup,NALL,TOTLKE,MXLIKE,MXLIKE/TOTLKE,
     +             Iunique
      CLOSE(15)
C
      RETURN
      END

C **********************************************************************
C
      SUBROUTINE GENPAR(SUR,MAXAGE,ISEED,CVADD2,BIAST,ISEEDZ,LPRJ,
     +                  GAMMAL,AR,PBIAS)
C
C     Generate parameters from prior distributions.  Step A8a
C
      USE DECL_SETUP
      USE DECL_CONVRS
C
C     Global variables
      DOUBLE PRECISION SUR(0:MXAGE),CVADD2,GAMMAL,AR,PBIAS
      INTEGER ISEED,MAXAGE,ISEEDZ
      LOGICAL LPRJ
C
C     Local variables
      DOUBLE PRECISION FECMAX,ERROR,PTEMP(4),CVALL2,BIAST,PRIOR
      INTEGER Age

      EXTERNAL PRIOR
C
C     Seed generation
      PTEMP(1) = -1000000.d0
      PTEMP(2) = -1.d0
      ISEEDZ = NINT(PRIOR('U',PTEMP,ISEED))
C
      MSYL = PRIOR(MSYLT,MSYLP,ISEED)
      MSYR = PRIOR(MSYRT,MSYRP,ISEED)
C
C     Generate the juvenile / adult transition age
      TRANSA =  PRIOR(APART,APARP,ISEED)
C
C     Generate the juvenile natural survival rate  NOT USED HERE
1100  JUVS = PRIOR(JMORTT,JMORTP,ISEED)
      IF (JUVS.LE.0.d0.OR.JUVS.GE.1.d0) GOTO 1100
C     Note: these juvenile values are not used - value is reset in HITPEG
C
C     Generate the adult natural survival rate (note constraints)
1200  ADULTS =  PRIOR(AMORTT,AMORTP,ISEED)
      IF (ADULTS.LT.JUVS) GOTO 1200
      IF (ADULTS.LE.0.d0.OR.ADULTS.GE.1.d0) GOTO 1200
C
C     Set up the SUR array = survival rate - do juveniles later {Eqn A1.2}
      DO 6 AGE = TRANSA+1,MAXAGE
       SUR(AGE) = ADULTS
   6  CONTINUE
C
C     Generate the target pregnancy rate
      FecMax = PRIOR(PREGNT,PREGNP,ISEED)
      PREGTG = 1.d0 / FecMax
C
C     Generate an age-at-maturity (and convert to first parturition)
      MAT50 = PRIOR(TPART,TPARP,ISEED)
      MAT50 = MAT50 + 1
C
C     Generate a target population size
      CVADD2 = PRIOR(CVADT,CVADP,ISEED)
      CVADD2 = CVADD2 * CVADD2
      CVAll2 = CVADD2 + TARGP(2)**2
      PTEMP(1) = 0.d0
      PTEMP(2) = SQRT(CVALL2)
      PTEMP(3) = -10.d0
      PTEMP(4) =  10.d0
      Error = PRIOR(TARGT,PTEMP,ISEED) - CVAll2/2.d0
      PTARGT = TARGP(1)*EXP(Error)*1000.d0/BIAST
C
C     Generate Lower end of the Bias prior
      PBIAS = PRIOR(PBIAST,PBIASP,ISEED)

C     Generate 1+ carrying capacity for trials starting after catching begun
      IF (LPRJ) K1 = PRIOR(CARRYT,CARRYP,ISEED)*1000.0
C
C     Generate values for Gamma and AR (Inertia model)
      GammaL = PRIOR(GammaLT,GammaLP,ISEED)
      AR = PRIOR(ART,ARP,ISEED)
C
C
      RETURN
      END
C
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION HITPYR (PARS, IERR)
C
C  Step A8c
C  HITPYR calculates the difference between the target hit and the
C  target sought (=PTARGT in IYRTAR) for the population shooting routine
C
C  PARS(1) = Initial 1+ population size to be used
C
      USE DECL_STKVRS
      USE DECL_CONVRS

      DOUBLE PRECISION PARS(1),PINIT,PHIT
      INTEGER IERR,OK

C     Project the population ahead given PARS
      IERR = 0
      
      PINIT = PARS(1)
      IF (INITYR.EQ.IYRPRJ) THEN
C       Start model when population pristine. PARS(1) = Test value for K
        PINIT = MAX(7000.D0, MIN(60000.D0,PINIT))
        CALL SETKA(PINIT,PINIT,PHIT,OK)
      ELSE
C       Start model in more recent year.
C       PARS(1) = Test value of P at start of projection. K is fixed =generated parameter
        PINIT = MIN(K1,PINIT)
        CALL SETKA(K1,PINIT,PHIT,OK)
      ENDIF
C      WRITE(*,600) PINIT,OK,PTARGT,PHIT,PHIT-PTARGT
600   FORMAT(1x,F9.3,1x,I1,3(1x,F9.3))      

C     Population went -ve if OK=0
      IF (OK.EQ.0) THEN
        HITPYR = -PTARGT
      ELSE
C       Find the difference between the target and projected values
        HITPYR = PHIT - PTARGT
      ENDIF
C
      RETURN
      END
C
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION HITPEG (PARS, IERR)
C
C  Subroutine calculates the difference between the target maximum pregnancy
C  rate and the model prediction
C
      USE DECL_STKVRS
      USE DECL_CONVRS
      USE DECL_SETUP

      DOUBLE PRECISION PARS(1)
      INTEGER IERR
C
C     Local variables
      INTEGER Age,OK
C
C     Check for out of bounds
      IERR = 0
      IF (PARS(1).LE.   0.d0) PARS(1) = 0.000001d0
      IF (PARS(1).GE.1000.d0) PARS(1) = 1000.d0
C
C     Set up juv. SURvival array (adult values set in GENPAR)
      JUVS = EXP(-PARS(1)/1000.d0)
      DO 1010 Age = 0,TRANSA
        SUR(Age) = JUVS
1010  CONTINUE
      IF (TRANSA.GT.0) SUR(0) = JUVS*ADULTS
C
C     Find the difference between the target and projected value  {Eqn A7.4}
      OK = 1
      CALL MSYPAR (OK)
      IF (OK.EQ.1 .OR. ISTOCH.EQ.-1.OR.ISTOCH.EQ.-2) THEN
        IF (ISTOCH.EQ.-1) THEN
          INVFEC = FEC*EXP(GAMMAL*(1+AR))
        ELSEIF (ISTOCH.EQ.-2) THEN
          INVFEC = FEC*EXP(GAMMAL)
        ELSEIF (ISTOCH.EQ.-3) THEN
          INVFEC = FEC*(1.0d0 + A1*AR)/(1.d0+FEC*(1.d0+A1*AR)*SUMSUR)
        ELSE  
          INVFEC = FEC*(1.0d0 + A1)/(1.d0+FEC*(1.d0+A1)*SUMSUR)
        ENDIF
        HITPEG = (PREGTG - INVFEC)*10000.d0
      ELSE
        HITPEG = 100000.D0
      ENDIF

      RETURN
      END
C
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION PRIOR(DISTRI,PAR,ISEED)
C
C     Generate random numbers from priors
C
C     Global variables
      CHARACTER*1 DISTRI
      DOUBLE PRECISION PAR(4)
      INTEGER ISEED
C
C     Local variables 
      INTEGER ISEEDA,INEXT,INEXTP,JSEED
      DOUBLE PRECISION MA(55),RAN1,XNORM,GAMMAZ,GENBET
      EXTERNAL RAN1,XNORM,GAMMAZ,GENBET
      SAVE INEXT,INEXTP,MA

      IF (DISTRI.EQ.'U'.OR.DISTRI.EQ.'u') THEN
C       Uniform between par(1) and (2)
        PRIOR = RAN1(ISEED,MA,INEXT,INEXTP)*(PAR(2)-PAR(1))+PAR(1)

      ELSE IF (DISTRI.EQ.'L'.OR.DISTRI.EQ.'l') THEN
        PRIOR = RAN1(ISEED,MA,INEXT,INEXTP)*
     +   (LOG(PAR(2))-LOG(PAR(1)))+LOG(PAR(1))
        PRIOR = EXP(PRIOR)

      ELSE IF (DISTRI.EQ.'B'.OR.DISTRI.EQ.'b') THEN
C       Beta distribitoon
        JSEED = -1*RAN1(ISEED,MA,INEXT,INEXTP)*100000
        PRIOR = GENBET(PAR(1),PAR(2),JSEED)

      ELSE IF (DISTRI.EQ.'D'.OR.DISTRI.EQ.'d') THEN
C       Uniform integer between par(1) & (2)
        PRIOR = RAN1(ISEED,MA,INEXT,INEXTP)*(PAR(2)-PAR(1)+1)+PAR(1)
        PRIOR = INT(PRIOR)

      ELSE IF (DISTRI.EQ.'N'.OR.DISTRI.EQ.'n') THEN
C       Normal, mean par(1), sd par(2), between par(3) & (4)
8988    PRIOR = XNORM(PAR(2),PAR(1),ISEED,MA,INEXT,INEXTP)
        IF (PRIOR.LE.PAR(3).OR.PRIOR.GE.PAR(4)) GOTO 8988

      ELSE IF (DISTRI.EQ.'I'.OR.DISTRI.EQ.'i') THEN
C       Normal, mean par(1), sd par(2), nearest integer between par(3) & (4)
8989    PRIOR = XNORM(PAR(2),PAR(1),ISEED,MA,INEXT,INEXTP)
        PRIOR = NINT(PRIOR)
        IF (PRIOR.LT.PAR(3).OR.PRIOR.GT.PAR(4)) GOTO 8989

      ELSE IF (DISTRI.EQ.'G'.OR.DISTRI.EQ.'g') THEN
        ISEEDA = -(RAN1(ISEED,MA,INEXT,INEXTP)*1000000.d0+1.d0)
        PRIOR = GAMMAZ(PAR(1),PAR(2),ISEEDA)+PAR(3)
      ENDIF

      RETURN
      END

C **********************************************************************
C
      SUBROUTINE FINDEXTRAS(NRY)
C
C This subroutine applies bisection to find the extra S which keeps the populations stable
C
      USE DECL_SETUP
      USE DECL_STKVRS
      USE DECL_CONVRS
      IMPLICIT NONE

      INTEGER NRY

      DOUBLE PRECISION SUNRM(0:MXAGE),SUNRF(0:MXAGE),
     +    SRM(0:MXAGE),SRF(0:MXAGE),SN1PLUS,CM,CF,
     +    SMIN,SMAX,DEPL,INITAdult
      INTEGER L,Istep,OK,AGE,IYR
C
C     Save the age structure
       SN1PLUS = N1PLUS(-1)
       DO 10100 L = 0,MXAGE
        SRM(L) = RM(L)
        SRF(L) = RF(L)
        SUNRM(L) = UNRM(L)
        SUNRF(L) = UNRF(L)
10100  CONTINUE        
C
C     Reference level of surviva;    
      SMIN = SUR(MAXAGE)
      SMAX = 1.000
      INITADULT = SMIN

      DO 20000 Istep = 1,30
       ADULTS = (SMIN+SMAX)/2.0
C
C      Specify survival
       DO 21000 AGE = TRANSA+1,MAXAGE
        SUR(AGE) = ADULTS
21000  CONTINUE         
C
C      Compute the projected result
       DEPL = 0
       DO 22100 IYR = -1,IENDY2-1 
        CM = 0.0d0
        CF = 0.0d0
        CALL STKUPA (IYR,CM,CF,OK,NRY,NEED(0))
         IF (IYR.GE.IENDY2-50) DEPL = DEPL + N1PLUS(IYR)
22100  CONTINUE         
22000  CONTINUE      
       DEPL = DEPL/N1PLUS(INITYR)/50.0
C
C      Bisect
       IF (DEPL.LT.1) THEN
        SMIN = ADULTS
       ELSE
        SMAX = ADULTS
       ENDIF
C
C      Restore the age structure
       N1PLUS(-1) = SN1PLUS
       DO 23100 L = 0,MXAGE
        RM(L) = SRM(L)
        RF(L) = SRF(L)
        UNRM(L) = SUNRM(L)
        UNRF(L) = SUNRF(L)
23100  CONTINUE        
C
20000 CONTINUE
      IF (ABS(DEPL-1).GT.0.0001) WRITE(*,*) INITADULT,ADULTS,DEPL
C
      RETURN
      END



