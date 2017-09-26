C *********************************************************************

      DOUBLE PRECISION FUNCTION Bin1(Mean,Prob)

C     This subroutine implements the binomial*

      COMMON / STOCHM / RandNo,NormNO,LASTQ,Ivar,Qvar,ISTOC2
      DOUBLE PRECISION RandNo(1300000),NormNO(8500),LASTQ
      INTEGER Ivar,Qvar,ISTOC2
C
C     Global variables
      DOUBLE PRECISION Mean,Prob,Randd,RNN
      INTEGER INN, GenBin
      EXTERNAL GenBin
C
C     Quick check
      IF (Prob.LE.0.D0) THEN
        Bin1 = 0.d0
      ELSE IF (Prob.GE.1.D0) THEN
        Bin1 = Mean
C
C     What to do depends on whether this a stochastic model
      ELSE IF (ISTOC2.GE.1) THEN
C
        Ivar = Ivar + 1
        Randd = RandNo(Ivar)
        INN = INT(Mean)
        RNN = GenBin(Randd,Prob,INN)
        Bin1 = RNN + (Mean-INN)*Prob
C 
      ELSE
C
C       Deterministic model
        Bin1 = Mean*Prob
C
      ENDIF

      RETURN
      END

C *********************************************************************

      DOUBLE PRECISION FUNCTION Bin2(Mean,Prob)

C     This subroutine implements the binomial* (as BIN1 but from opposite end)

      COMMON / STOCHM / RandNo,NormNO,LASTQ,Ivar,Qvar,ISTOC2
      DOUBLE PRECISION RandNo(1300000),NormNO(8500),LASTQ
      INTEGER Ivar,Qvar,ISTOC2
C
C     Global variables
      DOUBLE PRECISION Mean,Prob,Randd,RNN
      INTEGER INN, GenBin
      EXTERNAL GenBin
C
C     Quick check
      IF (Prob.LE.0.D0) THEN
        Bin2 = 0.d0
      ELSE IF (Prob.GE.1.D0) THEN
        Bin2 = Mean
C
C     What to do depends on whether this a stochastic model
      ELSE IF (ISTOC2.GE.1) THEN
C
        Ivar = Ivar + 1
        Randd = RandNo(Ivar)
        INN = INT(Mean)
        RNN = GenBin(Randd,1.d0-Prob,INN)
        Bin2 = INN - RNN + (Mean-INN)*Prob
C
      ELSE
C
C       Deterministic model 
        Bin2 = Mean*Prob 
C        
      ENDIF

      RETURN
      END

C--------------------------------------------------------------------------
C
      INTEGER FUNCTION GenBin(Rand,Lamda,NN)
C
C     This subroutine generates from a Binomial distribution
C
C     Global variables
      DOUBLE PRECISION Rand,Lamda,Nall
      INTEGER NN
C      
      COMMON / Bintemp / BinCon,MaxN
      DOUBLE PRECISION BinCon(0:500,0:500)
      INTEGER MaxN
C
C     Local variables
      DOUBLE PRECISION P,Q,PR,Ompr,Bound,YY,Cump,Temp,L1,L2
      INTEGER Status,II
C
      IF (NN.EQ.0) THEN
       GenBin = 0
       RETURN
      ENDIF

      IF (NN.LE.MaxN) THEN
C
C       Binomial generator is poor for low N so use array BINCON 
        CUMP = 0.d0
        L1 = log(Lamda)
        L2 = log(1.d0-Lamda)
        DO 2000 II = 0,NN
          YY = II
          Temp = BinCon(NN,II) + YY*L1 + (NN-II)*L2
          CUMP = CUMP + Exp(Temp)
          IF (Cump.GT.Rand) THEN
            GenBin = II
            Return
          ENDIF
2000    CONTINUE
C
      ELSE
        P = Rand
        Q = 1.d0 - P
        Pr = Lamda
        OMPR = 1.d0 - Lamda
        Nall = NN
        CALL CDFBIN(2,p,q,YY,Nall,Pr,Ompr,Status,Bound)
        IF (STatus.EQ.1) THEN
          CALL CDFBIN(2,p,q,YY,Nall,OmPr,pr,Status,Bound)
          YY = Nall - YY
        ENDIF  
c        IF (STatus.NE.0) WRITE(*,*) YY,p,Pr,Nall,Status,Bound
      ENDIF  

      GenBin = YY
C
      RETURN
      END
C
C =====================================================================
C
      SUBROUTINE BinSet()
C
C     BINSET sets log [N! / n! (N-n)! ] for use in generating binomials
C
      COMMON / Bintemp / BinCon,MaxN
      DOUBLE PRECISION BinCon(0:500,0:500)
      INTEGER MaxN
C
C     Local variables
      INTEGER N1,II
      DOUBLE PRECISION NF1,NF2,Nfact
C
      MaxN = 500
      DO 1000 N1 = 1,MaxN
        NFact = 0
        DO 1100 II = 1,N1
         Nfact = NFact + log(DBLE(II))
1100    CONTINUE
C       Nfact = log(N1!)
        NF2 = NFact
        NF1 = 0.D0
        DO 2000 II = 0,N1
          BinCon(N1,II) = Nfact - Nf1 - Nf2
          IF (N1-II-1.LE.0) THEN 
C           N-n =< 1. Gives BinCon=0
            NF2 = 0.d0
            NF1 = NFact
          ELSE
            NF2 = NF2 - log(N1-II+0.d0)
            NF1 = NF1 + log(II+1.d0)
          ENDIF
2000    CONTINUE
C
1000  CONTINUE
C
      RETURN
      END
      
C *********************************************************************

      SUBROUTINE SETRAN(ISEED,MAXAGE,ISTOCH,NYRS)

C     This subroutine generates ALL the random variables needed for
C     the stochastic model.

C     Note: size of RandNo array is to allow for 8500 replicates in
C     calculation of RY.

      COMMON / STOCHM / RandNo,NormNO,LASTQ,Ivar,Qvar,ISTOC2
      DOUBLE PRECISION RandNo(1300000),NormNO(8500),LASTQ
      INTEGER Ivar,Qvar,ISTOC2
C
C     Global variables
      INTEGER ISEED,MAXAGE,ISTOCH,NYRS
C
C     Local variable
      INTEGER INEXT,INEXTP,NVAR,YEAR,AGESEX,INEX2,INEXP2,ISEED2
      DOUBLE PRECISION MA(55),XNORM,R,ran1,MA2(55)
      EXTERNAL RAN1,XNORM
C
      ISTOC2 = ISTOCH
      ISEED2 = -INT(RAN1(ISEED,MA,INEXT,INEXTP)*100000.D0)

      NVAR = 0
      IF (NYRS.GT.8500) STOP 'SETRAN ERROR: increase STOCHM array sizes'
      DO 2000 YEAR = 1,NYRS
       DO 1000 AGESEX = 1,4*(MAXAGE+3)
         NVAR = NVAR + 1
  10     R = RAN1(ISEED2,MA2,INEX2,INEXP2)
         IF (R.LT.0.000000001) GO TO 10
         RANDNO(NVAR) = R
1000   CONTINUE
       NORMNO(YEAR) = XNORM(1.D0,0.D0,ISEED2,MA2,INEX2,INEXP2)
C      Reseed generator each year
       ISEED2 = -INT(RAN1(ISEED,MA,INEXT,INEXTP)*100000.D0)
2000  CONTINUE

      RETURN
      END

C ***********************************************************************

      SUBROUTINE SORT(X,M)

C     Use A QUICK-SORT TO SORT ALL THE DATA
C     (C)opyright A.E. Punt (1987)

      DOUBLE PRECISION X(10000),ST1(10000),MID
      INTEGER M,LEFT(10000),RIGHT(10000),STKLEN,LEFTS,RIGHTS,LS,RS,IC

C     Check for Daft call
      IF (M.LT.2) RETURN

C     Set up initial conditions
      LEFT(1) = 1
      RIGHT(1) = M
      STKLEN = 1

99    IF (STKLEN.EQ.0) GOTO 100

C     Set up the Pointers for this run
      MID = x(LEFT(STKLEN))
      LEFTS = LEFT(STKLEN)
      RIGHTS = RIGHT(STKLEN)
      LS = LEFT(STKLEN)
      RS = RIGHT(STKLEN)

C     Do a one-level sort
      DO 10 IC = LEFT(STKLEN)+1,RIGHT(STKLEN)

C      Check whether the current is less than the middle
       IF (X(IC).GT.MID) THEN
         ST1(RIGHTS) = X(IC)
         RIGHTS = RIGHTS - 1
       ELSE
         ST1(LEFTS) = X(IC)
         LEFTS = LEFTS + 1
       ENDIF
10    CONTINUE

C     Store the middle value
      ST1(LEFTS) = x(LEFT(STKLEN))

C     Replace the data
      DO 11 IC = LEFT(STKLEN),RIGHT(STKLEN)
       x(IC) = ST1(IC)
11    CONTINUE
      STKLEN = STKLEN - 1

C     update right pointer
      IF ((LEFTS-LS).GT.1) THEN
        STKLEN = STKLEN + 1
        LEFT(STKLEN) = LS
        RIGHT(STKLEN) = LEFTS - 1
      ENDIF

C     update left pointer
      IF ((RS-RIGHTS).GT.1) THEN
        STKLEN = STKLEN + 1
        LEFT(STKLEN) = RIGHTS + 1
        RIGHT(STKLEN) = RS
      ENDIF

      GOTO 99
100   CONTINUE

      RETURN
      END
