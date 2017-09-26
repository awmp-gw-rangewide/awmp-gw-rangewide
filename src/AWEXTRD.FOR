C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      FUNCTION RAN1 (ISEED,MA,INEXT,INEXTP)

C     Copyright Numerical Recipes Software
C     Reference: Press, Flannery, Teukolsky & Vetterling: Numerical Recipes,
C     CUP, Cambridge 1986 (Page 199).
C
C     Function returns a uniform random deviate between 0.0 & 1.0.
C     Set ISEED to any negative value to reinitialize the sequence.
C     Note: ISEED MUST be -ve to initialise routine. It is reset to 1 on exit.
C
      DOUBLE PRECISION MA(55),MJ,MK,MBIG,MSEED,MZ,FAC,RAN1
      INTEGER ISEED,INEXT,INEXTP,I,K,II
      PARAMETER (MBIG=4000000.D0,MSEED=1618033.D0,MZ=0.D0,FAC=1.D0/MBIG)

      IF (ISEED.LT.0.D0) THEN
        MJ = MSEED-IABS(ISEED)
        MJ = MOD(MJ,MBIG)
        MA(55) = MJ
        MK = 1
        DO 11 I = 1,54
          II = MOD(21*I,55)
          MA(II) = MK
          MK = MJ-MK
          IF (MK.LT.MZ) MK = MK+MBIG
          MJ = MA(II)
   11   CONTINUE
        DO 12 K = 1,4
        DO 12 I = 1,55
          MA(I) = MA(I)-MA(1+MOD(I+30,55))
          IF (MA(I).LT.MZ) MA(I) = MA(I)+MBIG
   12   CONTINUE
        INEXT = 0
        INEXTP = 31
        ISEED = 1
      ENDIF
      INEXT = INEXT+1
      IF (INEXT.EQ.56) INEXT = 1
      INEXTP = INEXTP+1
      IF (INEXTP.EQ.56) INEXTP = 1
      MJ = MA(INEXT)-MA(INEXTP)
      IF (MJ.LT.MZ) MJ = MJ+MBIG
      MA(INEXT) = MJ
      RAN1 = MJ*FAC
      END


C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      FUNCTION XNORM (SIGG,MEAN,ISEED,MA,INEXT,INEXTP)

C     Uniform to Normal conversion routine

      DOUBLE PRECISION XNORM, Z1, Z2, MEAN, SIGG, RAN1, MA(55)
      INTEGER ISEED, INEXT,INEXTP
      EXTERNAL RAN1

      Z1 = RAN1(ISEED,MA,INEXT,INEXTP)
    1 Z2 = RAN1(ISEED,MA,INEXT,INEXTP)
      IF (Z2.LE.0.D0) GO TO 1

      XNORM = SIN(6.238319D0*Z1) * SQRT(-2.D0*LOG(Z2)) * SIGG+MEAN

      RETURN
      END

C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      FUNCTION POISSN (XM,RANNO)

      DOUBLE PRECISION F,SS,R,XM,RANNO,POISSN
      INTEGER I

      R = RANNO
      F = EXP(-XM)
      SS = F
      I = 0
   99 IF (SS.GE.R) GOTO 100
      I = I + 1
      F = F*XM/DBLE(I)
      SS = SS + F
      GOTO 99
  100 POISSN = DBLE(I)

      END

C --------------------------------------------------------------------------
C --------------------------------------------------------------------------
C
      FUNCTION GAMMAZ (PARX,PARS,ISEED)
C
C     Approximate random gamma generator: PARX = mean; PARS= sd
C
      EXTERNAL RAN1
      DOUBLE PRECISION PARX,PARS,X,S,RAN1,MA(55),P,F,FN,GAMMAZ
      INTEGER ISEED,INEXT,INEXTP,I
C
      X = PARX
      S = PARS
C
C     Convert to gamma parameters
      s = s*s / x
C
      P = 0.D0
      IF (S.GT.0.D0) THEN
        X = X/S
        I = INT(X)
        FN = AINT(X)
        IF (I.GT.0) THEN
C
C         Count down from i to zero
100       IF (I.NE.0) THEN
            F = RAN1(ISEED,MA,INEXT,INEXTP)
            IF (F.LE.0.D0) THEN
              WRITE(*,*) 'WARNING F=0 in GAMMAZ',F
              F = 0.00001D0
            ENDIF
            P = P + LOG(F)
            I = I - 1
            GOTO 100
          ENDIF
        ENDIF
C
C       Usually this is the case (unless x is an integer)
        IF (X.GT.FN) THEN
          F = RAN1(ISEED,MA,INEXT,INEXTP)
          IF (F.LE.0.D0) THEN
            WRITE(*,*) 'WARNING F=0 in GAMMAZ',F
            F = 0.00001D0
          ENDIF
          GAMMAZ = -S*( (X-FN)*LOG(F) + P )
        ELSE
          GAMMAZ = -S*P
        ENDIF

      ELSE
C
        GAMMAZ = X
C
      ENDIF
C
      RETURN
      END
C
C--------------------------------------------------------------------------
C
      FUNCTION GENBET(AA,BB,ISEED)

C                              LEGALITIES
C
C Code that appeared in an ACM publication is subject to their
C algorithms policy:
C
C     Submittal of an algorithm for publication in one of the ACM
C     Transactions implies that unrestricted use of the algorithm within a
C     computer is permissible . General permission to copy and distribute
C     the algorithm without fee is granted provided that the copies are not
C     made or distributed for direct commercial advantage.  The ACM
C     copyright notice and the title of the publication and its date appear,
C     and notice is given that copying is by permission of the Association
C     for Computing Machinery. To copy otherwise, or to republish, requires
C     a fee and/or specific permission.
C
C     Krogh, F. Algorithms Policy. ACM Tran. Math. Softw. 13(1987), 183-186.
C
C We place the Ranlib code that we have written in the public domain.  
C
C
C                                 NO WARRANTY
C     
C     WE PROVIDE ABSOLUTELY  NO WARRANTY  OF ANY  KIND  EITHER  EXPRESSED OR
C     IMPLIED,  INCLUDING BUT   NOT LIMITED TO,  THE  IMPLIED  WARRANTIES OF
C     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK
C     AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS  WITH YOU.  SHOULD
C     THIS PROGRAM PROVE  DEFECTIVE, YOU ASSUME  THE COST  OF  ALL NECESSARY
C     SERVICING, REPAIR OR CORRECTION.
C     
C     IN NO  EVENT  SHALL THE UNIVERSITY  OF TEXAS OR  ANY  OF ITS COMPONENT
C     INSTITUTIONS INCLUDING M. D.   ANDERSON HOSPITAL BE LIABLE  TO YOU FOR
C     DAMAGES, INCLUDING ANY  LOST PROFITS, LOST MONIES,   OR OTHER SPECIAL,
C     INCIDENTAL   OR  CONSEQUENTIAL DAMAGES   ARISING   OUT  OF  THE USE OR
C     INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA OR
C     ITS ANALYSIS BEING  RENDERED INACCURATE OR  LOSSES SUSTAINED  BY THIRD
C     PARTIES) THE PROGRAM.
C     
C**********************************************************************
C
C     REAL FUNCTION GENBET( A, B )
C               GeNerate BETa random deviate
C
C                              Function
C
C     Returns a single random deviate from the beta distribution with
C     parameters A and B.  The density of the beta is
C               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1
C
C                              Method
C     R. C. H. Cheng
C     Generating Beta Variatew with Nonintegral Shape Parameters
C     Communications of the ACM, 21:317-322  (1978)
C     (Algorithms BB and BC)
C
C**********************************************************************
C     .. Parameters ..
C     EXPMAX  Close to the largest number that can be exponentiated
C     INFNTY  Close to the largest representable single precision number
      DOUBLE PRECISION GENBET, EXPMAX, INFNTY
      PARAMETER (EXPMAX=89.0D0, INFNTY=1.0E38)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION AA,BB
      INTEGER ISEED
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,ALPHA,B,BETA,DELTA,GAMMA,K1,K2,OLDA,OLDB,R,S,T,
     +       U1,U2,V,W,Y,Z
      LOGICAL QSAME
C     ..
C     .. External & Intrinsic Functions ..
      DOUBLE PRECISION RAN1, MA(55)
      EXTERNAL RAN1
      INTEGER INEXT,INEXTP
      INTRINSIC EXP,LOG,MAX,MIN,SQRT
C     ..
C     .. Save statement ..
      SAVE OLDA,OLDB,ALPHA,BETA,GAMMA,K1,K2
C     ..
C     .. Data statements ..
      DATA OLDA,OLDB/-1,-1/
C     ..
C     .. Executable Statements ..
      QSAME = (OLDA.EQ.AA) .AND. (OLDB.EQ.BB)
      IF (QSAME) GO TO 20
      IF (.NOT. (AA.LE.0.0.OR.BB.LE.0.0)) GO TO 10
      WRITE (*,*) ' AA OR BB <= 0 IN GENBET - ABORT!'
      WRITE (*,*) ' AA: ',AA,' BB ',BB
      STOP ' AA OR BB <= 0 IN GENBET - ABORT!'

   10 OLDA = AA
      OLDB = BB
   20 IF (.NOT. (MIN(AA,BB).GT.1.0)) GO TO 100


C     Alborithm BB

C     Initialize
C
      IF (QSAME) GO TO 30
      A = MIN(AA,BB)
      B = MAX(AA,BB)
      ALPHA = A + B
      BETA = SQRT((ALPHA-2.0)/ (2.0*A*B-ALPHA))
      GAMMA = A + 1.0/BETA
   30 CONTINUE
   40 U1 = RAN1(ISEED,MA,INEXT,INEXTP)
C
C     Step 1
C
      U2 = RAN1(ISEED,MA,INEXT,INEXTP)
      V = BETA*LOG(U1/ (1.0-U1))
      IF (.NOT. (V.GT.EXPMAX)) GO TO 50
      W = INFNTY
      GO TO 60

   50 W = A*EXP(V)
   60 Z = U1**2*U2
      R = GAMMA*V - 1.3862944
      S = A + R - W
C
C     Step 2
C
      IF ((S+2.609438).GE. (5.0*Z)) GO TO 70
C
C     Step 3
C
      T = LOG(Z)
      IF (S.GT.T) GO TO 70
C
C     Step 4
C
      IF ((R+ALPHA*LOG(ALPHA/ (B+W))).LT.T) GO TO 40
C
C     Step 5
C
   70 IF (.NOT. (AA.EQ.A)) GO TO 80
      GENBET = W/ (B+W)
      GO TO 90

   80 GENBET = B/ (B+W)
   90 GO TO 230


C     Algorithm BC
C
C     Initialize
C
  100 IF (QSAME) GO TO 110
      A = MAX(AA,BB)
      B = MIN(AA,BB)
      ALPHA = A + B
      BETA = 1.0/B
      DELTA = 1.0 + A - B
      K1 = DELTA* (0.0138889+0.0416667*B)/ (A*BETA-0.777778)
      K2 = 0.25 + (0.5+0.25/DELTA)*B
  110 CONTINUE
  120 U1 = RAN1(ISEED,MA,INEXT,INEXTP)
C
C     Step 1
C
      U2 = RAN1(ISEED,MA,INEXT,INEXTP)
      IF (U1.GE.0.5) GO TO 130
C
C     Step 2
C
      Y = U1*U2
      Z = U1*Y
      IF ((0.25*U2+Z-Y).GE.K1) GO TO 120
      GO TO 170
C
C     Step 3
C
  130 Z = U1**2*U2
      IF (.NOT. (Z.LE.0.25)) GO TO 160
      V = BETA*LOG(U1/ (1.0-U1))
      IF (.NOT. (V.GT.EXPMAX)) GO TO 140
      W = INFNTY
      GO TO 150

  140 W = A*EXP(V)
  150 GO TO 200

  160 IF (Z.GE.K2) GO TO 120
C
C     Step 4
C
C
C     Step 5
C
  170 V = BETA*LOG(U1/ (1.0-U1))
      IF (.NOT. (V.GT.EXPMAX)) GO TO 180
      W = INFNTY
      GO TO 190

  180 W = A*EXP(V)
  190 IF ((ALPHA* (LOG(ALPHA/ (B+W))+V)-1.3862944).LT.LOG(Z)) GO TO 120
C
C     Step 6
C
  200 IF (.NOT. (A.EQ.AA)) GO TO 210
      GENBET = W/ (B+W)
      GO TO 220

  210 GENBET = B/ (B+W)
  220 CONTINUE
  230 RETURN

      END
      
C -----------------------------------------------------------------------------

      SUBROUTINE ZBRAC(FUNC,PARS,M,X1,X2,F1,F2,SUCCES)
C
C  This subroutine is based on the Numerical Recipes Software Library
C  (Refer, W.H.Press, B.P.Flannery, S.A.Teukolsky & W.T.Vetterling 1986.  
C  Numerical Recipes: The Art of Scientific Computing.  CUP, Cambridge. 818pp.
C
C  Given a function FUNC and an initial guessed range X1 to X2, the
C  routine expands the range geometrically until a root is bracketted
C  by the return values X1 and X2 (in which case SUCCES returns as
C  .TRUE.) or until the range becomes unacceptably large (in which
C  case SUCCES returns as .FALSE.).  The parameters to FUNC are passed
C  through the vector PARS.  X1 and X2 are passed to FUNC through
C  PARS(M).  The corresponding function values F1 and F2 are returned.
C  Success is guaranteed for a function which has opposite sign for
C  sufficiently large and small arguments.
C
      EXTERNAL FUNC
      DOUBLE PRECISION FUNC,PARS(4),FACTOR,X1,X2,F1,F2
      INTEGER NTRY,M,IERR,J
      LOGICAL SUCCES
      PARAMETER (FACTOR=1.6D0,NTRY=50)
C
      IF(X1.EQ.X2)PAUSE 'You have to guess an initial range'
      PARS(M) = X1
      F1 = FUNC(PARS,IERR)
      IF (IERR .NE. 0) THEN
        SUCCES = .FALSE.
        RETURN
      ENDIF
      PARS(M) = X2
      F2 = FUNC(PARS,IERR)
      IF (IERR .NE. 0) THEN
        SUCCES = .FALSE.
        RETURN
      ENDIF
      SUCCES=.TRUE.
      DO 11 J=1,NTRY
        IF(F1*F2.LT.0.D0)RETURN
        IF(ABS(F1).LT.ABS(F2))THEN
          X1=X1+FACTOR*(X1-X2)
          PARS(M) = X1
          F1 = FUNC(PARS,IERR)
          IF (IERR .NE. 0) THEN
            SUCCES = .FALSE.
            RETURN
          ENDIF
        ELSE
          X2=X2+FACTOR*(X2-X1)
          PARS(M) = X2
          F2 = FUNC(PARS,IERR)
          IF (IERR .NE. 0) THEN
            SUCCES = .FALSE.
            RETURN
          ENDIF
        ENDIF
11    CONTINUE
      SUCCES=.FALSE.
      RETURN
      END
C
C  ***********************************************************************
C
      SUBROUTINE ZBRENT(FUNC,PARS,M,X1,X2,FX1,FX2,TOL,IERR)

C  This subroutine is based on the Numerical Recipes Software Library
C  (Refer, W.H.Press, B.P.Flannery, S.A.Teukolsky & W.T.Vetterling 1986.  
C  Numerical Recipes: The Art of Scientific Computing.  CUP, Cambridge. 818pp.
C
C  Using Brent's method, find the root of a function FUNC known to
C  lie between X1 and X2 (with function values FX1 and FX2).  The
C  parameters to FUNC are passed through the vector PARS.  The root
C  is returned through PARS(M)
C
      EXTERNAL FUNC
      DOUBLE PRECISION FUNC,PARS(4),X1,X2,FX1,FX2,TOL,EPS,A,FA,B,FB,C,
     +       D,E,TOL1,XM,S,P,Q,R,FC
      INTEGER M,ITMAX,ITER,IERR
      PARAMETER (ITMAX=100,EPS=3.E-5)
C
      A=X1
      FA = FX1
      B=X2
      FB = FX2
C
      IF(FB*FA.GT.0.D0) PAUSE 'Root must be bracketed for ZBRENT.'
      FC=FB
      DO 11 ITER=1,ITMAX
C
        IF(FB*FC.GT.0.D0) THEN
C  Rename A, B, C and adjust bounding interval D
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF
C
C  Check for convergence
        IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        TOL1=2.D0*EPS*ABS(B)+0.5D0*TOL
        XM=.5D0*(C-B)
        IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.D0)THEN
          PARS(M)=B
          RETURN
        ENDIF
        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
C  Attempt inverse quadratic interpolation
          S=FB/FA
          IF(A.EQ.C) THEN
            P=2.D0*XM*S
            Q=1.D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.D0*XM*Q*(Q-R)-(B-A)*(R-1.D0))
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)
          ENDIF
C
C  Check whether in bounds
          IF(P.GT.0.D0) Q=-Q
          P=ABS(P)
          IF(2.D0*P .LT. MIN(3.D0*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
C  Accept interpolation
            E=D
            D=P/Q
          ELSE
C  Interpolation failed, use bisection
            D=XM
            E=D
          ENDIF
        ELSE
C  Bounds decreasing too slowly, use bisection
          D=XM
          E=D
        ENDIF
C  Save latest best guess
        A=B
        FA=FB
        IF(ABS(D) .GT. TOL1) THEN
          B=B+D
        ELSE
          B=B+SIGN(TOL1,XM)
        ENDIF
C  Evaluate new trial root
        PARS(M) = B
        FB = FUNC(PARS,IERR)
        IF (IERR .NE. 0) RETURN
11    CONTINUE
C
      PAUSE 'ZBRENT exceeding maximum iterations.'
      PARS(M)=B
      RETURN
      END

C -----------------------------------------------------------------------------

      SUBROUTINE ZBRAC2(FUNC,PARS,M,X1,X2,F1,F2,SUCCES)
C
C  This subroutine is based on the Numerical Recipes Software Library
C  (Refer, W.H.Press, B.P.Flannery, S.A.Teukolsky & W.T.Vetterling 1986.  
C  Numerical Recipes: The Art of Scientific Computing.  CUP, Cambridge. 818pp.
C
C  Given a function FUNC and an initial guessed range X1 to X2, the
C  routine expands the range geometrically until a root is bracketted
C  by the return values X1 and X2 (in which case SUCCES returns as
C  .TRUE.) or until the range becomes unacceptably large (in which
C  case SUCCES returns as .FALSE.).  The parameters to FUNC are passed
C  through the vector PARS.  X1 and X2 are passed to FUNC through
C  PARS(M).  The corresponding function values F1 and F2 are returned.
C  Success is guaranteed for a function which has opposite sign for
C  sufficiently large and small arguments.
C
      EXTERNAL FUNC
      DOUBLE PRECISION FUNC,PARS(4),FACTOR,X1,X2,F1,F2
      INTEGER NTRY,M,IERR,J
      LOGICAL SUCCES
      PARAMETER (FACTOR=1.6D0,NTRY=50)
C
      IF(X1.EQ.X2)PAUSE 'You have to guess an initial range'
      PARS(M) = X1
      F1 = FUNC(PARS,IERR)
      IF (IERR .NE. 0) THEN
        SUCCES = .FALSE.
        RETURN
      ENDIF
      PARS(M) = X2
      F2 = FUNC(PARS,IERR)
      IF (IERR .NE. 0) THEN
        SUCCES = .FALSE.
        RETURN
      ENDIF
      SUCCES=.TRUE.
      DO 11 J=1,NTRY
        IF(F1*F2.LT.0.D0)RETURN
        IF(ABS(F1).LT.ABS(F2))THEN
          X1=X1+FACTOR*(X1-X2)
          PARS(M) = X1
          F1 = FUNC(PARS,IERR)
          IF (IERR .NE. 0) THEN
            SUCCES = .FALSE.
            RETURN
          ENDIF
        ELSE
          X2=X2+FACTOR*(X2-X1)
          PARS(M) = X2
          F2 = FUNC(PARS,IERR)
          IF (IERR .NE. 0) THEN
            SUCCES = .FALSE.
            RETURN
          ENDIF
        ENDIF
11    CONTINUE
      SUCCES=.FALSE.
      RETURN
      END
C
C  ***********************************************************************
C
      SUBROUTINE ZBRNT2(FUNC,PARS,M,X1,X2,FX1,FX2,TOL,IERR)

C  This subroutine is based on the Numerical Recipes Software Library
C  (Refer, W.H.Press, B.P.Flannery, S.A.Teukolsky & W.T.Vetterling 1986.  
C  Numerical Recipes: The Art of Scientific Computing.  CUP, Cambridge. 818pp.
C
C  Using Brent's method, find the root of a function FUNC known to
C  lie between X1 and X2 (with function values FX1 and FX2).  The
C  parameters to FUNC are passed through the vector PARS.  The root
C  is returned through PARS(M)
C
      EXTERNAL FUNC
      DOUBLE PRECISION FUNC,PARS(4),X1,X2,FX1,FX2,TOL,EPS,A,FA,B,FB,C,
     +       D,E,TOL1,XM,S,P,Q,R,FC
      INTEGER M,ITMAX,ITER,IERR
      PARAMETER (ITMAX=200,EPS=3.E-10)
C
      A=X1
      FA = FX1
      B=X2
      FB = FX2
C
      IF(FB*FA.GT.0.D0) PAUSE 'Root must be bracketed for ZBRNT2'
      FC=FB
      DO 11 ITER=1,ITMAX
C
        IF(FB*FC.GT.0.D0) THEN
C  Rename A, B, C and adjust bounding interval D
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF
C
C  Check for convergence
        IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        TOL1=2.D0*EPS*ABS(B)+0.5D0*TOL
        XM=.5D0*(C-B)
        IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.D0)THEN
          PARS(M)=B
          RETURN
        ENDIF
        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
C  Attempt inverse quadratic interpolation
          S=FB/FA
          IF(A.EQ.C) THEN
            P=2.D0*XM*S
            Q=1.D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.D0*XM*Q*(Q-R)-(B-A)*(R-1.D0))
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)
          ENDIF
C
C  Check whether in bounds
          IF(P.GT.0.D0) Q=-Q
          P=ABS(P)
          IF(2.D0*P .LT. MIN(3.D0*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
C  Accept interpolation
            E=D
            D=P/Q
          ELSE
C  Interpolation failed, use bisection
            D=XM
            E=D
          ENDIF
        ELSE
C  Bounds decreasing too slowly, use bisection
          D=XM
          E=D
        ENDIF
C  Save latest best guess
        A=B
        FA=FB
        IF(ABS(D) .GT. TOL1) THEN
          B=B+D
        ELSE
          B=B+SIGN(TOL1,XM)
        ENDIF
C  Evaluate new trial root
        PARS(M) = B
        FB = FUNC(PARS,IERR)
        IF (IERR .NE. 0) RETURN
11    CONTINUE
C
      PAUSE 'ZBRNT2 exceeding maximum iterations.'
      PARS(M)=B
      RETURN
      END
