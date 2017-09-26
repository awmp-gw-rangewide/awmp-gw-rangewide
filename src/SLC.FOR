C     Last change:  CA    6 Feb 2003    2:42 pm
C The following code is an outline for developers to modify.
C The only data available is that passed into these routines
C
C ***********************************************************************
C ***********************************************************************
C
C    SLC                                        10 September 1999
C
C ***********************************************************************
C
      SUBROUTINE ISLC
C
C     Initialise the SLC (subsistence limit calculation) if required
C
      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE RSLC (INITYR,ISURV,NSIM)
C
C     Reset the SLC at the start of each new replicate, if required
C
C     All years are measured on a scale such that management begins in year 0
C     INITYR Year of first premanagement catch (-165 to -1)
C     ISURV  Year of first sightings survey (-27 to -1). 
C     NSIM   Simulation number (i.e. replicate number)

      INTEGER INITYR,ISURV,NSIM

      RETURN
      END


C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE SLCG (CATM, CATF, CatMBY, CATFBY, CATMCAN, CATFCAN,
     +                SIGHT, CVX, IYR, INITYR, ISURV,
     +                NEED, CATCHQ, NSIM, IQUOTA, SLAOPT,IYRMAN)

C     Subroutine sets strike quota CATCHQ for GRAY WHALES

C     All years are measured on a scale such that management begins in year 0
C     CATM(I) Catch of males in year I where I = INITYR, IYR-1
C     CATF(I) Catch of females in year I
C     SIGHT(I)Absolute abundance estimate, in year I  eg sightings. 
C             Set to -1 if no data available that year.
C     CVX(I)  Estimated CV of the sightings estimate. Set to -1 if no data.
C     IYR     Current year on scale with 0 = 1st year of management
C     INITYR  Year of first premanagement catch (-450 to 0)
C     ISURV   Year of first sightings survey (-39 to -1).
C     NEED    Current need level (as total no. of whales over next IQUOTA yrs).
C     IQUOTA  Number of years for which the quota is set
C             (annual catch = CATCHQ/IQUOTA)
C     CATCHQ  Total strike limit for IYR to IYR+IQUOTA-1, which is 
C             set by this subroutine
C     NSIM    Simulation number (i.e. replicate number)

      DOUBLE PRECISION CATM(-450:99),CATF(-450:99),SIGHT(-39:99),
     +       CVX(-39:99), NEED,CATCHQ,SLA
      DOUBLE PRECISION CATMBY(-450:99),CATFBY(-450:99),
     +       CATMCAN(-450:99),CATFCAN(-450:99) 
      REAL*8 YrDiff,TERM1,TERM2,TERM3,TERM4,ESTIMATE,CVFINAL
      INTEGER IYR,INITYR,ISURV,NSIM, IQUOTA, SLAOPT, CNT,IYRMAN
C
C     Dump the data (useful for testing)
      IF (SLAOPT.EQ.4.OR.SLAOPT.EQ.2) THEN
       OPEN(UNIT=979,FILE="SLAUSE.DAT")
       WRITE(979,6011) IYRMAN+INITYR,IYRMAN+IYR
       WRITE(979,6012) NEED,IQUOTA
       WRITE(979,6021)
       DO 30100 IY = INITYR,IYR-1
        WRITE(979,6022) IYRMAN+IY,CATM(IY),CATF(IY),CATMBY(IY),
     +      CATFBY(IY),CATMCAN(IY),CATFCAN(IY)
30100  CONTINUE
       CNT = 0      
       DO 30200 IY = -39,IYR-1
        IF (CVX(IY).GT.0) CNT = CNT + 1
30200  CONTINUE       
       WRITE(979,6031) CNT
       DO 30300 IY = -39,IYR-1
        IF (CVX(IY).GT.0) THEN
         WRITE(979,6032) IYRMAN+IY,SIGHT(IY),CVX(IY)
        ENDIF
30300  CONTINUE       
       CLOSE(979) 
C       IF (NEED.GT.100) THEN
C        WRITE(*,*) IY,NEED,IQUOTA
C       ENDIF 
      ENDIF
C
C     Set Strike Limit to Need
      IF (SLAOPT.EQ.1) CATCHQ = NEED
      
C
C     Set Strike Limit to Interm SLA
      IF (SLAOPT.EQ.2) THEN
C
       DO 10000 IY = IYR-1,INITYR,-1
        IF (CVX(IY).GT.0) THEN
         SLA = 0.02*SIGHT(IY)*EXP(-1.645*CVX(IY))*IQUOTA
C        IF (SLA.LE.NEED) WRITE(*,*) IY,CVX(IY),SIGHT(IY),SLA,NEED
         GOTO 10100
        ENDIF
10000  CONTINUE      
10100  CATCHQ = MIN(NEED, SLA) 
C
      ENDIF
C
C     Set Strike Limit to Weight-Average SLA
      IF (SLAOPT.EQ.3) THEN
C
C      Alternative (weighted) method
       TERM1 = 0
       TERM2 = 0
       TERM3 = 0
       TERM4 = 0
C      WRITE(*,*) IYR
       DO 20000 IY = IYR-1,-39,-1
        YrDiff = (IYR-IY)
        IF (CVX(IY).GT.0) THEN
         TERM1 = TERM1 + 0.9**YrDiff*log(SIGHT(IY))/CVX(IY)**2.0
         TERM2 = TERM2 + 0.9**YrDiff/CVX(IY)**2.0
         TERM3 = TERM3 + 0.9**(2*YrDiff)/CVX(IY)**2.0
         TERM4 = TERM4 + 0.9**YrDiff/CVX(IY)**2.0    
C         WRITE(*,*) IY,YrDiff,SIGHT(IY),CVX(IY)
        ENDIF
20000  CONTINUE       
       ESTIMATE = EXP(TERM1/TERM2)
       CVFINAL = SQRT(TERM3)/TERM4
C       WRITE(*,*) ESTIMATE,CVFINAL
       SLA = 0.02*ESTIMATE*EXP(-1.645*CVFINAL)*IQUOTA
       CATCHQ = MIN(NEED, SLA) 
C
      ENDIF

C
C     Set Strike Limit to Zero
      IF (SLAOPT.EQ.5) THEN
       CATCHQ = 0
      ENDIF 
C
C     User specified SLA called "SLAUSE.EXE"
      IF (SLAOPT.EQ.4) THEN
       CALL SYSTEM("SLAUSE.EXE > a.a")
       OPEN(UNIT=979,FILE="SLAUSE.OUT")
       READ(979,*) CATCHQ
       CLOSE(979)
      ENDIF
C
      RETURN
6011  FORMAT("# First and last years"/I4,1x,I4)
6012  FORMAT("# Need and number of years in the block"/F9.4,1x,I6)
6021  FORMAT("# Historical catch data (by sex and type)")
6022  FORMAT(1x,I4,1x,10(F9.2,1x,F9.2,5x))
6031  FORMAT("# Historical abundance data"/"# Number of points"/I4)
6032  FORMAT(1x,I4,1x,F12.5,1x,F12.7)
      END
