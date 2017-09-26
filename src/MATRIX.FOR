C **************************************************************************
C *   MATRIX METHODS LIBRARY
C *   ======================
C *
C *
C *************************************************************************

      SUBROUTINE prmat(X,d1,d2,n,m)

      INTEGER d1,d2,n,m,I,J
      DOUBLE PRECISION X(d1,d2)

      Do 10 I = 1,n
       WRITE(*,500) (X(I,J),J=1,m)
500    FORMAT(1x,10(1x,F7.3))
10    CONTINUE

      RETURN
      END

C *************************************************************************

      SUBROUTINE TRANSM(X,XT,dim1,dim2,n,m)

C     Transpose the matrix X

      INTEGER dim1,dim2,n,m,I,J
      DOUBLE PRECISION X(dim1,dim2),XT(dim2,dim1)

      DO 10 I = 1,n
       DO 10 J = 1,m 
        XT(J,I) = X(I,J)
10    CONTINUE

      RETURN
      END

C **************************************************************************

      SUBROUTINE MultM(A,B,C,d1,d2,d3,d4,d5,d6,n,m,p)

C     This subroutine multiple matricies A and B to give C
 
      INTEGER d1,d2,d3,d4,d5,d6,n,m,p,i,j,k
      DOUBLE PRECISION A(d1,d2),B(d3,d4),C(d5,d6)

      DO 10 I = 1,n
       DO 10 J = 1,p
        C(I,J) = 0.0
        DO 10 K = 1,m
         C(I,J) = C(I,J) + A(I,K)*B(K,J)
10    CONTINUE

      RETURN
      END

C **************************************************************************

      SUBROUTINE SOLVEM(X,Y,d1,theta,n)

C     This subroutine attempts to solve a set of linear equations

C     Global variables
      INTEGER d1,n
      DOUBLE PRECISION X(d1,d1),y(d1),theta(d1),XI(1000)

      CALL INVM(X,XI,d1,n)
      CALL MULTM(XI,Y,THETA,d1,d1,d1,1,d1,1,n,n,1)
      
      RETURN
      END

C **************************************************************************

      SUBROUTINE SOLVEL(X,Y,d1,d2,theta,n,m)

C     This subroutine performs a multiple linear regression

C     Global variables
      INTEGER d1,d2,n,m
      DOUBLE PRECISION X(d1,d2),y(d1),theta(d2)

C     Local variables
      DOUBLE PRECISION XT(1000),C(1000),CI(1000)

      CALL TRANSM(X,XT,d1,d2,n,m)
      CALL MULTM(XT,X,C,d2,d1,d1,d2,d2,d2,m,n,m)
      CALL INVM(C,CI,d2,m)
      CALL MULTM(CI,XT,C,d2,d2,d2,d1,d2,d1,m,m,n)
      CALL MULTM(C,Y,THETA,d2,d1,d1,1,d2,1,m,n,1)

      RETURN
      END

C **************************************************************************

      SUBROUTINE INVM(A,AI,dim1,n)

C     This subroutine computes the inverse of a matrix

C     Global variables
      INTEGER dim1,n
      DOUBLE PRECISION A(dim1,dim1),AI(dim1,dim1)

C     Local variables
      DOUBLE PRECISION INDEX(100),D
      INTEGER I,J

C     Set up an identity matrix
      DO 12 I = 1,n
        DO 11 J = 1,n
          AI(I,J) = 0.0
11      CONTINUE
        AI(I,I) = 1.0
12    CONTINUE

C     Perform a LU-decomposition
      CALL LUDCMP(A,n,dim1,INDEX,D)

C     Find the inverse
      DO 13 J = 1,N
        CALL LUBKSB(A,n,dim1,INDEX,AI(1,J))
13    CONTINUE

      RETURN
      END

C **************************************************************************

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)

      INTEGER II,I,LL,J,N,NP
      DOUBLE PRECISION A(NP,NP),INDX(N),B(N),SUM

      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END

C ************************************************************************

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)

      INTEGER N,NP,NMAX,I,J,K,IMAX
      DOUBLE PRECISION TINY

      PARAMETER (NMAX=100,TINY=1.0E-20)

      DOUBLE PRECISION A(NP,NP),INDX(N),VV(NMAX),SUM,AAMAX,DUM,D

      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.) WRITE(*,*) 'Singular matrix.'
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0.)A(N,N)=TINY
      RETURN
      END
