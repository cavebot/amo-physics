*     E04HEF Example Program Text.
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          N, M, NT, LJ, LV, LB, LIW, LW
      PARAMETER        (N=3,M=15,NT=3,LJ=M,LV=N,LB=N*(N+1)/2,LIW=1,
     +                 LW=7*N+M*N+2*M+N*N)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Arrays in Common ..
      DOUBLE PRECISION T(M,NT), Y(M)
*     .. Local Scalars ..
      DOUBLE PRECISION ETA, FSUMSQ, STEPMX, XTOL
      INTEGER          I, IFAIL, IPRINT, J, MAXCAL, NF, NITER
*     .. Local Arrays ..
      DOUBLE PRECISION B(LB), FJAC(LJ,N), FVEC(M), G(N), S(N), V(LV,N),
     +                 W(LW), X(N)
      INTEGER          IW(LIW)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         E04HEF, E04YAF, E04YBF, LSQFUN, LSQGRD, LSQHES,
     +                 LSQMON
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Common blocks ..
      COMMON           Y, T
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04HEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Observations of TJ (J = 1, 2, 3) are held in T(I, J)
*     (I = 1, 2, . . . , 15)
      DO 20 I = 1, M
         READ (NIN,*) Y(I), (T(I,J),J=1,NT)
   20 CONTINUE
*     Set up an arbitrary point at which to check the derivatives
      X(1) = 0.19D0
      X(2) = -1.34D0
      X(3) = 0.88D0
*     Check the 1st derivatives
      IFAIL = 0
*
      CALL E04YAF(M,N,LSQFUN,X,FVEC,FJAC,LJ,IW,LIW,W,LW,IFAIL)
*
*     Check the evaluation of B
      IFAIL = 0
*
      CALL E04YBF(M,N,LSQFUN,LSQHES,X,FVEC,FJAC,LJ,B,LB,IW,LIW,W,LW,
     +            IFAIL)
*
*     Continue setting parameters for E04HEF
*     * Set IPRINT to 1 to obtain output from LSQMON at each iteration *
      IPRINT = -1
      MAXCAL = 50*N
      ETA = 0.9D0
      XTOL = 10.0D0*SQRT(X02AJF())
*     We estimate that the minimum will be within 10 units of the
*     starting point
      STEPMX = 10.0D0
*     Set up the starting point
      X(1) = 0.5D0
      X(2) = 1.0D0
      X(3) = 1.5D0
      IFAIL = 1
*
      CALL E04HEF(M,N,LSQFUN,LSQHES,LSQMON,IPRINT,MAXCAL,ETA,XTOL,
     +            STEPMX,X,FSUMSQ,FVEC,FJAC,LJ,S,V,LV,NITER,NF,IW,LIW,W,
     +            LW,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Error exit type', IFAIL,
     +     ' - see routine document'
      END IF
      IF (IFAIL.NE.1) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'On exit, the sum of squares is', FSUMSQ
         WRITE (NOUT,99998) 'at the point', (X(J),J=1,N)
         CALL LSQGRD(M,N,FVEC,FJAC,LJ,G)
         WRITE (NOUT,99997) 'The corresponding gradient is',
     +     (G(J),J=1,N)
         WRITE (NOUT,*)
     +     '                           (machine dependent)'
         WRITE (NOUT,*) 'and the residuals are'
         WRITE (NOUT,99996) (FVEC(I),I=1,M)
      END IF
      STOP
*
99999 FORMAT (1X,A,I3,A)
99998 FORMAT (1X,A,3F12.4)
99997 FORMAT (1X,A,1P,3D12.3)
99996 FORMAT (1X,1P,D9.1)
      END
*
      SUBROUTINE LSQFUN(IFLAG,M,N,XC,FVECC,FJACC,LJC,IW,LIW,W,LW)
*     Routine to evaluate the residuals and their 1st derivatives
*     .. Parameters ..
      INTEGER           NT, MDEC
      PARAMETER         (NT=3,MDEC=15)
*     .. Scalar Arguments ..
      INTEGER           IFLAG, LIW, LJC, LW, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  FJACC(LJC,N), FVECC(M), W(LW), XC(N)
      INTEGER           IW(LIW)
*     .. Arrays in Common ..
      DOUBLE PRECISION  T(MDEC,NT), Y(MDEC)
*     .. Local Scalars ..
      DOUBLE PRECISION  DENOM, DUMMY
      INTEGER           I
*     .. Common blocks ..
      COMMON            Y, T
*     .. Executable Statements ..
      DO 20 I = 1, M
         DENOM = XC(2)*T(I,2) + XC(3)*T(I,3)
         FVECC(I) = XC(1) + T(I,1)/DENOM - Y(I)
         FJACC(I,1) = 1.0D0
         DUMMY = -1.0D0/(DENOM*DENOM)
         FJACC(I,2) = T(I,1)*T(I,2)*DUMMY
         FJACC(I,3) = T(I,1)*T(I,3)*DUMMY
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE LSQHES(IFLAG,M,N,FVECC,XC,B,LB,IW,LIW,W,LW)
*     Routine to compute the lower triangle of the matrix B
*     (stored by rows in the array B)
*     .. Parameters ..
      INTEGER           NT, MDEC
      PARAMETER         (NT=3,MDEC=15)
*     .. Scalar Arguments ..
      INTEGER           IFLAG, LB, LIW, LW, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  B(LB), FVECC(M), W(LW), XC(N)
      INTEGER           IW(LIW)
*     .. Arrays in Common ..
      DOUBLE PRECISION  T(MDEC,NT), Y(MDEC)
*     .. Local Scalars ..
      DOUBLE PRECISION  DUMMY, SUM22, SUM32, SUM33
      INTEGER           I
*     .. Common blocks ..
      COMMON            Y, T
*     .. Executable Statements ..
      B(1) = 0.0D0
      B(2) = 0.0D0
      SUM22 = 0.0D0
      SUM32 = 0.0D0
      SUM33 = 0.0D0
      DO 20 I = 1, M
         DUMMY = 2.0D0*T(I,1)/(XC(2)*T(I,2)+XC(3)*T(I,3))**3
         SUM22 = SUM22 + FVECC(I)*DUMMY*T(I,2)**2
         SUM32 = SUM32 + FVECC(I)*DUMMY*T(I,2)*T(I,3)
         SUM33 = SUM33 + FVECC(I)*DUMMY*T(I,3)**2
   20 CONTINUE
      B(3) = SUM22
      B(4) = 0.0D0
      B(5) = SUM32
      B(6) = SUM33
      RETURN
      END
*
      SUBROUTINE LSQMON(M,N,XC,FVECC,FJACC,LJC,S,IGRADE,NITER,NF,IW,LIW,
     +                  W,LW)
*     Monitoring routine
*     .. Parameters ..
      INTEGER           NDEC
      PARAMETER         (NDEC=3)
      INTEGER           NOUT
      PARAMETER         (NOUT=6)
*     .. Scalar Arguments ..
      INTEGER           IGRADE, LIW, LJC, LW, M, N, NF, NITER
*     .. Array Arguments ..
      DOUBLE PRECISION  FJACC(LJC,N), FVECC(M), S(N), W(LW), XC(N)
      INTEGER           IW(LIW)
*     .. Local Scalars ..
      DOUBLE PRECISION  FSUMSQ, GTG
      INTEGER           J
*     .. Local Arrays ..
      DOUBLE PRECISION  G(NDEC)
*     .. External Functions ..
      DOUBLE PRECISION  F06EAF
      EXTERNAL          F06EAF
*     .. External Subroutines ..
      EXTERNAL          LSQGRD
*     .. Executable Statements ..
      FSUMSQ = F06EAF(M,FVECC,1,FVECC,1)
      CALL LSQGRD(M,N,FVECC,FJACC,LJC,G)
      GTG = F06EAF(N,G,1,G,1)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  ' Itns    F evals          SUMSQ             GTG        grade'
      WRITE (NOUT,99999) NITER, NF, FSUMSQ, GTG, IGRADE
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +  '       X                    G           Singular values'
      DO 20 J = 1, N
         WRITE (NOUT,99998) XC(J), G(J), S(J)
   20 CONTINUE
      RETURN
*
99999 FORMAT (1X,I4,6X,I5,6X,1P,D13.5,6X,1P,D9.1,6X,I3)
99998 FORMAT (1X,1P,D13.5,10X,1P,D9.1,10X,1P,D9.1)
      END
*
      SUBROUTINE LSQGRD(M,N,FVECC,FJACC,LJC,G)
*     Routine to evaluate gradient of the sum of squares
*     .. Scalar Arguments ..
      INTEGER           LJC, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  FJACC(LJC,N), FVECC(M), G(N)
*     .. Local Scalars ..
      DOUBLE PRECISION  SUM
      INTEGER           I, J
*     .. Executable Statements ..
      DO 40 J = 1, N
         SUM = 0.0D0
         DO 20 I = 1, M
            SUM = SUM + FJACC(I,J)*FVECC(I)
   20    CONTINUE
         G(J) = SUM + SUM
   40 CONTINUE
      RETURN
      END
