*     E04FCF Example Program Text.
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          N, M, NT, LIW, LV, LJ, LW
      PARAMETER        (N=3,M=15,NT=3,LIW=1,LV=N,LJ=M,
     +                 LW=6*N+M*N+2*M+N*(N-1)/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Arrays in Common ..
      DOUBLE PRECISION T(M,NT), Y(M)
*     .. Local Scalars ..
      DOUBLE PRECISION ETA, FSUMSQ, STEPMX, XTOL
      INTEGER          I, IFAIL, IPRINT, J, MAXCAL, NF, NITER
*     .. Local Arrays ..
      DOUBLE PRECISION FJAC(M,N), FVEC(M), G(N), S(N), V(LV,N), W(LW),
     +                 X(N)
      INTEGER          IW(LIW)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         E04FCF, LSQFUN, LSQGRD, LSQMON
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Common blocks ..
      COMMON           Y, T
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04FCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Observations of TJ (J = 1, 2, 3) are held in T(I, J)
*     (I = 1, 2, . . . , 15)
      DO 20 I = 1, M
         READ (NIN,*) Y(I), (T(I,J),J=1,NT)
   20 CONTINUE
*     * Set IPRINT to 1 to obtain output from LSQMON at each iteration *
      IPRINT = -1
      MAXCAL = 400*N
      ETA = 0.5D0
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
      CALL E04FCF(M,N,LSQFUN,LSQMON,IPRINT,MAXCAL,ETA,XTOL,STEPMX,X,
     +            FSUMSQ,FVEC,FJAC,LJ,S,V,LV,NITER,NF,IW,LIW,W,LW,IFAIL)
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
         WRITE (NOUT,99997) 'The estimated gradient is',
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
      SUBROUTINE LSQFUN(IFLAG,M,N,XC,FVECC,IW,LIW,W,LW)
*     Routine to evaluate the residuals
*     .. Parameters ..
      INTEGER           MDEC, NT
      PARAMETER         (MDEC=15,NT=3)
*     .. Scalar Arguments ..
      INTEGER           IFLAG, LIW, LW, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION  FVECC(M), W(LW), XC(N)
      INTEGER           IW(LIW)
*     .. Arrays in Common ..
      DOUBLE PRECISION  T(MDEC,NT), Y(MDEC)
*     .. Local Scalars ..
      INTEGER           I
*     .. Common blocks ..
      COMMON            Y, T
*     .. Executable Statements ..
      DO 20 I = 1, M
         FVECC(I) = XC(1) + T(I,1)/(XC(2)*T(I,2)+XC(3)*T(I,3)) - Y(I)
   20 CONTINUE
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
     +  '  Itn      F evals        SUMSQ             GTG        Grade'
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
