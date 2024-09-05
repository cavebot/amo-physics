*     G02HDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=9,MMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION BETA, EPS, PSIP0, SIGMA, TOL
      INTEGER          I, IFAIL, INDW, ISIGMA, IX, J, K, M, MAXIT, N,
     +                 NIT, NITMON
*     .. Local Arrays ..
      DOUBLE PRECISION RS(NMAX), THETA(MMAX), WGT(NMAX),
     +                 WK(NMAX*(MMAX+4)), X(NMAX,MMAX), Y(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION CHI, PSI
      EXTERNAL         CHI, PSI
*     .. External Subroutines ..
      EXTERNAL         BETCAL, G02HDF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02HDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      CALL X04ABF(1,NOUT)
*     Read in the dimensions of X
      READ (NIN,*) N, M
      IF ((N.LE.NMAX) .AND. (M.LE.MMAX)) THEN
*        Read in the X matrix, the Y values and set X(i,1) to 1 for the
*        constant term
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=2,M), Y(I)
            X(I,1) = 1.0D0
   20    CONTINUE
*        Read in weights
         DO 40 I = 1, N
            READ (NIN,*) WGT(I)
   40    CONTINUE
         CALL BETCAL(N,WGT,BETA)
*        Set other parameter values
         IX = NMAX
         MAXIT = 50
         TOL = 0.5D-4
         EPS = 0.5D-5
         PSIP0 = 1.0D0
*        Set value of ISIGMA and initial value of SIGMA
         ISIGMA = 1
         SIGMA = 1.0D0
*        Set initial value of THETA
         DO 60 J = 1, M
            THETA(J) = 0.0D0
   60    CONTINUE
*        * Change NITMON to a positive value if monitoring information
*          is required *
         NITMON = 0
*        Schweppe type regression
         INDW = 1
         IFAIL = -1
*
         CALL G02HDF(CHI,PSI,PSIP0,BETA,INDW,ISIGMA,N,M,X,IX,Y,WGT,
     +               THETA,K,SIGMA,RS,TOL,EPS,MAXIT,NITMON,NIT,WK,IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0 .AND. IFAIL.NE.7) THEN
            WRITE (NOUT,99999) 'G02HDF fails, IFAIL = ', IFAIL
         ELSE
            IF (IFAIL.EQ.7) THEN
               WRITE (NOUT,99999) 'G02HDF returned IFAIL = ', IFAIL
               WRITE (NOUT,*)
     +           'Some of the following results may be unreliable'
            END IF
            WRITE (NOUT,99998) 'G02HDF required ', NIT,
     +        ' iterations to converge'
            WRITE (NOUT,99998) '                   K = ', K
            WRITE (NOUT,99997) '               Sigma = ', SIGMA
            WRITE (NOUT,*) '    THETA'
            DO 80 J = 1, M
               WRITE (NOUT,99996) THETA(J)
   80       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) '  Weights  Residuals'
            DO 100 I = 1, N
               WRITE (NOUT,99995) WGT(I), RS(I)
  100       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,I4,A)
99997 FORMAT (1X,A,F9.4)
99996 FORMAT (1X,F9.4)
99995 FORMAT (1X,2F9.4)
      END
*
      DOUBLE PRECISION FUNCTION PSI(T)
*     .. Parameters ..
      DOUBLE PRECISION              C
      PARAMETER                     (C=1.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Intrinsic Functions ..
      INTRINSIC                     ABS
*     .. Executable Statements ..
      IF (T.LE.-C) THEN
         PSI = -C
      ELSE IF (ABS(T).LT.C) THEN
         PSI = T
      ELSE
         PSI = C
      END IF
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION CHI(T)
*     .. Parameters ..
      DOUBLE PRECISION              DCHI
      PARAMETER                     (DCHI=1.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Local Scalars ..
      DOUBLE PRECISION              PS
*     .. Intrinsic Functions ..
      INTRINSIC                     ABS
*     .. Executable Statements ..
      PS = DCHI
      IF (ABS(T).LT.DCHI) PS = T
      CHI = PS*PS/2.0D0
      RETURN
      END
*
      SUBROUTINE BETCAL(N,WGT,BETA)
*     Calculate BETA for Schweppe type regression
*     .. Parameters ..
      DOUBLE PRECISION  DCHI
      PARAMETER         (DCHI=1.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  BETA
      INTEGER           N
*     .. Array Arguments ..
      DOUBLE PRECISION  WGT(N)
*     .. Local Scalars ..
      DOUBLE PRECISION  AMAXEX, ANORMC, B, D2, DC, DW, DW2, PC, W2
      INTEGER           I, IFAIL
*     .. External Functions ..
      DOUBLE PRECISION  S15ABF, X01AAF, X02AKF
      EXTERNAL          S15ABF, X01AAF, X02AKF
*     .. Intrinsic Functions ..
      INTRINSIC         EXP, LOG, DBLE, SQRT
*     .. Executable Statements ..
      AMAXEX = -LOG(X02AKF())
      ANORMC = SQRT(X01AAF(0.0D0)*2.0D0)
      D2 = DCHI*DCHI
      BETA = 0.0D0
      DO 20 I = 1, N
         W2 = WGT(I)*WGT(I)
         DW = WGT(I)*DCHI
         IFAIL = 0
         PC = S15ABF(DW,IFAIL)
         DW2 = DW*DW
         DC = 0.0D0
         IF (DW2.LT.AMAXEX) DC = EXP(-DW2/2.0D0)/ANORMC
         B = (-DW*DC+PC-0.5D0)/W2 + (1.0D0-PC)*D2
         BETA = B*W2/DBLE(N) + BETA
   20 CONTINUE
      RETURN
      END
