*     G02HBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=5,MMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION BD, BL, TOL
      INTEGER          I, IFAIL, IX, J, K, L1, L2, M, MAXIT, MM, N, NIT,
     +                 NITMON
*     .. Local Arrays ..
      DOUBLE PRECISION A(MMAX*(MMAX+1)/2), WK(MMAX*(MMAX+1)/2),
     +                 X(NMAX,MMAX), Z(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION UCV
      EXTERNAL         UCV
*     .. External Subroutines ..
      EXTERNAL         G02HBF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02HBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      CALL X04ABF(1,NOUT)
*     Read in the dimensions of X
      READ (NIN,*) N, M
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M.GT.0 .AND. M.LE.MMAX) THEN
*        Read in the X matrix
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,M)
   20    CONTINUE
         IX = NMAX
*        Read in the initial value of A
         MM = (M+1)*M/2
         READ (NIN,*) (A(J),J=1,MM)
*        Set the values remaining parameters
         BL = 0.9D0
         BD = 0.9D0
         MAXIT = 50
         TOL = 0.5D-4
         IFAIL = 0
*        * Change NITMON to a positive value if monitoring information
*          is required *
         NITMON = 0
*
         CALL G02HBF(UCV,N,M,X,IX,A,Z,BL,BD,TOL,MAXIT,NITMON,NIT,WK,
     +               IFAIL)
*
         WRITE (NOUT,99999) 'G02HBF required ', NIT,
     +     ' iterations to converge'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Matrix A'
         L2 = 0
         DO 40 J = 1, M
            L1 = L2 + 1
            L2 = L2 + J
            WRITE (NOUT,99998) (A(K),K=L1,L2)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector Z'
         DO 60 I = 1, N
            WRITE (NOUT,99998) Z(I)
   60    CONTINUE
*        Calculate Krasker-Welsch weights
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector of weights'
         DO 80 I = 1, N
            Z(I) = 1.0D0/Z(I)
            WRITE (NOUT,99998) Z(I)
   80    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I4,A)
99998 FORMAT (1X,6F9.4)
      END
*
      DOUBLE PRECISION FUNCTION UCV(T)
*     UCV function for Krasker-Welsch weights
*     .. Parameters ..
      DOUBLE PRECISION              UCVC
      PARAMETER                     (UCVC=2.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Local Scalars ..
      DOUBLE PRECISION              PC, PD, Q, Q2
      INTEGER                       IFAIL
*     .. External Functions ..
      DOUBLE PRECISION              S15ABF, X01AAF, X02AKF
      EXTERNAL                      S15ABF, X01AAF, X02AKF
*     .. Intrinsic Functions ..
      INTRINSIC                     EXP, LOG, SQRT
*     .. Executable Statements ..
      UCV = 1.0D0
      IF (T.NE.0.0D0) THEN
         Q = UCVC/T
         Q2 = Q*Q
         IFAIL = 0
         PC = S15ABF(Q,IFAIL)
         IF (Q2.LT.-LOG(X02AKF())) THEN
            PD = EXP(-Q2/2.0D0)/SQRT(X01AAF(0.0D0)*2.0D0)
         ELSE
            PD = 0.0D0
         END IF
         UCV = (2.0D0*PC-1.0D0)*(1.0D0-Q2) + Q2 - 2.0D0*Q*PD
      END IF
      RETURN
      END
