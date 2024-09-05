*     G02HLF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LDX
      PARAMETER        (NMAX=10,MMAX=3,LDX=NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION BD, BL, TOL
      INTEGER          I, IFAIL, INDM, J, K, L1, L2, M, MAXIT, MM, N,
     +                 NIT, NITMON
*     .. Local Arrays ..
      DOUBLE PRECISION A(MMAX*(MMAX+1)/2), COV(MMAX*(MMAX+1)/2),
     +                 THETA(MMAX), USERP(2), WK(MMAX*(MMAX+1)/2),
     +                 WT(NMAX), X(LDX,MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02HLF, UCV, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02HLF Example Program Results'
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
*        Read in the initial value of A
         MM = (M+1)*M/2
         READ (NIN,*) (A(J),J=1,MM)
*        Read in the initial value of THETA
         READ (NIN,*) (THETA(J),J=1,M)
*        Read in the values of the parameters of the ucv functions
         READ (NIN,*) USERP(1), USERP(2)
*        Set the values of remaining parameters
         INDM = 1
         BL = 0.9D0
         BD = 0.9D0
         MAXIT = 50
         TOL = 0.5D-4
*        * Change NITMON to a positive value if monitoring information
*          is required *
         NITMON = 0
         IFAIL = 0
*
         CALL G02HLF(UCV,USERP,INDM,N,M,X,LDX,COV,A,WT,THETA,BL,BD,
     +               MAXIT,NITMON,TOL,NIT,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'G02HLF required ', NIT,
     +     ' iterations to converge'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Robust covariance matrix'
         L2 = 0
         DO 40 J = 1, M
            L1 = L2 + 1
            L2 = L2 + J
            WRITE (NOUT,99998) (COV(K),K=L1,L2)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Robust estimates of THETA'
         DO 60 J = 1, M
            WRITE (NOUT,99997) THETA(J)
   60    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I4,A)
99998 FORMAT (1X,6F10.3)
99997 FORMAT (1X,F10.3)
      END
*
      SUBROUTINE UCV(T,USERP,U,UD,W,WD)
*     .. Scalar Arguments ..
      DOUBLE PRECISION T, U, UD, W, WD
*     .. Array Arguments ..
      DOUBLE PRECISION USERP(2)
*     .. Local Scalars ..
      DOUBLE PRECISION CU, CW, T2
*     .. Executable Statements ..
*     u function and derivative
      CU = USERP(1)
      U = 1.0D0
      UD = 0.0D0
      IF (T.NE.0) THEN
         T2 = T*T
         IF (T2.GT.CU) THEN
            U = CU/T2
            UD = -2.0D0*U/T
         END IF
      END IF
*     w function and derivative
      CW = USERP(2)
      IF (T.GT.CW) THEN
         W = CW/T
         WD = -W/T
      ELSE
         W = 1.0D0
         WD = 0.0D0
      END IF
      END
