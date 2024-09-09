*     G02HKF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, LDX
      PARAMETER        (NMAX=20,MMAX=5,LDX=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, TOL
      INTEGER          I, IFAIL, J, K, L1, L2, M, MAXIT, N, NIT, NITMON
*     .. Local Arrays ..
      DOUBLE PRECISION COV(MMAX*(MMAX+1)/2), THETA(MMAX),
     +                 WK(2*MMAX+NMAX+MMAX*(MMAX+1)/2), X(LDX,MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02HKF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02HKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      CALL X04ABF(1,NOUT)
*     Read in the dimensions of X
      READ (NIN,*) N, M
      IF ((N.LE.NMAX) .AND. (M.LE.MMAX)) THEN
*        Read in the X matrix
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,M)
   20    CONTINUE
*        Read in value of eps
         READ (NIN,*) EPS
*        Set up remaining parameters
         MAXIT = 100
         TOL = 0.5D-4
*        Set NITMON to positive value for iteration monitoring
         NITMON = 0
         IFAIL = 0
*
         CALL G02HKF(N,M,X,LDX,EPS,COV,THETA,MAXIT,NITMON,TOL,NIT,WK,
     +               IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'G02HKF required ', NIT,
     +     ' iterations to converge'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Covariance matrix'
         L2 = 0
         DO 40 J = 1, M
            L1 = L2 + 1
            L2 = L2 + J
            WRITE (NOUT,99998) (COV(K),K=L1,L2)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'THETA'
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
