*     F02BFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, MMAX
      PARAMETER        (NMAX=8,IA=(NMAX*(NMAX+1))/2,MMAX=8)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, EPS1, EPS2
      INTEGER          I, IZ, M, M1, M2, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA), B(NMAX), BETA(NMAX), C(NMAX), WU(MMAX),
     +                 X(MMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AYF, F02BFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02BFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M1, M2
      WRITE (NOUT,*)
      M = M2 - M1 + 1
      IF (N.LT.1 .OR. N.GT.NMAX .OR. M.LT.1 .OR. M.GT.MMAX) THEN
         WRITE (NOUT,*) 'N or M1 or M2 is out of range.'
         WRITE (NOUT,99999) 'N = ', N, '  M1 = ', M1, '  M2 = ', M2
         STOP
      END IF
      READ (NIN,*) (A(I),I=1,(N*(N+1))/2)
*
*     Householder reduction to tridiagonal form
      CALL F01AYF(N,0.0D0,A,IA,C,B,BETA)
*
      EPS = X02AJF()
      EPS1 = 0.0D0
*
*     Selected eigenvalues of tridiagonal matrix
      CALL F02BFF(C,B,BETA,N,M1,M2,M,EPS1,EPS,EPS2,IZ,X,WU)
*
      WRITE (NOUT,*) 'Eigenvalues'
      WRITE (NOUT,99998) (X(I),I=1,M)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'No. of bisections = ', IZ,
     +  ' (machine dependent)'
      WRITE (NOUT,*)
      WRITE (NOUT,99997) 'Error bound = ', EPS2, ' (machine dependent)'
      STOP
*
99999 FORMAT (1X,A,I5,A,I5,A,I5)
99998 FORMAT (1X,8F9.4)
99997 FORMAT (1X,A,D11.4,A)
      END
