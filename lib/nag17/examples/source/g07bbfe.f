*     G07BBF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=20)
*     .. Local Scalars ..
      DOUBLE PRECISION CORR, DEV, SEXMU, SEXSIG, TOL, XMU, XSIG
      INTEGER          I, IFAIL, MAXIT, N, NIT
      CHARACTER        METHOD
*     .. Local Arrays ..
      DOUBLE PRECISION WK(2*NMAX), X(NMAX), XC(NMAX)
      INTEGER          IC(NMAX), NOBS(4)
*     .. External Subroutines ..
      EXTERNAL         G07BBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07BBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, METHOD, XMU, XSIG, TOL, MAXIT
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (X(I),XC(I),IC(I),I=1,N)
         IFAIL = 0
*
         CALL G07BBF(METHOD,N,X,XC,IC,XMU,XSIG,TOL,MAXIT,SEXMU,SEXSIG,
     +               CORR,DEV,NOBS,NIT,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) ' Mean = ', XMU
         WRITE (NOUT,99999) ' Standard deviation = ', XSIG
         WRITE (NOUT,99999) ' Standard error of mean = ', SEXMU
         WRITE (NOUT,99999) ' Standard error of sigma = ', SEXSIG
         WRITE (NOUT,99999) ' Correlation coefficient = ', CORR
         WRITE (NOUT,99998)
     +     ' Number of right censored observations = ', NOBS(1)
         WRITE (NOUT,99998)
     +     ' Number of left censored observations = ', NOBS(2)
         WRITE (NOUT,99998)
     +     ' Number of interval censored observations = ', NOBS(3)
         WRITE (NOUT,99998)
     +     ' Number of exactly specified observations = ', NOBS(4)
         WRITE (NOUT,99998) ' Number of iterations = ', NIT
         WRITE (NOUT,99999) ' Log-likelihood = ', DEV
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.4)
99998 FORMAT (1X,A,I2)
      END
