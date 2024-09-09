*     G07EAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
*     .. Local Scalars ..
      DOUBLE PRECISION CLEVEL, ESTCL, THETA, THETAL, THETAU, WLOWER,
     +                 WUPPER
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      DOUBLE PRECISION WRK(4*NMAX), X(NMAX)
      INTEGER          IWRK(3*NMAX)
*     .. External Subroutines ..
      EXTERNAL         G07EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07EAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) N
      ELSE
         READ (NIN,*) (X(I),I=1,N)
         READ (NIN,*) CLEVEL
         IFAIL = 0
*
         CALL G07EAF('Exact',N,X,CLEVEL,THETA,THETAL,THETAU,ESTCL,
     +               WLOWER,WUPPER,WRK,IWRK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     ' Location estimator     Confidence Interval '
         WRITE (NOUT,*)
         WRITE (NOUT,99998) THETA, '( ', THETAL, ' , ', THETAU, ' )'
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Corresponding Wilcoxon statistics'
         WRITE (NOUT,*)
         WRITE (NOUT,99997) ' Lower : ', WLOWER
         WRITE (NOUT,99997) ' Upper : ', WUPPER
      END IF
      STOP
*
99999 FORMAT (1X,'N is less than 2 or greater than NMAX : N = ',I8)
99998 FORMAT (3X,F10.4,12X,A,F6.4,A,F6.4,A)
99997 FORMAT (A,F8.2)
      END
