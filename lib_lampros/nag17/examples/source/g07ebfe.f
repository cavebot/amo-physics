*     G07EBF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=100,MMAX=100)
*     .. Local Scalars ..
      DOUBLE PRECISION CLEVEL, ESTCL, THETA, THETAL, THETAU, ULOWER,
     +                 UUPPER
      INTEGER          I, IFAIL, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION WRK(3*(NMAX+MMAX)), X(NMAX), Y(MMAX)
      INTEGER          IWRK(3*NMAX)
*     .. External Subroutines ..
      EXTERNAL         G07EBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G07EBF Example Program Results'
*     Skip Heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      IF (N.LE.1 .OR. N.GT.NMAX .OR. M.LE.1 .OR. M.GT.MMAX) THEN
         WRITE (NOUT,99999) N, M
      ELSE
         READ (NIN,*)
         READ (NIN,*) (X(I),I=1,N)
         READ (NIN,*)
         READ (NIN,*) (Y(I),I=1,M)
         READ (NIN,*)
         READ (NIN,*) CLEVEL
         IFAIL = 0
*
         CALL G07EBF('Approx',N,X,M,Y,CLEVEL,THETA,THETAL,THETAU,ESTCL,
     +               ULOWER,UUPPER,WRK,IWRK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     ' Location estimator     Confidence Interval '
         WRITE (NOUT,*)
         WRITE (NOUT,99998) THETA, '( ', THETAL, ' , ', THETAU, ' )'
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Corresponding Mann-Whitney U statistics'
         WRITE (NOUT,*)
         WRITE (NOUT,99997) ' Lower : ', ULOWER
         WRITE (NOUT,99997) ' Upper : ', UUPPER
      END IF
      STOP
*
99999 FORMAT (4X,'N or M is out of range : N = ',I8,'  and M = ',I8)
99998 FORMAT (3X,F10.4,12X,A,F6.4,A,F6.4,A)
99997 FORMAT (A,F8.2)
      END
