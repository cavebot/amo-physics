*     F04JDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, NRA, LWORK
      PARAMETER        (NMAX=8,MMAX=NMAX,NRA=MMAX,LWORK=MMAX*(MMAX+4))
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION SIGMA, TOL
      INTEGER          I, IFAIL, IRANK, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NRA,NMAX), B(NMAX), WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         F04JDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04JDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      TOL = 5.0D-4
      WRITE (NOUT,*)
      IF (M.GT.0 .AND. M.LE.MMAX .AND. N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) (B(I),I=1,M)
         IFAIL = 0
*
         CALL F04JDF(M,N,A,NRA,B,TOL,SIGMA,IRANK,WORK,LWORK,IFAIL)
*
         WRITE (NOUT,*) 'Solution vector'
         WRITE (NOUT,99997) (B(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Standard error = ', SIGMA, '    Rank = ',
     +     IRANK
      ELSE
         WRITE (NOUT,99999) 'M or N out of range: M = ', M, '  N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,A,F6.3,A,I2)
99997 FORMAT (1X,8F9.3)
      END
