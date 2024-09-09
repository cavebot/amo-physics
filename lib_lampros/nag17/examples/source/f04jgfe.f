*     F04JGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX, NRA, LWORK
      PARAMETER        (MMAX=8,NMAX=MMAX,NRA=MMAX,LWORK=4*NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION SIGMA, TOL
      INTEGER          I, IFAIL, IRANK, J, M, N
      LOGICAL          SVD
*     .. Local Arrays ..
      DOUBLE PRECISION A(NRA,NMAX), B(MMAX), WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         F04JGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04JGF Example Program Results'
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
         CALL F04JGF(M,N,A,NRA,B,TOL,SVD,SIGMA,IRANK,WORK,LWORK,IFAIL)
*
         WRITE (NOUT,*) 'Solution vector'
         WRITE (NOUT,99996) (B(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Standard error = ', SIGMA, '    Rank = ',
     +     IRANK
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'SVD = ', SVD
      ELSE
         WRITE (NOUT,99999) 'M or N out of range: M = ', M, '  N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,A,F6.3,A,I2)
99997 FORMAT (1X,A,L2)
99996 FORMAT (1X,8F9.3)
      END
