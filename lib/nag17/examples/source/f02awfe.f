*     F02AWF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IAR, IAI
      PARAMETER        (NMAX=4,IAR=NMAX,IAI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION AI(IAI,NMAX), AR(IAR,NMAX), WK1(NMAX), WK2(NMAX),
     +                 WK3(NMAX), WR(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02AWF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02AWF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) ((AR(I,J),AI(I,J),J=1,N),I=1,N)
      IFAIL = 1
*
      CALL F02AWF(AR,IAR,AI,IAI,N,WR,WK1,WK2,WK3,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02AWF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (WR(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,F9.4)
      END
