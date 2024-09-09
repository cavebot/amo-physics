*     G05GBF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=10)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS
      INTEGER          I, IFAIL, J, LDC, N
*     .. Local Arrays ..
      DOUBLE PRECISION C(NMAX,NMAX), D(NMAX), WK(2*NMAX)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05GBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05GBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (D(I),I=1,N)
*
         WRITE (NOUT,*)
*
         LDC = NMAX
         CALL G05CBF(0)
         EPS = 0.0001D0
*
         IFAIL = 0
*
         CALL G05GBF(N,D,C,LDC,EPS,WK,IFAIL)
*
         DO 20 I = 1, N
            WRITE (NOUT,99999) (C(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,3F9.3)
      END
