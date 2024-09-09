*     G01JCF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=4)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION C, P, PDF, TOL
      INTEGER          I, IFAIL, MAXIT, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), RLAMDA(NMAX), WRK(1200)
      INTEGER          MULT(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01JCF
*     .. Data statements ..
      DATA             MAXIT, TOL/500, 0.0001D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01JCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '        A     MULT  RLAMDA'
   20 READ (NIN,*,END=60) N, C
      WRITE (NOUT,*)
      READ (NIN,*) (A(I),I=1,N)
      READ (NIN,*) (MULT(I),I=1,N)
      READ (NIN,*) (RLAMDA(I),I=1,N)
      IFAIL = -1
*
      CALL G01JCF(A,MULT,RLAMDA,N,C,P,PDF,TOL,MAXIT,WRK,IFAIL)
*
      IF (IFAIL.EQ.0 .OR. IFAIL.GE.4) THEN
         DO 40 I = 1, N
            WRITE (NOUT,99999) A(I), MULT(I), RLAMDA(I)
   40    CONTINUE
         WRITE (NOUT,99998) 'C = ', C, '    PROB = ', P
         GO TO 20
      END IF
   60 STOP
*
99999 FORMAT (1X,F10.2,I6,F9.2)
99998 FORMAT (1X,A,F6.2,A,F6.4)
      END
