*     F02AVF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA
      PARAMETER        (NMAX=8,IA=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), D(NMAX), E(NMAX), E2(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AGF, F02AVF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02AVF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
*     Householder reduction to tridiagonal form
      CALL F01AGF(N,0.0D0,A,IA,D,E,E2)
*
      EPS = X02AJF()
      IFAIL = 1
*
*     Eigenvalues of tridiagonal matrix
      CALL F02AVF(N,EPS,D,E,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02AVF. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99998) (D(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,F9.4)
      END
