*     F02SXF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=10,LDA=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N, NCOLY
      LOGICAL          WANTQ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), D(NMAX), DUMMY(1), E(NMAX),
     +                 WORK(2*NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02SWF, F02SXF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02SXF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.NMAX) THEN
         WRITE (NOUT,*) 'N is out of range.'
         WRITE (NOUT,99999) 'N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=I,N),I=1,N)
         NCOLY = 0
         WANTQ = .FALSE.
         IFAIL = 0
*
*        Reduce A to bidiagonal form
         CALL F02SWF(N,A,LDA,D,E,NCOLY,DUMMY,1,WANTQ,DUMMY,1,IFAIL)
*
*        Form the n by n orthogonal matrix  P'.
         NCOLY = 0
         IFAIL = 0
*
         CALL F02SXF(N,A,LDA,NCOLY,DUMMY,1,WORK,IFAIL)
*
         WRITE (NOUT,*) 'Matrix  P'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (A(J,I),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (3(1X,F8.4))
      END
