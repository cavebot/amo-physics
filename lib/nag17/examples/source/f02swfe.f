*     F02SWF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NCOLY, LDA, LDY
      PARAMETER        (NMAX=10,NCOLY=1,LDA=NMAX,LDY=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
      LOGICAL          WANTQ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), D(NMAX), DUMMY(1), E(NMAX), Y(LDY)
*     .. External Subroutines ..
      EXTERNAL         F02SWF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02SWF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.NMAX) THEN
         WRITE (NOUT,*) 'N is out of range.'
         WRITE (NOUT,99999) 'N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=I,N),I=1,N)
         READ (NIN,*) (Y(I),I=1,N)
         WANTQ = .FALSE.
         IFAIL = 0
*
*        Reduce A to bidiagonal form
         CALL F02SWF(N,A,LDA,D,E,NCOLY,Y,LDY,WANTQ,DUMMY,1,IFAIL)
*
         WRITE (NOUT,*) 'Diagonal elements of the bidiagonal matrix'
         WRITE (NOUT,99998) (D(I),I=1,N)
         IF (N.GT.1) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        'Super-diagonal elements of the bidiagonal matrix'
            WRITE (NOUT,99998) (E(I),I=1,N-1)
         END IF
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector Q''*Y'
         WRITE (NOUT,99998) (Y(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (3(1X,F8.4))
      END
