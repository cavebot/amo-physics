*     G01DHF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=20)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      DOUBLE PRECISION R(NMAX), X(NMAX)
      INTEGER          IWRK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01DHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01DHF Example Program Results'
      WRITE (NOUT,*)
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (X(I),I=1,N)
         IFAIL = 0
*
         CALL G01DHF('Savage','Average',N,X,R,IWRK,IFAIL)
*
         WRITE (NOUT,*) 'The Savage Scores : '
         WRITE (NOUT,*)
     +     '  (Average scores are used for tied observations)'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) (R(I),I=1,N)
      ELSE
         WRITE (NOUT,*) 'N is larger than NMAX'
      END IF
      STOP
*
99999 FORMAT (1X,F10.4)
      END
