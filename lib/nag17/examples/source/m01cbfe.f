*     M01CBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      INTEGER          IV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         M01CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01CBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GE.1 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (IV(I),I=1,N)
         IFAIL = 0
*
         CALL M01CBF(IV,1,N,'Descending',IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Sorted numbers'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) (IV(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,10I7)
      END
