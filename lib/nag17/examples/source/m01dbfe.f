*     M01DBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      INTEGER          IRANK(NMAX), IV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         M01DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01DBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GE.1 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (IV(I),I=1,N)
         IFAIL = 0
*
         CALL M01DBF(IV,1,N,'Descending',IRANK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '   Data   Ranks'
         WRITE (NOUT,*)
         DO 20 I = 1, N
            WRITE (NOUT,99999) IV(I), IRANK(I)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,2I7)
      END
