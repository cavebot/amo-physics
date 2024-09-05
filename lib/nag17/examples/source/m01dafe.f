*     M01DAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      DOUBLE PRECISION RV(NMAX)
      INTEGER          IRANK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         M01DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01DAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GE.1 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (RV(I),I=1,N)
         IFAIL = 0
*
         CALL M01DAF(RV,1,N,'Ascending',IRANK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '   Data   Ranks'
         WRITE (NOUT,*)
         DO 20 I = 1, N
            WRITE (NOUT,99999) RV(I), IRANK(I)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,F7.1,I7)
      END
