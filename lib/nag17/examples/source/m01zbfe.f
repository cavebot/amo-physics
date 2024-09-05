*     M01ZBF Example Program Text
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
      EXTERNAL         M01EAF, M01ZBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01ZBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GE.1 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (RV(I),I=1,N)
         READ (NIN,*) (IRANK(I),I=1,N)
         IFAIL = 0
*
         CALL M01ZBF(IRANK,1,N,IFAIL)
         CALL M01EAF(RV,1,N,IRANK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Numbers in rank order'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) (RV(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,10F7.1)
      END
