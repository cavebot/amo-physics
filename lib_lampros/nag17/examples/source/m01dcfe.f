*     M01DCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX
      PARAMETER        (MMAX=100)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, L1, L2, M
*     .. Local Arrays ..
      INTEGER          IRANK(MMAX)
      CHARACTER*12     CH(MMAX)
*     .. External Subroutines ..
      EXTERNAL         M01DCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01DCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      DO 20 M = 1, MMAX
         READ (NIN,'(A)',END=40) CH(M)
   20 CONTINUE
   40 M = M - 1
      L1 = 7
      L2 = 12
      IFAIL = 0
*
      CALL M01DCF(CH,1,M,L1,L2,'Reverse ASCII',IRANK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Records ranked on columns ', L1, '  to ', L2
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Data            Ranks'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (CH(I),IRANK(I),I=1,M)
      STOP
*
99999 FORMAT (1X,A,I2,A,I2)
99998 FORMAT (1X,A,I7)
      END
