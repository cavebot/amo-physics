*     M01ECF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX
      PARAMETER        (MMAX=100)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, M
*     .. Local Arrays ..
      INTEGER          IFREQ(MMAX), IRANK(MMAX)
      CHARACTER*6      CH(MMAX)
*     .. External Subroutines ..
      EXTERNAL         M01DBF, M01ECF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01ECF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      DO 20 M = 1, MMAX
         READ (NIN,99999,END=40) CH(M), IFREQ(M)
   20 CONTINUE
   40 M = M - 1
      IFAIL = 0
*
      CALL M01DBF(IFREQ,1,M,'Descending',IRANK,IFAIL)
      CALL M01ECF(CH,1,M,IRANK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Names in order of frequency'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (CH(I),I=1,M)
      STOP
*
99999 FORMAT (A6,I6)
99998 FORMAT (1X,A)
      END
