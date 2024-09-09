*     S14ADF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX
      PARAMETER        (MMAX=4)
*     .. Local Scalars ..
      DOUBLE PRECISION X
      INTEGER          I, IFAIL, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION W(MMAX)
*     .. External Subroutines ..
      EXTERNAL         S14ADF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S14ADF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +'      X           W(1)          W(2)          W(3)          W(4)'
      WRITE (NOUT,*)
      N = 0
      M = 4
   20 READ (NIN,*,END=40) X
*
      CALL S14ADF(X,N,M,W,IFAIL)
*
      WRITE (NOUT,99999) X, (W(I),I=1,M)
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,1P,5(D12.4,2X))
      END
