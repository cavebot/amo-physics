*     X03ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=3)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      COMPLEX*16       CX, DX
      INTEGER          I, IFAIL, ISIZEA, ISIZEB, ISTEPA, ISTEPB, J
      LOGICAL          SW
*     .. Local Arrays ..
      COMPLEX*16       A(N,N), B(N)
*     .. External Subroutines ..
      EXTERNAL         X03ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X03ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) ((A(I,J),J=1,N),I=1,N), (B(I),I=1,N)
      CX = (1.0D0,1.0D0)
      ISIZEA = N
      ISIZEB = N
      ISTEPA = 1
      ISTEPB = 1
      SW = .TRUE.
      IFAIL = 0
*
      CALL X03ABF(A(1,2),ISIZEA,B,ISIZEB,N,ISTEPA,ISTEPB,CX,DX,SW,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Result = ', DX
      STOP
*
99999 FORMAT (1X,A,'(',F3.0,',',F3.0,')')
      END
