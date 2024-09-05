*     F01BDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IB
      PARAMETER        (NMAX=8,IA=NMAX,IB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), DL(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01BDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),(B(I,J),J=1,N),I=1,N)
         IFAIL = 0
*
         CALL F01BDF(N,A,IA,B,IB,DL,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Lower triangle of Q'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (A(I,J),J=1,I)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Strict lower triangle of L'
         DO 40 I = 2, N
            WRITE (NOUT,99999) (B(I,J),J=1,I-1)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Diagonal of L'
         WRITE (NOUT,99999) (DL(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
