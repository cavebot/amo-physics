*     E02RBF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          L, M, IA, IB, IC, IW
      PARAMETER        (L=4,M=4,IA=L+1,IB=M+1,IC=IA+IB-1,IW=IB*(2*IB+3))
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ANS, TVAL, X
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION AA(IA), BB(IB), CC(IC), W(IW)
*     .. External Subroutines ..
      EXTERNAL         E02RAF, E02RBF
*     .. Intrinsic Functions ..
      INTRINSIC        EXP, DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02RBF Example Program Results'
      CC(1) = 1.0D0
      DO 20 I = 1, IC - 1
         CC(I+1) = CC(I)/DBLE(I)
   20 CONTINUE
      IFAIL = 0
*
      CALL E02RAF(IA,IB,CC,IC,AA,BB,W,IW,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    X        Pade           True'
      DO 40 I = 1, 10
         X = DBLE(I)/10.0D0
         IFAIL = 0
*
         CALL E02RBF(AA,IA,BB,IB,X,ANS,IFAIL)
*
         TVAL = EXP(X)
         WRITE (NOUT,99999) X, ANS, TVAL
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,F6.1,3D15.5)
      END
