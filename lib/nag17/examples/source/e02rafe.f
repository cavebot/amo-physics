*     E02RAF Example Program Text.
*     Mark 16 Revised. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          L, M, IA, IB, IC, IW
      PARAMETER        (L=4,M=4,IA=L+1,IB=M+1,IC=IA+IB-1,IW=IB*(2*IB+3))
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      LOGICAL          SCALE
      PARAMETER        (SCALE=.TRUE.)
*     .. Local Scalars ..
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION AA(IA), BB(IB), CC(IC), DD(IA+IB), W(IW),
     +                 WORK(2*(L+M+1)), Z(2,L+M)
*     .. External Subroutines ..
      EXTERNAL         C02AGF, E02RAF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02RAF Example Program Results'
*     Power series coefficients in CC
      CC(1) = 1.0D0
      DO 20 I = 1, IC - 1
         CC(I+1) = CC(I)/DBLE(I)
   20 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'The given series coefficients are'
      WRITE (NOUT,99999) (CC(I),I=1,IC)
      IFAIL = 0
*
      CALL E02RAF(IA,IB,CC,IC,AA,BB,W,IW,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Numerator coefficients'
      WRITE (NOUT,99999) (AA(I),I=1,IA)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Denominator coefficients'
      WRITE (NOUT,99999) (BB(I),I=1,IB)
*     Calculate zeros of the approximant using C02AGF
*     First need to reverse order of coefficients
      DO 40 I = 1, IA
         DD(IA-I+1) = AA(I)
   40 CONTINUE
      IFAIL = 0
*
      CALL C02AGF(DD,L,SCALE,Z,WORK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Zeros of approximant are at'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    Real part    Imag part'
      WRITE (NOUT,99998) (Z(1,I),Z(2,I),I=1,L)
*     Calculate poles of the approximant using C02AGF
*     Reverse order of coefficients
      DO 60 I = 1, IB
         DD(IB-I+1) = BB(I)
   60 CONTINUE
      IFAIL = 0
*
      CALL C02AGF(DD,M,SCALE,Z,WORK,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Poles of approximant are at'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    Real part    Imag part'
      WRITE (NOUT,99998) (Z(1,I),Z(2,I),I=1,M)
      STOP
*
99999 FORMAT (1X,5D13.4)
99998 FORMAT (1X,2D13.4)
      END
 
