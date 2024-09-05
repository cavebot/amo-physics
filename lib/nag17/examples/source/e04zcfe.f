*     E04ZCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJF
      INTEGER          I, IFAIL, J, K, LWORK, N, NCNLN, NROWJ
*     .. Local Arrays ..
      DOUBLE PRECISION C(20), CJAC(20,9), OBJGRD(9), WORK(351), X(9)
*     .. External Subroutines ..
      EXTERNAL         CONFUN, E04ZCF, OBJFUN
*     .. Data statements ..
      DATA             NROWJ/20/, LWORK/351/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04ZCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      N = 9
      NCNLN = 15
*     Read in two points and check the derivatives at each point.
      DO 20 K = 1, 2
         READ (NIN,99999) (X(J),J=1,N)
         IFAIL = 1
*
         CALL E04ZCF(N,NCNLN,NROWJ,CONFUN,OBJFUN,C,CJAC,OBJF,OBJGRD,X,
     +               WORK,LWORK,IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*) 'Derivatives probably correct at the point'
            WRITE (NOUT,99998) (X(J),J=1,N)
         ELSE IF (IFAIL.EQ.1) THEN
            WRITE (NOUT,*) 'Incorrect parameter supplied to E04ZCF'
            STOP
         ELSE IF (IFAIL.EQ.2) THEN
            WRITE (NOUT,*)
     +        'Probable error in derivative of objective function'
            WRITE (NOUT,99998) (X(J),J=1,N)
            WRITE (NOUT,*) 'The computed gradients are'
            WRITE (NOUT,99998) (OBJGRD(J),J=1,N)
         ELSE
            I = IFAIL - 2
            WRITE (NOUT,99997)
     +        'Probable error in derivative of constraint', I,
     +        '  at the point'
            WRITE (NOUT,99998) (X(J),J=1,N)
            WRITE (NOUT,*)
     +        'The computed gradients of this constraint are'
            WRITE (NOUT,99998) (CJAC(I,J),J=1,N)
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,9F6.2)
99998 FORMAT (1X,1P,5D12.4)
99997 FORMAT (1X,A,I4,A)
      END
*
      SUBROUTINE OBJFUN(MODE,N,X,OBJF,OBJGRD,NSTATE)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  OBJF
      INTEGER           MODE, N, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  OBJGRD(N), X(N)
*     .. Executable Statements ..
      OBJF = X(2)*X(6) - X(1)*X(7) + X(3)*X(7) + X(5)*X(8) - X(4)*X(9) -
     +       X(3)*X(8)
      OBJF = -OBJF
      OBJGRD(1) = X(7)
      OBJGRD(2) = -X(6)
      OBJGRD(3) = -X(7) + X(8)
      OBJGRD(4) = X(9)
      OBJGRD(5) = -X(8)
      OBJGRD(6) = -X(2)
      OBJGRD(7) = -X(3) + X(1)
      OBJGRD(8) = -X(5) + X(3)
      OBJGRD(9) = X(4)
      RETURN
      END
*
      SUBROUTINE CONFUN(MODE,NCNLN,N,NROWJ,X,C,CJAC,NSTATE)
*     .. Parameters ..
      DOUBLE PRECISION  ZERO, TWO
      PARAMETER         (ZERO=0.0D0,TWO=2.0D0)
*     .. Scalar Arguments ..
      INTEGER           MODE, N, NCNLN, NROWJ, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  C(NROWJ), CJAC(NROWJ,N), X(N)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
*     The zero elements of Jacobian matrix are set only once. This
*     occurs during the first call to CONFUN (NSTATE = 1).
      IF (NSTATE.EQ.1) THEN
         DO 40 J = 1, N
            DO 20 I = 1, NCNLN
               CJAC(I,J) = ZERO
   20       CONTINUE
   40    CONTINUE
      END IF
      C(1) = X(1)**2 + X(6)**2
      CJAC(1,1) = TWO*X(1)
      CJAC(1,6) = TWO*X(6)
      C(2) = (X(2)-X(1))**2 + (X(7)-X(6))**2
      CJAC(2,1) = -TWO*(X(2)-X(1))
      CJAC(2,2) = TWO*(X(2)-X(1))
      CJAC(2,6) = -TWO*(X(7)-X(6))
      CJAC(2,7) = TWO*(X(7)-X(6))
      C(3) = (X(3)-X(1))**2 + X(6)**2
      CJAC(3,1) = -TWO*(X(3)-X(1))
      CJAC(3,3) = TWO*(X(3)-X(1))
      CJAC(3,6) = TWO*X(6)
      C(4) = (X(1)-X(4))**2 + (X(6)-X(8))**2
      CJAC(4,1) = TWO*(X(1)-X(4))
      CJAC(4,4) = -TWO*(X(1)-X(4))
      CJAC(4,6) = TWO*(X(6)-X(8))
      CJAC(4,8) = -TWO*(X(6)-X(8))
      C(5) = (X(1)-X(5))**2 + (X(6)-X(9))**2
      CJAC(5,1) = TWO*(X(1)-X(5))
      CJAC(5,5) = -TWO*(X(1)-X(5))
      CJAC(5,6) = TWO*(X(6)-X(9))
      CJAC(5,9) = -TWO*(X(6)-X(9))
      C(6) = X(2)**2 + X(7)**2
      CJAC(6,2) = TWO*X(2)
      CJAC(6,7) = TWO*X(7)
      C(7) = (X(3)-X(2))**2 + X(7)**2
      CJAC(7,2) = -TWO*(X(3)-X(2))
      CJAC(7,3) = TWO*(X(3)-X(2))
      CJAC(7,7) = TWO*X(7)
      C(8) = (X(4)-X(2))**2 + (X(8)-X(7))**2
      CJAC(8,2) = -TWO*(X(4)-X(2))
      CJAC(8,4) = TWO*(X(4)-X(2))
      CJAC(8,7) = -TWO*(X(8)-X(7))
      CJAC(8,8) = TWO*(X(8)-X(7))
      C(9) = (X(2)-X(5))**2 + (X(7)-X(9))**2
      CJAC(9,2) = TWO*(X(2)-X(5))
      CJAC(9,5) = -TWO*(X(2)-X(5))
      CJAC(9,7) = TWO*(X(7)-X(9))
      CJAC(9,9) = -TWO*(X(7)-X(9))
      C(10) = X(3)**2
      CJAC(10,3) = TWO*X(3)
      C(11) = (X(4)-X(3))**2 + X(8)**2
      CJAC(11,3) = -TWO*(X(4)-X(3))
      CJAC(11,4) = TWO*(X(4)-X(3))
      CJAC(11,8) = TWO*X(8)
      C(12) = (X(5)-X(3))**2 + X(9)**2
      CJAC(12,3) = -TWO*(X(5)-X(3))
      CJAC(12,5) = TWO*(X(5)-X(3))
      CJAC(12,9) = TWO*X(9)
      C(13) = X(4)**2 + X(8)**2
      CJAC(13,4) = TWO*X(4)
      CJAC(13,8) = TWO*X(8)
      C(14) = (X(4)-X(5))**2 + (X(9)-X(8))**2
      CJAC(14,4) = TWO*(X(4)-X(5))
      CJAC(14,5) = -TWO*(X(4)-X(5))
      CJAC(14,8) = -TWO*(X(9)-X(8))
      CJAC(14,9) = TWO*(X(9)-X(8))
      C(15) = X(5)**2 + X(9)**2
      CJAC(15,5) = TWO*X(5)
      CJAC(15,9) = TWO*X(9)
      RETURN
      END
