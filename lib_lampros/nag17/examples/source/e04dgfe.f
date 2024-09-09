*     E04DGF Example Program Text
*     Mark 16 Revised. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=10)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJF
      INTEGER          I, IFAIL, ITER, N
*     .. Local Arrays ..
      DOUBLE PRECISION OBJGRD(NMAX), USER(1), WORK(13*NMAX), X(NMAX)
      INTEGER          IUSER(1), IWORK(NMAX+1)
*     .. External Subroutines ..
      EXTERNAL         E04DGF, OBJFUN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04DGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read X from data file
*
         READ (NIN,*) (X(I),I=1,N)
*
*        Solve the problem
*
         IFAIL = -1
*
         CALL E04DGF(N,OBJFUN,ITER,OBJF,OBJGRD,X,IWORK,WORK,IUSER,USER,
     +               IFAIL)
*
      END IF
      STOP
      END
*
      SUBROUTINE OBJFUN(MODE,N,X,OBJF,OBJGRD,NSTATE,IUSER,USER)
*     Routine to evaluate F(x) and its 1st derivatives.
*     .. Parameters ..
      DOUBLE PRECISION  ONE, TWO, FOUR
      PARAMETER         (ONE=1.0D0,TWO=2.0D0,FOUR=4.0D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  OBJF
      INTEGER           MODE, N, NSTATE
*     .. Array Arguments ..
      DOUBLE PRECISION  OBJGRD(N), USER(*), X(N)
      INTEGER           IUSER(*)
*     .. Local Scalars ..
      DOUBLE PRECISION  EXPX1, X1, X2
*     .. Intrinsic Functions ..
      INTRINSIC         EXP
*     .. Executable Statements ..
      X1 = X(1)
      X2 = X(2)
*
      EXPX1 = EXP(X1)
      OBJF = EXPX1*(FOUR*X1**2+TWO*X2**2+FOUR*X1*X2+TWO*X2+ONE)
*
      IF (MODE.EQ.2) THEN
         OBJGRD(1) = FOUR*EXPX1*(TWO*X1+X2) + OBJF
         OBJGRD(2) = TWO*EXPX1*(TWO*X2+TWO*X1+ONE)
      END IF
*
      RETURN
      END
