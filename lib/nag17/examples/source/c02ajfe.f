*     C02AJF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, C
      INTEGER          IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION ZLG(2), ZSM(2)
*     .. External Subroutines ..
      EXTERNAL         C02AJF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C02AJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) A, B, C
      IFAIL = 0
*
      CALL C02AJF(A,B,C,ZSM,ZLG,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Roots of quadratic equation'
      WRITE (NOUT,*)
      IF (ZSM(2).EQ.ZERO) THEN
*        2 real roots.
         WRITE (NOUT,99999) 'z = ', ZSM(1)
         WRITE (NOUT,99999) 'z = ', ZLG(1)
      ELSE
*        2 complex roots.
         WRITE (NOUT,99998) 'z = ', ZSM(1), ' +/- ', ABS(ZSM(2)), '*i'
      END IF
      STOP
*
99999 FORMAT (1X,A,1P,D12.4)
99998 FORMAT (1X,A,1P,D12.4,A,D12.4,A)
      END
