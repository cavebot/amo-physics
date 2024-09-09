*     C02AHF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION AI, AR, BI, BR, CI, CR
      INTEGER          IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION ZLG(2), ZSM(2)
*     .. External Subroutines ..
      EXTERNAL         C02AHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C02AHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) AR, AI, BR, BI, CR, CI
      IFAIL = 0
*
      CALL C02AHF(AR,AI,BR,BI,CR,CI,ZSM,ZLG,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Roots of quadratic equation'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'z = ', ZSM(1), ZSM(2), '*i'
      WRITE (NOUT,99999) 'z = ', ZLG(1), ZLG(2), '*i'
      STOP
*
99999 FORMAT (1X,A,1P,D12.4,SP,D14.4,A)
      END
