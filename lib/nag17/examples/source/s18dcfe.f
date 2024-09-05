*     S18DCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          N
      PARAMETER        (N=2)
*     .. Local Scalars ..
      COMPLEX*16       Z
      DOUBLE PRECISION FNU
      INTEGER          IFAIL, NZ
      CHARACTER*1      SCALE
*     .. Local Arrays ..
      COMPLEX*16       CY(N)
*     .. External Subroutines ..
      EXTERNAL         S18DCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'S18DCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Calling with N =', N
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +'   FNU            Z        SCALE       CY(1)              CY(2)
     +   NZ IFAIL'
      WRITE (NOUT,*)
   20 READ (NIN,*,END=40) FNU, Z, SCALE
      IFAIL = 0
*
      CALL S18DCF(FNU,Z,N,SCALE,CY,NZ,IFAIL)
*
      WRITE (NOUT,99998) FNU, Z, SCALE, CY(1), CY(2), NZ, IFAIL
      GO TO 20
   40 STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,F7.4,'  (',F7.3,',',F7.3,')   ',A,
     +       2('  (',F7.3,',',F7.3,')'),I4,I4)
      END
