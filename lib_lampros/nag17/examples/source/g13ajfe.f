*     G13AJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NPMAX, ISTMAX, IFVMAX, NXMAX, IW
      PARAMETER        (NPMAX=10,ISTMAX=10,IFVMAX=10,NXMAX=30,IW=250)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION C, RMS
      INTEGER          I, IFAIL, IFV, IST, KFC, NFV, NPAR, NST, NX
*     .. Local Arrays ..
      DOUBLE PRECISION FSD(IFVMAX), FVA(IFVMAX), PAR(NPMAX), ST(ISTMAX),
     +                 W(IW), X(NXMAX)
      INTEGER          ISF(4), MR(7)
*     .. External Subroutines ..
      EXTERNAL         G13AJF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13AJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX, (MR(I),I=1,7), NFV
      NPAR = MR(1) + MR(3) + MR(4) + MR(6)
      IF (NX.GT.0 .AND. NX.LE.NXMAX .AND. NPAR.GT.0 .AND. NPAR.LE.NPMAX)
     +    THEN
         READ (NIN,*) (X(I),I=1,NX)
         READ (NIN,*) (PAR(I),I=1,NPAR)
         READ (NIN,*) KFC, C
         IST = MR(4) + MR(7) + MR(2) + MR(5) + MR(3) + MAX(MR(1),MR(6)
     +         *MR(7))
         IFV = MAX(1,NFV)
         IFAIL = 0
*
         CALL G13AJF(MR,PAR,NPAR,C,KFC,X,NX,RMS,ST,IST,NST,NFV,FVA,FSD,
     +               IFV,ISF,W,IW,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'The residual mean square is ', RMS
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'The state set consists of ', NST, ' values'
         WRITE (NOUT,99997) (ST(I),I=1,NST)
         WRITE (NOUT,*)
         WRITE (NOUT,99996) 'The ', NFV,
     +     ' forecast values and standard errors are -'
         WRITE (NOUT,99995) (FVA(I),FSD(I),I=1,NFV)
      END IF
      STOP
*
99999 FORMAT (1X,A,F9.2)
99998 FORMAT (1X,A,I1,A)
99997 FORMAT (1X,4F11.4)
99996 FORMAT (1X,A,I2,A)
99995 FORMAT (10X,2F10.2)
      END
