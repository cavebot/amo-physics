*     G13AHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NPMAX, NSTMAX, NWAMAX, NFVMAX
      PARAMETER        (NPMAX=10,NSTMAX=40,NWAMAX=120,NFVMAX=25)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION C, RMS
      INTEGER          I, IFAIL, NFV, NPAR, NST, NWA
*     .. Local Arrays ..
      DOUBLE PRECISION FSD(NFVMAX), FVA(NFVMAX), PAR(NPMAX), ST(NSTMAX),
     +                 WA(NWAMAX)
      INTEGER          MR(7)
*     .. External Subroutines ..
      EXTERNAL         G13AHF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13AHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NFV
      READ (NIN,*) (MR(I),I=1,7)
      NPAR = MR(1) + MR(3) + MR(4) + MR(6)
      NST = MR(4)*MR(7) + MR(5)*MR(7) + MR(2) + MR(3) + MAX(MR(1),MR(6)
     +      *MR(7))
      NWA = 4*NPAR + 3*NST
      IF (NFV.GT.0 .AND. NFV.LE.NFVMAX .AND. NPAR.GT.0 .AND. NPAR.LE.
     +    NPMAX .AND. NST.GT.0 .AND. NST.LE.NSTMAX) THEN
         READ (NIN,*) (PAR(I),I=1,NPAR), C
         READ (NIN,*) (ST(I),I=1,NST)
         READ (NIN,*) RMS
         IFAIL = 0
*
         CALL G13AHF(ST,NST,MR,PAR,NPAR,C,RMS,NFV,FVA,FSD,WA,NWA,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'The required', NFV,
     +     ' forecast values are as follows'
         WRITE (NOUT,99999) (FVA(I),I=1,NFV)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'The standard deviations corresponding to the forecasts are'
         WRITE (NOUT,99999) (FSD(I),I=1,NFV)
      END IF
      STOP
*
99999 FORMAT (1X,8F8.4)
99998 FORMAT (1X,A,I3,A)
      END
