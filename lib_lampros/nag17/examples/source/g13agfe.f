*     G13AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NPMAX, NSTMAX, NNVMAX, NWAMAX
      PARAMETER        (NPMAX=10,NSTMAX=50,NNVMAX=50,NWAMAX=150)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION C
      INTEGER          I, IFAIL, NNV, NPAR, NST, NWA
*     .. Local Arrays ..
      DOUBLE PRECISION ANV(NNVMAX), ANVR(NNVMAX), PAR(NPMAX),
     +                 ST(NSTMAX), WA(NWAMAX)
      INTEGER          MR(7)
*     .. External Subroutines ..
      EXTERNAL         G13AGF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NNV
      READ (NIN,*) (MR(I),I=1,7)
      NPAR = MR(1) + MR(3) + MR(4) + MR(6)
      NST = MR(4)*MR(7) + MR(5)*MR(7) + MR(2) + MR(3) + MAX(MR(1),MR(6)
     +      *MR(7))
      NWA = 4*NPAR + 3*NST
      IF (NNV.GT.0 .AND. NNV.LE.NNVMAX .AND. NPAR.GT.0 .AND. NPAR.LE.
     +    NPMAX .AND. NST.GT.0 .AND. NST.LE.NSTMAX) THEN
         READ (NIN,*) (PAR(I),I=1,NPAR), C
         READ (NIN,*) (ST(I),I=1,NST)
         READ (NIN,*) (ANV(I),I=1,NNV)
         IFAIL = 0
*
         CALL G13AGF(ST,NST,MR,PAR,NPAR,C,ANV,NNV,ANVR,WA,NWA,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'The updated state set array now holds the values'
         WRITE (NOUT,99999) (ST(I),I=1,NST)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'The residuals corresponding to the', NNV
         WRITE (NOUT,*) 'values used to update the system are'
         WRITE (NOUT,99999) (ANVR(I),I=1,NNV)
      END IF
      STOP
*
99999 FORMAT (1X,8F8.4)
99998 FORMAT (1X,A,I3,A)
      END
