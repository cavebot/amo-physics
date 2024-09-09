*     G13BHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NSERMX, NFVMAX, NPMAX, IPARX, NSTTFM, IXXYN, IWA
      PARAMETER        (NSERMX=2,NFVMAX=40,NPMAX=10,IPARX=NPMAX,
     +                 NSTTFM=20,IXXYN=NFVMAX,IWA=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, KZEF, NFV, NPARA, NSER, NSTTF
*     .. Local Arrays ..
      DOUBLE PRECISION FSD(NFVMAX), FVA(NFVMAX), PARA(NPMAX),
     +                 PARX(IPARX,NSERMX), RMSXY(NSERMX), STTF(NSTTFM),
     +                 WA(IWA), XXYN(IXXYN,NSERMX)
      INTEGER          MR(7), MRX(7,NSERMX), MT(4,NSERMX)
*     .. External Subroutines ..
      EXTERNAL         G13BHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13BHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NSTTF, NSER, NFV, KZEF
      IF (NSTTF.GT.0 .AND. NSTTF.LE.NSTTFM .AND. NSER.GT.0 .AND.
     +    NSER.LE.NSERMX .AND. NFV.GT.0 .AND. NFV.LE.NFVMAX) THEN
         READ (NIN,*) (MR(I),I=1,7)
         DO 20 I = 1, 4
            READ (NIN,*) (MT(I,J),J=1,NSER)
   20    CONTINUE
         NPARA = 0
         DO 40 I = 1, NSER
            NPARA = NPARA + MT(2,I) + MT(3,I)
   40    CONTINUE
         NPARA = NPARA + MR(1) + MR(3) + MR(4) + MR(6) + NSER
         READ (NIN,*) (STTF(I),I=1,NSTTF)
         IF (NPARA.LE.NPMAX) THEN
            READ (NIN,*) (PARA(I),I=1,NPARA)
            DO 60 I = 1, NFV
               READ (NIN,*) (XXYN(I,J),J=1,NSER)
   60       CONTINUE
            DO 80 I = 1, 7
               READ (NIN,*) (MRX(I,J),J=1,NSER)
   80       CONTINUE
            DO 100 I = 1, NPARA
               READ (NIN,*) (PARX(I,J),J=1,NSER)
  100       CONTINUE
            READ (NIN,*) (RMSXY(I),I=1,NSER)
            IFAIL = 0
*
            CALL G13BHF(STTF,NSTTF,MR,NSER,MT,PARA,NPARA,NFV,XXYN,IXXYN,
     +                  MRX,PARX,IPARX,RMSXY,KZEF,FVA,FSD,WA,IWA,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        'The forecast values and their standard errors'
            WRITE (NOUT,*)
            WRITE (NOUT,*) '   I     FVA       FSD'
            WRITE (NOUT,*)
            DO 120 I = 1, NFV
               WRITE (NOUT,99999) I, FVA(I), FSD(I)
  120       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'The values of z(t) and n(t)'
            DO 140 I = 1, NFV
               WRITE (NOUT,99999) I, (XXYN(I,J),J=1,NSER)
  140       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,I4,2F10.4)
      END
