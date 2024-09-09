*     G13BJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NSERMX, NPMAX, IPARX, NFVMAX, ISTTF, NEVMAX,
     +                 IXXY, IWA, IMWA
      PARAMETER        (NSERMX=6,NPMAX=10,IPARX=8,NFVMAX=10,ISTTF=20,
     +                 NEVMAX=40,IXXY=NEVMAX+NFVMAX,IWA=1500,IMWA=250)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, KFC, KZEF, N, NEV, NFV, NPARA, NSER,
     +                 NSTTF
*     .. Local Arrays ..
      DOUBLE PRECISION FSD(NFVMAX), FVA(NFVMAX), PARA(NFVMAX),
     +                 PARX(IPARX,NSERMX), RMSXY(NSERMX), STTF(ISTTF),
     +                 WA(IWA), XXY(IXXY,NSERMX)
      INTEGER          MR(7), MRX(7,NSERMX), MT(4,NSERMX), MWA(IMWA)
*     .. External Subroutines ..
      EXTERNAL         G13BJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13BJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) KFC, NEV, NFV, NSER, KZEF
      IF (NSER.GT.0 .AND. NSER.LE.NSERMX .AND. NFV.GT.0 .AND. NFV.LE.
     +    NFVMAX .AND. NEV.GT.0 .AND. NEV.LE.NEVMAX) THEN
         READ (NIN,*) (MR(I),I=1,7)
         DO 20 I = 1, 4
            READ (NIN,*) (MT(I,J),J=1,NSER)
   20    CONTINUE
         NPARA = 0
         DO 40 I = 1, NSER
            NPARA = NPARA + MT(2,I) + MT(3,I)
   40    CONTINUE
         NPARA = NPARA + MR(1) + MR(3) + MR(4) + MR(6) + NSER
         IF (NPARA.LE.NPMAX) THEN
            READ (NIN,*) (PARA(I),I=1,NPARA)
            N = NEV + NFV
            DO 60 I = 1, N
               READ (NIN,*) (XXY(I,J),J=1,NSER)
   60       CONTINUE
            READ (NIN,*) (RMSXY(I),I=1,NSER)
            DO 80 I = 1, 7
               READ (NIN,*) (MRX(I,J),J=1,NSER)
   80       CONTINUE
            DO 100 I = 1, 5
               READ (NIN,*) (PARX(I,J),J=1,NSER)
  100       CONTINUE
            IFAIL = 1
*
            CALL G13BJF(MR,NSER,MT,PARA,NPARA,KFC,NEV,NFV,XXY,IXXY,KZEF,
     +                  RMSXY,MRX,PARX,IPARX,FVA,FSD,STTF,ISTTF,NSTTF,
     +                  WA,IWA,MWA,IMWA,IFAIL)
*
            WRITE (NOUT,*)
            IF (IFAIL.NE.0) WRITE (NOUT,99999) 'G13BJF fails. IFAIL =',
     +          IFAIL
            IF (IFAIL.EQ.0 .OR. IFAIL.EQ.8 .OR. IFAIL.EQ.9 .OR.
     +          IFAIL.EQ.11) THEN
               WRITE (NOUT,99999) 'After processing', NEV,
     +           ' sets of observations'
               WRITE (NOUT,99998) NSTTF,
     +           ' values of the state set are derived'
               WRITE (NOUT,*)
               WRITE (NOUT,99997) (STTF(I),I=1,NSTTF)
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'The residual mean square for the output'
               WRITE (NOUT,99996)
     +           'series is also derived and its value is', RMSXY(NSER)
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +           'The forecast values and their standard errors are'
               WRITE (NOUT,*)
               WRITE (NOUT,*) '   I       FVA       FSD'
               WRITE (NOUT,*)
               DO 120 I = 1, NFV
                  WRITE (NOUT,99995) I, FVA(I), FSD(I)
  120          CONTINUE
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'The values of z(t) and n(t) are'
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +   '   I      z1        z2        z3        z4        z5        n'
               WRITE (NOUT,*)
               DO 140 I = 1, N
                  WRITE (NOUT,99994) I, (XXY(I,J),J=1,NSER)
  140          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I3,A)
99998 FORMAT (1X,I3,A)
99997 FORMAT (1X,6F10.4)
99996 FORMAT (1X,A,F10.4)
99995 FORMAT (1X,I4,F10.3,F10.4)
99994 FORMAT (1X,I4,6F10.3)
      END
