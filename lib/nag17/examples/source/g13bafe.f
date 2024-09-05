*     G13BAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NXMAX, NPMAX, ISTMAX, IFVMAX, IW, IQXDMX, NYMAX,
     +                 NBMAX
      PARAMETER        (NXMAX=300,NPMAX=10,ISTMAX=20,IFVMAX=2,IW=2200,
     +                 IQXDMX=5,NYMAX=NXMAX+IQXDMX,NBMAX=320)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A1, A2, CX, CY, RMS
      INTEGER          I, IDD, IDW, IFAIL, II, IJ, IQXD, J, K, N, NB,
     +                 NI, NMR, NPAR, NPARX, NST, NWA, NX, NY
*     .. Local Arrays ..
      DOUBLE PRECISION B(NBMAX), FSD(IFVMAX), FVA(IFVMAX), PAR(NPMAX),
     +                 PARX(NPMAX), ST(ISTMAX), WA(IW), X(NXMAX),
     +                 Y(NYMAX)
      INTEGER          ISF(4), MR(14), MRX(7)
*     .. External Subroutines ..
      EXTERNAL         G13AJF, G13BAF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN, MOD
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX
      IF (NX.GT.0 .AND. NX.LE.NXMAX) THEN
         READ (NIN,*) (X(I),I=1,NX)
*        Read univariate ARIMA for series
         READ (NIN,*) (MRX(I),I=1,7)
         READ (NIN,*) CX
         NPARX = MRX(1) + MRX(3) + MRX(4) + MRX(6)
         IF (NPARX.GT.0 .AND. NPARX.LE.NPMAX) THEN
            READ (NIN,*) (PARX(I),I=1,NPARX)
*           Read model by which to filter series
            READ (NIN,*) (MR(I),I=1,7)
            NPAR = MR(1) + MR(3) + MR(4) + MR(6)
            IF (NPAR.GT.0 .AND. NPAR+NPARX.LE.NPMAX) THEN
               READ (NIN,*) (PAR(I),I=1,NPAR)
*              Initially backforecast QY values
*              (1) Reverse series in situ
               N = NX/2
               NI = NX
               DO 20 I = 1, N
                  A1 = X(I)
                  A2 = X(NI)
                  X(I) = A2
                  X(NI) = A1
                  NI = NI - 1
   20          CONTINUE
               IDD = MRX(2) + MRX(5)
*              (2) Possible sign reversal for ARIMA constant
               IF (MOD(IDD,2).NE.0) CX = -CX
*              (3) Calculate number of backforecasts required
               IQXD = MRX(3) + MRX(6)*MRX(7)
               IF (IQXD.NE.0) THEN
*                 (4) Set up parameter list for call to forecast
*                 routine G13AJF
                  IDW = IW
                  IFAIL = 0
*
                  CALL G13AJF(MRX,PARX,NPARX,CX,1,X,NX,RMS,ST,ISTMAX,
     +                        NST,IQXD,FVA,FSD,IFVMAX,ISF,WA,IW,IFAIL)
*
                  J = IQXD
                  DO 40 I = 1, IQXD
                     Y(I) = FVA(J)
                     J = J - 1
   40             CONTINUE
*                 Move series into Y
                  J = IQXD + 1
                  K = NX
                  DO 60 I = 1, NX
                     IF (J.GT.NYMAX) STOP
                     Y(J) = X(K)
                     J = J + 1
                     K = K - 1
   60             CONTINUE
               END IF
*              Calculate series length
               NY = NX + IQXD
*              Move ARIMA for series into MR
               DO 80 I = 1, 7
                  MR(7+I) = MRX(I)
   80          CONTINUE
*              Move parameters of ARIMA for Y into PAR
               DO 100 I = 1, NPARX
                  PAR(NPAR+I) = PARX(I)
  100          CONTINUE
               NPAR = NPAR + NPARX
*              Move constant and reset sign reversal
               CY = CX
               IF (MOD(IDD,2).NE.0) CY = -CY
*              Set parameters for call to filter routine G13BAF
               NMR = 14
               NWA = MR(3) + MR(6)*MR(7) + MR(8) + MR(9) + (MR(11)
     +               +MR(12))*MR(14)
               NWA = NWA*(NWA+2)
               NB = NY + MAX(MR(3)+MR(6)*MR(7),MR(1)+MR(2)+(MR(4)+MR(5))
     +              *MR(7))
               IF (NWA.LE.IW .AND. NB.LE.NBMAX) THEN
                  IFAIL = 0
*                 Filter series by call to G13BAF
                  CALL G13BAF(Y,NY,MR,NMR,PAR,NPAR,CY,WA,NWA,B,NB,IFAIL)
*
                  WRITE (NOUT,*)
                  WRITE (NOUT,*)
     +              '                 Original        Filtered'
                  WRITE (NOUT,*)
     +              'Backforecasts    y-series         series'
                  IF (IQXD.NE.0) THEN
                     IJ = -IQXD
                     DO 120 I = 1, IQXD
                        WRITE (NOUT,99999) IJ, Y(I), B(I)
                        IJ = IJ + 1
  120                CONTINUE
                  END IF
                  WRITE (NOUT,*)
                  WRITE (NOUT,*)
     + '       Filtered        Filtered        Filtered        Filtered'
                  WRITE (NOUT,*)
     +  '        series          series          series          series'
                  DO 140 I = IQXD + 1, NY, 4
                     WRITE (NOUT,99998) (II-IQXD,B(II),II=I,MIN(NY,I+3))
  140             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,I8,F17.4,F15.4)
99998 FORMAT (1X,I5,F9.4,I7,F9.4,I7,F9.4,I7,F9.4)
      END
