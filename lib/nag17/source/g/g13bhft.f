      SUBROUTINE G13BHF(STTF,NSTTF,MR,NSER,MT,PARA,NPARA,NFV,XXYN,IXXYN,
     *                  MRX,PARX,IPARX,RMSXY,KZEF,FVA,FSD,WA,IWA,IFAIL)
C     MARK 11 RELEASE. NAG COPYRIGHT 1983.
C     MARK 11A REVISED. IER-454 (JUN 1984).
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     SUBROUTINE G13BHF DERIVES FORECAST VALUES AND THEIR
C     STANDARD DEVIATIONS IN A TRANSFER FUNCTION CONTEXT
C
C
C     MXA,MXB,MXC AND MXD ARE USED IN DERIVATION OF IWAA
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='G13BHF')
C     .. Scalar Arguments ..
      INTEGER           IFAIL, IPARX, IWA, IXXYN, KZEF, NFV, NPARA,
     *                  NSER, NSTTF
C     .. Array Arguments ..
      DOUBLE PRECISION  FSD(NFV), FVA(NFV), PARA(NPARA),
     *                  PARX(IPARX,NSER), RMSXY(NSER), STTF(NSTTF),
     *                  WA(IWA), XXYN(IXXYN,NSER)
      INTEGER           MR(7), MRX(7,NSER), MT(4,NSER)
C     .. Local Scalars ..
      INTEGER           I, IERROR, IPS, IWAA, JSTTF, KQ, KSAAL, KSAEX,
     *                  KSPSI, KSZN, LPARA, LSTTF, MXA, MXB, MXC, MXD,
     *                  MXE, ND, NDD, NDS, NGW, NNB, NNP, NNQ, NNR, NP,
     *                  NPAR, NPARX, NPD, NPS, NPX, NQ, NQD, NQS, NS,
     *                  NWD, NXS
C     .. Local Arrays ..
      INTEGER           MPQS(4)
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. External Subroutines ..
      EXTERNAL          G13AJY, G13BEX, G13BHZ
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Executable Statements ..
      MXB = 0
      MXC = 0
      MXD = 1
C
C     G13AJY DERIVES INTEGERS ASSOCIATED WITH ORDERS
C     ARRAY FOR Y SERIES
C
      CALL G13AJY(MR,NP,ND,NQ,NPS,NDS,NQS,NS,NPD,NDD,NQD,MPQS,NPAR)
      MXA = NPAR
C
C     LSTTF AND LPARA GIVE THE NECESSARY LENGTHS OF STTF
C     AND PARA
C
      LSTTF = NS*NPS + NDD + NQ + MAX(NP,NS*NQS)
      JSTTF = NSTTF
      LPARA = NPAR + 1
      IF (NSER.LE.1) GO TO 40
      NXS = NSER - 1
C
C     UPDATE LSTTF,LPARA,ETC. FOR EACH X SERIES IN TURN
C
      IERROR = 6
      DO 20 I = 1, NXS
         IF (MT(4,I).LT.1 .OR. MT(4,I).GT.3) GO TO 60
         CALL G13BEX(MT,I,NSER,NNB,NNP,NNQ,NNR,NWD,NGW,NPX)
         KQ = NNB + NNQ + NNP
         LSTTF = LSTTF + KQ
         MXE = MAX(MRX(1,I),(MRX(7,I)*MRX(6,I)))
         MXE = MXE + MRX(7,I)*(MRX(4,I)+MRX(5,I)) + MRX(2,I) + MRX(3,I)
         JSTTF = MAX(JSTTF,MXE)
         LPARA = LPARA + NWD
         NPARX = MRX(1,I) + MRX(3,I) + MRX(4,I) + MRX(6,I)
         MXD = MAX(MXD,NPARX)
         MXB = MAX(MXB,NPX)
         MXC = MAX(MXC,KQ)
   20 CONTINUE
   40 MXB = MXB + NFV
      MXC = MXC + 1
      MXA = MAX(MXA,MXD)
C
C     SKIP OUT IF AN ERROR IS FOUND
C
      IERROR = 1
      IF (NSTTF.NE.LSTTF) GO TO 60
      IERROR = 2
      IF (NPARA.NE.LPARA) GO TO 60
      IERROR = 3
      IF (NFV.GT.IXXYN) GO TO 60
      IPS = MAX(JSTTF,MXA,MXB,MXC)
      IWAA = MAX(JSTTF+4*MXA,IPS)
      KQ = IWAA + 3*IPS + NFV
      IERROR = 4
      IF (IWA.LT.KQ) GO TO 60
      IERROR = 5
      IF (IPARX.LT.MXD) GO TO 60
C
C     DERIVE START POINTS OF COMPONENT ARRAYS OF WA.
C     WA STARTS IMPLICITLY AT WA(1).
C
      KSPSI = IWAA + 1
      KSAEX = KSPSI + IPS
      KSAAL = KSAEX + IPS
      KSZN = KSAAL + IPS
C
C     G13BHZ IS THE AUXILIARY ROUTINE WHICH CARRIES OUT
C     THE APPROPRIATE CALCULATIONS
C
      CALL G13BHZ(MR,PARA,NPARA,STTF,NSTTF,MT,NSER,MRX,PARX,IPARX,XXYN,
     *            IXXYN,RMSXY,NFV,KZEF,FVA,FSD,WA(KSPSI),WA(KSAEX)
     *            ,WA(KSAAL),WA(KSZN),IPS,WA,IWAA,0)
      IFAIL = 0
      RETURN
   60 IFAIL = P01ABF(IFAIL,IERROR,SRNAME,0,P01REC)
      RETURN
      END
