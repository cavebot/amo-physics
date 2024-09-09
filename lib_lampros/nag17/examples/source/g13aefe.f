*     G13AEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NPMAX, IEXMAX, NXMAX, IGHMAX, ISTMAX, IH, IWAMAX
      PARAMETER        (NPMAX=10,IEXMAX=50,NXMAX=50,IGHMAX=10,ISTMAX=10,
     +                 IH=IGHMAX,IWAMAX=350)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION C, S
      INTEGER          I, IEX, IFAIL, IGH, IST, ITC, IWA, J, KFC, KPIV,
     +                 KZSP, NDF, NEX, NGH, NIT, NPAR, NST, NX
*     .. Local Arrays ..
      DOUBLE PRECISION AL(IEXMAX), EX(IEXMAX), EXR(IEXMAX), G(IGHMAX),
     +                 H(IH,IGHMAX), HC(IH,IGHMAX), PAR(NPMAX),
     +                 SD(IGHMAX), ST(ISTMAX), WA(IWAMAX), X(NXMAX),
     +                 ZSP(4)
      INTEGER          ICOUNT(6), ISF(4), MR(7)
*     .. External Subroutines ..
      EXTERNAL         G13AEF, PIV
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13AEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX, (MR(I),I=1,7)
      IF (NX.GT.0 .AND. NX.LE.NXMAX) THEN
         READ (NIN,*) (X(I),I=1,NX)
         NPAR = MR(1) + MR(3) + MR(4) + MR(6)
         IF (NPAR.GT.0 .AND. NPAR.LE.NPMAX) THEN
            DO 20 I = 1, NPAR
               PAR(I) = 0.0D0
   20       CONTINUE
            KFC = 1
            C = 0.0D0
            IEX = MR(3) + (MR(6)*MR(7)) + NX
            IGH = MR(3) + (MR(6)*MR(7)) + NPAR + KFC
            IST = (MR(4)*MR(7)) + MR(2) + (MR(5)*MR(7)) + MR(3) +
     +            MAX(MR(1),(MR(6)*MR(7)))
*           * Set KPIV to 1 to obtain monitoring information *
            KPIV = 0
            NIT = 25
            ZSP(1) = 0.001D0
            ZSP(2) = 10.0D0
            ZSP(3) = 1000.0D0
            ZSP(4) = 0.0001D0
            KZSP = 1
            IWA = ((NX+1+MR(1)+(MR(4)*MR(7))+MR(3)+(MR(6)*MR(7)))*8) +
     +            (9*NPAR)
            IF (IEX.LE.IEXMAX .AND. IGH.LE.IGHMAX .AND. IST.LE.
     +          ISTMAX .AND. IWA.LE.IWAMAX) THEN
               IFAIL = 1
*
               CALL G13AEF(MR,PAR,NPAR,C,KFC,X,NX,ICOUNT,EX,EXR,AL,IEX,
     +                     S,G,IGH,SD,H,IH,ST,IST,NST,PIV,KPIV,NIT,ITC,
     +                     ZSP,KZSP,ISF,WA,IWA,HC,IFAIL)
*
               IF (IFAIL.NE.0) WRITE (NOUT,99999)
     +             'G13AEF fails. IFAIL = ', IFAIL
               IF (IFAIL.EQ.0 .OR. IFAIL.GE.7) THEN
                  NEX = ICOUNT(4)
                  NDF = ICOUNT(5)
                  NGH = ICOUNT(6)
                  WRITE (NOUT,*)
                  WRITE (NOUT,99998) 'Convergence was achieved after',
     +              ITC, ' cycles'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*)
     +'Final values of the PAR parameters and the constant are as follow
     +s'
                  WRITE (NOUT,99997) (PAR(I),I=1,NPAR), C
                  WRITE (NOUT,*)
                  WRITE (NOUT,99996) 'Residual sum of squares is', S,
     +              '  with', NDF, ' degrees of freedom'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) 'The final values of ZSP were'
                  WRITE (NOUT,99995) (ZSP(I),I=1,4)
                  WRITE (NOUT,*)
                  WRITE (NOUT,99999)
     +              'The number of parameters estimated was', NGH
                  WRITE (NOUT,*)
     +              '( backward forecasts, PAR and C, in that order )'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) 'The corresponding G array holds'
                  WRITE (NOUT,99994) (G(I),I=1,NGH)
                  IF ((IFAIL.EQ.0 .OR. IFAIL.EQ.9) .AND. ITC.GT.0) THEN
                     WRITE (NOUT,*)
                     WRITE (NOUT,*) 'The corresponding SD array holds'
                     WRITE (NOUT,99994) (SD(I),I=1,NGH)
                     WRITE (NOUT,*)
                     WRITE (NOUT,*)
     +             'The corresponding H matrix holds second derivatives'
                     WRITE (NOUT,*)
     +                 'in the upper half (including the main diagonal)'
                     WRITE (NOUT,*)
     +              'and correlation coefficients in the lower triangle'
                     DO 40 I = 1, NGH
                        WRITE (NOUT,99993) (H(I,J),J=1,NGH)
   40                CONTINUE
                  END IF
                  WRITE (NOUT,*)
                  WRITE (NOUT,99992) 'EX, EXR, and AL each hold', NEX,
     +              ' values made up of', ICOUNT(1),
     +              ' back forecast(s),'
                  WRITE (NOUT,99991) ICOUNT(2),
     +              ' differenced values, and'
                  WRITE (NOUT,99991) ICOUNT(3),
     +              ' element(s) of reconstituted information'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) '  EX'
                  WRITE (NOUT,99990) (EX(I),I=1,NEX)
                  IF (IFAIL.EQ.0 .OR. IFAIL.EQ.9) THEN
                     WRITE (NOUT,*)
                     WRITE (NOUT,*) '  EXR'
                     WRITE (NOUT,99990) (EXR(I),I=1,NEX)
                  END IF
                  IF (IFAIL.EQ.0) THEN
                     WRITE (NOUT,*)
                     WRITE (NOUT,*) '  AL'
                     WRITE (NOUT,99990) (AL(I),I=1,NEX)
                  END IF
                  IF (IFAIL.EQ.0 .OR. IFAIL.EQ.9) THEN
                     WRITE (NOUT,*)
                     WRITE (NOUT,99998) 'The state set consists of',
     +                 NST, ' values'
                     WRITE (NOUT,99990) (ST(I),I=1,NST)
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,I3,A)
99997 FORMAT (1X,4F10.4)
99996 FORMAT (1X,A,F10.3,A,I4,A)
99995 FORMAT (1X,4D15.4)
99994 FORMAT (1X,10F9.4)
99993 FORMAT (1X,6F11.3)
99992 FORMAT (1X,A,I5,A,I5,A)
99991 FORMAT (1X,I5,A)
99990 FORMAT (1X,5F11.4)
      END
*
      SUBROUTINE PIV(MR,PAR,NPAR,C,KFC,ICOUNT,S,G,H,IH,NGH,ITC,ZSP)
*     .. Parameters ..
      INTEGER        NOUT
      PARAMETER      (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION C, S
      INTEGER        IH, ITC, KFC, NGH, NPAR
*     .. Array Arguments ..
      DOUBLE PRECISION G(NGH), H(IH,NGH), PAR(NPAR), ZSP(4)
      INTEGER        ICOUNT(6), MR(7)
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Iteration', ITC,
     +  '  residual sum of squares = ', S
      RETURN
*
99999 FORMAT (1X,A,I3,A,D11.4)
      END
