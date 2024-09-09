*     G13BEF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NSERMX, NPMAX, NXXYMX, ISTTF, IXXY, ICM, IWA,
     +                 IMWA
      PARAMETER        (NSERMX=2,NPMAX=10,NXXYMX=50,ISTTF=20,
     +                 IXXY=NXXYMX,ICM=NPMAX,IWA=1500,IMWA=200)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION D, S
      INTEGER          I, IFAIL, ITC, J, KEF, KFC, KPRIV, KZEF, KZSP,
     +                 NDF, NDV, NIT, NPARA, NSER, NSTTF, NXXY
*     .. Local Arrays ..
      DOUBLE PRECISION CM(ICM,NPMAX), PARA(NPMAX), RES(NXXYMX),
     +                 SD(NPMAX), STTF(ISTTF), WA(IWA),
     +                 XXY(IXXY,NSERMX), ZSP(4)
      INTEGER          MR(7), MT(4,NSERMX), MWA(IMWA)
*     .. External Subroutines ..
      EXTERNAL         G13BEF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13BEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) KZEF, KFC, NXXY, NSER, KEF, NIT, KZSP
      IF (NXXY.GT.0 .AND. NXXY.LE.NXXYMX .AND. NSER.GT.0 .AND. NSER.LE.
     +    NSERMX) THEN
         IF (KZSP.NE.0) READ (NIN,*) ZSP
         CALL X04ABF(1,NOUT)
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
            DO 60 I = 1, NXXY
               READ (NIN,*) (XXY(I,J),J=1,NSER)
   60       CONTINUE
*           * Set KPRIV to 1 to obtain monitoring information *
            KPRIV = 0
            IFAIL = 1
*
            CALL G13BEF(MR,NSER,MT,PARA,NPARA,KFC,NXXY,XXY,IXXY,KEF,NIT,
     +                  KZSP,ZSP,ITC,SD,CM,ICM,S,D,NDF,KZEF,RES,STTF,
     +                  ISTTF,NSTTF,WA,IWA,MWA,IMWA,KPRIV,IFAIL)
*
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,99999) 'G13BEF fails. IFAIL =', IFAIL
            END IF
            IF (IFAIL.EQ.0 .OR. IFAIL.EQ.8 .OR. IFAIL.EQ.9 .OR.
     +          IFAIL.EQ.11) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,99999)
     +           'The number of iterations carried out is', ITC
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +    'Final values of the parameters and their standard deviations'
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +           '   I            PARA(I)                 SD'
               WRITE (NOUT,*)
               DO 80 I = 1, NPARA
                  WRITE (NOUT,99998) I, PARA(I), SD(I)
   80          CONTINUE
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'The correlation matrix is'
               WRITE (NOUT,*)
               WRITE (NOUT,99997) ((CM(I,J),J=1,NPARA),I=1,NPARA)
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'The residuals and the z and n values are'
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +           '   I         RES(I)          z(t)           n(t)'
               WRITE (NOUT,*)
               NDV = NXXY - MR(2) - MR(5)*MR(7)
               DO 100 I = 1, NXXY
                  IF (I.LE.NDV) THEN
                     WRITE (NOUT,99996) I, RES(I), (XXY(I,J),J=1,NSER)
                  ELSE
                     WRITE (NOUT,99995) I, (XXY(I,J),J=1,NSER)
                  END IF
  100          CONTINUE
               IF (MR(2).NE.0 .OR. MR(5).NE.0) THEN
                  WRITE (NOUT,*)
                  WRITE (NOUT,*)
     +      '** Note that the residuals relate to differenced values **'
               END IF
               WRITE (NOUT,*)
               WRITE (NOUT,99994) 'The state set consists of', NSTTF,
     +           ' values'
               WRITE (NOUT,*)
               WRITE (NOUT,99993) (STTF(I),I=1,NSTTF)
               WRITE (NOUT,*)
               WRITE (NOUT,99999) 'The number of degrees of freedom is',
     +           NDF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,I4,2F20.6)
99997 FORMAT (1X,5F10.4)
99996 FORMAT (1X,I4,3F15.3)
99995 FORMAT (1X,I4,F30.3,F15.3)
99994 FORMAT (1X,A,I4,A)
99993 FORMAT (1X,6F10.4)
      END
