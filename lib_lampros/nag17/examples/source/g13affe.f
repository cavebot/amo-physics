*     G13AFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NPMAX, NPC, ICM, NXMAX, IRSMAX
      PARAMETER        (NPMAX=10,NPC=NPMAX+1,ICM=NPC,NXMAX=50,
     +                 IRSMAX=550)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION C, S
      INTEGER          I, IFAIL, IPD, IQD, IRES, ITC, J, KFC, KPIV, NDF,
     +                 NIT, NPAR, NPPC, NRES, NST, NX
*     .. Local Arrays ..
      DOUBLE PRECISION CM(ICM,NPC), PAR(NPMAX), RES(IRSMAX), SD(NPC),
     +                 ST(NXMAX), X(NXMAX)
      INTEGER          ISF(4), MR(7)
*     .. External Subroutines ..
      EXTERNAL         G13AFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13AFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX, (MR(I),I=1,7)
      WRITE (NOUT,*)
      IF (NX.GT.0 .AND. NX.LE.NXMAX) THEN
         READ (NIN,*) (X(I),I=1,NX)
         NPAR = MR(1) + MR(3) + MR(4) + MR(6)
         IF (NPAR.GT.0 .AND. NPAR.LE.NPMAX) THEN
            DO 20 I = 1, NPAR
               PAR(I) = 0.0D0
   20       CONTINUE
            KFC = 1
            C = 0.0D0
            NPPC = NPAR + KFC
*           * Set KPIV to 1 to obtain monitoring information *
            KPIV = 0
            NIT = 50
            IQD = MR(6)*MR(7) + MR(3)
            IPD = MR(4)*MR(7) + MR(1)
            IRES = 15*IQD + 11*NX + 13*NPPC + 8*IPD + 12 + 2*(IQD+NPPC)
     +             **2
            IF (IRES.LE.IRSMAX) THEN
               IFAIL = 1
*
               CALL G13AFF(MR,PAR,NPAR,C,KFC,X,NX,S,NDF,SD,NPPC,CM,ICM,
     +                     ST,NST,KPIV,NIT,ITC,ISF,RES,IRES,NRES,IFAIL)
*
               IF (IFAIL.NE.0) WRITE (NOUT,99997)
     +             'G13AFF fails. IFAIL = ', IFAIL
               IF (IFAIL.EQ.0 .OR. IFAIL.GE.7) THEN
                  WRITE (NOUT,99996) 'Convergence was achieved after',
     +              ITC, ' cycles'
                  WRITE (NOUT,*)
                  WRITE (NOUT,*)
     +'Final values of the PAR parameters and the constant are as follow
     +s'
                  WRITE (NOUT,99995) (PAR(I),I=1,NPAR), C
                  WRITE (NOUT,*)
                  WRITE (NOUT,99994) 'Residual sum of squares is', S,
     +              '  with', NDF, ' degrees of freedom'
                  IF ((IFAIL.EQ.0 .OR. IFAIL.EQ.9) .AND. ITC.GT.0) THEN
                     WRITE (NOUT,*)
                     WRITE (NOUT,*) 'The corresponding SD array holds'
                     WRITE (NOUT,99993) (SD(I),I=1,NPPC)
                     WRITE (NOUT,*)
                     WRITE (NOUT,*)
     +                 'The correlation matrix is as follows'
                     DO 40 I = 1, NPPC
                        WRITE (NOUT,99992) (CM(I,J),J=1,NPPC)
   40                CONTINUE
                  END IF
                  IF (IFAIL.EQ.0 .OR. IFAIL.EQ.9) THEN
                     WRITE (NOUT,*)
                     WRITE (NOUT,99999) 'The residuals consist of',
     +                 NRES, ' values'
                     WRITE (NOUT,99998) (RES(I),I=1,NRES)
                     WRITE (NOUT,*)
                     WRITE (NOUT,99996) 'The state set consists of',
     +                 NST, ' values'
                     WRITE (NOUT,99992) (ST(I),I=1,NST)
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I4,A)
99998 FORMAT (1X,5F10.4)
99997 FORMAT (1X,A,I2)
99996 FORMAT (1X,A,I3,A)
99995 FORMAT (1X,4F10.4)
99994 FORMAT (1X,A,F10.3,A,I4,A)
99993 FORMAT (1X,10F9.4)
99992 FORMAT (1X,6F11.3)
      END
