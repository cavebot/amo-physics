*     G13ADF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NPMAX, NWA, NLMAX
      PARAMETER        (NPMAX=10,NWA=200,NLMAX=50)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RV, YV
      INTEGER          I, IFAIL, NL, NPAR
*     .. Local Arrays ..
      DOUBLE PRECISION PAR(NPMAX), R(NLMAX), WA(NWA)
      INTEGER          ISF(4), MR(7)
*     .. External Subroutines ..
      EXTERNAL         G13ADF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13ADF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NL
      READ (NIN,*) YV
      WRITE (NOUT,*)
      IF (NL.GT.0 .AND. NL.LE.NLMAX) THEN
         READ (NIN,*) (R(I),I=1,NL)
         READ (NIN,*) MR
         NPAR = MR(1) + MR(3) + MR(4) + MR(6)
         IF (NL.GT.0 .AND. NPAR.LE.NPMAX) THEN
            IFAIL = 1
*
            CALL G13ADF(MR,R,NL,YV,NPAR,WA,NWA,PAR,RV,ISF,IFAIL)
*
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,99999) 'G13ADF fails. IFAIL = ', IFAIL
               WRITE (NOUT,*)
            END IF
            IF (IFAIL.EQ.0 .OR. IFAIL.GE.7) THEN
               WRITE (NOUT,99998)
     +           'Parameter estimation success/failure indicator',
     +           (ISF(I),I=1,4)
               WRITE (NOUT,*)
               WRITE (NOUT,99997) 'ARIMA model parameter values ',
     +           (PAR(I),I=1,NPAR)
               WRITE (NOUT,*)
               WRITE (NOUT,99997) 'Residual variance', RV
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I1)
99998 FORMAT (1X,A,4I4)
99997 FORMAT (1X,A,5F10.5)
      END
