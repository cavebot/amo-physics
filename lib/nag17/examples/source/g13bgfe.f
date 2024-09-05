*     G13BGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NSERMX, NPMAX, NSTTFM, NNVMAX, IXXYN, IWA
      PARAMETER        (NSERMX=2,NPMAX=10,NSTTFM=20,NNVMAX=40,
     +                 IXXYN=NNVMAX,IWA=160)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, KZEF, NNV, NPARA, NSER, NSTTF
*     .. Local Arrays ..
      DOUBLE PRECISION PARA(NPMAX), RES(NNVMAX), STTF(NSTTFM), WA(IWA),
     +                 XXYN(IXXYN,NSERMX)
      INTEGER          MR(7), MT(4,NSERMX)
*     .. External Subroutines ..
      EXTERNAL         G13BGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13BGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NSTTF, NSER, NNV, KZEF
      IF (NSTTF.GT.0 .AND. NSTTF.LE.NSTTFM .AND. NSER.GT.0 .AND.
     +    NSER.LE.NSERMX .AND. NNV.GT.0 .AND. NNV.LE.NNVMAX) THEN
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
            DO 60 I = 1, NNV
               READ (NIN,*) (XXYN(I,J),J=1,NSER)
   60       CONTINUE
            IFAIL = 0
*
            CALL G13BGF(STTF,NSTTF,MR,NSER,MT,PARA,NPARA,NNV,XXYN,IXXYN,
     +                  KZEF,RES,WA,IWA,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'The updated state set'
            WRITE (NOUT,99999) (STTF(I),I=1,NSTTF)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'The residuals (after differencing)'
            DO 80 I = 1, NNV
               WRITE (NOUT,99998) I, RES(I)
   80       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'The values of z(t) and n(t)'
            DO 100 I = 1, NNV
               WRITE (NOUT,99998) I, (XXYN(I,J),J=1,NSER)
  100       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,6F10.4)
99998 FORMAT (1X,I4,2F10.4)
      END
