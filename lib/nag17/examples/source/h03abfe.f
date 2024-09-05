*     H03ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MAMAX, MBMAX, M, MMM
      PARAMETER        (MAMAX=5,MBMAX=5,M=MAMAX+MBMAX,MMM=MAMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION Z
      INTEGER          I, IFAIL, J, L, MA, MB
*     .. Local Arrays ..
      INTEGER          K11(M), K12(M), K15(M), K6(M), K7(M), K8(M),
     +                 K9(M), KOST(MMM,MBMAX)
*     .. External Subroutines ..
      EXTERNAL         H03ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'H03ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) MA, MB
      IF (MA.GT.0 .AND. MA.LE.MAMAX .AND. MB.GT.0 .AND. MB.LE.MBMAX)
     +    THEN
         READ (NIN,*) (K15(I),I=1,MA+MB)
         DO 20 I = 1, MA
            READ (NIN,*) (KOST(I,J),J=1,MB)
   20    CONTINUE
         IFAIL = 0
*
         CALL H03ABF(KOST,MMM,MA,MB,MA+MB,K15,200,K7,K9,L,K6,K8,K11,K12,
     +               Z,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Total cost = ', Z
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Goods from  to'
         WRITE (NOUT,*)
         WRITE (NOUT,99998) (K11(I),K6(I),K8(I),I=1,MA+MB-1)
      END IF
      STOP
*
99999 FORMAT (1X,A,F5.1)
99998 FORMAT (1X,I3,I6,I5)
      END
