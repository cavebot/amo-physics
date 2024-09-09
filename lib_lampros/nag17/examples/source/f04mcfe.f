*     F04MCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IRMAX, NRB, NRX, LALMAX
      PARAMETER        (NMAX=6,IRMAX=2,NRB=NMAX,NRX=NMAX,LALMAX=14)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IR, ISELEC, K, K1, K2, LAL, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LALMAX), AL(LALMAX), B(NRB,IRMAX), D(NMAX),
     +                 X(NRX,IRMAX)
      INTEGER          NROW(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01MCF, F04MCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04MCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (NROW(I),I=1,N)
         K2 = 0
         DO 20 I = 1, N
            K1 = K2 + 1
            K2 = K2 + NROW(I)
            READ (NIN,*) (A(K),K=K1,K2)
   20    CONTINUE
         READ (NIN,*) IR
         IF (IR.GT.0 .AND. IR.LE.IRMAX) THEN
            READ (NIN,*) ((B(I,K),K=1,IR),I=1,N)
            LAL = K2
            IF (LAL.LE.LALMAX) THEN
               IFAIL = 1
*
               CALL F01MCF(N,A,LAL,NROW,AL,D,IFAIL)
*
               IF (IFAIL.EQ.0) THEN
                  ISELEC = 1
                  IFAIL = 1
*
                  CALL F04MCF(N,AL,LAL,D,NROW,IR,B,NRB,ISELEC,X,NRX,
     +                        IFAIL)
*
                  IF (IFAIL.EQ.0) THEN
                     WRITE (NOUT,*) ' Solution'
                     DO 40 I = 1, N
                        WRITE (NOUT,99998) (X(I,K),K=1,IR)
   40                CONTINUE
                  ELSE
                     WRITE (NOUT,99999) 'F04MCF fails with IFAIL =',
     +                 IFAIL
                  END IF
               ELSE
                  WRITE (NOUT,99999) 'F01MCF fails with IFAIL =', IFAIL
               END IF
            ELSE
               WRITE (NOUT,*)
               WRITE (NOUT,99999) 'LAL is out of range: LAL = ', LAL
            END IF
         ELSE
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'IR is out of range: IR = ', IR
         END IF
      ELSE
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,8F9.3)
      END
