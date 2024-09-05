*     F01MCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, LALMAX
      PARAMETER        (NMAX=8,LALMAX=36)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, K, K1, K2, LAL, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LALMAX), AL(LALMAX), D(NMAX)
      INTEGER          NROW(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01MCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01MCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (NROW(I),I=1,N)
         K2 = 0
         DO 20 I = 1, N
            K1 = K2 + 1
            K2 = K2 + NROW(I)
            READ (NIN,*) (A(K),K=K1,K2)
   20    CONTINUE
         LAL = K2
         IF (LAL.LE.LALMAX) THEN
            IFAIL = 1
*
            CALL F01MCF(N,A,LAL,NROW,AL,D,IFAIL)
*
            WRITE (NOUT,*)
            IF (IFAIL.EQ.0) THEN
               WRITE (NOUT,*)
     +           '  I    D(I)   Row I of unit lower triangle'
               WRITE (NOUT,*)
               K2 = 0
               DO 40 I = 1, N
                  K1 = K2 + 1
                  K2 = K2 + NROW(I)
                  WRITE (NOUT,99999) I, D(I), (AL(K),K=K1,K2)
   40          CONTINUE
            ELSE
               WRITE (NOUT,99998) 'F01MCF fails with IFAIL =', IFAIL
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,I3,7F8.3)
99998 FORMAT (1X,A,I3)
      END
