*     G13DLF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          KMAX, NMAX, IK, IDMAX
      PARAMETER        (KMAX=3,NMAX=100,IK=KMAX,IDMAX=2)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, MAXD, MIND, N, ND
*     .. Local Arrays ..
      DOUBLE PRECISION DELTA(IK,IDMAX), W(IK,NMAX), WORK(KMAX*NMAX),
     +                 Z(IK,NMAX)
      INTEGER          ID(KMAX)
      CHARACTER        TR(KMAX)
*     .. External Subroutines ..
      EXTERNAL         G13DLF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13DLF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) K, N
      IF (K.GT.0 .AND. K.LE.KMAX .AND. N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (ID(I),I=1,K)
         MIND = 0
         MAXD = 0
         DO 20 I = 1, K
            MIND = MIN(MIND,ID(I))
            MAXD = MAX(MAXD,ID(I))
   20    CONTINUE
         IF (MIND.GE.0 .AND. MAXD.LE.IDMAX) THEN
            DO 40 I = 1, K
               READ (NIN,*) (Z(I,J),J=1,N)
   40       CONTINUE
            READ (NIN,*) (TR(I),I=1,K)
            IF (MAXD.GT.0) THEN
               DO 60 I = 1, K
                  READ (NIN,*) (DELTA(I,J),J=1,ID(I))
   60          CONTINUE
            END IF
            IFAIL = 0
*
            CALL G13DLF(K,N,Z,IK,TR,ID,DELTA,W,ND,WORK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Transformed/Differenced series'
            WRITE (NOUT,*) ' ------------------------------'
            DO 80 I = 1, K
               WRITE (NOUT,*)
               WRITE (NOUT,99999) ' Series ', I
               WRITE (NOUT,*) ' -----------'
               WRITE (NOUT,*)
               WRITE (NOUT,99998) ' Number of differenced values = '
     +           , ND
               WRITE (NOUT,*)
               WRITE (NOUT,99997) (W(I,J),J=1,ND)
   80       CONTINUE
*
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,I6)
99997 FORMAT (1X,8F9.3)
      END
