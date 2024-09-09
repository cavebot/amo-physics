*     G08ALF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          LDX, KMAX
      PARAMETER        (LDX=20,KMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION DF, P, Q
      INTEGER          I, IFAIL, J, K, N
*     .. Local Arrays ..
      DOUBLE PRECISION X(LDX,KMAX)
*     .. External Subroutines ..
      EXTERNAL         G08ALF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G08ALF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, K
      IF (N.LE.LDX .AND. K.LE.KMAX) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Cochrans Q test'
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Data matrix'
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,K)
            WRITE (NOUT,99999) (X(I,J),J=1,K)
   20    CONTINUE
         IFAIL = 0
*
         CALL G08ALF(N,K,X,LDX,Q,P,IFAIL)
*
         DF = DBLE(K-1)
         WRITE (NOUT,*)
         WRITE (NOUT,99998) 'Cochrans Q test statistic = ', Q
         WRITE (NOUT,99997) 'Degrees of freedom = ', DF
         WRITE (NOUT,99998) 'Upper-tail probability = ', P
      END IF
      STOP
*
99999 FORMAT (1X,3F6.1)
99998 FORMAT (1X,A,F12.4)
99997 FORMAT (1X,A,F6.1)
      END
