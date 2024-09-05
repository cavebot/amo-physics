*     G05EJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=8)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, K, M
*     .. Local Arrays ..
      INTEGER          IA(N), IB(N)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05EJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05EJF Example Program Results'
      WRITE (NOUT,*)
      CALL G05CBF(0)
      WRITE (NOUT,99999) 'Samples from the first ', N, ' integers'
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Sample size     Values'
      DO 20 I = 1, N
         IA(I) = I
   20 CONTINUE
      DO 40 M = 1, N
         IFAIL = 0
*
         CALL G05EJF(IA,N,IB,M,IFAIL)
*
         WRITE (NOUT,99998) M, (IB(K),K=1,M)
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I1,A)
99998 FORMAT (1X,I6,10X,8I3)
      END
