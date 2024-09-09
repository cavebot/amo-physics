*     G05EHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=8)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, M
*     .. Local Arrays ..
      INTEGER          INDEX(N)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05EHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05EHF Example Program Results'
      WRITE (NOUT,*)
      M = 10
      CALL G05CBF(0)
      WRITE (NOUT,99998) M, ' Permutations of first ', N, ' integers'
      WRITE (NOUT,*)
      DO 40 J = 1, M
         DO 20 I = 1, N
            INDEX(I) = I
   20    CONTINUE
         IFAIL = 0
*
         CALL G05EHF(INDEX,N,IFAIL)
*
         WRITE (NOUT,99999) (INDEX(K),K=1,N)
   40 CONTINUE
      STOP
*
99999 FORMAT (1X,8I3)
99998 FORMAT (1X,I2,A,I1,A)
      END
