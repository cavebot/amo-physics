*     F04MAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, N, LICN, LIRN, NN
      PARAMETER        (M=4,N=4,LICN=90,LIRN=50,NN=N*M)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DENSW, DROPTL
      INTEGER          I, IFAIL, J, JJ, K, MM1, NZ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LICN), ACC(2), B(NN), WKEEP(NN,3), WORK(NN,3)
      INTEGER          ICN(LICN), IKEEP(NN,2), INFORM(4), IRN(LIRN),
     +                 IWORK(NN,6), NOITS(2)
      LOGICAL          ABORT(3)
*     .. External Subroutines ..
      EXTERNAL         F01MAF, F04MAF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04MAF Example Program Results'
      CALL X04ABF(1,NOUT)
*
*     Initialise the right hand side vector B.
*
      MM1 = M - 1
      DO 40 I = 1, N
         K = (I-1)*M
         B(K+1) = 0.25D0
         DO 20 J = 2, MM1
            B(K+J) = 0.0D0
   20    CONTINUE
         B(K+M) = 0.25D0
   40 CONTINUE
      K = N*MM1
      DO 60 I = 1, M
         B(I) = B(I) + 0.25D0
         B(K+I) = B(K+I) + 0.25D0
   60 CONTINUE
*
*     Set up the coefficient matrix A.
*
      K = 0
      DO 80 I = 1, NN
         K = K + 1
         A(K) = 1.0D0
         IRN(K) = I
         ICN(K) = I
   80 CONTINUE
      DO 120 I = 1, N
         DO 100 J = 1, MM1
            K = K + 1
            JJ = (I-1)*N + J
            A(K) = -0.25D0
            IRN(K) = JJ
            ICN(K) = JJ + 1
  100    CONTINUE
  120 CONTINUE
      DO 140 I = N + 1, NN
         K = K + 1
         A(K) = -0.25D0
         IRN(K) = I - N
         ICN(K) = I
  140 CONTINUE
      NZ = K
*
*     Call F01MAF to perform the incomplete Cholesky factorization of
*     the sparse, symmetric, positive definite matrix A.
*
      DO 160 I = 1, 3
         ABORT(I) = .TRUE.
  160 CONTINUE
      DROPTL = 0.1D0
      DENSW = 0.8D0
      IFAIL = 111
*
      CALL F01MAF(NN,NZ,A,LICN,IRN,LIRN,ICN,DROPTL,DENSW,WKEEP,IKEEP,
     +            IWORK,ABORT,INFORM,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999)
     +     ' Error IFAIL returned from F01MAF with value', IFAIL
      ELSE
         WRITE (NOUT,99999) ' No. of elements of A ( and ICN ) used is '
     +     , INFORM(1)
         WRITE (NOUT,99999) ' No. of elements of IRN used is           '
     +     , INFORM(2)
         WRITE (NOUT,*)
*
*        Call F04MAF to solve the sparse, symmetric, positive definite
*        system of linear equations  A*X = B.
*
         NOITS(1) = 50
         ACC(1) = 0.0001D0
         IFAIL = 111
*
         CALL F04MAF(NN,NZ,A,LICN,IRN,LIRN,ICN,B,ACC,NOITS,WKEEP,WORK,
     +               IKEEP,INFORM,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999)
     +        ' Error IFAIL returned from F04MAF with value', IFAIL
         ELSE
            WRITE (NOUT,99999) ' Number of iterations = ', NOITS(2)
            WRITE (NOUT,99998) ' Norm of residual     = ', ACC(2)
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Solution vector'
            WRITE (NOUT,99997) (B(I),I=1,NN)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,A,D12.3)
99997 FORMAT (1X,4D15.5)
      END
