*     F02WDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX, NRA, NRR, NRPT, LWORK
      PARAMETER        (MMAX=10,NMAX=8,NRA=MMAX,NRR=NMAX,NRPT=NMAX,
     +                 LWORK=3*NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, IFAIL, IRANK, J, M, N
      LOGICAL          SVD
*     .. Local Arrays ..
      DOUBLE PRECISION A(NRA,NMAX), PT(NRPT,NMAX), R(NRR,NMAX),
     +                 SV(NMAX), WORK(LWORK), Z(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02WDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02WDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX .OR. M.LT.1 .OR. M.GT.MMAX) THEN
         WRITE (NOUT,99999) 'N or M out of range: N = ', N, '  M = ', M
         STOP
      END IF
      SVD = .TRUE.
      TOL = 5.0D-4
      READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
      IFAIL = 0
*
      CALL F02WDF(M,N,A,NRA,.FALSE.,WORK,TOL,SVD,IRANK,Z,SV,.TRUE.,R,
     +            NRR,.TRUE.,PT,NRPT,WORK,LWORK,IFAIL)
*
      WRITE (NOUT,99999) 'Rank of A is', IRANK
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Details of QU factorization'
      DO 20 I = 1, M
         WRITE (NOUT,99998) (A(I,J),J=1,N)
   20 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Vector Z'
      WRITE (NOUT,99998) (Z(I),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Matrix R'
      DO 40 I = 1, N
         WRITE (NOUT,99998) (R(I,J),J=1,N)
   40 CONTINUE
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Singular values'
      WRITE (NOUT,99998) (SV(I),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Matrix P**T'
      DO 60 I = 1, N
         WRITE (NOUT,99998) (PT(I,J),J=1,N)
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,8F9.3)
      END
