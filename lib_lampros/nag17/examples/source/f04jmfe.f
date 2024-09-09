*     F04JMF Example Program Text.
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, PMAX, LDA, LDB, LWORK
      PARAMETER        (MMAX=10,NMAX=10,PMAX=10,LDA=MMAX,LDB=PMAX,
     +                 LWORK=PMAX+NMAX+64*(MMAX+NMAX))
*     .. Local Scalars ..
      DOUBLE PRECISION RSS
      INTEGER          I, IFAIL, J, M, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,NMAX), C(MMAX), D(PMAX),
     +                 WORK(LWORK), X(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION DDOT
      EXTERNAL         DDOT
*     .. External Subroutines ..
      EXTERNAL         F04JMF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04JMF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N, P
      IF (M.LE.MMAX .AND. N.LE.NMAX .AND. P.LE.PMAX) THEN
*
*        Read A, B, C and D from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) ((B(I,J),J=1,N),I=1,P)
         READ (NIN,*) (C(I),I=1,M)
         READ (NIN,*) (D(I),I=1,P)
*
*        Solve the equality-constrained least-squares problem
*
*        minimize ||C - A*X|| (in the 2-norm) subject to B*X = D
*
         IFAIL = 0
*
         CALL F04JMF(M,N,P,A,LDA,B,LDB,C,D,X,WORK,LWORK,IFAIL)
*
*        Print least-squares solution
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Constrained least-squares solution'
         WRITE (NOUT,99999) (X(I),I=1,N)
*
*        Compute the residual sum of squares
*
         WRITE (NOUT,*)
         RSS = DDOT(M-N+P,C(N-P+1),1,C(N-P+1),1)
         WRITE (NOUT,99998) 'Residual sum of squares = ', RSS
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,A,1P,D10.2)
      END
