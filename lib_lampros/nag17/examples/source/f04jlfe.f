*     F04JLF Example Program Text.
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, PMAX, LDA, LDB, LWORK
      PARAMETER        (NMAX=10,MMAX=10,PMAX=10,LDA=NMAX,LDB=NMAX,
     +                 LWORK=MMAX+NMAX+64*(NMAX+PMAX))
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,MMAX), B(LDB,PMAX), D(NMAX), WORK(LWORK),
     +                 X(MMAX), Y(PMAX)
*     .. External Subroutines ..
      EXTERNAL         F04JLF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04JLF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, P
      IF (N.LE.NMAX .AND. M.LE.MMAX .AND. P.LE.PMAX) THEN
*
*        Read A, B and D from data file
*
         READ (NIN,*) ((A(I,J),J=1,M),I=1,N)
         READ (NIN,*) ((B(I,J),J=1,P),I=1,N)
         READ (NIN,*) (D(I),I=1,N)
*
*        Solve the weighted least-squares problem
*
*        minimize ||inv(B)*(D-A*X)|| (in the 2-norm)
*
         IFAIL = 0
*
         CALL F04JLF(M,N,P,A,LDA,B,LDB,D,X,Y,WORK,LWORK,IFAIL)
*
*        Print least-squares solution
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Least-squares solution'
         WRITE (NOUT,99999) (X(I),I=1,M)
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
