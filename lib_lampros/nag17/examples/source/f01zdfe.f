*     F01ZDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LDB
      PARAMETER        (NMAX=10,LDA=NMAX,LDB=LDA)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, KL, KU, M, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), B(LDB,NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01ZDF, X04DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01ZDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      READ (NIN,*) M, N, KL, KU
*     Read a banded matrix of size M by N. KL is the number of
*     sub-diagonals, KU the number of super-diagonals.
      DO 20 I = 1, N
         READ (NIN,*) (A(I,J),J=1,N)
   20 CONTINUE
*     Clear the packed matrix array B, so that no elements are
*     unassigned when we print B later.
      DO 60 J = 1, N
         DO 40 I = 1, KL + KU + 1
            B(I,J) = (0.0D+0,0.0D+0)
   40    CONTINUE
   60 CONTINUE
      IFAIL = 0
*     Print the unpacked matrix
      CALL X04DAF('G','X',N,N,A,LDA,'Unpacked Matrix A:',IFAIL)
      WRITE (NOUT,*)
*
*     Convert to packed matrix form
      CALL F01ZDF('Pack',M,N,KL,KU,A,LDA,B,LDB,IFAIL)
*
*     Print the packed matrix
      CALL X04DAF('G','X',KL+KU+1,N,B,LDB,'Packed Matrix B:',IFAIL)
      STOP
      END
