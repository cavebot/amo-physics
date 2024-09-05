*     F01ZBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LENB
      PARAMETER        (NMAX=10,LDA=NMAX,LENB=(NMAX*(NMAX+1))/2)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, LB, N
      CHARACTER        DIAG, UPLO
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), B(LENB)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         F01ZBF, X04DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01ZBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
      READ (NIN,*) N, UPLO, DIAG
*     Read a triangular matrix of order N
      DO 20 I = 1, N
         READ (NIN,*) (A(I,J),J=1,N)
   20 CONTINUE
      IFAIL = 0
*     Print the unpacked matrix
      CALL X04DBF(UPLO,DIAG,N,N,A,LDA,'B','F5.2','Unpacked Matrix A:',
     +            'I',RLABS,'I',CLABS,80,0,IFAIL)
      WRITE (NOUT,*)
*
*     Convert to packed vector form
      CALL F01ZBF('Pack',UPLO,DIAG,N,A,LDA,B,IFAIL)
*
      LB = N*(N+1)/2
*     Print the packed vector
      CALL X04DBF('G','X',LB,1,B,LB,'B','F5.2','Packed Vector B:','I',
     +            RLABS,'N',CLABS,80,0,IFAIL)
      STOP
      END
