*     F02FAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LWORK
      PARAMETER        (NMAX=8,LDA=NMAX,LWORK=64*NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), W(NMAX), WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         F02FAF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02FAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            READ (NIN,*) ((A(I,J),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
            READ (NIN,*) ((A(I,J),J=1,I),I=1,N)
         END IF
*
*        Compute eigenvalues and eigenvectors
*
         IFAIL = 0
*
         CALL F02FAF('Vectors',UPLO,N,A,LDA,W,WORK,LWORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (W(I),I=1,N)
         WRITE (NOUT,*)
*
         CALL X04CAF('General',' ',N,N,A,LDA,'Eigenvectors',IFAIL)
*
      END IF
      STOP
*
99999 FORMAT (3X,(8F8.4))
      END
