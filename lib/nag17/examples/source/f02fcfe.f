*     F02FCF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LDA, LDZ, LWORK
      PARAMETER        (NMAX=8,MMAX=3,LDA=NMAX,LDZ=NMAX,LWORK=64*NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION WL, WU
      INTEGER          I, IFAIL, IL, IU, J, M, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), W(NMAX), WORK(LWORK), Z(LDZ,MMAX)
      INTEGER          IWORK(5*NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02FCF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02FCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, IL, IU
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
*        Compute selected eigenvalues and eigenvectors
*
         IFAIL = 0
*
         CALL F02FCF('Vectors','Index',UPLO,N,A,LDA,WL,WU,IL,IU,MMAX,M,
     +               W,Z,LDZ,WORK,LWORK,IWORK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (W(I),I=1,M)
         WRITE (NOUT,*)
*
         CALL X04CAF('General',' ',N,M,Z,LDZ,'Eigenvectors',IFAIL)
*
      END IF
      STOP
*
99999 FORMAT (3X,(8F8.4))
      END
