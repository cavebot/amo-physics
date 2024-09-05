*     G03FAF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NNMAX
      PARAMETER        (NMAX=14,NNMAX=NMAX*(NMAX-1)/2)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, N, NDIM, NN
      CHARACTER        ROOTS
*     .. Local Arrays ..
      DOUBLE PRECISION D(NNMAX), E(NMAX), WK(NNMAX+9*NMAX), X(NMAX,NMAX)
      INTEGER          IWK(5*NMAX)
*     .. External Subroutines ..
      EXTERNAL         G01AGF, G03FAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03FAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NDIM, ROOTS
      IF (N.LE.NMAX) THEN
         NN = N*(N-1)/2
         READ (NIN,*) (D(I),I=1,NN)
         IFAIL = 0
*
         CALL G03FAF(ROOTS,N,D,NDIM,X,NMAX,E,WK,IWK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Scaled Eigenvalues'
         WRITE (NOUT,*)
         IF (ROOTS.EQ.'L' .OR. ROOTS.EQ.'l') THEN
            WRITE (NOUT,99999) (E(I),I=1,NDIM)
         ELSE
            WRITE (NOUT,99999) (E(I),I=1,N)
         END IF
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Co-ordinates'
         WRITE (NOUT,*)
         DO 20 I = 1, N
            WRITE (NOUT,99999) (X(I,J),J=1,NDIM)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Plot of first two dimensions'
         WRITE (NOUT,*)
         IFAIL = 0
         CALL G01AGF(X,X(1,2),N,IWK,50,18,IFAIL)
      END IF
      STOP
*
99999 FORMAT (8F10.4)
      END
