*     G03FCF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, NNMAX
      PARAMETER        (NMAX=14,MMAX=2,NNMAX=NMAX*(NMAX-1)/2)
*     .. Local Scalars ..
      DOUBLE PRECISION STRESS
      INTEGER          I, IFAIL, IOPT, ITER, J, LDX, N, NDIM, NN
      CHARACTER        TYPE
*     .. Local Arrays ..
      DOUBLE PRECISION D(NNMAX), DFIT(4*NNMAX), WK(NNMAX+15*NMAX*MMAX),
     +                 X(NMAX,NMAX)
      INTEGER          IWK(NNMAX+NMAX*NMAX+5)
*     .. External Subroutines ..
      EXTERNAL         G01AGF, G03FAF, G03FCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03FCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NDIM, TYPE
      IF (N.LE.NMAX) THEN
         NN = N*(N-1)/2
         READ (NIN,*) (D(I),I=1,NN)
         LDX = NMAX
         IFAIL = 0
         CALL G03FAF('L',N,D,NDIM,X,LDX,WK,WK(N+1),IWK,IFAIL)
         ITER = 0
         IOPT = 0
         IFAIL = 0
*
         CALL G03FCF(TYPE,N,NDIM,D,X,LDX,STRESS,DFIT,ITER,IOPT,WK,IWK,
     +               IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) STRESS
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Co-ordinates'
         WRITE (NOUT,*)
         DO 20 I = 1, N
            WRITE (NOUT,99998) (X(I,J),J=1,NDIM)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Plot of first two dimensions'
         WRITE (NOUT,*)
         IFAIL = 0
         CALL G01AGF(X(1,1),X(1,2),N,IWK,50,18,IFAIL)
      END IF
      STOP
*
99999 FORMAT (10X,'STRESS = ',D13.4)
99998 FORMAT (8F10.4)
      END
