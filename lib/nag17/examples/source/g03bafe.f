*     G03BAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=10,MMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION ACC, G
      INTEGER          I, IFAIL, ITER, J, K, MAXIT, NVAR
      CHARACTER        STAND
*     .. Local Arrays ..
      DOUBLE PRECISION FL(NMAX,MMAX), FLR(NMAX,MMAX), R(MMAX,MMAX),
     +                 WK(2*NMAX+MMAX*MMAX+5*(MMAX-1))
*     .. External Subroutines ..
      EXTERNAL         G03BAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NVAR, K, G, STAND, ACC, MAXIT
      IF (NVAR.LE.NMAX .AND. K.LE.MMAX) THEN
         DO 20 I = 1, NVAR
            READ (NIN,*) (FL(I,J),J=1,K)
   20    CONTINUE
         IFAIL = 0
*
         CALL G03BAF(STAND,G,NVAR,K,FL,NMAX,FLR,R,MMAX,ACC,MAXIT,ITER,
     +               WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '    Rotated factor loadings'
         WRITE (NOUT,*)
         DO 40 I = 1, NVAR
            WRITE (NOUT,99999) (FLR(I,J),J=1,K)
   40    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) '    Rotation matrix'
         WRITE (NOUT,*)
         DO 60 I = 1, K
            WRITE (NOUT,99999) (R(I,J),J=1,K)
   60    CONTINUE
      END IF
      STOP
*
99999 FORMAT (4(2X,6F8.3))
      END
