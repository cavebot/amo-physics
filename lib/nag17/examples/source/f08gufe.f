*     F08GUF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDZ
      PARAMETER        (NMAX=8,LDZ=NMAX)
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION VL, VU
      INTEGER          I, IFAIL, INFO, J, M, N, NSPLIT
      CHARACTER        UPLO
*     .. Local Arrays ..
      COMPLEX*16       AP(NMAX*(NMAX+1)/2), TAU(NMAX), WORK(NMAX),
     +                 Z(LDZ,NMAX)
      DOUBLE PRECISION D(NMAX), E(NMAX), RWORK(5*NMAX), W(NMAX)
      INTEGER          IBLOCK(NMAX), IFAILV(NMAX), ISPLIT(NMAX),
     +                 IWORK(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         DSTEBZ, X04DBF, ZHPTRD, ZSTEIN, ZUPMTR
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F08GUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            READ (NIN,*) ((AP(I+J*(J-1)/2),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
            READ (NIN,*) ((AP(I+(2*N-J)*(J-1)/2),J=1,I),I=1,N)
         END IF
*
*        Reduce A to tridiagonal form T = (Q**H)*A*Q
*
         CALL ZHPTRD(UPLO,N,AP,D,E,TAU,INFO)
*
*        Calculate the two smallest eigenvalues of T (same as A)
*
         CALL DSTEBZ('I','B',N,VL,VU,1,2,ZERO,D,E,M,NSPLIT,W,IBLOCK,
     +               ISPLIT,RWORK,IWORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.GT.0) THEN
            WRITE (NOUT,*) 'Failure to converge.'
         ELSE
            WRITE (NOUT,*) 'Eigenvalues'
            WRITE (NOUT,99999) (W(I),I=1,M)
*
*           Calculate the eigenvectors of T, storing the result in Z
*
            CALL ZSTEIN(N,D,E,M,W,IBLOCK,ISPLIT,Z,LDZ,RWORK,IWORK,
     +                  IFAILV,INFO)
*
            IF (INFO.GT.0) THEN
               WRITE (NOUT,*) 'Failure to converge.'
            ELSE
*
*              Calculate the eigenvectors of A = Q * (eigenvectors of T)
*
               CALL ZUPMTR('Left',UPLO,'No transpose',N,M,AP,TAU,Z,LDZ,
     +                     WORK,INFO)
*
*              Print eigenvectors
*
               WRITE (NOUT,*)
               IFAIL = 0
*
               CALL X04DBF('General',' ',N,M,Z,LDZ,'Bracketed','F7.4',
     +                     'Eigenvectors','Integer',RLABS,'Integer',
     +                     CLABS,80,0,IFAIL)
*
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (8X,4(F7.4,11X,:))
      END
