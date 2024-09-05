*     G13AUF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, KMAX
      PARAMETER        (NMAX=100,KMAX=NMAX/2)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, K, M, N, NSTEPX, NSTEPY
*     .. Local Arrays ..
      DOUBLE PRECISION MEAN(KMAX), RANGE(KMAX), Z(NMAX)
      INTEGER          ISORT(KMAX)
*     .. External Subroutines ..
      EXTERNAL         G01AGF, G13AUF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13AUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      CALL X04ABF(1,NOUT)
      READ (NIN,*) N, M
      IF (N.GE.M .AND. N.LE.NMAX .AND. M.GE.1) THEN
         READ (NIN,*) (Z(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '                                 Range-mean plot'
         WRITE (NOUT,*)
         K = N/M
         IFAIL = 0
*
         CALL G13AUF(N,Z,M,K,'RANGE',RANGE,MEAN,IFAIL)
*
*        Produce a scatterplot of range against mean or standard
*        deviation against mean.
         NSTEPX = 60
         NSTEPY = 35
*
         CALL G01AGF(MEAN,RANGE,K,ISORT,NSTEPX,NSTEPY,IFAIL)
*
      END IF
      STOP
      END
