*     F02UYF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NCOLB, LDB
      PARAMETER        (NMAX=10,NCOLB=1,LDB=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N, NCOLZ, NROWY
*     .. Local Arrays ..
*     Only max( 1, 2*( NMAX - 1 ) ) elements of WORK are needed here.
      COMPLEX*16       B(LDB), DUMMY(1)
      DOUBLE PRECISION D(NMAX), E(NMAX), WORK(4*NMAX)
*     .. External Subroutines ..
      EXTERNAL         F02UYF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02UYF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.NMAX) THEN
         WRITE (NOUT,*) 'N is out of range.'
         WRITE (NOUT,99999) 'N = ', N
      ELSE
         READ (NIN,*) (D(I),I=1,N)
         IF (N.GT.1) READ (NIN,*) (E(I),I=1,N-1)
         READ (NIN,*) (B(I),I=1,N)
         NROWY = 0
         NCOLZ = 0
         IFAIL = 0
*
*        Reduce A to diagonal form
         CALL F02UYF(N,D,E,NCOLB,B,LDB,NROWY,DUMMY,1,NCOLZ,DUMMY,1,WORK,
     +               IFAIL)
*
         WRITE (NOUT,*) 'Singular values of the bidiagonal matrix'
         WRITE (NOUT,99998) (D(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Vector Q''*B'
         WRITE (NOUT,99997) (B(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (3(1X,F8.4))
99997 FORMAT (3(' (',F7.4,',',F8.4,')',:))
      END
