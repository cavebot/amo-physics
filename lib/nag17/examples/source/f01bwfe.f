*     F01BWF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, M1MAX, IA
      PARAMETER        (NMAX=20,M1MAX=8,IA=M1MAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS
      INTEGER          I, IFAIL, J, M1, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), D(NMAX), E(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01BWF, F02AVF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BWF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M1
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M1.GT.0 .AND. M1.LE.M1MAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M1)
*
         CALL F01BWF(N,M1,A,IA,D,E)
*
         IF (M1.GT.1) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Diagonal'
            WRITE (NOUT,99999) (D(I),I=1,N)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Sub-diagonal'
            WRITE (NOUT,99999) (E(I),I=1,N)
            EPS = X02AJF()
            IFAIL = 0
*
            CALL F02AVF(N,EPS,D,E,IFAIL)
*
         END IF
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,99999) (D(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
