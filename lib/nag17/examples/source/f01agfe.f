*     F01AGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA
      PARAMETER        (NMAX=8,IA=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), D(NMAX), E(NMAX), E2(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01AGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
         CALL F01AGF(N,0.0D0,A,IA,D,E,E2)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '     D        E       E2'
         WRITE (NOUT,99999) (D(I),E(I),E2(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,3F9.4)
      END
