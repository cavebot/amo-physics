*     F01AJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IZ
      PARAMETER        (NMAX=8,IA=NMAX,IZ=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), D(NMAX), E(NMAX), Z(IZ,NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01AJF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
         CALL F01AJF(N,0.0D0,A,IA,D,E,Z,IZ)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Diagonal'
         WRITE (NOUT,99999) (D(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Sub-diagonal'
         WRITE (NOUT,99999) (E(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Array Z'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (Z(I,J),J=1,N)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
