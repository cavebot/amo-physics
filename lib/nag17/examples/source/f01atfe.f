*     F01ATF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA
      PARAMETER        (NMAX=8,IA=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IB, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), D(NMAX)
*     .. External Functions ..
      INTEGER          X02BHF
      EXTERNAL         X02BHF
*     .. External Subroutines ..
      EXTERNAL         F01ATF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01ATF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IB = X02BHF()
*
         CALL F01ATF(N,IB,A,IA,K,L,D)
*
         WRITE (NOUT,99998) 'K = ', K, ' L = ', L
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Balanced matrix (machine-dependent)'
         DO 20 I = 1, N
            WRITE (NOUT,99999) (A(I,J),J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Array D'
         WRITE (NOUT,99999) (D(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,A,I2,A,I2)
      END
