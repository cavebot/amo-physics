*     F01AVF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IAR, IAI
      PARAMETER        (NMAX=4,IAR=NMAX,IAI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IB, J, K, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION AI(IAI,NMAX), AR(IAR,NMAX), D(NMAX)
*     .. External Functions ..
      INTEGER          X02BHF
      EXTERNAL         X02BHF
*     .. External Subroutines ..
      EXTERNAL         F01AVF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01AVF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((AR(I,J),AI(I,J),J=1,N),I=1,N)
         IB = X02BHF()
*
         CALL F01AVF(N,IB,AR,IAR,AI,IAI,K,L,D)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Balanced matrix (machine-dependent)'
         DO 20 I = 1, N
            WRITE (NOUT,99998) (' (',AR(I,J),',',AI(I,J),')',J=1,N)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Array D'
         WRITE (NOUT,99997) (D(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I2,A,I2)
99998 FORMAT (1X,4(A,F7.3,A,F7.3,A))
99997 FORMAT (1X,4F9.4)
      END
