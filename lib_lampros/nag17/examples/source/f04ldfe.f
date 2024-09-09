*     F04LDF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, M1MAX, M2MAX, IR, IA, IL, IB
      PARAMETER        (NMAX=10,M1MAX=5,M2MAX=5,IR=1,IA=NMAX,IL=M1MAX,
     +                 IB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IV, J, M1, M2, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), AL(IL,NMAX), B(IB,IR)
      INTEGER          IN(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01LBF, F04LDF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04LDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M1, M2
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M1.GE.0 .AND. M1.LE.M1MAX .AND.
     +    M2.GE.0 .AND. M2.LE.M2MAX) THEN
         READ (NIN,*) ((A(J,I),J=1,MIN(N,M1+M2+1)),I=1,N)
         READ (NIN,*) ((B(I,J),J=1,IR),I=1,N)
         IFAIL = 1
*
         CALL F01LBF(N,M1,M2,A,IA,AL,IL,IN,IV,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99997) 'Error in F01LBF. IFAIL =', IFAIL
         ELSE
            IFAIL = 0
*
            CALL F04LDF(N,M1,M2,IR,A,IA,AL,IL,IN,B,IB,IFAIL)
*
            WRITE (NOUT,*) ' Solution'
            DO 20 I = 1, N
               WRITE (NOUT,99999) (B(I,J),J=1,IR)
   20       CONTINUE
         END IF
      ELSE
         WRITE (NOUT,99998) 'N or M1 or M2 is out of range: N = ', N,
     +     '  M1 = ', M1, '  M2 = ', M2
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,A,I5,A,I5,A,I5)
99997 FORMAT (1X,A,I2)
      END
