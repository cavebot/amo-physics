*     F01BVF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MA1MAX, MB1MAX, M3, IA, IB, IV
      PARAMETER        (NMAX=20,MA1MAX=8,MB1MAX=8,M3=3*MA1MAX+MB1MAX-4,
     +                 IA=MA1MAX,IB=MB1MAX,IV=MA1MAX+MB1MAX-2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, EPS1, EPS2
      INTEGER          I, IFAIL, IZ, J, K, M1, M2, MA1, MB1, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,NMAX), D(NMAX), E(NMAX),
     +                 E2(NMAX), R(NMAX), V(IV,M3), W(M3)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01BUF, F01BVF, F01BWF, F02BFF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01BVF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, MA1, MB1
      IF (N.GT.0 .AND. N.LE.NMAX .AND. MA1.GE.0 .AND. MA1.LE.
     +    MA1MAX .AND. MB1.GE.0 .AND. MB1.LE.MB1MAX) THEN
         READ (NIN,*) ((A(J,I),J=MAX(1,MA1+1-I),MA1),I=1,N)
         READ (NIN,*) ((B(J,I),J=MAX(1,MB1+1-I),MB1),I=1,N)
         K = N/2
         IFAIL = 0
*
         CALL F01BUF(N,MB1,K,B,IB,W,IFAIL)
         CALL F01BVF(N,MA1,MB1,M3,K,A,IA,B,IB,V,IV,W,IFAIL)
         CALL F01BWF(N,MA1,A,IA,D,E)
*
         EPS1 = 0.0D0
         EPS = X02AJF()
         DO 20 I = 1, N
            E2(I) = E(I)*E(I)
   20    CONTINUE
         READ (NIN,*) M1, M2
*
         CALL F02BFF(D,E,E2,N,M1,M2,M2-M1+1,EPS1,EPS,EPS2,IZ,R,W)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Selected eigenvalues'
         WRITE (NOUT,99999) (R(I),I=1,M2-M1+1)
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
      END
