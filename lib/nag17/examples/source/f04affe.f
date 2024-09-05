*     F04AFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IR, IA, IB, IX, IBB
      PARAMETER        (NMAX=8,IR=1,IA=NMAX,IB=NMAX,IX=NMAX,IBB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION D1, EPS
      INTEGER          I, ID, IFAIL, J, K, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), B(IB,IR), BB(IBB,IR), P(NMAX),
     +                 X(IX,IR)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F03AEF, F04AFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04AFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         IFAIL = 1
*
*        Cholesky decomposition
         CALL F03AEF(N,A,IA,P,D1,ID,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F03AEF. IFAIL =', IFAIL
         ELSE
            READ (NIN,*) ((B(I,J),J=1,IR),I=1,N)
            EPS = X02AJF()
            IFAIL = 1
*
*           Accurate solution of linear equations
            CALL F04AFF(N,IR,A,IA,P,B,IB,EPS,X,IX,BB,IBB,K,IFAIL)
*
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,99999) 'Error in F04AFF. IFAIL =', IFAIL
            ELSE
               WRITE (NOUT,*) ' Solution'
               DO 20 I = 1, N
                  WRITE (NOUT,99998) (X(I,J),J=1,IR)
   20          CONTINUE
            END IF
         END IF
      ELSE
         WRITE (NOUT,99999) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,8F9.4)
      END
