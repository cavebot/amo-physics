*     F04AHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IR, IAA, IA, IB, IX, IBB
      PARAMETER        (NMAX=8,IR=1,IAA=NMAX,IA=NMAX,IB=NMAX,IX=NMAX,
     +                 IBB=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION D1, EPS
      INTEGER          I, ID, IFAIL, J, K, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), AA(IAA,NMAX), B(IB,IR), BB(IBB,IR),
     +                 P(NMAX), X(IX,IR)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F03AFF, F04AHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04AHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) ((AA(I,J),J=1,N),I=1,N)
         DO 40 I = 1, N
            DO 20 J = 1, N
               A(J,I) = AA(J,I)
   20       CONTINUE
   40    CONTINUE
         EPS = X02AJF()
         IFAIL = 1
*
*        Crout decomposition
         CALL F03AFF(N,EPS,AA,IAA,D1,ID,P,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99998) 'Error in F03AFF. IFAIL =', IFAIL
         ELSE
            READ (NIN,*) ((B(I,J),J=1,IR),I=1,N)
            IFAIL = 1
*
*           Accurate solution of linear equations
            CALL F04AHF(N,IR,A,IA,AA,IAA,P,B,IB,EPS,X,IX,BB,IBB,K,IFAIL)
*
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,99998) 'Error in F04AHF. IFAIL =', IFAIL
            ELSE
               WRITE (NOUT,*) ' Solution'
               DO 60 I = 1, N
                  WRITE (NOUT,99999) (X(I,J),J=1,IR)
   60          CONTINUE
            END IF
         END IF
      ELSE
         WRITE (NOUT,99998) 'N is out of range: N = ', N
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,A,I5)
      END
