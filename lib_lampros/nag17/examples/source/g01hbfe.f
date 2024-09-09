*     G01HBF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LWK
      PARAMETER        (NMAX=10,LWK=2000)
*     .. Local Scalars ..
      DOUBLE PRECISION PROB, TOL
      INTEGER          I, IFAIL, J, N
      CHARACTER        TAIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), B(NMAX), SIG(NMAX,NMAX), WK(LWK),
     +                 XMU(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION G01HBF
      EXTERNAL         G01HBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G01HBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, TOL, TAIL
      IF (N.LE.NMAX) THEN
         READ (NIN,*) (XMU(J),J=1,N)
         DO 20 I = 1, N
            READ (NIN,*) (SIG(I,J),J=1,N)
   20    CONTINUE
         IF (TAIL.EQ.'C' .OR. TAIL.EQ.'c' .OR. TAIL.EQ.'U' .OR. TAIL.EQ.
     +       'u') READ (NIN,*) (A(J),J=1,N)
         IF (TAIL.EQ.'C' .OR. TAIL.EQ.'c' .OR. TAIL.EQ.'L' .OR. TAIL.EQ.
     +       'l') READ (NIN,*) (B(J),J=1,N)
         IFAIL = -1
*
         PROB = G01HBF(TAIL,N,A,B,XMU,SIG,NMAX,TOL,WK,LWK,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.GT.3) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Multivariate Normal probability = ',
     +        PROB
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,F6.4)
      END
