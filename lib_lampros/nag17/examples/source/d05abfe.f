*     D05ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, NT2P1
      PARAMETER        (NMAX=10,NT2P1=2*NMAX+1)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION ALPHA, W
*     .. Local Scalars ..
      DOUBLE PRECISION A, A1, B, CHEBR, D, E, LAMBDA, S, X
      INTEGER          I, IFAIL, N, SS
      LOGICAL          EV, ODOREV
*     .. Local Arrays ..
      DOUBLE PRECISION C(NMAX), CM(NMAX,NMAX), F(NMAX), F1(NMAX,1),
     +                 WK(2,NT2P1)
*     .. External Functions ..
      DOUBLE PRECISION C06DBF, GE, KE
      EXTERNAL         C06DBF, GE, KE
*     .. External Subroutines ..
      EXTERNAL         D05ABF
*     .. Common blocks ..
      COMMON           /AFRED2/ALPHA, W
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D05ABF Example Program Results'
      WRITE (NOUT,*)
      ODOREV = .TRUE.
      EV = .TRUE.
      LAMBDA = -0.3183D0
      A = -1.0D0
      B = 1.0D0
      ALPHA = 1.0D0
      W = ALPHA*ALPHA
      IF (ODOREV .AND. EV) THEN
         WRITE (NOUT,*) 'Solution is even'
      ELSE
         IF (ODOREV) WRITE (NOUT,*) 'Solution is odd'
      END IF
      DO 60 N = 5, NMAX, 5
         IFAIL = 1
*
         CALL D05ABF(KE,GE,LAMBDA,A,B,ODOREV,EV,N,CM,F1,WK,NMAX,NT2P1,F,
     +               C,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Results for N =', N
            WRITE (NOUT,*)
            WRITE (NOUT,*) '  I         F(I)         C(I)'
            DO 20 I = 1, N
               WRITE (NOUT,99998) I, F(I), C(I)
   20       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) '     X           F(X)'
            IF (ODOREV) THEN
               IF (EV) THEN
                  SS = 2
               ELSE
                  SS = 3
               END IF
            ELSE
               SS = 1
            END IF
            A1 = 0.5D0*(A+B)
            S = 0.5D0*(B-A)
            X = A1
            IF ( .NOT. ODOREV) THEN
               X = X - 5
            ELSE
               X = A1
            END IF
            D = 1.0D0/S
            S = 0.25D0*S
            E = B + 0.1D0*S
   40       CHEBR = C06DBF((X-A1)*D,C,N,SS)
            WRITE (NOUT,99997) X, CHEBR
            X = X + S
            IF (X.LT.E) GO TO 40
         ELSE
            IF (IFAIL.EQ.1) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Failure in D05ABF -'
               WRITE (NOUT,*) 'error in integration limits'
            ELSE
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Failure in D05ABF -'
               WRITE (NOUT,*) 'LAMBDA near eigenvalue'
            END IF
         END IF
   60 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,I3,F15.5,D15.5)
99997 FORMAT (1X,F8.4,F15.5)
      END
*
      DOUBLE PRECISION FUNCTION KE(X,S)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             S, X
*     .. Scalars in Common ..
      DOUBLE PRECISION             ALPHA, W
*     .. Common blocks ..
      COMMON                       /AFRED2/ALPHA, W
*     .. Executable Statements ..
      KE = ALPHA/(W+(X-S)*(X-S))
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION GE(X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION             X
*     .. Executable Statements ..
      GE = 1.0D0
      RETURN
      END
