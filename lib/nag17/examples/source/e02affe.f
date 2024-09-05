*     E02AFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, NP1MAX
      PARAMETER        (NMAX=199,NP1MAX=NMAX+1)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION FIT, PI, PIBY2N
      INTEGER          I, IFAIL, J, N, R
*     .. Local Arrays ..
      DOUBLE PRECISION AN(NP1MAX), F(NP1MAX), XCAP(NP1MAX)
*     .. External Functions ..
      DOUBLE PRECISION X01AAF
      EXTERNAL         X01AAF
*     .. External Subroutines ..
      EXTERNAL         E02AEF, E02AFF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE, SIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02AFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      PI = X01AAF(PI)
   20 READ (NIN,*,END=80) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         PIBY2N = 0.5D0*PI/DBLE(N)
         READ (NIN,*) (F(R),R=1,N+1)
         DO 40 R = 1, N + 1
            I = R - 1
*
*           The following method of evaluating  XCAP = cos(PI*I/N)
*           ensures that the computed value has a small relative error
*           and, moreover, is bounded in modulus by unity for all
*           I = 0, 1, ..., N.  (It is assumed that the sine routine
*           produces a result with a small relative error for values
*           of the argument between  -PI/4  and  PI/4).
*
            IF (4*I.LE.N) THEN
               XCAP(I+1) = 1.0D0 - 2.0D0*SIN(PIBY2N*I)**2
            ELSE IF (4*I.GT.3*N) THEN
               XCAP(I+1) = 2.0D0*SIN(PIBY2N*(N-I))**2 - 1.0D0
            ELSE
               XCAP(I+1) = SIN(PIBY2N*(N-2*I))
            END IF
   40    CONTINUE
         IFAIL = 0
*
         CALL E02AFF(N+1,F,AN,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '         Chebyshev'
         WRITE (NOUT,*) '  J   coefficient A(J)'
         WRITE (NOUT,99998) (J,AN(J),J=1,N+1)
         WRITE (NOUT,*)
         WRITE (NOUT,*) '  R    Abscissa   Ordinate      Fit'
         DO 60 R = 1, N + 1
            IFAIL = 0
*
            CALL E02AEF(N+1,AN,XCAP(R),FIT,IFAIL)
*
            WRITE (NOUT,99999) R, XCAP(R), F(R), FIT
   60    CONTINUE
         GO TO 20
      END IF
   80 STOP
*
99999 FORMAT (1X,I3,3F11.4)
99998 FORMAT (1X,I3,F14.7)
      END
