*     G02DDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=5,NMAX=12)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RSS, TOL
      INTEGER          I, IDF, IFAIL, IP, IRANK, J, M, N
      LOGICAL          SVD
      CHARACTER        WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV(MMAX*(MMAX+1)/2), P(MMAX*(MMAX+2)),
     +                 Q(NMAX,MMAX+1), SE(MMAX), WK(MMAX*MMAX+5*MMAX),
     +                 WT(NMAX), X(NMAX,MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02DDF, G02DEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02DDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, WEIGHT
      IF (N.LE.NMAX .AND. M.LT.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), Q(I,1), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), Q(I,1)
   40       CONTINUE
         END IF
*        Set tolerance
         TOL = 0.000001D0
         IP = 0
         DO 60 I = 1, M
            IFAIL = -1
*
*           Fit model using G02DEF
            CALL G02DEF(WEIGHT,N,IP,Q,NMAX,P,WT,X(1,I),RSS,TOL,IFAIL)
*
            IF (IFAIL.EQ.0) THEN
               IP = IP + 1
            ELSE IF (IFAIL.EQ.3) THEN
               WRITE (NOUT,*) ' * New variable not added *'
            ELSE
               GO TO 100
            END IF
   60    CONTINUE
         RSS = 0.0D0
         IFAIL = 0
*
         CALL G02DDF(N,IP,Q,NMAX,RSS,IDF,B,SE,COV,SVD,IRANK,P,TOL,WK,
     +               IFAIL)
*
         WRITE (NOUT,*)
         IF (SVD) THEN
            WRITE (NOUT,*) 'Model not of full rank'
            WRITE (NOUT,*)
         END IF
         WRITE (NOUT,99999) 'Residual sum of squares = ', RSS
         WRITE (NOUT,99998) 'Degrees of freedom = ', IDF
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Variable   Parameter estimate   Standard error'
         WRITE (NOUT,*)
         DO 80 J = 1, IP
            WRITE (NOUT,99997) J, B(J), SE(J)
   80    CONTINUE
      END IF
  100 CONTINUE
      STOP
*
99999 FORMAT (1X,A,D12.4)
99998 FORMAT (1X,A,I4)
99997 FORMAT (1X,I6,2D20.4)
      END
