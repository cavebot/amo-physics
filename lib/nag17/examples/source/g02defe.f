*     G02DEF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=5,NMAX=12)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RSS, RSST, TOL
      INTEGER          I, IDF, IFAIL, INDX, IP, IRANK, J, M, N
      LOGICAL          SVD
      CHARACTER        MEAN, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV(MMAX*(MMAX+1)/2), P(MMAX*(MMAX+2)),
     +                 Q(NMAX,MMAX+1), SE(MMAX), WK(MMAX*MMAX+5*MMAX),
     +                 WT(NMAX), X(NMAX,MMAX)
*     .. External Subroutines ..
      EXTERNAL         F06FBF, G02DDF, G02DEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02DEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, WEIGHT, MEAN
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
         IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') THEN
*
            CALL F06FBF(N,1.0D0,X(1,MMAX),1)
*
            IFAIL = 0
*
            CALL G02DEF(WEIGHT,N,IP,Q,NMAX,P,WT,X(1,MMAX),RSS,TOL,IFAIL)
*
            IP = 1
         END IF
   60    READ (5,*) INDX
         IF (INDX.GT.0) THEN
            IFAIL = -1
*
            CALL G02DEF(WEIGHT,N,IP,Q,NMAX,P,WT,X(1,INDX),RSS,TOL,IFAIL)
*
            IF (IFAIL.EQ.0) THEN
               IP = IP + 1
               WRITE (NOUT,*)
               WRITE (NOUT,99999) 'Variable', INDX, ' added'
               RSST = 0.0D0
               IFAIL = 0
*
               CALL G02DDF(N,IP,Q,NMAX,RSST,IDF,B,SE,COV,SVD,IRANK,P,
     +                     TOL,WK,IFAIL)
*
               IF (SVD) THEN
                  WRITE (NOUT,*) 'Model not of full rank'
                  WRITE (NOUT,*)
               END IF
               WRITE (NOUT,99998) 'Residual sum of squares = ', RSST
               WRITE (NOUT,99999) 'Degrees of freedom = ', IDF
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +           'Variable   Parameter estimate   Standard error'
               WRITE (NOUT,*)
               DO 80 J = 1, IP
                  WRITE (NOUT,99997) J, B(J), SE(J)
   80          CONTINUE
            ELSE IF (IFAIL.EQ.3) THEN
               WRITE (NOUT,*) ' * New variable not added *'
            ELSE
               GO TO 100
            END IF
            GO TO 60
         END IF
      END IF
  100 CONTINUE
      STOP
*
99999 FORMAT (1X,A,I4,A)
99998 FORMAT (1X,A,D13.4)
99997 FORMAT (1X,I6,2D20.4)
      END
