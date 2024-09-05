*     G02DFF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=5,NMAX=12)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RSS, TOL
      INTEGER          I, IDF, IFAIL, INDX, IP, IRANK, J, M, N
      LOGICAL          SVD
      CHARACTER        MEAN, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV((MMAX*MMAX+MMAX)/2), H(NMAX),
     +                 P(MMAX*(MMAX+2)), Q(NMAX,MMAX+1), RES(NMAX),
     +                 SE(MMAX), WK(5*(MMAX-1)+MMAX*MMAX), WT(NMAX),
     +                 X(NMAX,MMAX), Y(NMAX)
      INTEGER          ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02DAF, G02DDF, G02DFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02DFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, WEIGHT, MEAN
      IF (N.LE.NMAX .AND. M.LT.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), Y(I), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), Y(I)
   40       CONTINUE
         END IF
         DO 60 I = 1, M
            ISX(I) = 1
   60    CONTINUE
         IP = M
         IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') IP = IP + 1
*        Set tolerance
         TOL = 0.00001D0
         IFAIL = 0
*
         CALL G02DAF(MEAN,WEIGHT,N,X,NMAX,M,ISX,IP,Y,WT,RSS,IDF,B,SE,
     +               COV,RES,H,Q,NMAX,SVD,IRANK,P,TOL,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Results from full model'
         IF (SVD) THEN
            WRITE (NOUT,*) 'Model not of full rank'
            WRITE (NOUT,*)
         END IF
         WRITE (NOUT,99999) 'Residual sum of squares = ', RSS
         WRITE (NOUT,99998) 'Degrees of freedom = ', IDF
         WRITE (NOUT,*)
   80    READ (NIN,*) INDX
         IF (INDX.NE.0) THEN
            IFAIL = 0
*
            CALL G02DFF(IP,Q,NMAX,INDX,RSS,WK,IFAIL)
*
            IP = IP - 1
            IF (IP.EQ.0) THEN
               WRITE (NOUT,*) 'No terms left in model'
            ELSE
               WRITE (NOUT,99998) 'Variable', INDX, ' dropped'
               IFAIL = 0
*
               CALL G02DDF(N,IP,Q,NMAX,RSS,IDF,B,SE,COV,SVD,IRANK,P,TOL,
     +                     WK,IFAIL)
*
               WRITE (NOUT,99999) 'Residual sum of squares = ', RSS
               WRITE (NOUT,99998) 'Degrees of freedom = ', IDF
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Parameter estimate   Standard error'
               WRITE (NOUT,*)
               DO 100 J = 1, IP
                  WRITE (NOUT,99997) B(J), SE(J)
  100          CONTINUE
               GO TO 80
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,D13.4)
99998 FORMAT (1X,A,I4,A)
99997 FORMAT (1X,D15.4,D20.4)
      END
