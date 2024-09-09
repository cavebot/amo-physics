*     G02DCF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=5,NMAX=12)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RSS, TOL, WTN, YN
      INTEGER          I, IDF, IFAIL, IP, IRANK, J, M, N
      LOGICAL          SVD
      CHARACTER        MEAN, UPDATE, WEIGHT
*     .. Local Arrays ..
*
      DOUBLE PRECISION B(MMAX), COV(MMAX*(MMAX+1)/2), H(NMAX),
     +                 P(MMAX*(MMAX+2)), Q(NMAX,MMAX+1), RES(NMAX),
     +                 SE(MMAX), WK(5*(MMAX-1)+MMAX*MMAX), WT(NMAX),
     +                 X(MMAX), XM(NMAX,MMAX), Y(NMAX)
      INTEGER          ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02DAF, G02DCF, G02DDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02DCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, WEIGHT, MEAN
      WRITE (NOUT,*)
      IF (N.LE.NMAX .AND. M.LT.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (XM(I,J),J=1,M), Y(I), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (XM(I,J),J=1,M), Y(I)
   40       CONTINUE
         END IF
         READ (NIN,*) (ISX(J),J=1,M), IP
*        Set tolerance
         TOL = 0.00001D0
         IFAIL = 0
*
*        Fit initial model using G02DAF
         CALL G02DAF(MEAN,WEIGHT,N,XM,NMAX,M,ISX,IP,Y,WT,RSS,IDF,B,SE,
     +               COV,RES,H,Q,NMAX,SVD,IRANK,P,TOL,WK,IFAIL)
*
         WRITE (NOUT,*) 'Results from G02DAF'
         IF (SVD) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Model not of full rank'
         END IF
         WRITE (NOUT,99999) 'Residual sum of squares = ', RSS
         WRITE (NOUT,99998) 'Degrees of freedom = ', IDF
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Variable   Parameter estimate   Standard error'
         WRITE (NOUT,*)
         DO 60 J = 1, IP
            WRITE (NOUT,99997) J, B(J), SE(J)
   60    CONTINUE
   80    READ (NIN,*) UPDATE
         IF (UPDATE.NE.'S' .AND. UPDATE.NE.'s') THEN
            IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
               READ (NIN,*) (X(J),J=1,M), YN, WTN
            ELSE
               READ (NIN,*) (X(J),J=1,M), YN
            END IF
            IFAIL = 0
*
            CALL G02DCF(UPDATE,MEAN,WEIGHT,M,ISX,Q,NMAX,IP,X,1,YN,WTN,
     +                  RSS,WK,IFAIL)
*
            IF (UPDATE.EQ.'A' .OR. UPDATE.EQ.'a') THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +           'Results from adding an observation using G02DCF'
               N = N + 1
            ELSE IF (UPDATE.EQ.'D' .OR. UPDATE.EQ.'d') THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +           'Results from dropping an observation using G02DCF'
               N = N - 1
            END IF
            IFAIL = 0
*
            CALL G02DDF(N,IP,Q,NMAX,RSS,IDF,B,SE,COV,SVD,IRANK,P,TOL,WK,
     +                  IFAIL)
*
            WRITE (NOUT,99999) 'Residual sum of squares = ', RSS
            WRITE (NOUT,99998) 'Degrees of freedom = ', IDF
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +        'Variable   Parameter estimate   Standard error'
            WRITE (NOUT,*)
            DO 100 J = 1, IP
               WRITE (NOUT,99997) J, B(J), SE(J)
  100       CONTINUE
            GO TO 80
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,D12.4)
99998 FORMAT (1X,A,I4)
99997 FORMAT (1X,I6,2D20.4)
      END
