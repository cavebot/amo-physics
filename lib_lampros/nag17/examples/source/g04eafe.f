*     G04EAF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          MMAX, NMAX
      PARAMETER        (MMAX=5,NMAX=12)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RSS, TOL
      INTEGER          I, IDF, IFAIL, IP, IRANK, J, LDX, LEVELS, M, N
      LOGICAL          SVD
      CHARACTER        MEAN, TYPE, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV((MMAX*MMAX+MMAX)/2), H(NMAX),
     +                 P(MMAX*(MMAX+2)), Q(NMAX,MMAX+1), REP(MMAX),
     +                 RES(NMAX), SE(MMAX), V(MMAX),
     +                 WK(MMAX*MMAX+5*(MMAX-1)), WT(NMAX), X(NMAX,MMAX),
     +                 Y(NMAX)
      INTEGER          IFACT(NMAX), ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02DAF, G04EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G04EAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, LEVELS, TYPE, WEIGHT, MEAN
      WRITE (NOUT,*)
      IF (N.LE.NMAX .AND. LEVELS.LE.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) IFACT(I), Y(I), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) IFACT(I), Y(I)
   40       CONTINUE
         END IF
         IF (TYPE.EQ.'P' .OR. TYPE.EQ.'p') THEN
            READ (NIN,*) (V(J),J=1,LEVELS)
         END IF
*
*        Calculate dummy variables
*
         LDX = NMAX
         IFAIL = 0
*
         CALL G04EAF(TYPE,N,LEVELS,IFACT,X,LDX,V,REP,IFAIL)
*
         IF (TYPE.EQ.'C' .OR. TYPE.EQ.'c') THEN
            M = LEVELS
         ELSE
            M = LEVELS - 1
         END IF
         DO 60 J = 1, M
            ISX(J) = 1
   60    CONTINUE
         IP = M
         IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') IP = IP + 1
*        Set tolerance
         TOL = 0.00001D0
         IFAIL = 0
*
         CALL G02DAF(MEAN,WEIGHT,N,X,LDX,M,ISX,IP,Y,WT,RSS,IDF,B,SE,COV,
     +               RES,H,Q,NMAX,SVD,IRANK,P,TOL,WK,IFAIL)
*
         IF (SVD) THEN
            WRITE (NOUT,99999) 'Model not of full rank, rank = ',
     +        IRANK
            WRITE (NOUT,*)
         END IF
         WRITE (NOUT,99998) 'Residual sum of squares = ', RSS
         WRITE (NOUT,99999) 'Degrees of freedom = ', IDF
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'Variable   Parameter estimate   Standard error'
         WRITE (NOUT,*)
         DO 80 J = 1, IP
            WRITE (NOUT,99997) J, B(J), SE(J)
   80    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,A,D12.4)
99997 FORMAT (1X,I6,2D20.4)
      END
