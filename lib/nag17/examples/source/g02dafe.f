*     G02DAF Example Program Text
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
      CHARACTER        MEAN, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV((MMAX*MMAX+MMAX)/2), H(NMAX),
     +                 P(MMAX*(MMAX+2)), Q(NMAX,MMAX+1), RES(NMAX),
     +                 SE(MMAX), WK(MMAX*MMAX+5*(MMAX-1)), WT(NMAX),
     +                 X(NMAX,MMAX), Y(NMAX)
      INTEGER          ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02DAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, WEIGHT, MEAN
      WRITE (NOUT,*)
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
         READ (NIN,*) (ISX(J),J=1,M)
*        Calculate IP
         IP = 0
         IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') IP = IP + 1
         DO 60 I = 1, M
            IF (ISX(I).GT.0) IP = IP + 1
   60    CONTINUE
*        Set tolerance
         TOL = 0.00001D0
         IFAIL = 0
*
         CALL G02DAF(MEAN,WEIGHT,N,X,NMAX,M,ISX,IP,Y,WT,RSS,IDF,B,SE,
     +               COV,RES,H,Q,NMAX,SVD,IRANK,P,TOL,WK,IFAIL)
*
         IF (SVD) THEN
            WRITE (NOUT,99999) 'Model not of full rank, rank = ', IRANK
            WRITE (NOUT,*)
         END IF
         WRITE (NOUT,99998) 'Residual sum of squares = ', RSS
         WRITE (NOUT,99999) 'Degrees of freedom = ', IDF
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Variable   Parameter estimate   Standard error'
         WRITE (NOUT,*)
         DO 80 J = 1, IP
            WRITE (NOUT,99997) J, B(J), SE(J)
   80    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) '   Obs          Residuals              H'
         WRITE (NOUT,*)
         DO 100 I = 1, N
            WRITE (NOUT,99997) I, RES(I), H(I)
  100    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,A,D12.4)
99997 FORMAT (1X,I6,2D20.4)
      END
