*     G02GDF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=10,MMAX=2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, DEV, EPS, S, TOL
      INTEGER          I, IDF, IFAIL, IP, IPRINT, IRANK, J, M, MAXIT, N
      CHARACTER        LINK, MEAN, OFFSET, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), COV((MMAX*MMAX+MMAX)/2), SE(MMAX),
     +                 V(NMAX,7+MMAX), WK((MMAX*MMAX+3*MMAX+22)/2),
     +                 WT(NMAX), X(NMAX,MMAX), Y(NMAX)
      INTEGER          ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02GDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02GDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) LINK, MEAN, OFFSET, WEIGHT, N, M, S, IPRINT
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
         DO 60 J = 1, M
            IF (ISX(J).GT.0) IP = IP + 1
   60    CONTINUE
         IF (MEAN.EQ.'M' .OR. MEAN.EQ.'m') IP = IP + 1
         IF (LINK.EQ.'E' .OR. LINK.EQ.'e') READ (NIN,*) A
*        Set control parameters
         EPS = 0.000001D0
         TOL = 0.00005D0
         MAXIT = 10
         IFAIL = -1
*
         CALL G02GDF(LINK,MEAN,OFFSET,WEIGHT,N,X,NMAX,M,ISX,IP,Y,WT,S,A,
     +               DEV,IDF,B,IRANK,SE,COV,V,NMAX,TOL,MAXIT,IPRINT,EPS,
     +               WK,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.GE.7) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Deviance = ', DEV
            WRITE (NOUT,99998) 'Degrees of freedom = ', IDF
            WRITE (NOUT,*)
            WRITE (NOUT,*) '     Estimate     Standard error'
            WRITE (NOUT,*)
            DO 80 I = 1, IP
               WRITE (NOUT,99997) B(I), SE(I)
   80       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) '     Y        FV     Residual        H'
            WRITE (NOUT,*)
            DO 100 I = 1, N
               WRITE (NOUT,99996) Y(I), V(I,2), V(I,5), V(I,6)
  100       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,D12.4)
99998 FORMAT (1X,A,I2)
99997 FORMAT (1X,2F14.4)
99996 FORMAT (1X,F7.1,F10.2,F12.4,F10.3)
      END
