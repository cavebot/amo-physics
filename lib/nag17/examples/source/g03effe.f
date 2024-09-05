*     G03EFF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, KMAX
      PARAMETER        (NMAX=20,MMAX=5,KMAX=3)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, LDC, LDX, M, MAXIT, N, NVAR
      CHARACTER        WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION CMEANS(KMAX,MMAX), CSS(MMAX), CSW(MMAX),
     +                 WK(NMAX+2*KMAX), WT(NMAX), X(NMAX,MMAX)
      INTEGER          INC(NMAX), ISX(MMAX), IWK(NMAX+3*KMAX), NIC(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G03EFF
*     .. Executable Statements ..
*
      WRITE (NOUT,*) 'G03EFF Example Program Results'
*     Skip heading in the data file
      READ (NIN,*)
      READ (NIN,*) WEIGHT, N, M, NVAR, K, MAXIT
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M)
   40       CONTINUE
         END IF
         DO 60 I = 1, K
            READ (NIN,*) (CMEANS(I,J),J=1,NVAR)
   60    CONTINUE
         READ (NIN,*) (ISX(J),J=1,M)
         LDX = NMAX
         LDC = KMAX
         IFAIL = 0
*
         CALL G03EFF(WEIGHT,N,M,X,LDX,ISX,NVAR,K,CMEANS,LDC,WT,INC,NIC,
     +               CSS,CSW,MAXIT,IWK,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' The cluster each point belongs to'
         WRITE (NOUT,99999) (INC(I),I=1,N)
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' The number of points in each cluster'
         WRITE (NOUT,99999) (NIC(I),I=1,K)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     ' The within-cluster sum of weights of each cluster'
         WRITE (NOUT,99998) (CSW(I),I=1,K)
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     ' The within-cluster sum of squares of each cluster'
         WRITE (NOUT,99997) (CSS(I),I=1,K)
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' The final cluster centres'
         WRITE (NOUT,*)
     +     '                 1       2       3       4       5'
         DO 80 I = 1, K
            WRITE (NOUT,99996) I, (CMEANS(I,J),J=1,NVAR)
   80    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,10I6)
99998 FORMAT (1X,5F9.2)
99997 FORMAT (1X,5F13.4)
99996 FORMAT (1X,I5,5X,5F8.4)
      END
