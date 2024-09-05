*     G03CAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LWK
      PARAMETER        (NMAX=9,MMAX=9,LWK=349)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, L, M, N, NFAC, NVAR
      CHARACTER        MATRIX, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION COM(MMAX), E(MMAX), FL(MMAX,MMAX), PSI(MMAX),
     +                 RES(MMAX*(MMAX-1)/2), STAT(4), WK(LWK), WT(NMAX),
     +                 X(NMAX,MMAX)
      INTEGER          IOP(5), ISX(MMAX), IWK(4*MMAX+2)
*     .. External Subroutines ..
      EXTERNAL         G03CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03CAF Example Program Results'
*     Skip headings in data file
      READ (NIN,*)
      READ (NIN,*) MATRIX, WEIGHT, N, M, NVAR, NFAC
      IF (M.LE.MMAX .AND. (MATRIX.EQ.'C' .OR. MATRIX.EQ.'c' .OR. N.LE.
     +    NMAX)) THEN
         IF (MATRIX.EQ.'C' .OR. MATRIX.EQ.'c') THEN
            DO 20 I = 1, M
               READ (NIN,*) (X(I,J),J=1,M)
   20       CONTINUE
         ELSE
            IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
               DO 40 I = 1, N
                  READ (NIN,*) (X(I,J),J=1,M), WT(I)
   40          CONTINUE
            ELSE
               DO 60 I = 1, N
                  READ (NIN,*) (X(I,J),J=1,M)
   60          CONTINUE
            END IF
         END IF
         READ (NIN,*) (ISX(J),J=1,M)
         READ (NIN,*) (IOP(J),J=1,5)
         IFAIL = -1
*
         CALL G03CAF(MATRIX,WEIGHT,N,M,X,NMAX,NVAR,ISX,NFAC,WT,E,STAT,
     +               COM,PSI,RES,FL,MMAX,IOP,IWK,WK,LWK,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.GT.4) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Eigenvalues'
            WRITE (NOUT,*)
            WRITE (NOUT,99998) (E(J),J=1,M)
            WRITE (NOUT,*)
            WRITE (NOUT,99997) '     Test Statistic = ', STAT(2)
            WRITE (NOUT,99997) '                 df = ', STAT(3)
            WRITE (NOUT,99997) ' Significance level = ', STAT(4)
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Residuals'
            WRITE (NOUT,*)
            L = 1
            DO 80 I = 1, NVAR - 1
               WRITE (NOUT,99999) (RES(J),J=L,L+I-1)
               L = L + I
   80       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Loadings, Communalities and PSI'
            WRITE (NOUT,*)
            DO 100 I = 1, NVAR
               WRITE (NOUT,99999) (FL(I,J),J=1,NFAC), COM(I), PSI(I)
  100       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (2X,9F8.3)
99998 FORMAT (2X,6D12.4)
99997 FORMAT (A,F6.3)
      END
