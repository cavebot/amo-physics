*     G03CCF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LWK
      PARAMETER        (NMAX=20,MMAX=10,LWK=400)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NFAC, NVAR
      CHARACTER        MATRIX, METHOD, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION COM(MMAX), E(MMAX), FL(MMAX,MMAX), FS(MMAX,MMAX),
     +                 PSI(MMAX), R(MMAX,MMAX), STAT(4), WK(LWK),
     +                 WT(NMAX), X(NMAX,MMAX)
      INTEGER          IOP(5), ISX(MMAX), IWK(4*MMAX+2)
*     .. External Subroutines ..
      EXTERNAL         G03CAF, G03CCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03CCF Example Program Results'
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
         IFAIL = 1
*
         CALL G03CAF(MATRIX,WEIGHT,N,M,X,NMAX,NVAR,ISX,NFAC,WT,E,STAT,
     +               COM,PSI,R,FL,MMAX,IOP,IWK,WK,LWK,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.GT.4) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Loadings, Communalities and PSI'
            WRITE (NOUT,*)
            DO 80 I = 1, NVAR
               WRITE (NOUT,99999) (FL(I,J),J=1,NFAC), COM(I), PSI(I)
   80       CONTINUE
            READ (NIN,*) METHOD
            IFAIL = 0
*
            CALL G03CCF(METHOD,'U',NVAR,NFAC,FL,MMAX,PSI,E,R,MMAX,FS,
     +                  MMAX,WK,IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' Factor score coefficients'
            WRITE (NOUT,*)
            DO 100 I = 1, NVAR
               WRITE (NOUT,99999) (FS(I,J),J=1,NFAC)
  100       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (2X,4F8.3)
      END
