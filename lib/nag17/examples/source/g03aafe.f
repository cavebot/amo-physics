*     G03AAF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=12,MMAX=3)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NVAR
      CHARACTER        MATRIX, STD, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION E(MMAX,6), P(MMAX,MMAX), S(MMAX), V(NMAX,MMAX),
     +                 WK(MMAX*MMAX+5*(MMAX-1)), WT(NMAX), X(NMAX,MMAX)
      INTEGER          ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G03AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03AAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) MATRIX, STD, WEIGHT, N, M
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
         IF (WEIGHT.EQ.'U' .OR. WEIGHT.EQ.'u') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), WT(I)
   40       CONTINUE
         END IF
         READ (NIN,*) (ISX(J),J=1,M), NVAR
         IF (MATRIX.EQ.'S' .OR. MATRIX.EQ.'s') READ (NIN,*) (S(J),J=1,M)
         IFAIL = 0
*
         CALL G03AAF(MATRIX,STD,WEIGHT,N,M,X,NMAX,ISX,S,WT,NVAR,E,MMAX,
     +               P,MMAX,V,NMAX,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +   'Eigenvalues  Percentage  Cumulative     Chisq      DF     Sig'
         WRITE (NOUT,*) '              variation   variation'
         WRITE (NOUT,*)
         DO 60 I = 1, NVAR
            WRITE (NOUT,99999) (E(I,J),J=1,6)
   60    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         WRITE (NOUT,*)
         DO 80 I = 1, NVAR
            WRITE (NOUT,99998) (P(I,J),J=1,NVAR)
   80    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Principal component scores'
         WRITE (NOUT,*)
         DO 100 I = 1, N
            WRITE (NOUT,99997) I, (V(I,J),J=1,NVAR)
  100    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,F11.4,2F12.4,F10.4,F8.1,F8.4)
99998 FORMAT (1X,8F9.4)
99997 FORMAT (1X,I2,(8F9.3))
      END
