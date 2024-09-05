*     G03ZAF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=4,MMAX=3)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NVAR
*     .. Local Arrays ..
      DOUBLE PRECISION E(MMAX), S(MMAX), X(NMAX,MMAX), Z(NMAX,MMAX)
      INTEGER          ISX(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G03ZAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03ZAF Example Program Results'
*     Skip headings in data file
      READ (NIN,*)
      READ (NIN,*) N, M, NVAR
      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,M)
   20    CONTINUE
         READ (NIN,*) (ISX(J),J=1,M)
         READ (NIN,*) (E(J),J=1,M)
         READ (NIN,*) (S(J),J=1,M)
         IFAIL = 0
*
         CALL G03ZAF(N,M,X,NMAX,NVAR,ISX,S,E,Z,NMAX,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Standardized Values'
         DO 40 I = 1, N
            WRITE (NOUT,99999) (Z(I,J),J=1,NVAR)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,9F8.3)
      END
