*     F01CTF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LDB, LDC
      PARAMETER        (NMAX=6,LDA=NMAX,LDB=LDA,LDC=LDA)
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, BETA
      INTEGER          I, IFAIL, J, M, N, NCOLA, NCOLB, NROWA, NROWB
      CHARACTER        TRANSA, TRANSB
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,NMAX), C(LDC,NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01CTF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01CTF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*)
*     Read matrices A and B.
      READ (NIN,*,END=80) NROWA, NCOLA, TRANSA, ALPHA
*     Check that the arrays are large enough to hold the matrices.
      IF (NROWA.LE.LDA .AND. NCOLA.LE.NMAX) THEN
         DO 40 I = 1, NROWA
            READ (NIN,*) (A(I,J),J=1,NCOLA)
   40    CONTINUE
         READ (NIN,*) NROWB, NCOLB, TRANSB, BETA
         IF (NROWB.LE.LDB .AND. NCOLB.LE.NMAX) THEN
            DO 60 I = 1, NROWB
               READ (NIN,*) (B(I,J),J=1,NCOLB)
   60       CONTINUE
            IF (TRANSA.EQ.'N' .OR. TRANSA.EQ.'n') THEN
               M = NROWA
               N = NCOLA
            ELSE
               M = NCOLA
               N = NROWA
            END IF
            IFAIL = 0
*
*           Add the two matrices A and B.
            CALL F01CTF(TRANSA,TRANSB,M,N,ALPHA,A,LDA,BETA,B,LDB,C,LDC,
     +                  IFAIL)
*
*           Print the result matrix C.
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'TRANSA = ''', TRANSA, ''', TRANSB = ''',
     +        TRANSB, ''', ALPHA = ', ALPHA, ', BETA = ', BETA
            CALL X04CAF('G','X',M,N,C,LDC,'Matrix C:',IFAIL)
            WRITE (NOUT,*)
            GO TO 20
         END IF
      END IF
   80 CONTINUE
      STOP
*
99999 FORMAT (1X,5A,1P,D11.3,A,D11.3)
      END
