*     F01CWF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LDB, LDC
      PARAMETER        (NMAX=6,LDA=NMAX,LDB=LDA,LDC=LDA)
*     .. Local Scalars ..
      COMPLEX*16       ALPHA, BETA
      INTEGER          I, IFAIL, J, M, N, NCOLA, NCOLB, NROWA, NROWB
      CHARACTER        TRANSA, TRANSB
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), B(LDB,NMAX), C(LDC,NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01CWF, X04DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01CWF Example Program Results'
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
            CALL F01CWF(TRANSA,TRANSB,M,N,ALPHA,A,LDA,BETA,B,LDB,C,LDC,
     +                  IFAIL)
*
*           Print the result matrix C.
            WRITE (NOUT,99999) 'TRANSA = ''', TRANSA, ''', TRANSB = ''',
     +        TRANSB, ''','
            WRITE (NOUT,99998) 'ALPHA = (', ALPHA, '), BETA = (',
     +        BETA, ')'
            CALL X04DAF('G','X',M,N,C,LDC,'Matrix C:',IFAIL)
            WRITE (NOUT,*)
            GO TO 20
         END IF
      END IF
   80 STOP
*
99999 FORMAT (1X,A,A,A,A,A)
99998 FORMAT (1X,A,1P,D11.4,',',D11.4,A,D11.4,',',D11.4,A)
      END
