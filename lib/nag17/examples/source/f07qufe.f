*     F07QUF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=8)
*     .. Local Scalars ..
      DOUBLE PRECISION ANORM, RCOND
      INTEGER          I, INFO, J, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      COMPLEX*16       AP(NMAX*(NMAX+1)/2), WORK(2*NMAX)
      DOUBLE PRECISION RWORK(NMAX)
      INTEGER          IPIV(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION F06UGF, X02AJF
      EXTERNAL         F06UGF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         ZSPCON, ZSPTRF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F07QUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            READ (NIN,*) ((AP(I+J*(J-1)/2),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
            READ (NIN,*) ((AP(I+(2*N-J)*(J-1)/2),J=1,I),I=1,N)
         END IF
*
*        Compute norm of A
*
         ANORM = F06UGF('1-norm',UPLO,N,AP,RWORK)
*
*        Factorize A
*
         CALL ZSPTRF(UPLO,N,AP,IPIV,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.EQ.0) THEN
*
*           Estimate condition number
*
            CALL ZSPCON(UPLO,N,AP,IPIV,ANORM,RCOND,WORK,INFO)
*
            IF (RCOND.GE.X02AJF()) THEN
               WRITE (NOUT,99999) 'Estimate of condition number =',
     +           1.0D0/RCOND
            ELSE
               WRITE (NOUT,*) 'A is singular to working precision'
            END IF
         ELSE
            WRITE (NOUT,*) 'The factor D is singular'
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,1P,D10.2)
      END
