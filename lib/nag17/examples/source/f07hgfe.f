*     F07HGF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, KDMAX, LDAB
      PARAMETER        (NMAX=8,KDMAX=8,LDAB=KDMAX+1)
*     .. Local Scalars ..
      DOUBLE PRECISION ANORM, RCOND
      INTEGER          I, INFO, J, KD, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION AB(LDAB,NMAX), WORK(3*NMAX)
      INTEGER          IWORK(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION F06REF, X02AJF
      EXTERNAL         F06REF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         DPBCON, DPBTRF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F07HGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, KD
      IF (N.LE.NMAX .AND. KD.LE.KDMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            DO 20 I = 1, N
               READ (NIN,*) (AB(KD+1+I-J,J),J=I,MIN(N,I+KD))
   20       CONTINUE
         ELSE IF (UPLO.EQ.'L') THEN
            DO 40 I = 1, N
               READ (NIN,*) (AB(1+I-J,J),J=MAX(1,I-KD),I)
   40       CONTINUE
         END IF
*
*        Compute norm of A
*
         ANORM = F06REF('1-norm',UPLO,N,KD,AB,LDAB,WORK)
*
*        Factorize A
*
         CALL DPBTRF(UPLO,N,KD,AB,LDAB,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.EQ.0) THEN
*
*           Estimate condition number
*
            CALL DPBCON(UPLO,N,KD,AB,LDAB,ANORM,RCOND,WORK,IWORK,INFO)
*
            IF (RCOND.GE.X02AJF()) THEN
               WRITE (NOUT,99999) 'Estimate of condition number =',
     +           1.0D0/RCOND
            ELSE
               WRITE (NOUT,*) 'A is singular to working precision'
            END IF
         ELSE
            WRITE (NOUT,*) 'A is not positive-definite'
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,1P,D10.2)
      END
