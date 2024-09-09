*     F04ZCF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, KLMAX, KUMAX, LDA, LDX, NRHS
      PARAMETER        (NMAX=8,KLMAX=NMAX-1,KUMAX=NMAX-1,
     +                 LDA=2*KLMAX+KUMAX+1,LDX=NMAX,NRHS=1)
*     .. Local Scalars ..
      DOUBLE PRECISION ANORM, COND, ESTNRM
      INTEGER          I, ICASE, IFAIL, INFO, J, K, KL, KU, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), WORK(NMAX), X(LDX,NRHS)
      DOUBLE PRECISION RWORK(1)
      INTEGER          IPIV(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION F06UBF
      EXTERNAL         F06UBF
*     .. External Subroutines ..
      EXTERNAL         F04ZCF, ZGBTRF, ZGBTRS
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04ZCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, KL, KU
      IF (N.LE.NMAX .AND. KL.LE.KLMAX .AND. KU.LE.KUMAX) THEN
         K = KL + KU + 1
         READ (NIN,*) ((A(K+I-J,J),J=MAX(I-KL,1),MIN(I+KU,N)),I=1,N)
*
*         First compute the 1-norm of A.
*
         ANORM = F06UBF('1-norm',N,KL,KU,A(KL+1,1),LDA,RWORK)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Computed norm of A =', ANORM
*
*         Next estimate the 1-norm of inverse(A). We do not form the
*         inverse explicitly.
*         Factorise A into P*L*U.
*
         CALL ZGBTRF(N,N,KL,KU,A,LDA,IPIV,INFO)
*
         ICASE = 0
         IFAIL = 0
   20    CALL F04ZCF(ICASE,N,X,ESTNRM,WORK,IFAIL)
*
         IF (ICASE.EQ.0) THEN
            WRITE (NOUT,99999) 'Estimated norm of inverse(A) =',
     +        ESTNRM
         ELSE
            IF (ICASE.EQ.1) THEN
*
*                 Return X := inv(A)*X by solving A*Y = X, overwriting
*                 Y on X.
*
               CALL ZGBTRS('No transpose',N,KL,KU,NRHS,A,LDA,IPIV,X,LDX,
     +                     INFO)
*
            ELSE IF (ICASE.EQ.2) THEN
*
*                 Return X := conjg(inv(A)')*X by solving conjg(A')*Y
*                 = X, overwriting Y on X.
*
               CALL ZGBTRS('Conjugate transpose',N,KL,KU,NRHS,A,LDA,
     +                     IPIV,X,LDX,INFO)
*
            END IF
*             Continue until ICASE is returned as 0.
            GO TO 20
         END IF
         COND = ANORM*ESTNRM
         WRITE (NOUT,99998) 'Estimated condition number of A =', COND
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.4)
99998 FORMAT (1X,A,F6.1)
      END
