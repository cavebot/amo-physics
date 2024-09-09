*     F04YCF Example Program Text
*     Mark 15 Revised.  NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         EX1, EX2
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04YCF Example Program Results'
      CALL EX1
      CALL EX2
      STOP
      END
*
      SUBROUTINE EX1
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=20,LDA=NMAX)
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D+0)
*     .. Local Scalars ..
      DOUBLE PRECISION ANORM, COND, D1, EPS, ESTNRM
      INTEGER          I, ICASE, ID, IFAIL, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), P(NMAX), WORK(NMAX), X(NMAX)
      INTEGER          IWORK(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION F06EKF, X02AJF
      EXTERNAL         F06EKF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F03AFF, F04YCF, F06PJF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Example 1'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*)
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N =', N, '.'
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*        First compute the norm of A. F06EKF returns the sum of the
*        absolute values of a column of A.
         ANORM = ZERO
         DO 20 J = 1, N
            ANORM = MAX(ANORM,F06EKF(N,A(1,J),1))
   20    CONTINUE
         WRITE (NOUT,99998) 'Computed norm of A =', ANORM
*        Next estimate the norm of inverse(A). We do not form the
*        inverse explicitly.
         EPS = X02AJF()
         IFAIL = 0
*
*        Factorise A as P*A = L*U using F03AFF.
         CALL F03AFF(N,EPS,A,LDA,D1,ID,P,IFAIL)
         ICASE = 0
*
   40    CALL F04YCF(ICASE,N,X,ESTNRM,WORK,IWORK,IFAIL)
*
         IF (ICASE.NE.0) THEN
            IF (ICASE.EQ.1) THEN
*              Return the vector inv(P*A)*X by solving the equations
*              L*U*Y = X, overwriting Y on X. First solve L*Z = X for Z.
               CALL F06PJF('Lower','No Transpose','Non-Unit',N,A,LDA,X,
     +                     1)
*              Then solve U*Y = Z for Y.
               CALL F06PJF('Upper','No Transpose','Unit',N,A,LDA,X,1)
            ELSE IF (ICASE.EQ.2) THEN
*              Return the vector inv(P*A)'*X by solving U'*L'*Y = X,
*              overwriting Y on X. First solve U'*Z = X for Z.
               CALL F06PJF('Upper','Transpose','Unit',N,A,LDA,X,1)
*              Then solve L'*Y = Z for Y.
               CALL F06PJF('Lower','Transpose','Non-Unit',N,A,LDA,X,1)
            END IF
*           Continue until ICASE is returned as 0.
            GO TO 40
         ELSE
            WRITE (NOUT,99998) 'Estimated norm of inverse(A) =', ESTNRM
         END IF
         COND = ANORM*ESTNRM
         WRITE (NOUT,99997) 'Estimated condition number of A =', COND
         WRITE (NOUT,*)
      END IF
*
99999 FORMAT (1X,A,I5,A)
99998 FORMAT (1X,A,F8.4)
99997 FORMAT (1X,A,F5.1)
      END
*
      SUBROUTINE EX2
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NZMAX, LICN, LIRN
      PARAMETER        (NMAX=20,NZMAX=25,LICN=4*NZMAX,LIRN=2*NZMAX)
      DOUBLE PRECISION TENTH, ZERO
      PARAMETER        (TENTH=0.1D+0,ZERO=0.0D+0)
*     .. Local Scalars ..
      DOUBLE PRECISION ANORM, COND, ESTNRM, RESID, SUM, U
      INTEGER          I, ICASE, IFAIL, J, N, NZ
      LOGICAL          GROW, LBLOCK
*     .. Local Arrays ..
      DOUBLE PRECISION A(LICN), W(NMAX), WORK1(NMAX), X(NMAX)
      INTEGER          ICN(LICN), IDISP(10), IKEEP(5*NMAX), IRN(LIRN),
     +                 IW(8*NMAX), IWORK(NMAX)
      LOGICAL          ABORT(4)
*     .. External Subroutines ..
      EXTERNAL         F01BRF, F04AXF, F04YCF
*     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX
*     .. Executable Statements ..
      WRITE (NOUT,*)
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Example 2'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*)
*     Input N, the order of matrix A, and NZ, the number of non-zero
*     elements of A.
      READ (NIN,*) N, NZ
      WRITE (NOUT,*)
      IF (N.GT.NMAX .OR. NZ.GT.NZMAX) THEN
         WRITE (NOUT,99999) 'N or NZ is out of range: N =', N,
     +     ',   NZ =', NZ, '.'
      ELSE
*        Input the elements of A, along with row and column information.
         READ (NIN,*) (A(I),IRN(I),ICN(I),I=1,NZ)
*        First compute the norm of A.
         ANORM = 0
         DO 40 I = 1, N
            SUM = ZERO
            DO 20 J = 1, NZ
               IF (ICN(J).EQ.I) SUM = SUM + ABS(A(J))
   20       CONTINUE
            ANORM = MAX(ANORM,SUM)
   40    CONTINUE
         WRITE (NOUT,99998) 'Computed norm of A =', ANORM
*        Next estimate the norm of inverse(A). We do not form the
*        inverse explicitly.
*        Factorise A into L*U using F01BRF.
         U = TENTH
         LBLOCK = .TRUE.
         GROW = .TRUE.
         ABORT(1) = .TRUE.
         ABORT(2) = .TRUE.
         ABORT(3) = .FALSE.
         ABORT(4) = .TRUE.
         IFAIL = 110
*
         CALL F01BRF(N,NZ,A,LICN,IRN,LIRN,ICN,U,IKEEP,IW,W,LBLOCK,GROW,
     +               ABORT,IDISP,IFAIL)
         ICASE = 0
*
   60    CALL F04YCF(ICASE,N,X,ESTNRM,WORK1,IWORK,IFAIL)
*
         IF (ICASE.NE.0) THEN
*           Return X := inv(A)*X or X = inv(A)'*X, depending on the
*           value of ICASE, by solving A*Y = X or A'*Y = X,
*           overwriting Y on X.
            CALL F04AXF(N,A,LICN,ICN,IKEEP,X,W,ICASE,IDISP,RESID)
*           Continue until ICASE is returned as 0.
            GO TO 60
         ELSE
            WRITE (NOUT,99998) 'Estimated norm of inverse(A) =', ESTNRM
         END IF
         COND = ANORM*ESTNRM
         WRITE (NOUT,99997) 'Estimated condition number of A =', COND
      END IF
*
99999 FORMAT (1X,A,I5,A,I5,A)
99998 FORMAT (1X,A,F8.4)
99997 FORMAT (1X,A,F5.1)
      END
