*     F02BKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, IA, IAA, IZ, IB
      PARAMETER        (NMAX=8,MMAX=8,IA=NMAX,IAA=NMAX,IZ=NMAX,
     +                 IB=NMAX+2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DULUS, RLB, RUB
      INTEGER          I, IFAIL, J, K, L, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), AA(IAA,NMAX), B(IB,NMAX), U(NMAX),
     +                 V(NMAX), WI(NMAX), WR(NMAX), Z(IZ,NMAX)
      INTEGER          INTGER(NMAX)
      LOGICAL          C(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION A02ABF, X02AJF
      EXTERNAL         A02ABF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AKF, F02APF, F02BKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02BKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      WRITE (NOUT,*)
      IF (N.LT.1 .OR. N.GT.NMAX) THEN
         WRITE (NOUT,99999) 'N is out of range: N = ', N
         STOP
      END IF
      READ (NIN,*) RLB, RUB, ((A(I,J),J=1,N),I=1,N)
      K = 1
      L = N
*
*     Reduce to upper Hessenberg form
      CALL F01AKF(N,K,L,A,IA,INTGER)
*
*     F02APF destroys Hessenberg matrix. Copy it into AA.
      DO 40 I = 1, N
         DO 20 J = 1, N
            AA(I,J) = A(I,J)
   20    CONTINUE
   40 CONTINUE
      IFAIL = 1
*
*     Eigenvalues of upper Hessenberg matrix
      CALL F02APF(N,X02AJF(),AA,IAA,WR,WI,INTGER,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'Error in F02APF. IFAIL =', IFAIL
      ELSE
         M = 0
         DO 60 I = 1, N
            DULUS = A02ABF(WR(I),WI(I))
            C(I) = DULUS .GT. RLB .AND. DULUS .LT. RUB
            IF (C(I)) M = M + 1
   60    CONTINUE
         IF (M.GT.MMAX) THEN
            WRITE (NOUT,99999) 'Too many eigenvectors required. M = ', M
            STOP
         END IF
         IFAIL = 1
*
*        Selected eigenvectors of upper Hessenberg matrix
         CALL F02BKF(N,M,A,IA,WI,C,WR,Z,IZ,B,IB,U,V,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'Error in F02BKF. IFAIL =', IFAIL
         ELSE
            WRITE (NOUT,*) 'Eigenvalues'
            DO 80 I = 1, N
               IF (C(I)) WRITE (NOUT,99998) '(', WR(I), ',', WI(I), ')'
   80       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Contents of eigenvector array'
            DO 100 I = 1, N
               WRITE (NOUT,99997) (Z(I,J),J=1,M)
  100       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,A,F7.3,A,F7.3,A)
99997 FORMAT (1X,8F9.4)
      END
