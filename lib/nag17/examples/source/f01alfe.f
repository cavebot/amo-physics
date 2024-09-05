*     F01ALF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IA, IZ, IB
      PARAMETER        (NMAX=8,IA=NMAX,IZ=NMAX,IB=NMAX+2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DULUS, EPS, RLB, RUB
      INTEGER          I, IFAIL, J, K, L, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(IA,NMAX), AA(IA,NMAX), B(IB,NMAX), U(NMAX),
     +                 V(NMAX), WI(NMAX), WR(NMAX), Z(IZ,NMAX)
      INTEGER          ICNT(NMAX), INTGER(NMAX)
      LOGICAL          C(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION A02ABF, X02AJF
      EXTERNAL         A02ABF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AKF, F01ALF, F02APF, F02BKF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01ALF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) RLB, RUB
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         K = 1
         L = N
*
*        Reduce to upper Hessenberg form
         CALL F01AKF(N,K,L,A,IA,INTGER)
*
*        F02APF destroys Hessenberg matrix. Copy it into AA.
         DO 40 I = 1, N
            DO 20 J = 1, N
               AA(I,J) = A(I,J)
   20       CONTINUE
   40    CONTINUE
         IFAIL = 0
         EPS = X02AJF()
*
*        Eigenvalues of Hessenberg matrix
         CALL F02APF(N,EPS,AA,IA,WR,WI,ICNT,IFAIL)
*
         M = 0
         DO 60 I = 1, N
            DULUS = A02ABF(WR(I),WI(I))
            C(I) = DULUS .GT. RLB .AND. DULUS .LT. RUB
            IF (C(I)) M = M + 1
   60    CONTINUE
*
*        Selected eigenvectors of Hessenberg matrix
         CALL F02BKF(N,M,A,IA,WI,C,WR,Z,IZ,B,IB,U,V,IFAIL)
*
*        Eigenvectors of real matrix from those of Hessenberg matrix
         CALL F01ALF(K,L,M,A,IA,INTGER,Z,IZ,N)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         DO 80 I = 1, N
            IF (C(I)) WRITE (NOUT,99998) ' (', WR(I), ',', WI(I), ')'
   80    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 100 I = 1, N
            WRITE (NOUT,99999) (Z(I,J),J=1,M)
  100    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,8F9.4)
99998 FORMAT (1X,A,F7.3,A,F7.3,A)
      END
