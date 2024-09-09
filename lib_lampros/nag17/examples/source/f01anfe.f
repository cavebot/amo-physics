*     F01ANF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IAR, IAI, IZR, IZI, IBR, IBI
      PARAMETER        (NMAX=4,IAR=NMAX,IAI=NMAX,IZR=NMAX,IZI=NMAX,
     +                 IBR=NMAX,IBI=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DULUS, EPS, RLB, RUB
      INTEGER          I, IFAIL, J, K, L, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION AAI(IAI,NMAX), AAR(IAR,NMAX), AI(IAI,NMAX),
     +                 AR(IAR,NMAX), BI(IBI,NMAX), BR(IBR,NMAX),
     +                 U(NMAX), V(NMAX), WI(NMAX), WR(NMAX),
     +                 ZI(IZI,NMAX), ZR(IZR,NMAX)
      INTEGER          INTGER(NMAX)
      LOGICAL          C(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION A02ABF, X02AJF
      EXTERNAL         A02ABF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         F01AMF, F01ANF, F02ANF, F02BLF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01ANF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         READ (NIN,*) RLB, RUB
         READ (NIN,*) ((AR(I,J),AI(I,J),J=1,N),I=1,N)
         K = 1
         L = N
*
*        Reduce to upper Hessenberg form
         CALL F01AMF(N,K,L,AR,IAR,AI,IAI,INTGER)
*
*        F02ANF destroys Hessenberg form. Copy it into AAR and AAI.
         DO 40 I = 1, N
            DO 20 J = 1, N
               AAR(I,J) = AR(I,J)
               AAI(I,J) = AI(I,J)
   20       CONTINUE
   40    CONTINUE
         IFAIL = 0
         EPS = X02AJF()
*
*        Eigenvalues of Hessenberg matrix
         CALL F02ANF(N,EPS,AAR,IAR,AAI,IAI,WR,WI,IFAIL)
*
         M = 0
         DO 60 I = 1, N
            DULUS = A02ABF(WR(I),WI(I))
            C(I) = DULUS .GT. RLB .AND. DULUS .LT. RUB
            IF (C(I)) M = M + 1
   60    CONTINUE
*
*        Selected eigenvectors of Hessenberg matrix
         CALL F02BLF(N,M,AR,IAR,AI,IAI,WI,C,WR,ZR,IZR,ZI,IZI,BR,IBR,BI,
     +               IBI,U,V,IFAIL)
*
*        Eigenvectors of complex matrix from those of Hessenberg matrix
         CALL F01ANF(K,L,M,AR,IAR,AI,IAI,INTGER,ZR,IZR,ZI,IZI,N)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvalues'
         DO 80 I = 1, N
            IF (C(I)) WRITE (NOUT,99999) ' (', WR(I), ',', WI(I), ')'
   80    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Eigenvectors'
         DO 100 I = 1, N
            WRITE (NOUT,99999) (' (',ZR(I,J),',',ZI(I,J),')',J=1,M)
  100    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,4(A,F7.3,A,F7.3,A))
      END
