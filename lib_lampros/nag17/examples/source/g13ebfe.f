*     G13EBF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LMAX
      PARAMETER        (NMAX=6,MMAX=2,LMAX=2)
*     .. Local Scalars ..
      DOUBLE PRECISION DEV, TOL
      INTEGER          I, IFAIL, INFO, ISTEP, J, L, LDM, LDQ, LDS, M, N,
     +                 NCALL
      LOGICAL          FULL, STQ
      CHARACTER        TRANSF
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX,NMAX), AX(NMAX), B(NMAX,LMAX),
     +                 C(MMAX,NMAX), H(MMAX,MMAX), K(NMAX,MMAX),
     +                 P(NMAX,NMAX), Q(LMAX,LMAX), R(MMAX,MMAX),
     +                 S(NMAX,NMAX), U(NMAX,NMAX), US(NMAX,NMAX),
     +                 WK((NMAX+MMAX)*(NMAX+MMAX+LMAX)), X(NMAX),
     +                 Y(MMAX)
      INTEGER          IWK(MMAX)
*     .. External Functions ..
      DOUBLE PRECISION DDOT
      EXTERNAL         DDOT
*     .. External Subroutines ..
      EXTERNAL         DCOPY, DGEMV, DPOTRF, DSYRK, DTRSV, F06QHF,
     +                 G13EBF
*     .. Intrinsic Functions ..
      INTRINSIC        LOG
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13EBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NCALL, N, M, L, STQ, FULL
      IF (N.LE.NMAX .AND. M.LE.MMAX .AND. L.LE.LMAX) THEN
         LDS = NMAX
         LDM = MMAX
         LDQ = LMAX
         CALL F06QHF('G',N,N,0.0D0,0.0D0,S,LDS)
         READ (NIN,*) ((S(I,J),J=1,N),I=1,N)
         IF (FULL) THEN
            CALL DPOTRF('L',N,S,LDS,INFO)
            IF (INFO.GT.0) THEN
               WRITE (NOUT,*) ' S not positive definite'
               GO TO 100
            END IF
         END IF
         READ (NIN,*) (AX(I),I=1,N)
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         READ (NIN,*) ((B(I,J),J=1,L),I=1,N)
         READ (NIN,*) ((C(I,J),J=1,N),I=1,M)
         CALL F06QHF('G',M,M,0.0D0,0.0D0,R,LDM)
         READ (NIN,*) ((R(I,J),J=1,M),I=1,M)
         IF (FULL) THEN
            CALL DPOTRF('L',M,R,LDM,INFO)
            IF (INFO.GT.0) THEN
               WRITE (NOUT,*) ' R not positive definite'
               GO TO 100
            END IF
         END IF
         IF ( .NOT. STQ) THEN
            READ (NIN,*) ((Q(I,J),J=1,L),I=1,L)
            IF (FULL) THEN
               CALL DPOTRF('L',L,Q,LDQ,INFO)
               IF (INFO.GT.0) THEN
                  WRITE (NOUT,*) ' Q not positive definite'
                  GO TO 100
               END IF
            END IF
         END IF
 
         TOL = 0.0D0
         DEV = 0.0D0
         TRANSF = 'T'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '         Residuals'
         WRITE (NOUT,*)
*
*         Loop through data
*
         DO 40 ISTEP = 1, NCALL
            IFAIL = 0
 
            IF (ISTEP.EQ.1) THEN
*
*         Make first call to G13EBF
*
               CALL G13EBF('T',N,M,L,A,LDS,B,STQ,Q,LDQ,C,LDM,R,S,K,H,U,
     +                     TOL,IWK,WK,IFAIL)
*
               CALL DGEMV('N',N,N,1.0D0,U,LDS,AX,1,0.0D0,X,1)
            ELSE
*
               CALL G13EBF('H',N,M,L,A,LDS,B,STQ,Q,LDQ,C,LDM,R,S,K,H,U,
     +                     TOL,IWK,WK,IFAIL)
            END IF
*
            READ (NIN,*) (Y(I),I=1,M)
*
*        Perform time and measurement update
*
            CALL DGEMV('N',M,N,-1.0D0,C,LDM,X,1,1.0D0,Y,1)
            WRITE (NOUT,99999) (Y(I),I=1,M)
            CALL DGEMV('N',N,N,1.0D0,A,LDS,X,1,0.0D0,AX,1)
            CALL DGEMV('N',N,M,1.0D0,K,LDS,Y,1,1.0D0,AX,1)
            CALL DCOPY(N,AX,1,X,1)
*
*        Update loglikelihood
*
            CALL DTRSV('L','N','N',M,H,LDM,Y,1)
            DEV = DEV + DDOT(M,Y,1,Y,1)
            DO 20 I = 1, M
               DEV = DEV + 2.0D0*LOG(H(I,I))
   20       CONTINUE
            TRANSF = 'H'
   40    CONTINUE
*
*        Calculate back-transformed X
*
         CALL DGEMV('T',N,N,1.0D0,U,LDS,AX,1,0.0D0,X,1)
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Final X(I+1:I) '
         WRITE (NOUT,*)
         WRITE (NOUT,99999) (X(J),J=1,N)
*
*        Compute back-transformed P from S
*
         DO 60 I = 1, N
            CALL DGEMV('T',N-I+1,N,1.0D0,U(I,1),LDS,S(I,I),1,0.0D0,
     +                 US(1,I),1)
   60    CONTINUE
         CALL DSYRK('L','N',N,N,1.0D0,US,LDS,0.0D0,P,LDS)
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Final Value of P'
         WRITE (NOUT,*)
         DO 80 I = 1, N
            WRITE (NOUT,99999) (P(I,J),J=1,I)
   80    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99998) ' Deviance = ', DEV
      END IF
  100 CONTINUE
      STOP
*
99999 FORMAT (6F12.4)
99998 FORMAT (A,D13.4)
      END
