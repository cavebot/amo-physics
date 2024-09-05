*     D02SAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, M, NPOINT, IWP, NMMAX, IW1, IW2, N1
      PARAMETER        (N=3,M=4,NPOINT=3,IWP=NPOINT,NMMAX=M,IW1=NMMAX,
     +                 IW2=3*M+23,N1=N)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      DOUBLE PRECISION XEND
      INTEGER          ICAP
*     .. Local Scalars ..
      DOUBLE PRECISION YMAX
      INTEGER          I, ICOUNT, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION DP(M), E(N), P(M), PE(M), PF(M), W(IW1,IW2),
     +                 WP(IWP,6)
*     .. External Functions ..
      LOGICAL          CONSTR
      EXTERNAL         CONSTR
*     .. External Subroutines ..
      EXTERNAL         BC, D02SAF, D02SAS, EQN, FCN, PRSOL, RANGE,
     +                 X04ABF
*     .. Common blocks ..
      COMMON           /ENDDAT/XEND, ICAP
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D02SAF Example Program Results'
      ICAP = 0
      ICOUNT = 0
      YMAX = 0.0D0
      XEND = 5.0D0
      DO 20 I = 1, M
         PE(I) = 1.0D-3
         PF(I) = 1.0D-6
         DP(I) = 0.0D0
   20 CONTINUE
      DO 40 I = 1, N
         E(I) = 1.0D-5
   40 CONTINUE
      CALL X04ABF(1,NOUT)
      DO 80 I = 1, NPOINT - 1
         DO 60 J = 1, 3
            WP(I,J) = 0.0D0
   60    CONTINUE
   80 CONTINUE
      P(1) = 1.2D0
      P(2) = 0.032D0
      P(3) = 2.5D0
      P(4) = 0.02D0
      IFAIL = 1
*
*     * To obtain monitoring information, replace the name D02SAS
*     by D02HBX in the next statement and declare D02HBX as external *
*
      CALL D02SAF(P,M,N,N1,PE,PF,E,DP,NPOINT,WP,IWP,ICOUNT,RANGE,BC,FCN,
     +            EQN,CONSTR,YMAX,D02SAS,PRSOL,W,IW1,IW2,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'IFAIL = ', IFAIL
         IF (IFAIL.GE.4) THEN
            IF (IFAIL.LE.12) WRITE (NOUT,99998) 'WP(NPOINT,1) = ',
     +          WP(NPOINT,1)
            IF (IFAIL.LE.6) THEN
               WRITE (NOUT,99998) 'WP(1,5) = ', WP(1,5)
               WRITE (NOUT,99997) 'W(.,1) ', (W(I,1),I=1,N)
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,A,F10.4)
99997 FORMAT (1X,A,10D10.3)
      END
*
      SUBROUTINE EQN(F,Q,P,M)
*     .. Scalar Arguments ..
      INTEGER        M, Q
*     .. Array Arguments ..
      DOUBLE PRECISION F(Q), P(M)
*     .. Executable Statements ..
      F(1) = 0.02D0 - P(4) - 1.0D-5*P(3)
      RETURN
      END
*
      SUBROUTINE FCN(X,Y,F,N,P,M,I)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER        I, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), P(M), Y(N)
*     .. Intrinsic Functions ..
      INTRINSIC      COS, TAN
*     .. Executable Statements ..
      F(1) = TAN(Y(3))
      IF (I.EQ.1) THEN
         F(2) = -0.032D0*TAN(Y(3))/Y(2) - 0.02D0*Y(2)/COS(Y(3))
         F(3) = -0.032D0/Y(2)**2
      ELSE
         F(2) = -P(2)*TAN(Y(3))/Y(2) - P(4)*Y(2)/COS(Y(3))
         F(3) = -P(2)/Y(2)**2
      END IF
      RETURN
      END
*
      SUBROUTINE BC(F,G,P,M,N)
*     .. Scalar Arguments ..
      INTEGER       M, N
*     .. Array Arguments ..
      DOUBLE PRECISION F(N), G(N), P(M)
*     .. Executable Statements ..
      F(1) = 0.0D0
      F(2) = 0.5D0
      F(3) = P(1)
      G(1) = 0.0D0
      G(2) = 0.45D0
      G(3) = -1.2D0
      RETURN
      END
*
      SUBROUTINE RANGE(X,NPOINT,P,M)
*     .. Scalar Arguments ..
      INTEGER          M, NPOINT
*     .. Array Arguments ..
      DOUBLE PRECISION P(M), X(NPOINT)
*     .. Scalars in Common ..
      DOUBLE PRECISION XEND
      INTEGER          ICAP
*     .. Common blocks ..
      COMMON           /ENDDAT/XEND, ICAP
*     .. Executable Statements ..
      X(1) = 0.0D0
      X(2) = P(3)
      X(3) = XEND
      RETURN
      END
*
      SUBROUTINE PRSOL(X,Y,N)
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER          N
*     .. Array Arguments ..
      DOUBLE PRECISION Y(N)
*     .. Scalars in Common ..
      DOUBLE PRECISION XEND
      INTEGER          ICAP
*     .. Local Scalars ..
      INTEGER          I
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Common blocks ..
      COMMON           /ENDDAT/XEND, ICAP
*     .. Executable Statements ..
      IF (ICAP.NE.1) THEN
         ICAP = 1
         WRITE (NOUT,*)
         WRITE (NOUT,*) '      X       Y(1)      Y(2)      Y(3)'
      END IF
      WRITE (NOUT,99999) X, (Y(I),I=1,N)
      X = X + 0.5D0
      IF (ABS(X-XEND).LT.0.25D0) X = XEND
      RETURN
*
99999 FORMAT (1X,F9.3,3F10.4)
      END
*
      LOGICAL FUNCTION CONSTR(P,M)
*     .. Scalar Arguments ..
      INTEGER                 M
*     .. Array Arguments ..
      DOUBLE PRECISION        P(M)
*     .. Local Scalars ..
      INTEGER                 I
*     .. Executable Statements ..
      CONSTR = .TRUE.
      DO 20 I = 1, M
         IF (P(I).LT.0.0D0) CONSTR = .FALSE.
   20 CONTINUE
      IF (P(3).GT.5.0D0) CONSTR = .FALSE.
      RETURN
      END
