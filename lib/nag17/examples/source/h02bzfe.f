*     H02BZF Example Program Text
*     Mark 16 Revised. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=10,MMAX=10)
      INTEGER          LDA
      PARAMETER        (LDA=MMAX)
      INTEGER          LIWORK, LRWORK
      PARAMETER        (LIWORK=1000,LRWORK=1000)
*     .. Local Scalars ..
      DOUBLE PRECISION BIGBND, INIVAL, OBJMIP, TOLFES, TOLIV
      INTEGER          I, IFAIL, INTFST, ITMAX, J, M, MAXDPT, MAXNOD,
     +                 MSGLVL, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), BL(NMAX+MMAX), BU(NMAX+MMAX),
     +                 CLAMDA(NMAX+MMAX), CVEC(NMAX), RWORK(LRWORK),
     +                 X(NMAX)
      INTEGER          INTVAR(NMAX), ISTATE(NMAX+MMAX), IWORK(LIWORK)
      CHARACTER*8      NAMES(NMAX+MMAX)
*     .. External Subroutines ..
      EXTERNAL         H02BBF, H02BZF, OUTSOL
*     .. Executable Statements ..
      WRITE (NOUT,*) 'H02BZF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
*
*        Read ITMAX, MSGLVL, MAXNOD, INTFST, MAXDPT, TOLFES, TOLIV,
*        CVEC, A, BIGBND, BL, BU, INTVAR and X from data file
*
         READ (NIN,*) ITMAX, MSGLVL
         READ (NIN,*) MAXNOD
         READ (NIN,*) INTFST, MAXDPT
         READ (NIN,*) TOLFES, TOLIV
         READ (NIN,*) (CVEC(J),J=1,N)
         READ (NIN,*) (NAMES(J),(A(I,J),I=1,M),J=1,N)
         READ (NIN,*) BIGBND
         READ (NIN,*) (BL(I),I=1,N)
         READ (NIN,*) (NAMES(N+I),BL(N+I),I=1,M)
         READ (NIN,*) (BU(I),I=1,N+M)
         READ (NIN,*) (INTVAR(I),I=1,N)
         READ (NIN,*) (X(I),I=1,N)
*
*        Solve the IP problem using H02BBF
*
         IFAIL = -1
*
         CALL H02BBF(ITMAX,MSGLVL,N,M,A,LDA,BL,BU,INTVAR,CVEC,MAXNOD,
     +               INTFST,MAXDPT,TOLIV,TOLFES,BIGBND,X,OBJMIP,IWORK,
     +               LIWORK,RWORK,LRWORK,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.EQ.7 .OR. IFAIL.EQ.9) THEN
            WRITE (NOUT,99999) 'IP objective value = ', OBJMIP
*
*           Get information about the solution
*
            IFAIL = 0
*
            CALL H02BZF(N,M,BL,BU,CLAMDA,ISTATE,IWORK,LIWORK,RWORK,
     +                  LRWORK,IFAIL)
*
*           Print the solution
*
            CALL OUTSOL(N,M,A,LDA,BL,BU,X,ISTATE,CLAMDA,BIGBND,NAMES,
     +                  NOUT)
*
*           Increase the energy requirements and solve the modified IP
*           problem using the current IP solution as the starting point
*
            INIVAL = BL(N+1)
            READ (NIN,*) BL(N+1)
            WRITE (NOUT,99998)
     +        'Increase the energy requirements from', INIVAL, 'to',
     +        BL(N+1)
*
            IFAIL = -1
*
            CALL H02BBF(ITMAX,MSGLVL,N,M,A,LDA,BL,BU,INTVAR,CVEC,MAXNOD,
     +                  INTFST,MAXDPT,TOLIV,TOLFES,BIGBND,X,OBJMIP,
     +                  IWORK,LIWORK,RWORK,LRWORK,IFAIL)
*
            IF (IFAIL.EQ.0 .OR. IFAIL.EQ.7 .OR. IFAIL.EQ.9) THEN
               WRITE (NOUT,99999) 'IP objective value = ', OBJMIP
*
*              Get information about the solution
*
               IFAIL = 0
*
               CALL H02BZF(N,M,BL,BU,CLAMDA,ISTATE,IWORK,LIWORK,RWORK,
     +                     LRWORK,IFAIL)
*
*              Print the solution
*
               CALL OUTSOL(N,M,A,LDA,BL,BU,X,ISTATE,CLAMDA,BIGBND,NAMES,
     +                     NOUT)
*
            ELSE
               WRITE (NOUT,99997) ' H02BBF terminated with IFAIL = '
     +           , IFAIL
            END IF
         ELSE
            WRITE (NOUT,99997) ' H02BBF terminated with IFAIL = ',
     +        IFAIL
         END IF
      END IF
      STOP
*
99999 FORMAT (//1X,A,1P,G16.4)
99998 FORMAT (//1X,A,2X,1P,G10.4,2X,A,2X,1P,G10.4)
99997 FORMAT (1X,A,I3)
      END
      SUBROUTINE OUTSOL(N,M,A,LDA,BL,BU,X,ISTATE,CLAMDA,BIGBND,NAMES,
     +                  NOUT)
*     .. Scalar Arguments ..
      DOUBLE PRECISION  BIGBND
      INTEGER           LDA, M, N, NOUT
*     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), BL(N+M), BU(N+M), CLAMDA(N+M), X(N)
      INTEGER           ISTATE(N+M)
      CHARACTER*8       NAMES(N+M)
*     .. Local Scalars ..
      DOUBLE PRECISION  B1, B2, RES, RES2, V, WLAM
      INTEGER           IS, J, K
      CHARACTER*80      REC
*     .. Local Arrays ..
      CHARACTER*2       LSTATE(-2:4)
*     .. External Functions ..
      DOUBLE PRECISION  DDOT
      EXTERNAL          DDOT
*     .. Intrinsic Functions ..
      INTRINSIC         ABS
*     .. Data statements ..
      DATA              LSTATE(-2)/'--'/, LSTATE(-1)/'++'/,
     +                  LSTATE(0)/'FR'/, LSTATE(1)/'LL'/,
     +                  LSTATE(2)/'UL'/, LSTATE(3)/'EQ'/,
     +                  LSTATE(4)/'TF'/
*     .. Executable Statements ..
*
      WRITE (NOUT,99999)
      DO 20 J = 1, N + M
         B1 = BL(J)
         B2 = BU(J)
         WLAM = CLAMDA(J)
         IS = ISTATE(J)
         IF (J.LE.N) THEN
*           The variables  x.
            K = J
            V = X(J)
         ELSE
*           The linear constraints  A*x.
            IF (J.EQ.N+1) WRITE (NOUT,99998)
            K = J - N
            V = DDOT(N,A(K,1),LDA,X,1)
         END IF
*
*        Print a line for the j-th variable or constraint.
*
         RES = V - B1
         RES2 = B2 - V
         IF (ABS(RES).GT.ABS(RES2)) RES = RES2
         WRITE (REC,99997) NAMES(J), LSTATE(IS), V, B1, B2, WLAM,
     +     RES
         IF (B1.LE.-BIGBND) REC(29:42) = '     None      '
         IF (B2.GE.BIGBND) REC(43:56) = '     None      '
         WRITE (NOUT,'(A)') REC
   20 CONTINUE
      RETURN
*
99999 FORMAT (//1X,'Varbl',3X,'State',5X,'Value',5X,'Lower Bound',3X,
     +       'Upper Bound',4X,'Lagr Mult',3X,'Residual',/)
99998 FORMAT (//1X,'L Con',3X,'State',5X,'Value',5X,'Lower Bound',3X,
     +       'Upper Bound',4X,'Lagr Mult',3X,'Residual',/)
99997 FORMAT (1X,A8,2X,A2,1X,1P,3G14.4,1P,G12.4,1P,G12.4)
      END
