*     F02FJF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NMAX, LA, LRWORK, KMAX, LWORK, LIWORK, NRX
      PARAMETER        (NMAX=16,LA=10*NMAX,LRWORK=1,KMAX=6,
     +                 LWORK=5*KMAX+2*NMAX,LIWORK=2*LA+7*NMAX+1,
     +                 NRX=NMAX)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Scalars in Common ..
      INTEGER          NNZ
*     .. Arrays in Common ..
      DOUBLE PRECISION A(LA)
      INTEGER          ICOL(LA), IPIV(NMAX), IROW(LA), ISTR(NMAX+1)
*     .. Local Scalars ..
      DOUBLE PRECISION DSCALE, DTOL, TOL
      INTEGER          I, IFAIL, J, K, L, LFILL, M, N, NNZC, NOITS,
     +                 NOVECS, NPIVM
      CHARACTER        MIC, PSTRAT
*     .. Local Arrays ..
      DOUBLE PRECISION D(NMAX), RWORK(LRWORK), WORK(LWORK), X(NRX,KMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      DOUBLE PRECISION DOT
      EXTERNAL         DOT
*     .. External Subroutines ..
      EXTERNAL         F02FJF, F02FJZ, F11JAF, IMAGE
*     .. Common blocks ..
      COMMON           /BLOCK1/A, IROW, ICOL, IPIV, ISTR, NNZ
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F02FJF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, K, TOL
      WRITE (NOUT,*)
      IF (N.LT.5 .OR. N.GT.16) THEN
         WRITE (NOUT,99999) 'N is out of range.  N =', N
      ELSE IF (M.LT.1 .OR. M.GE.K .OR. K.GT.KMAX) THEN
         WRITE (NOUT,99999) 'M or K out of range.  M =', M,
     +     '   K =', K
      ELSE
*
*        Set up the sparse symmetric coefficient matrix A.
*
         L = 0
         DO 20 I = 1, N
            IF (I.GE.5) THEN
               L = L + 1
               A(L) = -0.25D0
               IROW(L) = I
               ICOL(L) = I - 4
            END IF
            IF (I.GE.2) THEN
               L = L + 1
               A(L) = -0.25D0
               IROW(L) = I
               ICOL(L) = I - 1
            END IF
            L = L + 1
            A(L) = 1.0D0
            IROW(L) = I
            ICOL(L) = I
   20    CONTINUE
         NNZ = L
*
*        Call F11JAF to find an incomplete Cholesky factorisation of A.
*
         LFILL = 2
         DTOL = 0.0D0
         MIC = 'Modified'
         DSCALE = 0.0D0
         PSTRAT = 'Markowitz'
         IFAIL = 1
*
         CALL F11JAF(N,NNZ,A,LA,IROW,ICOL,LFILL,DTOL,MIC,DSCALE,PSTRAT,
     +               IPIV,ISTR,NNZC,NPIVM,IWORK,LIWORK,IFAIL)
*
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'F11JAF fails. IFAIL =', IFAIL
         ELSE
*
*           Call F02FJF to find eigenvalues and eigenvectors.
            IFAIL = 1
*           * To obtain monitoring information from the supplied
*           subroutine MONIT, replace the name F02FJZ by MONIT in
*           the next statement, and declare MONIT as external *
*
            NOITS = 1000
            NOVECS = 0
*
            CALL F02FJF(N,M,K,NOITS,TOL,DOT,IMAGE,F02FJZ,NOVECS,X,NRX,D,
     +                  WORK,LWORK,RWORK,LRWORK,IWORK,LIWORK,IFAIL)
*
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,99999) 'Warning - F02FJF returns IFAIL ='
     +           , IFAIL
            END IF
            IF (IFAIL.GE.0 .AND. IFAIL.NE.1 .AND. IFAIL.LE.4 .AND. M.GE.
     +          1) THEN
               DO 40 I = 1, M
                  D(I) = 1.0D0/D(I)
   40          CONTINUE
               WRITE (NOUT,*) 'Final results'
               WRITE (NOUT,*)
               WRITE (NOUT,*) '  Eigenvalues'
               WRITE (NOUT,99998) (D(I),I=1,M)
               WRITE (NOUT,*)
               WRITE (NOUT,*) '  Eigenvectors'
               WRITE (NOUT,99998) ((X(I,J),J=1,M),I=1,N)
            END IF
         END IF
      END IF
   60 STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (1X,1P,4D12.3)
      END
*
      DOUBLE PRECISION FUNCTION DOT(IFLAG,N,Z,W,RWORK,LRWORK,IWORK,
     +                              LIWORK)
*     This function implements the dot product - transpose(W)*B*Z.
*     DOT assumes that N is at least 3.
*     .. Scalar Arguments ..
      INTEGER                       IFLAG, LIWORK, LRWORK, N
*     .. Array Arguments ..
      DOUBLE PRECISION              RWORK(LRWORK), W(N), Z(N)
      INTEGER                       IWORK(LIWORK)
*     .. Local Scalars ..
      DOUBLE PRECISION              S
      INTEGER                       I
*     .. Executable Statements ..
      S = 0.0D0
      S = S + (Z(1)-0.5D0*Z(2))*W(1)
      S = S + (-0.5D0*Z(N-1)+Z(N))*W(N)
      DO 20 I = 2, N - 1
         S = S + (-0.5D0*Z(I-1)+Z(I)-0.5D0*Z(I+1))*W(I)
   20 CONTINUE
      DOT = S
      RETURN
      END
*
      SUBROUTINE IMAGE(IFLAG,N,Z,W,RWORK,LRWORK,IWORK,LIWORK)
*     This routine solves  A*W = B*Z  for W.
*     The routine assumes that N is at least 3.
*     A, IROW, ICOL, IPIV, ISTR and NNZ must be as returned by routine
*     F11JAF.
*     .. Parameters ..
      INTEGER          NMAX, LA, LWORK
      PARAMETER        (NMAX=16,LA=10*NMAX,LWORK=6*NMAX)
*     .. Scalar Arguments ..
      INTEGER          IFLAG, LIWORK, LRWORK, N
*     .. Array Arguments ..
      DOUBLE PRECISION RWORK(LRWORK), W(N), Z(N)
      INTEGER          IWORK(LIWORK)
*     .. Scalars in Common ..
      INTEGER          NNZ
*     .. Arrays in Common ..
      DOUBLE PRECISION A(LA)
      INTEGER          ICOL(LA), IPIV(NMAX), IROW(LA), ISTR(NMAX+1)
*     .. Local Scalars ..
      DOUBLE PRECISION RNORM, TOL
      INTEGER          IFAIL, ITN, J, MAXITN
      CHARACTER*2      METHOD
*     .. Local Arrays ..
      DOUBLE PRECISION RHS(NMAX), WORK(LWORK)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         F11JCF
*     .. Common blocks ..
      COMMON           /BLOCK1/A, IROW, ICOL, IPIV, ISTR, NNZ
*     .. Executable Statements ..
*
*     Form B*Z in RHS and initialize W to zero.
*
      RHS(1) = Z(1) - 0.5D0*Z(2)
      W(1) = 0.0D0
      RHS(N) = -0.5D0*Z(N-1) + Z(N)
      W(N) = 0.0D0
      DO 20 J = 2, N - 1
         RHS(J) = -0.5D0*Z(J-1) + Z(J) - 0.5D0*Z(J+1)
         W(J) = 0.0D0
   20 CONTINUE
*
*     Call F11JCF to solve the equations  A*W = B*Z.
*
      METHOD = 'CG'
      TOL = X02AJF()
      MAXITN = 100
      IFAIL = 1
*
      CALL F11JCF(METHOD,N,NNZ,A,LA,IROW,ICOL,IPIV,ISTR,RHS,TOL,MAXITN,
     +            W,RNORM,ITN,WORK,LWORK,IFAIL)
*
      IF (IFAIL.GT.0) IFLAG = -IFAIL
      RETURN
      END
*
      SUBROUTINE MONIT(ISTATE,NEXTIT,NEVALS,NEVECS,K,F,D)
*     Monitoring routine for F02FJF.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalar Arguments ..
      INTEGER          ISTATE, K, NEVALS, NEVECS, NEXTIT
*     .. Array Arguments ..
      DOUBLE PRECISION D(K), F(K)
*     .. Local Scalars ..
      INTEGER          I
*     .. Executable Statements ..
      IF (ISTATE.NE.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) '  ISTATE = ', ISTATE, ' NEXTIT = ', NEXTIT
         WRITE (NOUT,99999) '  NEVALS = ', NEVALS, ' NEVECS = ', NEVECS
         WRITE (NOUT,*) '       F           D'
         WRITE (NOUT,99998) (F(I),D(I),I=1,K)
      END IF
      RETURN
*
99999 FORMAT (1X,A,I4,A,I4)
99998 FORMAT (1X,1P,D11.3,3X,D11.3)
      END
