*     F11GAF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LA, LIWORK, LWORK
      PARAMETER        (NMAX=1000,LA=10000,LIWORK=2*LA+7*NMAX+1,
     +                 LWORK=6*NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION ANORM, DSCALE, DTOL, SIGERR, SIGMAX, SIGTOL,
     +                 STPLHS, STPRHS, TOL
      INTEGER          I, IFAIL, IFAIL1, IREVCM, ITERM, ITN, ITS, LFILL,
     +                 LWREQ, MAXITN, MAXITS, MONIT, N, NNZ, NNZC, NPIVM
      LOGICAL          LOOP
      CHARACTER        MIC, NORM, PRECON, PSTRAT, SIGCMP, WEIGHT
      CHARACTER*6      METHOD
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA), B(NMAX), WORK(LWORK), X(NMAX)
      INTEGER          ICOL(LA), IPIV(NMAX), IROW(LA), ISTR(NMAX+1),
     +                 IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         F11GAF, F11GBF, F11GCF, F11JAF, F11JBF, F11XEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F11GAF Example Program Results'
*
*     Skip heading in data file
*
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*     Read or initialize the parameters for the iterative solver
*
         READ (NIN,*) METHOD
         READ (NIN,*) PRECON, SIGCMP, NORM, WEIGHT, ITERM
         READ (NIN,*) TOL, MAXITN
         READ (NIN,*) MONIT
         ANORM = 0.0D0
         SIGMAX = 0.0D0
         SIGTOL = 1.0D-2
         MAXITS = N
*
*     Read the parameters for the preconditioner
*
         READ (NIN,*) LFILL, DTOL
         READ (NIN,*) MIC, DSCALE
         READ (NIN,*) PSTRAT
*
*     Read the number of non-zero elements of the matrix A, then read
*     the non-zero elements
*
         READ (NIN,*) NNZ
         DO 20 I = 1, NNZ
            READ (NIN,*) A(I), IROW(I), ICOL(I)
   20    CONTINUE
*
*     Read right-hand side vector b and initial approximate solution x
*
         READ (NIN,*) (B(I),I=1,N)
         READ (NIN,*) (X(I),I=1,N)
*
*     Initialize any other parameters
*
*
*     Calculate incomplete Cholesky factorization
*
         IFAIL = 0
         CALL F11JAF(N,NNZ,A,LA,IROW,ICOL,LFILL,DTOL,MIC,DSCALE,PSTRAT,
     +               IPIV,ISTR,NNZC,NPIVM,IWORK,LIWORK,IFAIL)
*
*     Call F11GAF to initialize the solver
*
         IFAIL = 0
         CALL F11GAF(METHOD,PRECON,SIGCMP,NORM,WEIGHT,ITERM,N,TOL,
     +               MAXITN,ANORM,SIGMAX,SIGTOL,MAXITS,MONIT,LWREQ,
     +               IFAIL)
*
*     Call repeatedly F11GBF to solve the equations
*     Note that the arrays B and X are overwritten
*
*     On final exit, X will contain the solution and B the residual
*     vector
*
         IFAIL = 0
         IREVCM = 0
         LOOP = .TRUE.
*
         LWREQ = LWORK
   40    CONTINUE
         CALL F11GBF(IREVCM,X,B,WORK,LWREQ,IFAIL)
         IF (IREVCM.EQ.1) THEN
            IFAIL1 = -1
            CALL F11XEF(N,NNZ,A,IROW,ICOL,'No checking',X,B,IFAIL1)
            IF (IFAIL1.NE.0) IREVCM = 6
         ELSE IF (IREVCM.EQ.2) THEN
            IFAIL1 = -1
            CALL F11JBF(N,A,LA,IROW,ICOL,IPIV,ISTR,'No checking',X,B,
     +                  IFAIL1)
            IF (IFAIL1.NE.0) IREVCM = 6
         ELSE IF (IREVCM.EQ.3) THEN
            IFAIL1 = 0
            CALL F11GCF(ITN,STPLHS,STPRHS,ANORM,SIGMAX,ITS,SIGERR,
     +                  IFAIL1)
            WRITE (NOUT,99999) ITN, STPLHS
            WRITE (NOUT,99998)
            WRITE (NOUT,99997) (X(I),B(I),I=1,N)
         ELSE IF (IREVCM.EQ.4) THEN
            LOOP = .FALSE.
         END IF
         IF (LOOP) GO TO 40
*
*     Obtain information about the computation
*
         IFAIL1 = 0
         CALL F11GCF(ITN,STPLHS,STPRHS,ANORM,SIGMAX,ITS,SIGERR,IFAIL1)
*
*     Print the output data
*
         WRITE (NOUT,99996)
         WRITE (NOUT,99995)
     +     'Number of iterations for convergence:    ', ITN
         WRITE (NOUT,99994)
     +     'Residual norm:                           ', STPLHS
         WRITE (NOUT,99994)
     +     'Right-hand side of termination criterion:', STPRHS
         WRITE (NOUT,99994)
     +     '1-norm of matrix A:                      ', ANORM
         WRITE (NOUT,99994)
     +     'Largest singular value of A_bar:         ', SIGMAX
*
*     Output x
*
         WRITE (NOUT,99998)
         WRITE (NOUT,99997) (X(I),B(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (/1X,'Monitoring at iteration no.',I4,/1X,1P,'residual no',
     +       'rm: ',D14.4)
99998 FORMAT (2X,'Solution vector',2X,'Residual vector')
99997 FORMAT (1X,1P,D16.4,1X,D16.4)
99996 FORMAT (/1X,'Final Results')
99995 FORMAT (1X,A,I4)
99994 FORMAT (1X,A,1P,D14.4)
      END
