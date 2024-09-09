*     F11JDF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LA, LIWORK, LWORK
      PARAMETER        (NMAX=1000,LA=10000,LIWORK=NMAX+1,LWORK=6*NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION ANORM, OMEGA, SIGERR, SIGMAX, SIGTOL, STPLHS,
     +                 STPRHS, TOL
      INTEGER          I, IFAIL, IREVCM, ITERM, ITN, ITS, LWNEED,
     +                 MAXITN, MAXITS, MONIT, N, NNZ
      CHARACTER        CKJDF, CKXEF, NORM, PRECON, SIGCMP, WEIGHT
      CHARACTER*6      METHOD
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA), B(NMAX), RDIAG(NMAX), WORK(LWORK), X(NMAX)
      INTEGER          ICOL(LA), IROW(LA), IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         F11GAF, F11GBF, F11GCF, F11JDF, F11XEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F11JDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*
*     Read algorithmic parameters
*
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
         READ (NIN,*) NNZ
         READ (NIN,*) METHOD
         READ (NIN,*) PRECON, SIGCMP, NORM, ITERM
         READ (NIN,*) TOL, MAXITN
         READ (NIN,*) ANORM, SIGMAX
         READ (NIN,*) SIGTOL, MAXITS
         READ (NIN,*) OMEGA
*
*     Read the matrix A
*
         DO 20 I = 1, NNZ
            READ (NIN,*) A(I), IROW(I), ICOL(I)
   20    CONTINUE
*
*     Read right-hand side vector b and initial approximate solution x
*
         READ (NIN,*) (B(I),I=1,N)
         READ (NIN,*) (X(I),I=1,N)
*
*     Call F11GAF to initialize solver
*
         WEIGHT = 'N'
         MONIT = 0
         IFAIL = 0
         CALL F11GAF(METHOD,PRECON,SIGCMP,NORM,WEIGHT,ITERM,N,TOL,
     +               MAXITN,ANORM,SIGMAX,SIGTOL,MAXITS,MONIT,LWNEED,
     +               IFAIL)
*
*     Calculate reciprocal diagonal matrix elements.
*
         DO 40 I = 1, N
            IWORK(I) = 0
   40    CONTINUE
*
         DO 60 I = 1, NNZ
            IF (IROW(I).EQ.ICOL(I)) THEN
               IWORK(IROW(I)) = IWORK(IROW(I)) + 1
               IF (A(I).NE.0.0D0) THEN
                  RDIAG(IROW(I)) = 1.0D0/A(I)
               ELSE
                  WRITE (NOUT,*)
     +              'Matrix has a zero diagonal element'
                  GO TO 140
               END IF
            END IF
   60    CONTINUE
*
         DO 80 I = 1, N
            IF (IWORK(I).EQ.0) THEN
               WRITE (NOUT,*)
     +           'Matrix has a missing diagonal element'
               GO TO 140
            END IF
            IF (IWORK(I).GE.2) THEN
               WRITE (NOUT,*)
     +           'Matrix has a multiple diagonal element'
               GO TO 140
            END IF
   80    CONTINUE
*
*     Call F11GBF to solve the linear system
*
         IREVCM = 0
         CKXEF = 'C'
         CKJDF = 'C'
*
  100    CONTINUE
*
         CALL F11GBF(IREVCM,X,B,WORK,LWORK,IFAIL)
*
         IF (IREVCM.EQ.1) THEN
*
*         Compute matrix vector product
*
            CALL F11XEF(N,NNZ,A,IROW,ICOL,CKXEF,X,B,IFAIL)
            CKXEF = 'N'
            GO TO 100
*
         ELSE IF (IREVCM.EQ.2) THEN
*
*         SSOR preconditioning
*
            CALL F11JDF(N,NNZ,A,IROW,ICOL,RDIAG,OMEGA,CKJDF,X,B,IWORK,
     +                  IFAIL)
            CKJDF = 'N'
            GO TO 100
*
         ELSE IF (IREVCM.EQ.4) THEN
*
*         Termination
*
            CALL F11GCF(ITN,STPLHS,STPRHS,ANORM,SIGMAX,ITS,SIGERR,IFAIL)
*
            WRITE (NOUT,99999) 'Converged in', ITN,
     +        ' iterations'
            WRITE (NOUT,99998) 'Final residual norm =',
     +        STPLHS
*
*         Output x
*
            DO 120 I = 1, N
               WRITE (NOUT,99997) X(I)
  120       CONTINUE
*
         END IF
*
  140    CONTINUE
      END IF
      STOP
99999 FORMAT (1X,A,I10,A)
99998 FORMAT (1X,A,1P,D16.3)
99997 FORMAT (1X,1P,D16.4)
      END
