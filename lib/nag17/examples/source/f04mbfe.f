*     F04MBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, LRWORK, LIWORK
      PARAMETER        (N=10,LRWORK=1,LIWORK=1)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Scalars in Common ..
      INTEGER          JOB
*     .. Arrays in Common ..
      DOUBLE PRECISION D(N), E(N)
*     .. Local Scalars ..
      DOUBLE PRECISION ACOND, ANORM, RNORM, RTOL, SHFT, XNORM
      INTEGER          I, IFAIL, INFORM, ITN, ITNLIM, MSGLVL
      LOGICAL          PRECON
*     .. Local Arrays ..
      DOUBLE PRECISION B(N), RWORK(LRWORK), WORK(N,5), X(N)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         APROD, F04MBF, MSOLVE, X04ABF
*     .. Common blocks ..
      COMMON           /USER/D, E, JOB
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F04MBF Example Program Results'
      WRITE (NOUT,*)
      CALL X04ABF(1,NOUT)
*
*     Set up the matrix M with the diagonal elements in D and
*     the off-diagonal elements in E.
*
      D(1) = 2.0D0
      DO 20 I = 2, N
         D(I) = 2.0D0
         E(I) = 1.0D0
   20 CONTINUE
*
*     Set JOB to zero so that F04FAF factorizes M on the first call
*     inside MSOLVE.
*
      JOB = 0
*
*     Initialize RHS and other quantities required by F04MBF.
*
      B(1) = 6.0D0
      DO 40 I = 2, N - 1
         B(I) = 4.0D0
   40 CONTINUE
      B(N) = 6.0D0
      PRECON = .TRUE.
      SHFT = 0.0D0
      RTOL = 0.00001D0
      ITNLIM = 100
*     * Set MSGLVL to 2 to get output at each iteration *
      MSGLVL = 1
      IFAIL = 1
*
      CALL F04MBF(N,B,X,APROD,MSOLVE,PRECON,SHFT,RTOL,ITNLIM,MSGLVL,ITN,
     +            ANORM,ACOND,RNORM,XNORM,WORK,RWORK,LRWORK,IWORK,
     +            LIWORK,INFORM,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'F04MBF fails. IFAIL =', IFAIL
      ELSE
         WRITE (NOUT,*) 'Solution returned by F04MBF'
         WRITE (NOUT,99998) (X(I),I=1,N)
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,5F9.3)
      END
*
      SUBROUTINE APROD(IFLAG,N,X,Y,RWORK,LRWORK,IWORK,LIWORK)
*     APROD  returns Y = A*X for a given X.
*     .. Scalar Arguments ..
      INTEGER          IFLAG, LIWORK, LRWORK, N
*     .. Array Arguments ..
      DOUBLE PRECISION RWORK(LRWORK), X(N), Y(N)
      INTEGER          IWORK(LIWORK)
*     .. Local Scalars ..
      INTEGER          I
*     .. Executable Statements ..
      Y(1) = 2.0D0*X(1) + X(2) + 3.0D0*X(N)
      DO 20 I = 2, N - 1
         Y(I) = X(I-1) + 2.0D0*X(I) + X(I+1)
   20 CONTINUE
      Y(N) = 3.0D0*X(1) + X(N-1) + 2.0D0*X(N)
      RETURN
      END
*
      SUBROUTINE MSOLVE(IFLAG,N,X,Y,RWORK,LRWORK,IWORK,LIWORK)
*     Given X, MSOLVE solves the equations M*Y = X for Y, without
*     altering X, by calling the NAG Library routine F04FAF.
*     JOB has initially been set to zero in the calling program.
*     .. Parameters ..
      INTEGER           NN
      PARAMETER         (NN=10)
*     .. Scalar Arguments ..
      INTEGER           IFLAG, LIWORK, LRWORK, N
*     .. Array Arguments ..
      DOUBLE PRECISION  RWORK(LRWORK), X(N), Y(N)
      INTEGER           IWORK(LIWORK)
*     .. Scalars in Common ..
      INTEGER           JOB
*     .. Arrays in Common ..
      DOUBLE PRECISION  D(NN), E(NN)
*     .. Local Scalars ..
      INTEGER           I, IFAIL
*     .. External Subroutines ..
      EXTERNAL          F04FAF
*     .. Common blocks ..
      COMMON            /USER/D, E, JOB
*     .. Executable Statements ..
      DO 20 I = 1, N
         Y(I) = X(I)
   20 CONTINUE
      IFAIL = 1
*
      CALL F04FAF(JOB,N,D,E,Y,IFAIL)
*
      IF (IFAIL.NE.0) THEN
         IFLAG = -IFAIL
      ELSE
*        After first call to F04FAF, M will be factorized, so we
*        set JOB = 1.
         IF (JOB.EQ.0) JOB = 1
      END IF
      RETURN
      END
