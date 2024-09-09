*     F01RKF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDPH
      PARAMETER        (MMAX=10,NMAX=20,LDA=MMAX,LDPH=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, M, N, NROWP
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), PH(LDPH,NMAX), THETA(NMAX),
     +                 WORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01RJF, F01RKF
*     .. Intrinsic Functions ..
      INTRINSIC        DCONJG
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01RKF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      WRITE (NOUT,*)
      IF ((M.GT.MMAX) .OR. (N.GT.NMAX)) THEN
         WRITE (NOUT,*) 'M or N is out of range.'
         WRITE (NOUT,99999) 'M = ', M, '   N = ', N
      ELSE
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         IFAIL = 0
*
*        Find the RQ factorization of A
         CALL F01RJF(M,N,A,LDA,THETA,IFAIL)
*
*        Copy the array A into PH and form the n by n matrix conjg(P')
         DO 40 J = 1, N
            DO 20 I = 1, M
               PH(I,J) = A(I,J)
   20       CONTINUE
   40    CONTINUE
         NROWP = N
         IFAIL = 0
*
         CALL F01RKF('Separate',M,N,NROWP,PH,LDPH,THETA,WORK,IFAIL)
*
         WRITE (NOUT,*) 'Matrix  P'
         DO 60 I = 1, N
            WRITE (NOUT,99998) (DCONJG(PH(J,I)),J=1,NROWP)
   60    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I5,A,I5)
99998 FORMAT (5(' (',F6.3,',',F6.3,')',:))
      END
