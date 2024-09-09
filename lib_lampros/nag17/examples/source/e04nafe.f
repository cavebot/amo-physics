*     E04NAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, NCLIN, NROWA, NCTOTL, NROWH, NROWH1, NCOLH,
     +                 NCOLH1, LIWORK, LWORK
      PARAMETER        (N=7,NCLIN=7,NROWA=NCLIN,NCTOTL=N+NCLIN,NROWH=1,
     +                 NROWH1=7,NCOLH=1,NCOLH1=7,LIWORK=2*N,
     +                 LWORK=2*N*N+5*N+NCLIN+2*NCLIN*NROWA)
      DOUBLE PRECISION ZERO, TWO
      PARAMETER        (ZERO=0.0D0,TWO=2.0D0)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION BIGBND, EPSMCH, OBJ, RTEPS
      INTEGER          I, IFAIL, ITER, ITMAX, J, MSGLVL
      LOGICAL          COLD, LP, ORTHOG
*     .. Local Arrays ..
      DOUBLE PRECISION A(NROWA,N), BL(NCTOTL), BU(NCTOTL),
     +                 CLAMDA(NCTOTL), CVEC(N), FEATOL(NCTOTL),
     +                 HESS(NROWH,NCOLH), HESS1(NROWH1,NCOLH1),
     +                 WORK(LWORK), X(N)
      INTEGER          ISTATE(NCTOTL), IWORK(LIWORK)
*     .. External Functions ..
      DOUBLE PRECISION X02AJF
      EXTERNAL         X02AJF
*     .. External Subroutines ..
      EXTERNAL         E04NAF, QPHES1, QPHES2, X04ABF
*     .. Intrinsic Functions ..
      INTRINSIC        SQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04NAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      CALL X04ABF(1,NOUT)
      ITMAX = 20
*     * Change MSGLVL to a value .GE. 5 to get intermediate output *
      MSGLVL = 1
      BIGBND = 1.0D10
      EPSMCH = X02AJF()
      RTEPS = SQRT(EPSMCH)
      DO 20 J = 1, NCTOTL
         FEATOL(J) = RTEPS
   20 CONTINUE
      COLD = .TRUE.
      LP = .FALSE.
      ORTHOG = .TRUE.
*
      READ (NIN,*) (CVEC(J),J=1,N)
      READ (NIN,*) ((A(I,J),J=1,N),I=1,NCLIN)
      READ (NIN,*) (BL(J),J=1,NCTOTL)
      READ (NIN,*) (BU(J),J=1,NCTOTL)
      READ (NIN,*) (X(J),J=1,N)
*
*     Solve the problem from a cold start.
*     The Hessian is defined implicitly by subroutine QPHES1.
*
      IFAIL = 1
*
      CALL E04NAF(ITMAX,MSGLVL,N,NCLIN,NCTOTL,NROWA,NROWH,NCOLH,BIGBND,
     +            A,BL,BU,CVEC,FEATOL,HESS,QPHES1,COLD,LP,ORTHOG,X,
     +            ISTATE,ITER,OBJ,CLAMDA,IWORK,LIWORK,WORK,LWORK,IFAIL)
*
      IF (IFAIL.EQ.0) THEN
*
*        The following is for illustrative purposes only. We do a warm
*        start with the final working set of the previous run.
*        This time we store the Hessian explicitly in HESS1, and use
*        the corresponding subroutine QPHES2.
*
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     'A run of the same example with a warm start ...'
         COLD = .FALSE.
         DO 60 J = 1, N
            DO 40 I = 1, N
               HESS1(I,J) = ZERO
   40       CONTINUE
            IF (J.LE.5) HESS1(J,J) = TWO
            IF (J.GT.5) HESS1(J,J) = -TWO
   60    CONTINUE
         HESS1(3,4) = TWO
         HESS1(4,3) = TWO
         HESS1(6,7) = -TWO
         HESS1(7,6) = -TWO
         IFAIL = 1
*
         CALL E04NAF(ITMAX,MSGLVL,N,NCLIN,NCTOTL,NROWA,NROWH1,NCOLH1,
     +               BIGBND,A,BL,BU,CVEC,FEATOL,HESS1,QPHES2,COLD,LP,
     +               ORTHOG,X,ISTATE,ITER,OBJ,CLAMDA,IWORK,LIWORK,WORK,
     +               LWORK,IFAIL)
*
      END IF
      IF (IFAIL.GT.0) THEN
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'E04NAF terminated with IFAIL =', IFAIL
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE QPHES1(N,NROWH,NCOLH,JTHCOL,HESS,X,HX)
*     In this version of  QPHESS  the Hessian matrix is implicit.
*     The array  HESS  is not accessed.  There is no special coding
*     for the case  JTHCOL .GT. 0.
*     .. Parameters ..
      DOUBLE PRECISION  TWO
      PARAMETER         (TWO=2.0D0)
*     .. Scalar Arguments ..
      INTEGER           JTHCOL, N, NCOLH, NROWH
*     .. Array Arguments ..
      DOUBLE PRECISION  HESS(NROWH,NCOLH), HX(N), X(N)
*     .. Executable Statements ..
      HX(1) = TWO*X(1)
      HX(2) = TWO*X(2)
      HX(3) = TWO*(X(3)+X(4))
      HX(4) = HX(3)
      HX(5) = TWO*X(5)
      HX(6) = -TWO*(X(6)+X(7))
      HX(7) = HX(6)
      RETURN
      END
*
      SUBROUTINE QPHES2(N,NROWH,NCOLH,JTHCOL,HESS,X,HX)
*     In this version of QPHESS, the matrix  H  is stored in  HESS
*     as a full two-dimensional array.
*     .. Scalar Arguments ..
      INTEGER           JTHCOL, N, NCOLH, NROWH
*     .. Array Arguments ..
      DOUBLE PRECISION  HESS(NROWH,NCOLH), HX(N), X(N)
*     .. Local Scalars ..
      INTEGER           I, J
*     .. Executable Statements ..
      IF (JTHCOL.NE.0) THEN
*        Special case -- extract one column of  H.
         DO 20 I = 1, N
            HX(I) = HESS(I,JTHCOL)
   20    CONTINUE
      ELSE
*        NORMAL CASE.
         DO 40 I = 1, N
            HX(I) = 0.0D0
   40    CONTINUE
         DO 80 J = 1, N
            DO 60 I = 1, N
               HX(I) = HX(I) + HESS(I,J)*X(J)
   60       CONTINUE
   80    CONTINUE
      END IF
      RETURN
      END
*
      SUBROUTINE QPHES3(N,NROWH,NCOLH,JTHCOL,HESS,X,HX)
*     (For illustration only - not used by the example program)
*     In this version of QPHESS, the symmetric part of H is
*     stored in the lower half of the two-dimensional array HESS,
*     i.e., in the elements  HESS(I,J),  I .GE. J.
*     .. Scalar Arguments ..
      INTEGER           JTHCOL, N, NCOLH, NROWH
*     .. Array Arguments ..
      DOUBLE PRECISION  HESS(NROWH,NCOLH), HX(N), X(N)
*     .. Local Scalars ..
      DOUBLE PRECISION  S, XJ
      INTEGER           I, J, JP1, JP1P1, JTHPN, NM1, NUM
*     .. Executable Statements ..
      IF (JTHCOL.NE.0) THEN
*        Special case -- extract one column of  H.
         DO 20 J = 1, JTHCOL
            HX(J) = HESS(JTHCOL,J)
   20    CONTINUE
         NUM = N - JTHCOL
         JP1 = JTHCOL + 1
         IF (NUM.GT.0) THEN
            JP1P1 = JP1
            JTHPN = JTHCOL + NUM - 1
            DO 40 I = JTHCOL, JTHPN
               HX(JP1P1) = HESS(JP1,I)
   40       CONTINUE
         END IF
      ELSE
*        Normal case.
         DO 80 I = 1, N
            S = 0.0D0
            DO 60 J = I, N
               S = S + HESS(J,I)*X(J)
   60       CONTINUE
            HX(I) = S
   80    CONTINUE
         IF (N.GT.1) THEN
            NM1 = N - 1
            DO 120 J = 1, NM1
               XJ = X(J)
               JP1 = J + 1
               DO 100 I = JP1, N
                  HX(I) = HX(I) + HESS(I,J)*XJ
  100          CONTINUE
  120       CONTINUE
         END IF
      END IF
      RETURN
      END
*
      SUBROUTINE QPHES4(N,NROWH,NCOLH,JTHCOL,HESS,X,HX)
*     (For illustration only - not used by the example program)
*     In this version of QPHESS, the symmetric part of H is
*     stored in the one-dimensional array  HESS.  Note that
*     NROWH is used to define the length of HESS, and must
*     be at least N*(N + 1)/2.  The parameter NCOLH is not used
*     here, but it must be set to 1 for the call to E04NAF.
*     .. Scalar Arguments ..
      INTEGER           JTHCOL, N, NCOLH, NROWH
*     .. Array Arguments ..
      DOUBLE PRECISION  HESS(NROWH), HX(N), X(N)
*     .. Local Scalars ..
      DOUBLE PRECISION  S, XJ
      INTEGER           I, INC, J, JP1, JP1PN, L, LP1, NM1, NUM
*     .. Executable Statements ..
      IF (JTHCOL.NE.0) THEN
*        Special case -- extract one column of  H.
         L = JTHCOL
         INC = N
         DO 20 I = 1, JTHCOL
            HX(I) = HESS(L)
            INC = INC - 1
            L = L + INC
   20    CONTINUE
         L = L - INC + 1
         NUM = N - JTHCOL
         JP1 = JTHCOL + 1
         IF (NUM.GT.0) THEN
            LP1 = L
            JP1PN = JP1 + NUM - 1
            DO 40 I = JP1, JP1PN
               HX(I) = HESS(LP1)
               LP1 = LP1 + 1
   40       CONTINUE
         END IF
      ELSE
*        Normal case.
         L = 0
         DO 80 I = 1, N
            S = 0.0D0
            DO 60 J = I, N
               L = L + 1
               S = S + HESS(L)*X(J)
   60       CONTINUE
            HX(I) = S
   80    CONTINUE
         IF (N.GT.1) THEN
            L = 0
            NM1 = N - 1
            DO 120 J = 1, NM1
               XJ = X(J)
               L = L + 1
               JP1 = J + 1
               DO 100 I = JP1, N
                  L = L + 1
                  HX(I) = HX(I) + HESS(L)*XJ
  100          CONTINUE
  120       CONTINUE
         END IF
      END IF
      RETURN
      END
