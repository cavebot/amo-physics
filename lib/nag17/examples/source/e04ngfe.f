*     E04NGF Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NCMAX
      PARAMETER        (NMAX=10,NCMAX=10)
      INTEGER          LDA
      PARAMETER        (LDA=NMAX)
      INTEGER          LIWORK, LWORK
      PARAMETER        (LIWORK=1000,LWORK=10000)
*     .. Local Scalars ..
      DOUBLE PRECISION OBJ
      INTEGER          I, IFAIL, INFORM, ITER, J, LDH, N, NCLIN
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), AX(NCMAX), BL(NMAX+NCMAX),
     +                 BU(NMAX+NCMAX), CLAMDA(NMAX+NCMAX), CVEC(NMAX),
     +                 H(NMAX*(NMAX+1)/2), WORK(LWORK), X(NMAX)
      INTEGER          ISTATE(NMAX+NCMAX), IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         E04NFF, E04NGF, E04NHF, QPHESS, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E04NGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NCLIN
      IF (N.LE.NMAX .AND. NCLIN.LE.NCMAX) THEN
*
*        Read CVEC, A, BL, BU, X, UPLO and H from data file
*
         READ (NIN,*) (CVEC(I),I=1,N)
         READ (NIN,*) ((A(I,J),J=1,N),I=1,NCLIN)
         READ (NIN,*) (BL(I),I=1,N+NCLIN)
         READ (NIN,*) (BU(I),I=1,N+NCLIN)
         READ (NIN,*) (X(I),I=1,N)
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
*           Read the upper triangle of H
            READ (NIN,*) ((H(J+(2*N-I)*(I-1)/2),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
*           Read the lower triangle of H
            READ (NIN,*) ((H(I+(2*N-J)*(J-1)/2),J=1,I),I=1,N)
         END IF
         LDH = N*(N+1)/2
*
*        Set four options using E04NHF
*
         CALL E04NHF(' Print Level = 1 ')
*
         CALL E04NHF(' Check Frequency = 10 ')
*
         CALL E04NHF(' Crash Tolerance = 0.05 ')
*
         CALL E04NHF(' Infinite Bound Size = 1.0D+25 ')
*
*        Set the unit number for advisory messages to NOUT
*
         CALL X04ABF(1,NOUT)
*
*        Read the options file for the remaining options
*
         CALL E04NGF(NIN,INFORM)
*
         IF (INFORM.NE.0) THEN
            WRITE (NOUT,99999) 'E04NGF terminated with INFORM = ',
     +        INFORM
            STOP
         END IF
*
*        Solve the problem
*
         IFAIL = -1
*
         CALL E04NFF(N,NCLIN,A,LDA,BL,BU,CVEC,H,LDH,QPHESS,ISTATE,X,
     +               ITER,OBJ,AX,CLAMDA,IWORK,LIWORK,WORK,LWORK,IFAIL)
*
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
      END
*
      SUBROUTINE QPHESS(N,JTHCOL,HESS,LDHESS,X,HX)
*     In this version of QPHESS, the lower triangle of the matrix H is
*     stored in packed form (by columns) in the one-dimensional array
*     HESS. More precisely, the lower triangle of H must be stored with
*     element H(i,j) in HESS(i+(2*N-j)*(j-1)/2) for i .ge. j.
*     Note that storing the lower triangle of H in packed form (by
*     columns) is equivalent to storing the upper triangle of H in
*     packed form (by rows).
*     Note also that LDHESS is used to define the length of HESS, and
*     must therefore be at least N*(N+1)/2.
*     .. Scalar Arguments ..
      INTEGER           JTHCOL, LDHESS, N
*     .. Array Arguments ..
      DOUBLE PRECISION  HESS(LDHESS), HX(N), X(N)
*     .. Local Scalars ..
      DOUBLE PRECISION  S
      INTEGER           I, INC, J, L, LP1
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
         IF (JTHCOL.LT.N) THEN
            LP1 = L
            DO 40 I = JTHCOL + 1, N
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
         L = 0
         DO 120 J = 1, N - 1
            L = L + 1
            DO 100 I = J + 1, N
               L = L + 1
               HX(I) = HX(I) + HESS(L)*X(J)
  100       CONTINUE
  120    CONTINUE
      END IF
      RETURN
      END
