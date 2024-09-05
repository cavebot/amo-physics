*     M01DZF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX
      PARAMETER        (NMAX=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Arrays in Common ..
      DOUBLE PRECISION RV(NMAX)
      INTEGER          IV(NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, N
*     .. Local Arrays ..
      INTEGER          IRANK(NMAX)
*     .. External Functions ..
      LOGICAL          COMPAR
      EXTERNAL         COMPAR
*     .. External Subroutines ..
      EXTERNAL         M01DZF, M01ZAF
*     .. Common blocks ..
      COMMON           RV, IV
*     .. Executable Statements ..
      WRITE (NOUT,*) 'M01DZF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.GE.1 .AND. N.LE.NMAX) THEN
         READ (NIN,*) (IV(I),RV(I),I=1,N)
         IFAIL = 0
*
         CALL M01DZF(COMPAR,1,N,IRANK,IFAIL)
         CALL M01ZAF(IRANK,1,N,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) '   Data in sorted order'
         WRITE (NOUT,*)
         DO 20 I = 1, N
            WRITE (NOUT,99999) IV(IRANK(I)), RV(IRANK(I))
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,I7,F7.1)
      END
*
      LOGICAL FUNCTION COMPAR(I,J)
*     .. Parameters ..
      INTEGER                 NMAX
      PARAMETER               (NMAX=100)
*     .. Scalar Arguments ..
      INTEGER                 I, J
*     .. Arrays in Common ..
      DOUBLE PRECISION        RV(NMAX)
      INTEGER                 IV(NMAX)
*     .. Common blocks ..
      COMMON                  RV, IV
*     .. Executable Statements ..
      IF (IV(I).NE.IV(J)) THEN
         COMPAR = IV(I) .GT. IV(J)
      ELSE
         IF (IV(I).LT.0) THEN
            COMPAR = RV(I) .LT. RV(J)
         ELSE IF (IV(I).GT.0) THEN
            COMPAR = RV(I) .GT. RV(J)
         ELSE
            COMPAR = I .LT. J
         END IF
      END IF
      RETURN
      END
