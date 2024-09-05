*     D01EAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NDIM, NFUN, IRCLS, MXCLS, LENWRK
      PARAMETER        (NDIM=4,NFUN=10,
     +                 IRCLS=2**NDIM+2*NDIM*NDIM+2*NDIM+1,MXCLS=IRCLS,
     +                 LENWRK=6*NDIM+9*NFUN+(NDIM+NFUN+2)
     +                 *(1+MXCLS/IRCLS))
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ABSREQ, RELREQ
      INTEGER          I, IFAIL, MAXCLS, MINCLS, MULFAC, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NDIM), ABSEST(NFUN), B(NDIM), FINEST(NFUN),
     +                 WRKSTR(LENWRK)
*     .. External Subroutines ..
      EXTERNAL         D01EAF, FUNSUB
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01EAF Example Program Results'
      DO 20 N = 1, NDIM
         A(N) = 0.0D0
         B(N) = 1.0D0
   20 CONTINUE
      MINCLS = 0
      MAXCLS = MXCLS
      ABSREQ = 0.0D0
      RELREQ = 1.0D-3
      IF (NDIM.LE.10) THEN
         MULFAC = 2**NDIM
      ELSE
         MULFAC = 2*NDIM**3
      END IF
   40 IFAIL = -1
*
      CALL D01EAF(NDIM,A,B,MINCLS,MAXCLS,NFUN,FUNSUB,ABSREQ,RELREQ,
     +            LENWRK,WRKSTR,FINEST,ABSEST,IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.GT.0) THEN
         IF (IFAIL.EQ.1 .OR. IFAIL.EQ.3) THEN
            WRITE (NOUT,99999) 'Results so far (', MINCLS,
     +        ' FUNSUB calls in last call of D01EAF)'
            WRITE (NOUT,*)
            WRITE (NOUT,*) '   I       Integral   Estimated error'
            DO 60 I = 1, NFUN
               WRITE (NOUT,99998) I, FINEST(I), ABSEST(I)
   60       CONTINUE
            WRITE (NOUT,*)
            MINCLS = -1
            MAXCLS = MAXCLS*MULFAC
            GO TO 40
         END IF
      ELSE
         WRITE (NOUT,99999) 'Final results (', MINCLS,
     +     ' FUNSUB calls in last call of D01EAF)'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '   I       Integral   Estimated error'
         DO 80 I = 1, NFUN
            WRITE (NOUT,99998) I, FINEST(I), ABSEST(I)
   80    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I7,A)
99998 FORMAT (1X,I4,2F14.4)
      END
*
      SUBROUTINE FUNSUB(NDIM,Z,NFUN,F)
*     .. Scalar Arguments ..
      INTEGER           NDIM, NFUN
*     .. Array Arguments ..
      DOUBLE PRECISION  F(NFUN), Z(NDIM)
*     .. Local Scalars ..
      DOUBLE PRECISION  SUM
      INTEGER           I, N
*     .. Intrinsic Functions ..
      INTRINSIC         LOG, DBLE, SIN
*     .. Executable Statements ..
      SUM = 0.0D0
      DO 20 N = 1, NDIM
         SUM = SUM + DBLE(N)*Z(N)
   20 CONTINUE
      DO 40 I = 1, NFUN
         F(I) = LOG(SUM)*SIN(DBLE(I)+SUM)
   40 CONTINUE
      RETURN
      END
