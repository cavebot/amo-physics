*     C05ZAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, N, LDFJAC
      PARAMETER        (M=15,N=3,LDFJAC=M)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, MODE
*     .. Local Arrays ..
      DOUBLE PRECISION ERR(M), FJAC(LDFJAC,N), FVEC(M), FVECP(M), X(N),
     +                 XP(N)
*     .. External Subroutines ..
      EXTERNAL         C05ZAF, FCN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'C05ZAF Example Program Results'
      X(1) = 9.2D-1
      X(2) = 1.3D-1
      X(3) = 5.4D-1
      MODE = 1
*
      CALL C05ZAF(M,N,X,FVEC,FJAC,LDFJAC,XP,FVECP,MODE,ERR)
*
      CALL FCN(M,N,X,FVEC,FJAC,LDFJAC,1)
      CALL FCN(M,N,X,FVEC,FJAC,LDFJAC,2)
      CALL FCN(M,N,XP,FVECP,FJAC,LDFJAC,1)
*
      MODE = 2
*
      CALL C05ZAF(M,N,X,FVEC,FJAC,LDFJAC,XP,FVECP,MODE,ERR)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '     FVEC at X = ', (X(I),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (FVEC(I),I=1,M)
      WRITE (NOUT,*)
      WRITE (NOUT,99999) '     FVECP at XP = ', (XP(I),I=1,N)
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (FVECP(I),I=1,M)
      WRITE (NOUT,*)
      WRITE (NOUT,*) '     ERR'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (ERR(I),I=1,M)
      STOP
*
99999 FORMAT (1X,A,3F12.7)
99998 FORMAT (5X,3F12.4)
      END
*
      SUBROUTINE FCN(M,N,X,FVEC,FJAC,LDFJAC,IFLAG)
*     .. Parameters ..
      INTEGER        M1
      PARAMETER      (M1=15)
*     .. Scalar Arguments ..
      INTEGER        IFLAG, LDFJAC, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION FJAC(LDFJAC,N), FVEC(M), X(N)
*     .. Local Scalars ..
      DOUBLE PRECISION TMP1, TMP2, TMP3, TMP4
      INTEGER        I
*     .. Local Arrays ..
      DOUBLE PRECISION Y(M1)
*     .. Data statements ..
      DATA           Y/1.4D-1, 1.8D-1, 2.2D-1, 2.5D-1, 2.9D-1, 3.2D-1,
     +               3.5D-1, 3.9D-1, 3.7D-1, 5.8D-1, 7.3D-1, 9.6D-1,
     +               1.34D0, 2.1D0, 4.39D0/
*     .. Executable Statements ..
      IF (IFLAG.NE.2) THEN
         DO 20 I = 1, M
            TMP1 = I
            TMP2 = M + 1 - I
            TMP3 = TMP1
            IF (I.GT.(M+1)/2) TMP3 = TMP2
            FVEC(I) = Y(I) - (X(1)+TMP1/(X(2)*TMP2+X(3)*TMP3))
   20    CONTINUE
      ELSE
         DO 40 I = 1, M
            TMP1 = I
            TMP2 = M + 1 - I
*
*           Error introduced into next statement for illustration.
*           Corrected statement should read    TMP3 = TMP1 .
*
            TMP3 = TMP2
            IF (I.GT.(M+1)/2) TMP3 = TMP2
            TMP4 = (X(2)*TMP2+X(3)*TMP3)**2
            FJAC(I,1) = -1.0D0
            FJAC(I,2) = TMP1*TMP2/TMP4
            FJAC(I,3) = TMP1*TMP3/TMP4
   40    CONTINUE
      END IF
      RETURN
      END
