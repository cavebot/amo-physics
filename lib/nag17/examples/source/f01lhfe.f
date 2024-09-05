*     F01LHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NBLMAX, NMAX, IRMAX, LENA, LDB
      PARAMETER        (NBLMAX=10,NMAX=20,IRMAX=5,LENA=200,LDB=NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, IFAIL, INDEX, IR, J, K, N, NBASEK, NBLOKS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LENA), B(LDB,IRMAX)
      INTEGER          BLKSTR(3,NBLMAX), PIVOT(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F01LHF, F04LHF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01LHF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NBLOKS
      WRITE (NOUT,*)
      IF (NBLOKS.LE.NBLMAX) THEN
         NBASEK = 0
         N = 0
         DO 40 I = 1, NBLOKS
            READ (NIN,*) (BLKSTR(J,I),J=1,3)
            DO 20 K = 1, BLKSTR(1,I)
               IF (NBASEK+BLKSTR(2,I)*BLKSTR(1,I).GT.LENA) THEN
                  WRITE (NOUT,*)
     +              ' Array A is too small for this problem'
                  STOP
               ELSE
                  READ (NIN,*) (A(NBASEK+(J-1)*BLKSTR(1,I)+K),J=1,
     +              BLKSTR(2,I))
               END IF
   20       CONTINUE
            NBASEK = NBASEK + BLKSTR(2,I)*BLKSTR(1,I)
            N = N + BLKSTR(1,I)
   40    CONTINUE
         IF (N.GT.NMAX) THEN
            WRITE (NOUT,*) ' N is too large'
            STOP
         END IF
         TOL = 0.0D0
         IFAIL = -1
*
         CALL F01LHF(N,NBLOKS,BLKSTR,A,LENA,PIVOT,TOL,INDEX,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
            READ (NIN,*) IR
            IF (IR.LE.IRMAX) THEN
               READ (NIN,*) ((B(I,J),I=1,N),J=1,IR)
               IFAIL = -1
*
               CALL F04LHF('N',N,NBLOKS,BLKSTR,A,LENA,PIVOT,B,LDB,IR,
     +                     IFAIL)
*
               IF (IFAIL.EQ.0) THEN
                  WRITE (NOUT,*) 'Component Solution'
                  WRITE (NOUT,*)
                  DO 60 I = 1, N
                     WRITE (NOUT,99999) I, (B(I,J),J=1,IR)
   60             CONTINUE
               END IF
            ELSE
               WRITE (NOUT,*) ' Too many right hand sides specified'
            END IF
         END IF
      ELSE
         WRITE (NOUT,*) ' NBLOKS is invalid'
      END IF
      STOP
*
99999 FORMAT (1X,I5,6X,5F6.4)
      END
