      SUBROUTINE F06SSF( UPLO, N, ALPHA, X, INCX, Y, INCY, AP )
C     MARK 13 RE-ISSUE. NAG COPYRIGHT 1988.
C     .. Entry Points ..
      ENTRY      ZHPR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, AP )
C     .. Scalar Arguments ..
      COMPLEX*16         ALPHA
      INTEGER            INCX, INCY, N
      CHARACTER*1        UPLO
C     .. Array Arguments ..
      COMPLEX*16         AP( * ), X( * ), Y( * )
C     ..
C
C  Purpose
C  =======
C
C  ZHPR2  performs the hermitian rank 2 operation
C
C     A := alpha*x*conjg( y' ) + conjg( alpha )*y*conjg( x' ) + A,
C
C  where alpha is a scalar, x and y are n element vectors and A is an
C  n by n hermitian matrix.
C
C  Parameters
C  ==========
C
C  UPLO   - CHARACTER*1.
C           On entry, UPLO specifies whether the upper or lower
C           triangular part of the matrix A is supplied in the packed
C           array AP as follows:
C
C              UPLO = 'U' or 'u'   The upper triangular part of A is
C                                  supplied in AP.
C
C              UPLO = 'L' or 'l'   The lower triangular part of A is
C                                  supplied in AP.
C
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the order of the matrix A.
C           N must be at least zero.
C           Unchanged on exit.
C
C  ALPHA  - COMPLEX*16      .
C           On entry, ALPHA specifies the scalar alpha.
C           Unchanged on exit.
C
C  X      - COMPLEX*16       array of dimension at least
C           ( 1 + ( n - 1 )*abs( INCX ) ).
C           Before entry, the incremented array X must contain the n
C           element vector x.
C           Unchanged on exit.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must not be zero.
C           Unchanged on exit.
C
C  Y      - COMPLEX*16       array of dimension at least
C           ( 1 + ( n - 1 )*abs( INCY ) ).
C           Before entry, the incremented array Y must contain the n
C           element vector y.
C           Unchanged on exit.
C
C  INCY   - INTEGER.
C           On entry, INCY specifies the increment for the elements of
C           Y. INCY must not be zero.
C           Unchanged on exit.
C
C  AP     - COMPLEX*16       array of DIMENSION at least
C           ( ( n*( n + 1 ) )/2 ).
C           Before entry with  UPLO = 'U' or 'u', the array AP must
C           contain the upper triangular part of the hermitian matrix
C           packed sequentially, column by column, so that AP( 1 )
C           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
C           and a( 2, 2 ) respectively, and so on. On exit, the array
C           AP is overwritten by the upper triangular part of the
C           updated matrix.
C           Before entry with UPLO = 'L' or 'l', the array AP must
C           contain the lower triangular part of the hermitian matrix
C           packed sequentially, column by column, so that AP( 1 )
C           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
C           and a( 3, 1 ) respectively, and so on. On exit, the array
C           AP is overwritten by the lower triangular part of the
C           updated matrix.
C           Note that the imaginary parts of the diagonal elements need
C           not be set, they are assumed to be zero, and on exit they
C           are set to zero.
C
C
C  Level 2 Blas routine.
C
C  -- Written on 22-October-1986.
C     Jack Dongarra, Argonne National Lab.
C     Jeremy Du Croz, Nag Central Office.
C     Sven Hammarling, Nag Central Office.
C     Richard Hanson, Sandia National Labs.
C
C
C     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) )
C     .. Local Scalars ..
      COMPLEX*16         TEMP1, TEMP2
      INTEGER            I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY
C     .. External Subroutines ..
      EXTERNAL           F06AAZ
C     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, DBLE
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IF     ( .NOT.(UPLO.EQ.'U' .OR. UPLO.EQ.'u').AND.
     $         .NOT.(UPLO.EQ.'L' .OR. UPLO.EQ.'l')      )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      END IF
      IF( INFO.NE.0 )THEN
         CALL F06AAZ( 'F06SSF/ZHPR2 ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
C
C     Set up the start points in X and Y if the increments are not both
C     unity.
C
      IF( ( INCX.NE.1 ).OR.( INCY.NE.1 ) )THEN
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( N - 1 )*INCX
         END IF
         IF( INCY.GT.0 )THEN
            KY = 1
         ELSE
            KY = 1 - ( N - 1 )*INCY
         END IF
         JX = KX
         JY = KY
      END IF
C
C     Start the operations. In this version the elements of the array AP
C     are accessed sequentially with one pass through AP.
C
      KK = 1
      IF( (UPLO.EQ.'U' .OR. UPLO.EQ.'u') )THEN
C
C        Form  A  when upper triangle is stored in AP.
C
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 20, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*DCONJG( Y( J ) )
                  TEMP2 = DCONJG( ALPHA*X( J ) )
                  K     = KK
                  DO 10, I = 1, J - 1
                     AP( K ) = AP( K ) + X( I )*TEMP1 + Y( I )*TEMP2
                     K       = K       + 1
   10             CONTINUE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) ) +
     $                               DBLE( X( J )*TEMP1 + Y( J )*TEMP2 )
               ELSE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) )
               END IF
               KK = KK + J
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1 = ALPHA*DCONJG( Y( JY ) )
                  TEMP2 = DCONJG( ALPHA*X( JX ) )
                  IX    = KX
                  IY    = KY
                  DO 30, K = KK, KK + J - 2
                     AP( K ) = AP( K ) + X( IX )*TEMP1 + Y( IY )*TEMP2
                     IX      = IX      + INCX
                     IY      = IY      + INCY
   30             CONTINUE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) ) +
     $                               DBLE( X( JX )*TEMP1 +
     $                                     Y( JY )*TEMP2 )
               ELSE
                  AP( KK + J - 1 ) = DBLE( AP( KK + J - 1 ) )
               END IF
               JX = JX + INCX
               JY = JY + INCY
               KK = KK + J
   40       CONTINUE
         END IF
      ELSE
C
C        Form  A  when lower triangle is stored in AP.
C
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, J = 1, N
               IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
                  TEMP1   = ALPHA*DCONJG( Y( J ) )
                  TEMP2   = DCONJG( ALPHA*X( J ) )
                  AP( KK ) = DBLE( AP( KK ) ) +
     $                       DBLE( X( J )*TEMP1 + Y( J )*TEMP2 )
                  K        = KK               + 1
                  DO 50, I = J + 1, N
                     AP( K ) = AP( K ) + X( I )*TEMP1 + Y( I )*TEMP2
                     K       = K       + 1
   50             CONTINUE
               ELSE
                  AP( KK ) = DBLE( AP( KK ) )
               END IF
               KK = KK + N - J + 1
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
                  TEMP1    = ALPHA*DCONJG( Y( JY ) )
                  TEMP2    = DCONJG( ALPHA*X( JX ) )
                  AP( KK ) = DBLE( AP( KK ) ) +
     $                       DBLE( X( JX )*TEMP1 + Y( JY )*TEMP2 )
                  IX       = JX
                  IY       = JY
                  DO 70, K = KK + 1, KK + N - J
                     IX      = IX      + INCX
                     IY      = IY      + INCY
                     AP( K ) = AP( K ) + X( IX )*TEMP1 + Y( IY )*TEMP2
   70             CONTINUE
               ELSE
                  AP( KK ) = DBLE( AP( KK ) )
               END IF
               JX = JX + INCX
               JY = JY + INCY
               KK = KK + N - J + 1
   80       CONTINUE
         END IF
      END IF
C
      RETURN
C
C     End of F06SSF (ZHPR2 ).
C
      END
