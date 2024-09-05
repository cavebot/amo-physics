      SUBROUTINE F06PDF( UPLO, N, K, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C     BND VERSION FOR VECTOR MACHINES
C     .. Entry Points ..
      ENTRY      DSBMV ( UPLO, N, K, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
C     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, K, LDA, N
      CHARACTER*1        UPLO
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
C     ..
C
C  Purpose
C  =======
C
C  DSBMV  performs the matrix-vector  operation
C
C     y := alpha*A*x + beta*y,
C
C  where alpha and beta are scalars, x and y are n element vectors and
C  A is an n by n symmetric band matrix, with k super-diagonals.
C
C  Parameters
C  ==========
C
C  UPLO   - CHARACTER*1.
C           On entry, UPLO specifies whether the upper or lower
C           triangular part of the band matrix A is being supplied as
C           follows:
C
C              UPLO = 'U' or 'u'   The upper triangular part of A is
C                                  being supplied.
C
C              UPLO = 'L' or 'l'   The lower triangular part of A is
C                                  being supplied.
C
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the order of the matrix A.
C           N must be at least zero.
C           Unchanged on exit.
C
C  K      - INTEGER.
C           On entry, K specifies the number of super-diagonals of the
C           matrix A. K must satisfy  0 .le. K.
C           Unchanged on exit.
C
C  ALPHA  - DOUBLE PRECISION.
C           On entry, ALPHA specifies the scalar alpha.
C           Unchanged on exit.
C
C  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
C           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )
C           by n part of the array A must contain the upper triangular
C           band part of the symmetric matrix, supplied column by
C           column, with the leading diagonal of the matrix in row
C           ( k + 1 ) of the array, the first super-diagonal starting at
C           position 2 in row k, and so on. The top left k by k triangle
C           of the array A is not referenced.
C           The following program segment will transfer the upper
C           triangular part of a symmetric band matrix from conventional
C           full matrix storage to band storage:
C
C                 DO 20, J = 1, N
C                    M = K + 1 - J
C                    DO 10, I = MAX( 1, J - K ), J
C                       A( M + I, J ) = matrix( I, J )
C              10    CONTINUE
C              20 CONTINUE
C
C           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )
C           by n part of the array A must contain the lower triangular
C           band part of the symmetric matrix, supplied column by
C           column, with the leading diagonal of the matrix in row 1 of
C           the array, the first sub-diagonal starting at position 1 in
C           row 2, and so on. The bottom right k by k triangle of the
C           array A is not referenced.
C           The following program segment will transfer the lower
C           triangular part of a symmetric band matrix from conventional
C           full matrix storage to band storage:
C
C                 DO 20, J = 1, N
C                    M = 1 - J
C                    DO 10, I = J, MIN( N, J + K )
C                       A( M + I, J ) = matrix( I, J )
C              10    CONTINUE
C              20 CONTINUE
C
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. LDA must be at least
C           ( k + 1 ).
C           Unchanged on exit.
C
C  X      - DOUBLE PRECISION array of DIMENSION at least
C           ( 1 + ( n - 1 )*abs( INCX ) ).
C           Before entry, the incremented array X must contain the
C           vector x.
C           Unchanged on exit.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must not be zero.
C           Unchanged on exit.
C
C  BETA   - DOUBLE PRECISION.
C           On entry, BETA specifies the scalar beta.
C           Unchanged on exit.
C
C  Y      - DOUBLE PRECISION array of DIMENSION at least
C           ( 1 + ( n - 1 )*abs( INCY ) ).
C           Before entry, the incremented array Y must contain the
C           vector y. On exit, Y is overwritten by the updated vector y.
C
C  INCY   - INTEGER.
C           On entry, INCY specifies the increment for the elements of
C           Y. INCY must not be zero.
C           Unchanged on exit.
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
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     .. Local Scalars ..
      INTEGER            I, INFO, IY, J, JX, JY, KPLUS1, KX, KY, L, LX,
     $                   LY
C     .. External Subroutines ..
      EXTERNAL           F06AAZ
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
      ELSE IF( K.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.( K + 1 ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL F06AAZ( 'F06PDF/DSBMV ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( ( N.EQ.0 ).OR.( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
C
C     Set up the start points in  X  and  Y.
C
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
C
C     Start the operations. In this version the array A is accessed
C     by rows.
C
C     First form  y := beta*y.
C
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, N
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, N
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, N
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, N
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( (UPLO.EQ.'U' .OR. UPLO.EQ.'u') )THEN
C
C        Form  y  when upper triangle of A is stored.
C
         KPLUS1 = K + 1
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 60, I = 1, K
               L = KPLUS1 - I
               DO 50, J = L + 1, N
                  Y( J - L ) = Y( J - L ) + ALPHA*X( J )*A( I, J )
   50          CONTINUE
               DO 55, J = L + 1, N
                  Y( J ) = Y( J ) + ALPHA*X( J - L )*A( I, J )
   55          CONTINUE
   60       CONTINUE
            DO 65, J = 1, N
               Y( J ) = Y( J ) + ALPHA*X( J )*A( K + 1, J)
   65       CONTINUE
         ELSE
            LX = K*INCX
            LY = K*INCY
            DO 80, I = 1, K
               JX = KX     + LX
               JY = KY
               L  = KPLUS1 - I
               DO 70, J = L + 1, N
                  Y( JY ) = Y( JY ) + ALPHA*X( JX )*A( I, J )
                  JX      = JX      + INCX
                  JY      = JY      + INCY
   70          CONTINUE
               JX = KX
               JY = KY     + LY
               DO 75, J = L + 1, N
                  Y( JY ) = Y( JY ) + ALPHA*X( JX )*A( I, J )
                  JX      = JX      + INCX
                  JY      = JY      + INCY
   75          CONTINUE
               LX = LX - INCX
               LY = LY - INCY
   80       CONTINUE
            JX = KX
            JY = KY
            DO 85, J = 1, N
               Y( JY ) = Y( JY ) + ALPHA*X( JX )*A( K + 1, J )
               JX      = JX      + INCX
               JY      = JY      + INCY
   85       CONTINUE
         END IF
      ELSE
C
C        Form  y  when lower triangle of A is stored.
C
         IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
            DO 100, I = 2, K + 1
               L = I - 1
               DO 90, J = 1, N - L
                  Y( J + L ) = Y( J + L ) + ALPHA*X( J )*A( I, J )
   90          CONTINUE
               DO 95, J = 1, N - L
                  Y( J ) = Y( J ) + ALPHA*X(J + L )*A( I, J )
   95          CONTINUE
  100       CONTINUE
            DO 105, J = 1, N
               Y( J ) = Y( J ) + ALPHA*X( J )*A( 1, J )
  105       CONTINUE
         ELSE
            LX = INCX
            LY = INCY
            DO 120, I = 2, K + 1
               JX = KX
               JY = KY + LY
               L  = I  - 1
               DO 110, J = 1, N - L
                  Y( JY ) = Y( JY ) + ALPHA*X( JX )*A( I, J )
                  JX      = JX      + INCX
                  JY      = JY      + INCY
  110          CONTINUE
               JX = KX + LX
               JY = KY
               DO 115, J = 1, N - L
                  Y( JY ) = Y( JY ) + ALPHA*X( JX )*A( I, J )
                  JX      = JX      + INCX
                  JY      = JY      + INCY
  115          CONTINUE
               LX = LX + INCX
               LY = LY + INCY
  120       CONTINUE
            JX = KX
            JY = KY
            DO 125, J = 1, N
               Y( JY ) = Y( JY ) + ALPHA*X( JX )*A( 1, J )
               JX      = JX      + INCX
               JY      = JY      + INCY
  125       CONTINUE
         END IF
      END IF
C
      RETURN
C
C     End of F06PDF (DSBMV ).
C
      END
