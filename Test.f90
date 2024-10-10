MODULE constantes_fisicas
    IMPLICIT NONE
    REAL, PARAMETER :: G = 6.67430e-11  ! Constante gravitacional (m^3 kg^-1 s^-2)
    REAL, PARAMETER :: dt = 0.01        ! Passo de tempo (s)
    INTEGER, PARAMETER :: N = 3         ! Número de partículas
  END MODULE constantes_fisicas
   
  MODULE tipos_particula
    IMPLICIT NONE
   
    TYPE :: Particula
      REAL :: x, y, z         ! Posição
      REAL :: vx, vy, vz      ! Velocidade
      REAL :: ax, ay, az      ! Aceleração
      REAL :: massa           ! Massa da partícula
    END TYPE Particula
   
  END MODULE tipos_particula
   
  PROGRAM simulacao_particulas
    USE constantes_fisicas
    USE tipos_particula
    IMPLICIT NONE
   
    TYPE(Particula), DIMENSION(N) :: particulas
    INTEGER :: i, j
    REAL :: distancia, forca_gravitacional, dx, dy, dz, r3
   
    ! Inicializando partículas (posição, velocidade e massa)
    CALL inicializar_particulas(particulas)
   
    PRINT *, 'Tempo (s)', 'Partícula', 'Posição (x, y, z)', 'Velocidade (vx, vy, vz)'
    DO i = 1, 10000  ! Simular por 10.000 passos de tempo
      ! Calcular novas acelerações a partir das forças gravitacionais
      CALL calcular_aceleracao(particulas)
   
      ! Atualizar posições e velocidades usando integração Verlet
      CALL integrar_verlet(particulas)
   
      ! Exibir resultados a cada 1000 passos
      IF (MOD(i, 1000) == 0) THEN
        CALL exibir_resultados(particulas, i*dt)
      END IF
    END DO
  END PROGRAM simulacao_particulas
   
  ! Subrotina para inicializar as partículas
  SUBROUTINE inicializar_particulas(particulas)
    USE constantes_fisicas
    USE tipos_particula
    IMPLICIT NONE
    TYPE(Particula), DIMENSION(N), INTENT(OUT) :: particulas
   
    ! Exemplo de inicialização para 3 partículas
    particulas(1) = Particula(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0e24)
    particulas(2) = Particula(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0e22)
    particulas(3) = Particula(0.0, 1.0, 0.0, -1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0e23)
  END SUBROUTINE inicializar_particulas
   
  ! Subrotina para calcular a aceleração das partículas
  SUBROUTINE calcular_aceleracao(particulas)
    USE constantes_fisicas
    USE tipos_particula
    IMPLICIT NONE
    TYPE(Particula), DIMENSION(N), INTENT(INOUT) :: particulas
    INTEGER :: i, j
    REAL :: dx, dy, dz, r2, r3, forca
   
    ! Zerar acelerações antes do cálculo
    DO i = 1, N
      particulas(i)%ax = 0.0
      particulas(i)%ay = 0.0
      particulas(i)%az = 0.0
    END DO
   
    ! Calcular aceleração gravitacional entre as partículas
    DO i = 1, N
      DO j = i + 1, N
        dx = particulas(j)%x - particulas(i)%x
        dy = particulas(j)%y - particulas(i)%y
        dz = particulas(j)%z - particulas(i)%z
        r2 = dx**2 + dy**2 + dz**2
        r3 = SQRT(r2) * r2
   
        IF (r2 > 0.0) THEN
          forca = G * particulas(i)%massa * particulas(j)%massa / r3
          particulas(i)%ax = particulas(i)%ax + forca * dx / particulas(i)%massa
          particulas(i)%ay = particulas(i)%ay + forca * dy / particulas(i)%massa
          particulas(i)%az = particulas(i)%az + forca * dz / particulas(i)%massa
          particulas(j)%ax = particulas(j)%ax - forca * dx / particulas(j)%massa
          particulas(j)%ay = particulas(j)%ay - forca * dy / particulas(j)%massa
          particulas(j)%az = particulas(j)%az - forca * dz / particulas(j)%massa
        END IF
      END DO
    END DO
  END SUBROUTINE calcular_aceleracao
   
  ! Subrotina para integrar usando o método de Verlet
  SUBROUTINE integrar_verlet(particulas)
    USE constantes_fisicas
    USE tipos_particula
    IMPLICIT NONE
    TYPE(Particula), DIMENSION(N), INTENT(INOUT) :: particulas
    INTEGER :: i
   
    DO i = 1, N
      ! Atualizar posição
      particulas(i)%x = particulas(i)%x + particulas(i)%vx * dt + 0.5 * particulas(i)%ax * dt**2
      particulas(i)%y = particulas(i)%y + particulas(i)%vy * dt + 0.5 * particulas(i)%ay * dt**2
      particulas(i)%z = particulas(i)%z + particulas(i)%vz * dt + 0.5 * particulas(i)%az * dt**2
   
      ! Atualizar velocidade
      particulas(i)%vx = particulas(i)%vx + 0.5 * particulas(i)%ax * dt
      particulas(i)%vy = particulas(i)%vy + 0.5 * particulas(i)%ay * dt
      particulas(i)%vz = particulas(i)%vz + 0.5 * particulas(i)%az * dt
    END DO
  END SUBROUTINE integrar_verlet
   
  ! Subrotina para exibir os resultados
  SUBROUTINE exibir_resultados(particulas, tempo)
    USE tipos_particula
    IMPLICIT NONE
    TYPE(Particula), DIMENSION(N), INTENT(IN) :: particulas
    REAL, INTENT(IN) :: tempo
    INTEGER :: i
   
    DO i = 1, N
      PRINT *, tempo, i, particulas(i)%x, particulas(i)%y, particulas(i)%z, particulas(i)%vx, particulas(i)%vy, particulas(i)%vz
    END DO
  END SUBROUTINE exibir_resultados