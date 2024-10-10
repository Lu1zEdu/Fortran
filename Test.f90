MODULE constantes_fisicas
  IMPLICIT NONE
  DOUBLE PRECISION, PARAMETER :: G = 6.67430d-11  ! Constante gravitacional (m^3 kg^-1 s^-2)
  DOUBLE PRECISION, PARAMETER :: dt = 0.01d0      ! Passo de tempo (s)
  INTEGER, PARAMETER :: N = 3                     ! Número de partículas
END MODULE constantes_fisicas

MODULE tipos_particula
  IMPLICIT NONE

  TYPE :: Particula
      DOUBLE PRECISION :: x, y, z    ! Posição
      DOUBLE PRECISION :: vx, vy, vz ! Velocidade
      DOUBLE PRECISION :: ax, ay, az ! Aceleração
      DOUBLE PRECISION :: massa      ! Massa da partícula
  END TYPE Particula

END MODULE tipos_particula

PROGRAM simulacao_particulas
  USE constantes_fisicas
  USE tipos_particula
  IMPLICIT NONE

  TYPE(Particula), DIMENSION(N) :: particulas
  INTEGER :: i

  PRINT *, 'Tempo (s)', 'Partícula', 'Posição (x, y, z)', 'Velocidade (vx, vy, vz)'
  
  CALL inicializar_particulas(particulas)  ! Inicializar partículas

  DO i = 1, 10000  ! Simular por 10.000 passos de tempo
      CALL calcular_aceleracao(particulas)
      CALL integrar_verlet(particulas)

      IF (MOD(i, 1000) == 0) THEN
          CALL exibir_resultados(particulas, i * dt)
      END IF
  END DO
END PROGRAM simulacao_particulas

SUBROUTINE inicializar_particulas(particulas)
  USE constantes_fisicas
  USE tipos_particula
  IMPLICIT NONE
  TYPE(Particula), DIMENSION(N), INTENT(OUT) :: particulas
  INTEGER :: i
  DOUBLE PRECISION :: random_x, random_y, random_z

  CALL RANDOM_SEED()  ! Inicializa o gerador de números aleatórios

  DO i = 1, N
      CALL RANDOM_NUMBER(random_x)  ! Gera número aleatório para x
      CALL RANDOM_NUMBER(random_y)  ! Gera número aleatório para y
      CALL RANDOM_NUMBER(random_z)  ! Gera número aleatório para z
      ! Inicializa todos os componentes da partícula
      particulas(i) = Particula(random_x * 10.0d0, random_y * 10.0d0, random_z * 10.0d0, &
                                 0.0d0, 0.0d0, 0.0d0, &
                                 0.0d0, 0.0d0, 0.0d0, &
                                 1.0d24)  ! Massa da partícula
  END DO
END SUBROUTINE inicializar_particulas

SUBROUTINE calcular_aceleracao(particulas)
  USE constantes_fisicas
  USE tipos_particula
  IMPLICIT NONE
  TYPE(Particula), DIMENSION(N), INTENT(INOUT) :: particulas
  INTEGER :: i, j
  DOUBLE PRECISION :: dx, dy, dz, r2, r3, forca

  DO i = 1, N
      particulas(i)%ax = 0.0d0
      particulas(i)%ay = 0.0d0
      particulas(i)%az = 0.0d0
  END DO

  DO i = 1, N
      DO j = i + 1, N
          dx = particulas(j)%x - particulas(i)%x
          dy = particulas(j)%y - particulas(i)%y
          dz = particulas(j)%z - particulas(i)%z
          r2 = dx**2 + dy**2 + dz**2
          r3 = r2 * SQRT(r2)

          IF (r2 > 0.0d0) THEN
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

SUBROUTINE integrar_verlet(particulas)
  USE constantes_fisicas
  USE tipos_particula
  IMPLICIT NONE
  TYPE(Particula), DIMENSION(N), INTENT(INOUT) :: particulas
  INTEGER :: i
  DOUBLE PRECISION :: ax_old, ay_old, az_old

  DO i = 1, N
      ax_old = particulas(i)%ax
      ay_old = particulas(i)%ay
      az_old = particulas(i)%az

      particulas(i)%x = particulas(i)%x + particulas(i)%vx * dt + 0.5d0 * ax_old * dt**2
      particulas(i)%y = particulas(i)%y + particulas(i)%vy * dt + 0.5d0 * ay_old * dt**2
      particulas(i)%z = particulas(i)%z + particulas(i)%vz * dt + 0.5d0 * az_old * dt**2

      particulas(i)%vx = particulas(i)%vx + 0.5d0 * (ax_old + particulas(i)%ax) * dt
      particulas(i)%vy = particulas(i)%vy + 0.5d0 * (ay_old + particulas(i)%ay) * dt
      particulas(i)%vz = particulas(i)%vz + 0.5d0 * (az_old + particulas(i)%az) * dt
  END DO
END SUBROUTINE integrar_verlet

SUBROUTINE exibir_resultados(particulas, tempo)
  USE constantes_fisicas  ! Added this line to make N visible
  USE tipos_particula
  IMPLICIT NONE
  TYPE(Particula), DIMENSION(N), INTENT(IN) :: particulas
  DOUBLE PRECISION, INTENT(IN) :: tempo
  INTEGER :: i

  DO i = 1, N
      PRINT '(F10.2, 3X, I2, 3X, 3(F10.4, 3X), 3(F10.4, 3X))', tempo, i, &
            particulas(i)%x, particulas(i)%y, particulas(i)%z, &
            particulas(i)%vx, particulas(i)%vy, particulas(i)%vz
  END DO
END SUBROUTINE exibir_resultados