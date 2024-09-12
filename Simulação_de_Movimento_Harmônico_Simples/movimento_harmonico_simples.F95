PROGRAM movimento_harmonico_simples
  IMPLICIT NONE
  REAL :: amplitude, periodo, omega, tempo, dt
  INTEGER :: i, num_passos
  REAL :: deslocamento, velocidade

  ! Inicializa os parâmetros
  PRINT *, 'Simulação de Movimento Harmônico Simples'
  PRINT *, 'Digite a amplitude (em metros):'
  READ *, amplitude
  PRINT *, 'Digite o período (em segundos):'
  READ *, periodo
  PRINT *, 'Digite o número de passos para a simulação:'
  READ *, num_passos

  ! Calcula a frequência angular
  omega = 2.0 * 3.141592653589793 / periodo
  dt = periodo / num_passos

  ! Cabeçalho da tabela de resultados
  PRINT *, 'Tempo (s)', 'Deslocamento (m)', 'Velocidade (m/s)'

  ! Loop de simulação
  DO i = 0, num_passos
     tempo = i * dt
     deslocamento = amplitude * SIN(omega * tempo)
     velocidade = amplitude * omega * COS(omega * tempo)

     ! Exibe os resultados
     PRINT *, tempo, deslocamento, velocidade
  END DO

  PRINT *, 'Simulação concluída.'

END PROGRAM movimento_harmonico_simples
