PROGRAM forca
  IMPLICIT NONE
  INTEGER :: num_erros, i, posicao, tam_palavra, aleatorio
  CHARACTER(LEN=15) :: palavras(5), palavra_secreta, letra, palavra_adivinhada
  LOGICAL :: acertou
  CHARACTER(LEN=6), DIMENSION(6) :: boneco
  CALL RANDOM_SEED() ! Inicializa a semente para geração aleatória

  ! Lista de palavras
  palavras(1) = 'computador'
  palavras(2) = 'fortran'
  palavras(3) = 'programacao'
  palavras(4) = 'universidade'
  palavras(5) = 'linguagem'
  palavras(6) = 'algoritmo'
  palavras(7) = 'tecnologia'
  palavras(8) = 'desenvolvimento'
  palavras(9) = 'software'
  palavras(10) = 'hardware'
  palavras(11) = 'internet'
  palavras(12) = 'dados'
  palavras(13) = 'sistemas'
  palavras(14) = 'memoria'
  palavras(15) = 'processador'
  palavras(16) = 'armazenamento'
  palavras(17) = 'rede'
  palavras(18) = 'navegador'
  palavras(19) = 'compilador'
  palavras(20) = 'inteligencia'
  palavras(21) = 'artificial'
  palavras(22) = 'aplicativo'
  palavras(23) = 'robotica'
  palavras(24) = 'criptografia'
  palavras(25) = 'matematica'
  palavras(26) = 'engenharia'
  palavras(27) = 'automacao'
  palavras(28) = 'virtualizacao'
  palavras(29) = 'seguranca'
  palavras(30) = 'cloud'
  palavras(31) = 'servidor'
  palavras(32) = 'cliente'
  palavras(33) = 'backup'
  palavras(34) = 'criptografia'
  palavras(35) = 'bigdata'
  palavras(36) = 'machinelearning'
  palavras(37) = 'rede_neural'
  palavras(38) = 'deep_learning'
  palavras(39) = 'javascript'
  palavras(40) = 'python'
  palavras(41) = 'framework'
  palavras(42) = 'biblioteca'
  palavras(43) = 'codigo'
  palavras(44) = 'debugging'
  palavras(45) = 'virtual_machine'

  ! Seleciona uma palavra aleatória da lista
  CALL RANDOM_NUMBER(aleatorio)
  aleatorio = INT(aleatorio * 5) + 1
  palavra_secreta = TRIM(ADJUSTL(palavras(aleatorio)))
  tam_palavra = LEN_TRIM(palavra_secreta)
  
  ! Inicializa o estado do jogo
  palavra_adivinhada = REPEAT('*', tam_palavra)
  num_erros = 0
  boneco = (/'  O  ', '  |  ', ' /|\ ', '  |  ', ' / \ ', '_____'/)

  PRINT *, 'Bem-vindo ao jogo da Forca!'
  PRINT *, 'Tente adivinhar a palavra secreta.'

  ! Laço principal do jogo
  DO WHILE (num_erros < 6 .AND. palavra_adivinhada /= palavra_secreta)
     PRINT *, 'Palavra atual: ', palavra_adivinhada
     PRINT *, 'Digite uma letra: '
     READ *, letra

     acertou = .FALSE.
     ! Verifica se a letra está na palavra secreta
     DO i = 1, tam_palavra
        IF (palavra_secreta(i:i) == letra) THEN
           palavra_adivinhada(i:i) = letra
           acertou = .TRUE.
        END IF
     END DO

     ! Se a letra não foi encontrada, aumenta o número de erros
     IF (.NOT. acertou) THEN
        num_erros = num_erros + 1
        PRINT *, 'Letra incorreta! Você errou ', num_erros, ' vez(es).'
        ! Exibe o boneco com partes faltando
        CALL desenha_boneco(boneco, num_erros)
     ELSE
        PRINT *, 'Letra correta!'
     END IF
  END DO

  ! Fim do jogo
  IF (palavra_adivinhada == palavra_secreta) THEN
     PRINT *, 'Parabéns! Você adivinhou a palavra: ', palavra_secreta
  ELSE
     PRINT *, 'Você perdeu! A palavra era: ', palavra_secreta
  END IF

CONTAINS

  SUBROUTINE desenha_boneco(boneco, num_erros)
     CHARACTER(LEN=6), DIMENSION(6) :: boneco
     INTEGER :: num_erros, i
     ! Desenha o boneco com base no número de erros
     PRINT *, 'Boneco:'
     DO i = 1, num_erros
        PRINT *, boneco(i)
     END DO
  END SUBROUTINE desenha_boneco

END PROGRAM forca
