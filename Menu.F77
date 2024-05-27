      PROGRAM RESTAURANT_MENU
      IMPLICIT NONE

      CHARACTER*20 NOME_RESTAURANTE(100)
      CHARACTER*20 CATEGORIA_RESTAURANTE(100)
      LOGICAL ATIVO(100)
      INTEGER N_RESTAURANTES

      DATA N_RESTAURANTES /3/
      DATA NOME_RESTAURANTE /'Praça', 'Pizza Suprema', 'Cantina'/
      DATA CATEGORIA_RESTAURANTE /'Japonesa', 'Pizza', 'Italiano'/
      DATA ATIVO /.FALSE., .TRUE., .FALSE./

      CALL MAIN
      END

      SUBROUTINE MAIN
      CALL LIMPAR_TELA
      CALL EXIBIR_NOME_DO_PROGRAMA
      CALL EXIBIR_OPCOES
      CALL ESCOLHER_OPCAO
      END

      SUBROUTINE EXIBIR_NOME_DO_PROGRAMA
      PRINT *, 'RESTAURANT MANAGEMENT SYSTEM'
      PRINT *, '****************************'
      END

      SUBROUTINE EXIBIR_OPCOES
      PRINT *, '1. Cadastrar restaurante'
      PRINT *, '2. Listar restaurantes'
      PRINT *, '3. Alternar estado do restaurante'
      PRINT *, '4. Sair'
      PRINT *, ''
      END

      SUBROUTINE FINALIZAR_APP
      CALL EXIBIR_SUBTITULO('Finalizar app')
      STOP
      END

      SUBROUTINE VOLTAR_AO_MENU_PRINCIPAL
      CHARACTER*1 DUMMY
      PRINT *, 'Digite uma tecla para voltar ao menu'
      READ *, DUMMY
      CALL MAIN
      END

      SUBROUTINE OPCAO_INVALIDA
      PRINT *, 'Opção inválida!'
      CALL VOLTAR_AO_MENU_PRINCIPAL
      END

      SUBROUTINE EXIBIR_SUBTITULO(TEXTO)
      CHARACTER*(*) TEXTO
      INTEGER I, N
      N = LEN_TRIM(TEXTO)
      DO I = 1, N
         WRITE(*,'(A)',ADVANCE='NO') '*'
      END DO
      PRINT *, ''
      PRINT *, TEXTO
      DO I = 1, N
         WRITE(*,'(A)',ADVANCE='NO') '*'
      END DO
      PRINT *, ''
      PRINT *, ''
      END

      SUBROUTINE CADASTRAR_NOVO_RESTAURANTE
      CHARACTER*20 NOME, CATEGORIA
      CALL EXIBIR_SUBTITULO('Cadastro de novos restaurantes')
      PRINT *, 'Digite o nome do restaurante que deseja cadastrar:'
      READ *, NOME
      PRINT *, 'Digite o nome da categoria do restaurante', NOME, ':'
      READ *, CATEGORIA
      N_RESTAURANTES = N_RESTAURANTES + 1
      NOME_RESTAURANTE(N_RESTAURANTES) = NOME
      CATEGORIA_RESTAURANTE(N_RESTAURANTES) = CATEGORIA
      ATIVO(N_RESTAURANTES) = .FALSE.
      PRINT *, 'O restaurante', NOME, 'foi cadastrado com sucesso!'
      CALL VOLTAR_AO_MENU_PRINCIPAL
      END

      SUBROUTINE LISTAR_RESTAURANTES
      INTEGER I
      CALL EXIBIR_SUBTITULO('Listando restaurantes')
      PRINT *, 'Nome do restaurante       | Categoria            | Status'
      DO I = 1, N_RESTAURANTES
         PRINT *, NOME_RESTAURANTE(I), ' | ', CATEGORIA_RESTAURANTE(I), ' | ', &
                  TRIM(ADJUSTL('desativado') // (ATIVO(I) .AND. 'ativado'))
      END DO
      CALL VOLTAR_AO_MENU_PRINCIPAL
      END

      SUBROUTINE ALTERNAR_ESTADO_RESTAURANTE
      CHARACTER*20 NOME
      INTEGER I
      LOGICAL ENCONTRADO
      CALL EXIBIR_SUBTITULO('Alterando estado do restaurante')
      PRINT *, 'Digite o nome do restaurante que deseja alterar o estado:'
      READ *, NOME
      ENCONTRADO = .FALSE.
      DO I = 1, N_RESTAURANTES
         IF (TRIM(NOME_RESTAURANTE(I)) .EQ. TRIM(NOME)) THEN
            ENCONTRADO = .TRUE.
            ATIVO(I) = .NOT. ATIVO(I)
            IF (ATIVO(I)) THEN
               PRINT *, 'O restaurante', NOME, 'foi ativado com sucesso!'
            ELSE
               PRINT *, 'O restaurante', NOME, 'foi desativado com sucesso!'
            END IF
         END IF
      END DO
      IF (.NOT. ENCONTRADO) THEN
         PRINT *, 'O restaurante não foi encontrado'
      END IF
      CALL VOLTAR_AO_MENU_PRINCIPAL
      END

      SUBROUTINE ESCOLHER_OPCAO
      INTEGER OPCAO_ESCOLHIDA
      PRINT *, 'Escolha uma opção:'
      READ *, OPCAO_ESCOLHIDA

      SELECT CASE (OPCAO_ESCOLHIDA)
      CASE (1)
         CALL CADASTRAR_NOVO_RESTAURANTE
      CASE (2)
         CALL LISTAR_RESTAURANTES
      CASE (3)
         CALL ALTERNAR_ESTADO_RESTAURANTE
      CASE (4)
         CALL FINALIZAR_APP
      CASE DEFAULT
         CALL OPCAO_INVALIDA
      END SELECT
      END

      SUBROUTINE LIMPAR_TELA
      PRINT *, CHAR(12)
      END
