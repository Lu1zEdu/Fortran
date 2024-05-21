    PROGRAM soma
    IMPLICIT NONE
      
    REAL :: num1, num2, resultado

    ! Leitura dos números
    PRINT *, 'Digite o primeiro número:'
    READ *, num1
    PRINT *, 'Digite o segundo número:'
    READ *, num2

    ! Calcula a soma
    resultado = num1 + num2

    ! Imprime o resultado
    PRINT *, 'A soma é: ', resultado

    END PROGRAM soma
