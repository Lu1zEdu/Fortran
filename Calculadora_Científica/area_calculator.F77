      PROGRAM AREA_CALCULATOR
      IMPLICIT NONE
      INTEGER CHOICE
      REAL SIDE, LENGTH, WIDTH, BASE, HEIGHT, RADIUS, AREA

      PRINT *, 'Calculadora de Áreas'
      PRINT *, 'Escolha a forma geométrica:'
      PRINT *, '1. Quadrado'
      PRINT *, '2. Retângulo'
      PRINT *, '3. Triângulo'
      PRINT *, '4. Círculo'
      PRINT *, 'Digite o número da forma escolhida:'
      READ *, CHOICE

      IF (CHOICE .EQ. 1) THEN
         PRINT *, 'Você escolheu: Quadrado'
         PRINT *, 'Digite o comprimento do lado:'
         READ *, SIDE
         AREA = SIDE * SIDE
         PRINT *, 'A área do quadrado é: ', AREA
      ELSE IF (CHOICE .EQ. 2) THEN
         PRINT *, 'Você escolheu: Retângulo'
         PRINT *, 'Digite o comprimento:'
         READ *, LENGTH
         PRINT *, 'Digite a largura:'
         READ *, WIDTH
         AREA = LENGTH * WIDTH
         PRINT *, 'A área do retângulo é: ', AREA
      ELSE IF (CHOICE .EQ. 3) THEN
         PRINT *, 'Você escolheu: Triângulo'
         PRINT *, 'Digite a base:'
         READ *, BASE
         PRINT *, 'Digite a altura:'
         READ *, HEIGHT
         AREA = 0.5 * BASE * HEIGHT
         PRINT *, 'A área do triângulo é: ', AREA
      ELSE IF (CHOICE .EQ. 4) THEN
         PRINT *, 'Você escolheu: Círculo'
         PRINT *, 'Digite o raio:'
         READ *, RADIUS
         AREA = 3.1415927 * RADIUS * RADIUS
         PRINT *, 'A área do círculo é: ', AREA
      ELSE
         PRINT *, 'Opção inválida!'
      END IF

      END PROGRAM AREA_CALCULATOR
