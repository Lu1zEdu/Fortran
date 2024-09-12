## Explicação do Código

### Parâmetros de Entrada

- **Amplitude**: A máxima extensão da oscilação.
- **Período**: Tempo para um ciclo completo de oscilação.
- **Número de Passos**: Para discretizar o tempo na simulação.

### Cálculos

1. **Frequência Angular** \( \omega \): Calculada a partir do período.
   \[
   \omega = \frac{2 \pi}{\text{Período}}
   \]

2. **Deslocamento e Velocidade**: A cada passo, o deslocamento e a velocidade são calculados usando as funções seno e cosseno:
   - **Deslocamento** \( x(t) \):
     \[
     x(t) = \text{Amplitude} \times \sin(\omega t)
     \]
   - **Velocidade** \( v(t) \):
     \[
     v(t) = \frac{d}{dt} x(t) = \text{Amplitude} \times \omega \times \cos(\omega t)
     \]

### Saída

- **Tempo, Deslocamento e Velocidade**: Impressos a cada passo da simulação.
