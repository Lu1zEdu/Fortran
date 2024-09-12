## Explicação do Código

### Escala e Mapeamento
Cada pixel da imagem é mapeado para um número complexo 
\[ c = \text{real}_c + \text{imag}_c \times i \]
de acordo com a escala definida no plano (que vai de `xmin` a `xmax` para o eixo real, e de `ymin` a `ymax` para o eixo imaginário).

### Iteração da Fórmula
A equação 
\[ z_{n+1} = z_n^2 + c \]
é iterada para cada ponto até que o valor de 
\[ |z| \]
exceda 2, ou o número máximo de iterações seja atingido. O número de iterações é armazenado em uma matriz `image`.

### Saída em Arquivo
Os valores da matriz são escritos em um arquivo de texto, `mandelbrot.dat`, que pode ser visualizado usando ferramentas como Gnuplot ou outras ferramentas de plotagem de dados.

## Visualização com Gnuplot
Para visualizar a imagem gerada, você pode usar o Gnuplot. A seguir está um exemplo de como carregar e exibir o arquivo `mandelbrot.dat` no Gnuplot.

1. Abra o terminal e inicie o Gnuplot:
    ```bash
    gnuplot
    ```

2. Dentro do Gnuplot, use o comando abaixo para visualizar os dados:
    ```bash
    set view map
    set pm3d
    splot 'mandelbrot.dat' matrix with image
    ```

## Considerações Finais

- **Performance**: O Fortran é muito rápido para cálculos numéricos, então este programa deve ser eficiente mesmo com um grande número de iterações.
- **Ajustes**: Você pode ajustar os parâmetros `width`, `height`, `max_iter`, `xmin`, `xmax`, `ymin`, e `ymax` para explorar diferentes áreas do fractal ou aumentar a resolução da imagem.
- **Fractal de Julia**: Para implementar o conjunto de Julia, você só precisa alterar a lógica para que o valor de 
\[ c \]
seja fixo e o valor inicial de 
\[ z \]
varie conforme o mapeamento dos pixels.

Isso permitirá que você gere fractais complexos e visualize as imagens com o Gnuplot ou outras ferramentas de gráficos.
