# Robô de Limpeza em Prolog

Este projeto implementa um robô de limpeza em um mapa 8x8 utilizando a linguagem Prolog. O robô é capaz de se mover pelo mapa, evitar obstáculos e limpar pontos sujos até atingir seu destino.

### Objetivo

Implementar um programa em Prolog que simule um robô para limpeza de uma sala. A sala deve ser representada como uma matriz (tabuleiro) previamente mapeada.

### Requisitos

1. **Mapeamento da Sala:**
   - A sala é representada como um tabuleiro (matriz 8x8).
   - Cada posição da sala pode possuir sujeira ou obstáculo.

2. **Movimentação do Robô:**
   - O robô pode se mover para a esquerda, direita, frente, trás ou diagonalmente.
   - As relações necessárias para representar a mudança de estado do robô foram construídas.

3. **Heurísticas de Custo e Avaliação:**
   - O robô deve percorrer todos os estados com sujeira para limpar toda a sala.
   - Deve percorrer o menor percurso possível.

4. **Estados Iniciais e Finais:**
   - Posição inicial e posição final no mapa são estabelecidas.
   - Ao visitar um estado com sujeira, ele é marcado como limpo usando `assert/1` e `retract/1`.

5. **Avaliação de Performance:**
   - Avaliar a performance dos algoritmos Best First, Branch and Bound, Hill Climbing e A*.
   - O custo é o número de posições percorridas.
   - A avaliação é calculada como a distância de Manhattan.

## Estrutura do Projeto

### Declaração do Mapa

Os pontos no mapa são definidos como:

```prolog
ponto(0, 0).
...
ponto(7, 7).
```

### Pontos Sujos e Bloqueados

Os pontos sujos e bloqueados são definidos dinamicamente:

```prolog
:- dynamic sujo/1, robo/1, objetivo/1.

sujo(ponto(0, 0)).
...
bloqueado(ponto(6, 3)).
```

### Estado Inicial do Robô e Destino

```prolog
robo(ponto(3, 3)).
destino(ponto(7, 3)).
```

### Funções de Movimentação

As funções `cima`, `baixo`, `esquerda`, e `direita` definem como o robô pode se mover:

```prolog
cima(ponto(X, Y), ponto(X, Z)) :- ...
baixo(ponto(X, Y), ponto(X, Z)) :- ...
esquerda(ponto(X, Y), ponto(Z, Y)) :- ...
direita(ponto(X, Y), ponto(Z, Y)) :- ...
```

### Algoritmos de Busca

Este projeto implementa vários algoritmos de busca para encontrar o melhor caminho:

- **A* (A Estrela)**
- **Branch and Bound**
- **Hill Climb**
- **Best First**

### Execução

Para executar o robô de limpeza, utilize um dos algoritmos de busca definidos:

```prolog
?- limpar(Tabuleiro, Caminho, aEstrela).
```

### Funções Auxiliares

- **Distância de Manhattan:** Calcula a distância entre dois pontos.
- **Manipulação de Lista:** Funções para manipulação e ordenação de listas.
- **Heurísticas:** Diferentes métodos para decidir qual sujeira limpar primeiro.
- **Desenho da Sala:** Funções para visualizar o estado atual da sala.

## Avaliação de Algoritmos

Para avaliar a performance dos algoritmos, utilize o predicado `calcular_tempo_e_nos_expandidos/3`:

```prolog
calcular_tempo_e_nos_expandidos(limpar(Tabuleiro, Caminho, aEstrela), Tempo, NosExpandidos).
```

## Contribuições

Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou enviar pull requests.

