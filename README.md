---
papersize: a4paper
geometry: margin=35mm
fontfamily: mathptmx
fontsize: 12pt
numbersections: true
linestretch: 1.25
colorlinks: true
header-includes:
    - \usepackage{bm}
    - \usepackage{nicefrac}
    - \renewcommand\thesection{}
    - \renewcommand\thesubsection{\arabic{subsection}}
    - \setcounter{secnumdepth}{3} 
    - \widowpenalty=10000
    - \clubpenalty=10000 
...

This package computes estimates of years lost when someone dies and does so for each risk group available in the data. This facilitates computations of years lost under alternative assumptions about the selection mechanism of those who died from the rest of the population (surviving cohort).

Years lost are computed from the actuarial tables published by the Czech Statistical Office available at [https://www.czso.cz/csu/czso/umrtnostni_tabulky](https://www.czso.cz/csu/czso/umrtnostni_tabulky).

# Computing years lost in general

Denote $q_x$ the probability of a person dying at age $x$ conditional on being alive at $x-1$ from the actuarial tables. A person who died at the age $x-1$, had she not died at that age, would survive $y$ more years, dying at age $x - 1 + y$. Let $\bm{y} = (1,  \ldots, 104 - x -1)$ be the vector of potential years lost (104 is the maximum age of death in the actuarial tables, which determines the maximum possible years lost).
Let $\bm{d} = (q_x, d_2, \ldots, d_{104 - x - 1})$ be the vector probabilities of dying at ages $x$ to 104, where the individual $d$'s are computed as
\begin{align*}
d_y &= \frac{\hat{d_y}}{\sum{\hat{d_y}}}, \qquad \text{where} \\
\hat{d}_y &= q_{x - 1 + y}\prod_{i =0}^{y - 1}{(1 -q_{x + i})},
\end{align*}
where the product gives the probability of being alive at $x - 1 + y$. Note that the probability of a person dying at some point is equal to one but $\sum \hat{d_y}$ is not guaranteed to add up to one. To make sure $\sum d_y = 1$, we normalize $\hat{d_y}$'s by dividing by $\sum\hat{d_y}$. Her years lost due to dying at $x - 1$ are then
\begin{align*}
L = \bm{y}' \bm{d}.
\end{align*}

# Computing risk group-specific years lost 

Think of $\bm{y}$ as identifier of risk brackets from which a person who died at $x-1$ is drawn. Let risk-group $r$ be
\begin{align*}
g_r = \sum_1^r d_y,
\end{align*}
and let $\bm{z_r} = (1, \ldots, r)$, where $r \leq 104 - x -1$, and $\bm{d_r} = [(q_x, d_2, \ldots, d_r) / g_r]$
then the risk group-specific years lost are
\begin{align*}
L_r = \bm{z_r}' \bm{d_r}
\end{align*}

Thus, $L_1 = 1$ and $L_{104 - x -1} = L.$ We note that these two extremes have the interpretation of years lost of those "dying with Covid" and those "dying from Covid", respectively, while $L_r, r \in (2, \ldots,  104 - x - 2)$ are years lost of those between the two extremes. 

# Comparing $L$ and life expectancy

$L$ can be directly compared to the life expectancy in the actuarial tables (column "ex"). 

