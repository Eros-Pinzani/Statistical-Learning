# Homework 1 — Omitted Variable Bias: Simulazione Monte Carlo

## Obiettivo

Questo script analizza il problema della **variabile omessa** (Omitted Variable Bias, OVB) in un modello di regressione lineare. L'obiettivo è mostrare come la correlazione tra le variabili esplicative influenzi la distorsione della stima di β₁ quando una variabile rilevante viene esclusa dal modello.

---

## 2. Bias al variare di ρ e β₂ (Secondo Blocco)

Analisi sistematica del bias simulato su una griglia di correlazioni `ρ ∈ [-0.95, 0.95]` per quattro valori di `β₂ ∈ {-2, -1, 1, 2}`.

Per ogni coppia (ρ, β₂) si calcola:
```
Bias simulato = E[β̂₁_omitted] - β₁
```

**il grafico mostra:**

Quattro curve colorate, una per ciascun valore di β₂, con punti evidenziati in corrispondenza di ρ ∈ {-0.95, -0.5, -0.2, 0, 0.2, 0.5, 0.95}.

- **Andamento lineare in ρ**: all'aumentare della correlazione il bias cresce proporzionalmente.
- **Bias = 0 se e solo se ρ = 0**: l'assenza di correlazione annulla l'OVB indipendentemente da β₂.
- **Segno del bias** determinato dal prodotto sign(ρ) × sign(β₂):
  - β₂ > 0 e ρ > 0 → bias positivo (sovrastima di β₁)
  - β₂ < 0 e ρ > 0 → bias negativo (sottostima di β₁)
- **Ampiezza del bias** proporzionale a |β₂|: curve più lontane dall'asse orizzontale corrispondono a β₂ con valore assoluto maggiore.
- **Simmetria attorno a ρ = 0**, coerente con la formula teorica dell'OVB (Omitted Variable Bias) nel caso standardizzato (Var(x₁) = Var(x₂) = 1):
```
Bias(β̂₁) = β₂ · ρ
```

