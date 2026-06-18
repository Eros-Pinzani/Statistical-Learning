# 🐉 The Chronicles of OLS: La Maledizione della Variabile Omessa

Bentornati, viandanti della Statistica e ostinati esploratori del Machine Learning. Dopo aver domato l'inganno della Random Forest, la nostra eroica (e instabile) compagnia deve affrontare una nuova, invisibile minaccia che serpeggia nei sotterranei della regressione.

---

## 🛡️ La Compagnia dei Minimi Quadrati Ordinari

I nostri eroi sono tornati, armati di nuove reliquie e ancora più dipendenti dalla caffeina:

*   **👑 Eros Pinzani - *Il Dungeon Master***: Creatore dei mondi simulati (DGP). Fissa il fato di tutto l'universo con la potente formula magica `set.seed(123)` per garantire che gli eventi siano immutabili e riproducibili[cite: 1].
*   **🃏 Lorenzo Maria Pennelli - *Il Bardo delle Euristiche***: Cerca ancora di capire perché la sua pergamena stima uno zero quando il bersaglio dell'oracolo era chiaramente un uno. 
*   **⚔️ Francesco Faillace - *Il Guerriero del Tuning***: Colui che evoca l'incantesimo `n_sim <- 1000` iterazioni a mani nude, senza mai far crashare il server dell'accademia[cite: 1].

---

## 📜 La Nuova Quest: Caccia al Fantasma $x_2$

Questa volta il nemico non è un drago gigantesco o un dataset corrotto, ma un assassino invisibile e letale: il **Demone della Variabile Omessa**[cite: 1].

Il Dungeon Master ha forgiato il mondo (il *Data Generating Process*) unendo due entità: $x_1$, il nostro eroe della luce con un potere $\beta_1 = 1$, e $x_2$, un oscuro signore dei sotterranei con una forza distruttiva pari a $\beta_2 = -2$[cite: 1]. L'obiettivo della compagnia è semplice: scoprire la vera forza di $x_1$ scagliando l'incantesimo **OLS** (*Ordinary Least Squares*, Minimi Quadrati Ordinari)[cite: 1].

Ma attenzione, ci sono due diverse dimensioni in cui il party potrebbe imbattersi:

### 🪤 Scenario 1: La Palude della Covarianza
In questa regione infida e corrotta, $x_1$ e $x_2$ sono legati da un mistico patto di sangue ($\rho = 0.5$)[cite: 1].
* Se il party per pigrizia decide di invocare un modello che include solo $x_1$ (il *modello omesso*), scatta istantaneamente la **Maledizione del Bias Sistematico**[cite: 1].
* Poiché l'oscuro $x_2$ è segretamente correlato a $x_1$ e ha un effetto reale sul piano materiale ($\beta_2 = -2$), l'incantesimo OLS si confonde e viene irreparabilmente contaminato[cite: 1].
* La percezione della forza di $x_1$ viene trascinata nell'abisso: il modello deve farsi carico del potere oscuro di $x_2$, abbattendo la stima di $x_1$ da un glorioso $1$ a un misero $0$[cite: 1]. 

### 🌿 Scenario 2: Le Pianure dell'Indipendenza
In questa dimensione idilliaca, i due guerrieri non si sono mai incontrati e operano nell'ombra ($\rho = 0$)[cite: 1].
* Francesco, stanco di combattere, può tranquillamente dimenticarsi di includere $x_2$ nella formula magica del modello[cite: 1].
* Poiché non c'è alcun legame tra le variabili, l'omissione non contamina la stima di $x_1$, e l'incantesimo riesce miracolosamente a centrare il bersaglio vero ($\beta_1 = 1$)[cite: 1].
* Tuttavia, l'hubris ha sempre un costo: ignorare la variabile reale $x_2$ rende l'incantesimo di divinazione molto più instabile, aumentando l'errore casuale e rendendo i colpi del party meno precisi[cite: 1].

---

## 🔮 Il Rituale di Evocazione (Monte Carlo)

Lorenzo e Francesco, saggiamente, si rifiutano di fidarsi del fato di un singolo campione estrattivo. Decidono così di invocare l'antico e potente **Rituale di Monte Carlo**[cite: 1].
* Ripetono la genesi del mondo per ben 1000 volte di fila[cite: 1].
* Ad ogni giro generano un nuovo esercito di 1000 anime (osservazioni), estratte direttamente dai meandri di una *Normale Bivariata* usando la runa proibita `mvrnorm`[cite: 1].
* Tracciando gli istogrammi di queste battaglie epiche, la verità viene a galla[cite: 1].
* Le curve di densità non mentono: mentre il *Modello Completo* trionfa sempre e si concentra sul bersaglio a prescindere dallo scenario, il *Modello Omesso* nello Scenario 1 fallisce miseramente, deviando inesorabilmente sul baratro dello zero[cite: 1].

---

## 🛠️ Strumenti Magici (Stack Tecnologico)

| Artefatto | Potere |
| :--- | :--- |
| **R** | L'antico dialetto per tessere le trame del fato statistico[cite: 1]. |
| **L'Amuleto MASS** | Per evocare mostri e variabili direttamene da matrici di covarianza oscure[cite: 1]. |
| **lm()** | Lo spadone base a due mani per minimizzare i quadrati degli errori[cite: 1]. |
| **Grafici a Densità (col_dens_omit & col_dens_full)** | Pitture rupestri necessarie a convincere il Villaggio (il Professore) della bontà delle proprie gesta[cite: 1]. |

---

## ⚠️ Le Ultime Parole Famose

> "Ma sì dai, buttiamo nel modello solo la $x_1$, tanto questa misteriosa $x_2$ a cosa servirà mai?" 
> — *Ultime parole di un Data Scientist prima di essere vaporizzato dal Bias.*

**Regola d'oro del Dungeon:** Il Demone della Variabile Omessa può materializzarsi SOLO se la variabile ignorata possiede un reale potere di impatto ($\beta_2 \neq 0$) ED è telepaticamente connessa (correlata) con i tuoi alleati già in campo[cite: 1]. Se manca una delle due, sei salvo... ma potresti comunque colpire a vuoto per la troppa varianza[cite: 1]. 

**Sopravvissuti (a stento) alla simulazione:**
*Eros, Lorenzo & Francesco*
