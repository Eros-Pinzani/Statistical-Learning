# 🐉 Le Cronache dell'Overfitting: Lo Spin-off - La Caccia al Mimic del Leakage

Benvenuti, viandanti solitari e paladini dell'onestà metodologica. La *Compagnia dell'Errore Quadratico Medio* si è temporaneamente divisa: mentre Lorenzo affronta i labirinti dell'Irrapresentabilità in R, il vostro Dungeon Master ha dovuto imbracciare lo scudo per scendere nel livello più infido del dungeon di **Statistical Learning 2026**.

Questo repository contiene il diario di bordo e gli incantesimi (script) usati per sconfiggere il mostro più subdolo dell'intera baronia del Machine Learning: **Il Data Leakage da Pre-Selezione**. 

---

## 👑 L'Eroe Solitario: Eros Pinzani
**Classe:** IL CAPO (Sottoclasse: *Inquisitore della Validazione*)  
**Abilità Speciale:** *Epurazione dell'Illusione.* Spezza i falsi miraggi di AUC elevate scoprendo dove i dati di validation hanno inquinato il training.  
**Background:** Colui che detiene il controllo del Master Repository ha notato una terribile anomalia: modelli alimentati a puro rumore che dichiaravano performance divine. Capito l'inganno, ha giurato di purificare la pipeline a colpi di Cross-Validation Annidata. La sua pazienza decade esponenzialmente se qualcuno esegue uno scaling o un filtraggio prima del loop di split.

---

## 📜 La Quest: Il Demone della Pre-Selezione

La missione consiste nel dimostrare come una procedura apparentemente innocente possa evocare un'illusione statistica micidiale, trasformando un dataset di **puro rumore casuale** (dove il vero segnale è totalmente assente, $Y \perp X$, e l'AUC reale dovrebbe essere rigidamente $0.5$) in un falso clone vincente.

### ⚔️ La Via Corrotta (Leaky Pipeline)
Il viandante calcola la correlazione tra tutte le $p$ variabili e il target $Y$ sull'**intero dataset**, seleziona le top-$k$ e solo dopo lancia la Cross-Validation. Questo fa penetrare informazioni del Validation Fold nel Training Fold. Il risultato? Un'AUC specchietto per le allodole che sfiora l'**0.81** su dati completamente inutili.

### 🛡️ La Via Ortodossa (Honest Pipeline)
Il Paladino confina la selezione delle feature rigorosamente **dentro ogni singolo fold**. Le informazioni non filtrano, il validation rimane vergine e l'AUC crolla giustamente a **0.5**, svelando la vera natura del rumore.

---

## 🔮 Il Grimorio Matematico: L'Equazione dell'Ottimismo

Il potere del demone non è infinito; risponde a precise leggi geometriche. Eros ha mappato analiticamente la crescita del falso ottimismo indotto dal caso su $p$ variabili indipendenti attraverso la formula asintotica:

$$\mathbb{E}[\max_{j=1}^{p}|r_{j}|] \approx \sqrt{\frac{2 \log p}{n}}$$

Più l'esercito dei mostri è grande ($p$ elevato, tipico dei regni della bioinformatica) e più le tue truppe sono esigue ($n$ piccolo), più l'illusione diventa potente e letale.

---

## 🗺️ Le Grandi Battaglie (I 4 Esperimenti)

### 1. La Valle di Ambroise e McLachlan ($n=200, p=500, k=20$)
Il primo scontro frontale. La pipeline corrotta fa credere al reame di aver trovato un drago d'oro (AUC = **0.808**). La pipeline di Eros spezza l'incantesimo riportando il valore a un misero **0.507**. Una mappa di calore (Heatmap) certifica che le zone più calde del leakage coincidono con i deserti ad alta dimensionalità.

### 2. La Consacrazione della Profezia
Mettendo alla prova l'equazione dell'ottimismo contro le simulazioni Monte Carlo, Eros dimostra che la formula teorica spiega ben oltre il **92%** della varianza empirica ($R^2 > 0.92$). La matematica degli antichi non mente.

### 3. La Palude Autoregressiva $AR(1)$
Il demone tenta di difendersi iniettando forte correlazione ($\rho$) tra le variabili per confondere i filtri. L'ottimismo diminuisce poiché lo spazio dimensionale effettivo si restringe, ma la pipeline corrotta genera comunque un'AUC distorta (~0.56). Il leakage è mitigato, ma non sconfitto.

### 4. Il Crollo dei Falsi Idoli: Filtro Univariato vs Ridge
In molti credono che usare un algoritmo sofisticato protegga dal male. Falso. Eros schiera una regressione Ridge penalizzata $L_2$ fuori dalla CV. Il risultato è identico: l'AUC barata sale a **0.788**. Solo l'invocazione della **Nested Cross-Validation** (Validazione Annidata a doppio loop) chiude definitivamente la backdoor architetturale, purificando il modello.

---

## 🛠️ Inventario e Magie (Tech Stack)

Per questa caccia all'errore, il DM ha scelto di officiare i riti nell'antico linguaggio **R**.

| Artefatto | Potere |
| :--- | :--- |
| **R Language** | La lingua sacra dei Monaci della Validazione. |
| **glmnet** | Per forgiare i filtri Ridge ed estrarre i coefficienti di punizione. |
| **MASS** | Per evocare le matrici di covarianza $AR(1)$ con la magia nera di `mvrnorm`. |
| **ggplot2 & patchwork** | Per tessere gli arazzi grafici che mostrano il crollo dell'illusione. |
| **dplyr & tidyr** | Per purificare e riordinare le tabelle dei risultati Monte Carlo. |

---

## ⚠️ Pergamena di Avviso per i Naviganti

> "Se osi toccare i parametri sacri nel pannello di controllo, ricordati: impostare `QUICK_RUN <- FALSE` scatenerà una tempesta di simulazioni che bloccherà il tuo calcolatore per svariati minuti. Configura il Master Seed con devozione per non alterare il destino."

```r
# Impostazioni di Iniziazione del Dungeon
QUICK_RUN   <- FALSE 
B            <- if (QUICK_RUN) 30 else 100 # Iterazioni del rito Monte Carlo
FOLDS        <- 10   # Portali di validazione esterni
FOLDS_INNER  <- 5    # Portali di validazione interni (Nested Ridge)
SEED         <- 2025 # Il Master Seed immutabile
```
**Sviluppato con rigore metodologico**, zero sconti al leakage e fiumi di inchiostro da: 
_Eros Pinzani - Il Dungeon Master del Gradiente_
