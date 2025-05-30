#  cviceni6

Cvičení 6 je zaměřeno na použití návrhových vzorů State a Strategy. Úkolem bude implementovat řízení sekvence semaforů
s ohledem na denní dobu. Výsledkem bude model ulice, na které se nacházejí za sebou umístění semafory. Pro jednoduchost
budeme uvažovat stejnou vzdálenost mezi semafory. Každý semafor bude mít cyklus červená - oranžová - zelená - oranžová - červená.
Pro každou barvu je určena doba, po kterou má svítit - červená a zelená 10 časových jednotek, oranžová 2 časové jednotky.
Kromě toho je možné semafor vypnout, nebude svítit žádné světlo na semaforu. Sekvence semaforů bude řízena tak, aby vznikala
"zelená vlna" podle denní doby, dopoledne jedním směrem, odpoledne opačným směrem.

Implementace ve cvičení 6 bude v následujících krocích:

1) Implementace semaforu a přepínání jeho stavů s použitím návrhového vzoru State
Příprava třídy TrafficLight a přípava stavů Stop, Prepare, Go, Attention.

2) Implementace ulice se semafory a přípava logiky pro postupné přepínání světel semaforů řídící se algoritmem podle denní doby.
Implementace strategií řízení semaforů MorningStrategy a EveningStrategy podle denní doby. Implementace modelu ulice Street,
který bude obsahovat seznam za sebou stojících semaforů. Implementace volání logiky zvolené strategie.

