#include "2integrate.hpp"
#include <omp.h>
#include <algorithm>


// Nasledujici funkce odhaduji urcity integral funkce 'integrand'. Dolni mez integralu odpovida promenne
// 'a', horni mez pak ziskame jako a + step_size*step_count. Integral odhadujeme pomoci obdelnikove metody
// za pouziti 'step_count' stejne sirokych obdelniku.


double integrate_sequential(Integrand integrand, double a, double step_size, size_t step_count) {
    // Vypocet urciteho integralu se da chapat jako vypocet plochy pod krivkou funkce. Tuto plochu nebudeme
    // pocitat exaktne, ale odhadneme ji pomoci mnoziny obdelniku (a jejich obsahu). Kazdy z techto obdelniku
    // ma svuj levy okraj na pozici a + step_size*i a pravy okraj na pozici a + step_size*(i+1). Pro vypocet
    // vysky obdelniku existuje mnoho pristupu. My budeme uvazovat ten nejsnazsi a to sice pouzijeme hodnotu
    // integrovane funkce "uprostred" intervalu (tedy na pozici a + step_size*i + step_size/2 ).

    // Obsahy obdelniku scitame do promenne 'acc'
    double acc = 0.0;

    for (size_t i = 0; i < step_count; i++) {
        // Nyni prochazime jednotlive obdelniky a pricitame jejich obsah do promenne 'acc'.
        // Nejprve si vypocteme "prostredek" intervalu [a + step_size*i, a + step_size*(i+1):
        double cx = a + (double)(2 * i + 1) * step_size / 2.0;

        // Nyni pricteme obsah obdelniku. Jeho sirka odpovida promenne 'step_size'. Vysku odhadneme
        // hodnotou funkce uprostred intervalu (tj., hodnotou integrand(cx)).
        acc += integrand(cx) * step_size;
    }

    // Nakonec nascitany obsah vratime jako odhad urciteho integralu
    return acc;
}

double integrate_omp_critical(Integrand integrand, double a, double step_size, size_t step_count) {
    // Nyni vypocet integralu z funkce 'integrate_sequential' zparalelizujeme za pouziti direktiv OpenMP
    // #pragma omp parallel (slouzi ke spusteni tymu vlaken) a #pragma omp critical (ktera slouzi k vytvoreni
    // kriticke sekce, kterou v jeden cas muze vykonavat pouze jedno vlakno).
    // Obsahy obdelniku opet budeme scitat do promenne 'acc'.

    double acc = 0.0;

    #pragma omp parallel
    {
        // Nyni jsme spustili tym vlaken, kdy kazde vlakno vykonavat kod uvnitr tohoto bloku. Podobne jako
        // v pripade mapovani z prvniho a druheho cviceni chceme, abychom kazdy obdelnik zpracovali prave jednou
        // (jinak bychom jeho obsah zapocitali dvakrat, coz by vedlo k chybnemu odhadu urciteho integralu).
        //
        // Toho lze dosahnout napriklad tak, ze kazde vlakno dostane prirazeny svuj rozsah obdelniku (promenne
        // 'i') obdobne jako ve funkci 'map_manual_ranges' z minuleho cviceni. Narozdil od reseni z minuleho
        // cviceni, v OpenMP nemuzeme jednoduse vlaknu predat rozsah, ktery ma pocitat. Misto toho si vlakno
        // musi tento rozsah dopocitat na zaklade znalosti, kolik vlaken celkem bezi (promenna 'threads') a
        // o kolikate vlakno (v ramci tymu 'threads' vlaken) se jedna.
        int threads = omp_get_num_threads();
        int thread_id = omp_get_thread_num();

        // Nejprve si zjistime, kolik prvku pripada na kazde vlakno.
        size_t chunk_size = 1 + step_count / threads;

        // Vlakna s nizsim ID zpracovavaji prvky [0 .. chunk_size*thread_id - 1]. Na nas tedy pripadaji prvky
        // [ chunk_size*thread_id .. chunk_size*thread_id + chunk_size - 1]. Vsimete si minimalizace,
        // ktera nam zajisti, ze se nikdy nebudeme pokouset zpracovavat obdelniky, ktere nemame.
        size_t begin = chunk_size * thread_id;
        size_t end = std::min(chunk_size * (thread_id + 1), step_count);

        // Nyni uz zname svuj rozsah a muzeme zacit zpracovavat obdelniky z tohoto rozsahu. Tento kod uz je
        // podobny sekvencni verzi s jednim rozdilem....
        for (size_t i = begin; i < end; i++) {
            double cx = a + (double)(2 * i + 1) * step_size / 2.0;
            double rectangle = integrand(cx) * step_size;

            // Jelikoz vice vlaken pristupuje ke stejne promenne soucasne, musime tento pristup synchronizovat.
            // Zde tuto synchronizaci provadime vytvorenim kriticke sekce.
            #pragma omp critical
            {
                // Jelikoz tuto kritickou sekci bude provadet pouze jedno vlakno, mame zajisteno, ze zmena
                // hodnoty promenne 'acc' probehne tak, jak ma.
                acc += rectangle;
            }

            // V tomto pripade vstupujeme do kriticke sekce pokazde, kdyz chceme pricist obsah kazdeho
            // obdelniku. Kritickou sekci proto budeme vykonavat hodnekrat, coz muze mit (a bude mit)
            // negativni dopad na vykon aplikace. Jak bychom se tomuto mohli vyhnout?
        }
    }

    return acc;
}

double
integrate_omp_atomic(Integrand integrand, double a, double step_size, size_t step_count) {
    // Vstup do kriticke sekce je velmi drahy. V mnoha pripadech (zejmena kdyz dochazi ke kolizim) je
    // potreba asistence jadra operacniho systemu, cemuz bychom se radi vyhnuli. Pro jednoduche operace
    // nad jednou promennou si ale muzeme vystacit s jednodussim typem zamku - atomickou operaci. Tento
    // zamek za nas vyresi procesor na hardwarove urovni a je ve vetsine pripadu rychlejsi.

    double acc = 0.0;

    #pragma omp parallel
    {
        int threads = omp_get_num_threads();
        int thread_id = omp_get_thread_num();
        size_t chunk_size = 1 + step_count / threads;

        size_t begin = chunk_size * thread_id;
        size_t end = std::min(chunk_size * (thread_id + 1), step_count);

        for (size_t i = begin; i < end; i++) {
            double cx = a + (double)(2 * i + 1) * step_size / 2.0;
            double rectangle = integrand(cx) * step_size;

            // Pricteni hodnoty do promenne je prave jednou z operaci, ktere procesor dokaze provest
            // atomicky. Abychom OpenMP rekli, ze po procesoru tento atomicky pristup vyzadujeme,
            // pouzijeme direktivu #pragma omp atomic.
            #pragma omp atomic
            acc += rectangle;

            // Obdobne jako ve funkci 'integrate_omp_critical', i zde budeme provadet zbytecne mnoho
            // atomickych operaci. Slo by tuto atomickou operaci nejak presunout za for cyklus, aby
            // ji kazde vlakno provedlo pouze jednou?
        }
    }

    return acc;
}

double integrate_omp_reduction(Integrand integrand, double a, double step_size, size_t step_count) {
    double acc = 0.0;

    // OpenMP poskytuje nastroj, ktery synchronizaci vyresi za nas. Direktiva 'reduction(+:acc)' zajisti,
    // ze se castecne vysledky, ktere jednotliva vlakna napocitaji do promenne 'acc', po dokonceni vypoctu
    // sectou.
    #pragma omp parallel reduction(+:acc)
    {
        int threads = omp_get_num_threads();
        int thread_id = omp_get_thread_num();
        size_t chunk_size = 1 + step_count / threads;

        size_t begin = chunk_size * thread_id;
        size_t end = std::min(chunk_size * (thread_id + 1), step_count);

        for (size_t i = begin; i < end; i++) {
            double cx = a + (double)(2 * i + 1) * step_size / 2.0;
            acc += integrand(cx) * step_size;
        }
    }

    return acc;
}

double integrate_omp_for_static(Integrand integrand, double a, double step_size, size_t step_count) {
    // To, co jsme naimplementovali ve funkci 'integrate_omp_reduction', lze udelat v OpenMP jednoduseji.
    // Misto toho, abychom si rozsahy [begin, end) pro jednotliva vlakna pocitali rucne, muzeme rict OpenMP,
    // at je napocita za nas. Jedine, co k tomu potrebujeme je pouzit direktivu `#pragma omp for` pred for
    // smyckou, kterou chceme paralelizovat.

    double acc = 0.0;

    // V nasledujicim kodu kombinujeme dve direktivy soucasne. Direktivu `#pragma omp parallel`, ktera zajisti
    // spusteni vlaken, a direktivu `#pragma omp for`, ktera rozdeli for smycku mezi spustena vlakna. Stejne
    // jako v predchozim pripade, musime pouzit `reduction(+:acc)`, abychom vyresili pristup ke sdilene promenne.
    // V tomto pripade vyuzivame staticky scheduling, `schedule(static)`, ktery praci mezi jednotliva vlakna
    // rozdeli zhruba stejne, jako jsme to udelali rucne ve funkci 'integrate_omp_reduction'.
    #pragma omp parallel for schedule(static) reduction(+:acc)
    for (ptrdiff_t i = 0; i < (ptrdiff_t)step_count; i++) {
        double cx = a + (double)(2 * i + 1) * step_size / 2.0;
        acc += integrand(cx) * step_size;
    }

    return acc;
}

double integrate_omp_for_dynamic(Integrand integrand, double a, double step_size, size_t step_count) {
    // Abychom vyuzili vypocetni zdroje efektivne, je potreba praci mezi jednotlive procesory rozdelit
    // pokud mozno rovnomerne. V opacnem pripade nektera vlakna skonci drive nez ostatni, a nektera jadra
    // tak budou nevyuzita. Staticky scheduling priradi stejny pocet uloh jednotlivym vlaknum, a bohuzel
    // nam tak negarantuje rovnomerne rozdeleni prace. V pripade, ze ulohy netrvaji stejne dlouho a "budeme
    // mit smulu", se nam muze stat, ze vsechny casove narocne ulohy pripadnou jednomu vlaknu. To pak bude
    // potrebovat vice casu nez ostatni.

    double acc = 0.0;

    // Jednim z moznych reseni je pouziti dynamickeho schedulingu, schedule(dynamic). Ve chvili, kdy
    // vlakno dokonci pridelenou praci, si zazada o dalsi (pokud je k dispozici).
    #pragma omp parallel for schedule(dynamic) reduction(+:acc)
    for (ptrdiff_t i = 0; i < (ptrdiff_t)step_count; i++) {
        double cx = a + (double)(2 * i + 1) * step_size / 2.0;
        acc += integrand(cx) * step_size;
    }

    return acc;
}
