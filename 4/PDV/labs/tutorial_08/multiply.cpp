#include "multiply.h"

#include <string>
#include <vector>
#include <iostream>
#include "sparse.h"

sparse_vector multiply_sequential(const sparse_matrix &A, const sparse_vector &x) {
    sparse_vector result;                                 // Vystupni ridky vektor
    const std::vector<entry> & x_entries = x.entries();   // Nenulove prvky vektoru 'x'
    const auto x_size = static_cast<unsigned>(x_entries.size());         // Pocet nenulovych prvku vektoru 'x'

	const auto nz_rows = static_cast<unsigned>(A.size());   // Pocet nenulovych radku matice 'A'

    // Matici 'A' prochazime po radcich. Pro kazdy nenulovy radek matice 'row' provedeme
    // nasobeni 'row' * 'x'. Pokud je vysledek tohoto skalarniho soucinu nenulovy, vlozime
    // ho do vystupniho vektoru 'result'.
    for(unsigned int row_idx = 0 ; row_idx < nz_rows ; row_idx++) {
        const matrix_row & row = A[row_idx];                   // Nacteme si radek 'row_idx'-ty nenulovy radek 'A'
        const std::vector<entry> row_entries = row.entries();  // ... nacteme si jeho nenulove prvky
        const auto row_size = static_cast<unsigned>(row_entries.size());      // ... a jejich pocet

        // Nyni musime najit nenulove prvky, ktere se nachazi v obou vektorech 'row' a 'x'.
        // Jelikoz jsou tyto prvky serazene podle indexu, staci nam projit vektory 'row_entries'
        // a 'x_entries' pouze jednou.

        unsigned int x_i = 0;
        unsigned int row_i = 0;

        double acc = 0.0;
        while(x_i < x_size && row_i < row_size) {
            if(x_entries[x_i].index < row_entries[row_i].index) {
                // Pokud aktualni prvek v 'x' ma nizsi index nez aktualni prvek v 'row', muzeme ho
                // preskocit. (V 'row' uz budou pouze prvky s vyssim indexem nez, row_entries[row_i]
                // a tedy prvek s indexem x_entries[x_i] se v 'row' nenachazi.)
                x_i++;
            } else if(x_entries[x_i].index > row_entries[row_i].index) {
                // Obdobne pro opacny pripad:
                row_i++;
            } else {
                // Zajimava situace nastava jen tehdy, kdyz jsme narazili na index, ktery se nachazi
                // v obou vektorech 'x' a 'row'. V tu chvili prvky pronasobime a pricteme je.
                acc += x_entries[x_i++].value * row_entries[row_i++].value;
                // ... a soucasne se posuneme na dalsi nenulove prvky ve vektorech.
            }
        }

        // Pokud je vysledek nenula, pridej novy nenulovy prvek do vystupniho vektoru na pozici,
        // ktera odpovida aktualnimu indexu radku matice 'A'.
        if(std::abs(acc) != 0.0) result.set(row.index, acc);
    }

    return result;
}

sparse_vector merge(const sparse_vector & a, const sparse_vector & b);

sparse_vector multiply_parallel(const sparse_matrix &A, const sparse_vector &x) {

    // Pro implementaci paralelniho vypoctu A*x se vam pravdepodobne bude hodit
    // redukce, ktera "secte" ridke vektory (sparse_vector) vypoctene ruznymi
    // vlakny. My vam ji zde pro jednoduchost deklarujeme - vlastni metodu pro
    // zkombinovani vektoru ale budete muset napsat sami.
    #pragma omp declare reduction(merge : \
                                  sparse_vector : \
                                  omp_out = merge(omp_out, omp_in)) \
                        initializer(omp_priv = sparse_vector())

    sparse_vector result;
    const std::vector<entry>& x_entries = x.entries();
    const auto x_size = static_cast<unsigned>(x_entries.size());

	const auto nz_rows = static_cast<unsigned>(A.size());

    #pragma omp parallel for schedule(dynamic) reduction(merge : result)
    for(unsigned int row_idx = 0 ; row_idx < nz_rows ; row_idx++) {
        try {
            const matrix_row& row = A[row_idx];
            const std::vector<entry> row_entries = row.entries();
            const auto row_size = static_cast<unsigned>(row_entries.size());

            // Doimplementujte paralelni vypocet A*x. Muzete vychazet z metody
            // multiply_sequential(...).

         
        } catch(const std::string& s) {
            std::cerr << s << " (in multiply_parallel)" << std::endl;
        }
    }

    return result;
}

sparse_vector merge(const sparse_vector& a, const sparse_vector& b) {
    const std::vector<entry>& a_entries = a.entries();
    const std::vector<entry>& b_entries = b.entries();
    const auto a_size = static_cast<unsigned>(a_entries.size());
    const auto b_size = static_cast<unsigned>(b_entries.size());

    sparse_vector result;
    result.reserve(a_size + b_size);

    try {
        // Doplnte logiku "sliti" dvou serazenych poli prvku dvou vektoru 'a' a 'b'
        // (vektory 'a_entries' a 'b_entries'). Vysledne prvky vkladejte do vektoru
        // 'result' pomoci metody 'result.set(...)'. Muzete predpokladat, ze vektory
        // 'a' a 'b' neobsahuji prvky se stejnym indexem.

    } catch(std::string s) {
        std::cerr << s << " (in merge)" << std::endl;
    }

    return result;
}
