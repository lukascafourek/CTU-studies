#include "multiply.hpp"

/**
 * Merge two sparse vectors into a single one. Assumes that there are no indices with non-zero
 * entries in both vectors.
 */
[[maybe_unused]] sparse_vector merge(const sparse_vector& a, const sparse_vector& b) {
    const std::vector<entry>& ae = a.entries();
    const std::vector<entry>& be = b.entries();

    sparse_vector result{};
    result.reserve(ae.size() + be.size());

    size_t a_i = 0;
    size_t b_i = 0;
    while (a_i < ae.size() && b_i < be.size()) {
        if (ae[a_i].index < be[b_i].index) {
            // output from `a`
            result.add_entry(ae[a_i]);
            a_i++;
        } else {
            // output from `b`
            result.add_entry(be[b_i]);
            b_i++;
        }
    }

    // output the leftovers (only one of these for loops will be active)
    for (; a_i < ae.size(); a_i++) {
        result.add_entry(ae[a_i]);
    }
    for (; b_i < be.size(); b_i++) {
        result.add_entry(be[b_i]);
    }

    return result;
}

sparse_vector multiply_sequential(const sparse_matrix& A, const sparse_vector& x) {
    const std::vector<entry>& x_entries = x.entries(); // Nenulove prvky vektoru 'x'
    sparse_vector result; // Vystupni ridky vektor

    // Matici 'A' prochazime po radcich. Pro kazdy nenulovy radek matice 'row' provedeme
    // nasobeni 'row' * 'x'. Pokud je vysledek tohoto skalarniho soucinu nenulovy, vlozime
    // ho do vystupniho vektoru 'result'.
    for (size_t row_idx = 0; row_idx < A.size(); row_idx++) { // NOLINT(modernize-loop-convert)
        const matrix_row& row = A[row_idx]; // Nacteme si radek 'row_idx'-ty nenulovy radek 'A'
        const std::vector<entry>& row_entries = row.entries(); // ... nacteme si jeho nenulove prvky

        // Nyni musime najit nenulove prvky, ktere se nachazi v obou vektorech 'row' a 'x'.
        // Jelikoz jsou tyto prvky serazene podle indexu, staci nam projit vektory 'row_entries'
        // a 'x_entries' pouze jednou.

        size_t x_i = 0;
        size_t row_i = 0;
        double acc = 0.0;
        while (x_i < x_entries.size() && row_i < row_entries.size()) {
            if (x_entries[x_i].index < row_entries[row_i].index) {
                // Pokud aktualni prvek v 'x' ma nizsi index nez aktualni prvek v 'row', muzeme ho
                // preskocit. (V 'row' uz budou pouze prvky s vyssim indexem nez, row_entries[row_i]
                // a tedy prvek s indexem x_entries[x_i] se v 'row' nenachazi.)
                x_i++;
            } else if (x_entries[x_i].index > row_entries[row_i].index) {
                // Obdobne pro opacny pripad:
                row_i++;
            } else {
                // Zajimava situace nastava jen tehdy, kdyz jsme narazili na index, ktery se nachazi
                // v obou vektorech 'x' a 'row'. V tu chvili prvky pronasobime a pricteme je.
                acc += x_entries[x_i].value * row_entries[row_i].value;
                // ... a soucasne se posuneme na dalsi nenulove prvky ve vektorech.
                x_i++;
                row_i++;
            }
        }

        // Pokud je vysledek nenulovy, pridej novy nenulovy prvek do vystupniho vektoru na pozici,
        // ktera odpovida aktualnimu indexu radku matice 'A'.
        if (acc != 0.0) {
            result.add_entry({row.index, acc});
        }
    }

    return result;
}

sparse_vector multiply_parallel(const sparse_matrix& A, const sparse_vector& x) {
    // Pro implementaci paralelniho vypoctu A*x se vam pravdepodobne bude hodit
    // redukce, ktera "secte" ridke vektory (sparse_vector) vypoctene ruznymi
    // vlakny. My vam ji zde pro jednoduchost deklarujeme - vlastni metodu pro
    // zkombinovani vektoru ale budete muset napsat sami.
    #pragma omp declare reduction(merge : sparse_vector : omp_out = merge(omp_out, omp_in)) \
                        initializer(omp_priv = sparse_vector())

    const std::vector<entry>& x_entries = x.entries();
    sparse_vector result{};

    // `schedule(monotonic:dynamic)` is necessary, otherwise the OpenMP runtime is allowed
    //  to execute iterations out-of-order, even inside a single thread
    #pragma omp parallel for schedule(monotonic:dynamic) reduction(merge : result)
    for (size_t row_idx = 0; row_idx < A.size(); row_idx++) { // NOLINT(modernize-loop-convert)
        const matrix_row& row = A[row_idx];
        const std::vector<entry>& row_entries = row.entries();

        size_t x_i = 0;
        size_t row_i = 0;
        double acc = 0.0;
        while (x_i < x_entries.size() && row_i < row_entries.size()) {
            if (x_entries[x_i].index < row_entries[row_i].index) {
                x_i++;
            } else if (x_entries[x_i].index > row_entries[row_i].index) {
                row_i++;
            } else {
                acc += x_entries[x_i].value * row_entries[row_i].value;
                x_i++;
                row_i++;
            }
        }

        if (acc != 0.0) {
            result.add_entry({row.index, acc});
        }
    }

    return result;
}
