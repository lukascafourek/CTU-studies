#pragma once

#include <cstdint>
#include <atomic>

class bst_tree {
public:
    // Trida node reprezentuje uzel binarniho vyhledavaciho stromu. Definici teto tridy si
    // muzete upravit, zachovejte ale prosim clenske promenne left, right (pointery, ktere
    // mohou byt atomicke i neatomicke) a clenskou promennou data.
    class node {
    public:
        std::atomic<node*> left{nullptr}; // Ukazatel na koren leveho podstromu
        std::atomic<node*> right{nullptr}; // Ukazatel na koren praveho podstromu

        // Pro pripomenuti: V binarnim vyhledavacim strome jsou uzly s nizsi hodnotou v levem
        // podstromu a uzly s vyssi hodnotou v pravem podstromu.

        int64_t data; // Hodnota aktualniho uzlu

        // Konstruktor, ktery nastavi hodnotu aktualniho uzlu
        explicit node(int64_t data) : data(data) {}
    };

    // Ukazatel na koren stromu
    std::atomic<node*> root{nullptr};

    // Destruktor stromu, ktery uvolni alokovanou pamet
    ~bst_tree();

    // Pomocná rekurzivní funkce pro vložení uzlu do stromu
    void insert_recursive(std::atomic<node*>& current, int64_t data);

    // Metoda pro vkladani prvku do binarniho vyhledavaciho stromu, kterou budete implementovat
    void insert(int64_t data);
};
