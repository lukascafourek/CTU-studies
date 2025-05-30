#include "../pdv_lib/pdv_lib.hpp"
#include <iostream>
#include <string>
#include "PDVCrypt.hpp"

int main() {
    // Nejprve nacteme sifrovaci tabulku ze souboru secret.dat
    std::ifstream secret_if("./secret.dat", std::ios::binary);
    if (!secret_if) {
        throw std::runtime_error("Could not open './secret.dat'. Are you in the right directory?");
    }
    PDVCrypt crypt(" ABCDEFGHIJKLMNOPQRSTUVWXYZ", secret_if);

    // Pote ze souboru encrypted.txt nacteme retezce ktere mame za ukol desifrovat.
    std::vector<std::pair<std::string, enc_params>> encrypted;
    std::ifstream encrypted_if("./encrypted.txt");
    if (!encrypted_if) {
        throw std::runtime_error("Could not open './encrypted.txt'.");
    }
    enc_params params;
    while (encrypted_if >> params.p1 >> params.p2 >> params.start >> params.steps) {
        std::string s;
        getline(encrypted_if, s);
        getline(encrypted_if, s);
        encrypted.emplace_back(s.substr(1, s.length() - 2), params);
    }

    const int size = static_cast<int>(encrypted.size());

    pdv::benchmark("Decryption", [&] {
        // Nasledujici for smycka zpracovava jednotlive retezce a desifruje je zavolanim metody
        //  `decrypt` tridy PDVCrypt. Jelikoz desifrovani jednoho retezce nezavisi na desifrovani
        //  jineho, lze tuto smycku dobre zparalelizovat.
        for (int i = 0; i < size; i++) {
            auto& enc = encrypted[i];
            crypt.decrypt(enc.first, enc.second);
        }
    });

    for (auto& enc : encrypted) {
        std::cout << enc.first << "\n";
    }

    return 0;
}
