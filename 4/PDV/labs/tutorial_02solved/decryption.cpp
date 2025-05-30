//
// Created by karel on 12.2.18.
//

#include <vector>
#include <thread>
#include <mutex>
#include <atomic>
#include <cmath>
#include "decryption.h"

using namespace std;

void decrypt_sequential(const PDVCrypt &crypt, std::vector<std::pair<std::string, enc_params>> &encrypted, unsigned int) {
    // V sekvencni verzi je pocet vlaken (numThreads) ignorovany
    const auto size = static_cast<unsigned long>(encrypted.size());
    for(unsigned long i = 0 ; i < size ; i++) {
        auto & enc = encrypted[i];
        crypt.decrypt(enc.first, enc.second);
    }
}

void decrypt_openmp(const PDVCrypt &crypt, std::vector<std::pair<std::string, enc_params>> &encrypted, unsigned int numThreads) {
    const long size = static_cast<long>(encrypted.size());

    #pragma omp parallel for num_threads(numThreads)
    for(long i = 0 ; i < size ; i++) {
        auto & enc = encrypted[i];
        crypt.decrypt(enc.first, enc.second);
    }
}

void decrypt_threads_1(const PDVCrypt &crypt, std::vector<std::pair<std::string, enc_params>> &encrypted, unsigned int numThreads) {
    const unsigned long size = static_cast<unsigned long>(encrypted.size());
    unsigned long i = 0;
    
    // process je "vnorena" funkce (lambda funkce) void process(), ktera ma pristup
    // ke vsem promennym
    auto process = [&]() {
        while(i < size) {
            auto & enc = encrypted[i];
            i++;
            crypt.decrypt(enc.first, enc.second);
        }
    };

    // Spustte 'numThreads' vlaken, ktera budou spolecne resit desifrovani retezcu!
    // Vlakna budou desifrovat za pouziti funkce 'process'
    
    // Reseni:

    std::vector<std::thread> threads; // Vytvorime si vektor, ve kterem si zapamatujeme vytvorena vlakna
    for(unsigned t = 0; t < numThreads; ++t) {
	    threads.emplace_back(process); // Vytvorim nove vlakno, ktere bude vykonavat process a
                                                   // vlozim ho do vektoru 'threads'

        // Pozor! Nasledujici zapis neni korektni:
        //   std::thread t(process);
        //   threads.push_back(t);
        // V C/C++ (pokud nerekneme jinak) dochazi k vytvoreni/prirazeni objektu
        // kopirovanim (viz. copy constructor, copy assignment). To znamena, ze
        // my se v tomto kodu snazime kopirovat vlakno 't' do vektoru 'threads' -
        // - ale operace zkopirovani vlakna neni definovana.
        //
        // Tento problem muzeme obejit tim, ze rekneme kompilatoru, ze chceme
        // vlakno 't' do vektoru 'threads' pouze presunout:
        //   std::thread t(process);
        //   threads.push_back(std::move(t));
        // V tu chvili ale uz nemuzeme puvodni promennou 't' pouzivat.
        
        
        
        // Zde by bylo chybou zavolat
        //   threads[t].join();
        // Tim bychom dosahli toho, ze spustime prvni vlakno a budeme na nej cekat
        // jeste pred tim, nez spustime vlakna zbyvajici.
    }

    // Proto musime cekat na vlakna az pote, co vsechna vlakna spustime:
    for(auto& thread: threads) {
	    thread.join();
    }
}


// Kod metody 'decrypt_threads_1' je nekorektni, protoze vice vlaken pristupuje
// nesynchronizovane ke stejne promenne 'i'. Muze se tak stat, ze vice vlaken
// pouzije stejnou hodnotu promenne 'i', coz ma za nasledek nekorektni vysledek
// desifrovani.
void decrypt_threads_2(const PDVCrypt &crypt, std::vector<std::pair<std::string, enc_params>> &encrypted, unsigned int numThreads) {
	const auto size = static_cast<unsigned long>(encrypted.size());
    unsigned long i = 0;

    // Prvnim pristupem, jak se tomuto problemu vyhnout je "zamknout" si promennou
    // 't' na nezbytne dlouhou dobu (tedy na dobu, co nacitam aktualni hodnotu
    // promenne a inkrementuji ji). To muzeme udelat pomoci mutexu:
    std::mutex mutex;
    

    auto process_lock_unlock = [&]() {

        // V podmince while cyklu uz pracuji s promennou 'i', mel bych si ji proto
        // zamknout uz zde. Jinak by se mi totiz mohlo stat, ze mezi kontrolou
        // hodnoty promenne i a jejim vyuzitim v tele cyklu dojde ke zmene jeji
        // hodnoty jinym vlaknem - coz muze mit za nasledek, ze v dobe kdy 'i'
        // vyuzivam uz tato podminka neplati.
        std::unique_lock<std::mutex> unique_lock(mutex);    // Zamykam mutex 'm'
															// K jeho odemceni dojde automaticky pote, co
															// promenna 'lock' zanikne (destruktorem objektu).

        while(i < size) {
			auto& enc = encrypted[i];
			i++;

            // Zde koncim praci s promennou 'i' a nasleduje proces desifrovani.
            // Aby mohlo vice vlaken desifrovat soucasne, musime zamek uvolnit.
            unique_lock.unlock();
            
            crypt.decrypt(enc.first, enc.second);

            // Nyni se vracime na zacatek cyklu a budeme znova kontrolovat
            // podminku pro promennou i. Je potreba si proto zamek opet
            // obnovit:
            unique_lock.lock();

            // Rucni zamykani a odemykani mutexu je ale nachylne k chybam (napriklad
            // se nam muze snadno stat, ze zapomeneme zamek znova zamknout). Bezpecnejsi
            // variantou proto muze byt ponechat spravu mutexu ciste na objektu typu
            // 'std::unique_lock', jak ilustruje nasledujici reseni:
        }
    };

    auto process = [&]() {
    	while(true) {
    		unsigned long my_i;

    		// Potrebujeme, aby v kriticke sekci bylo pouze nacteni aktualni hodnoty
    		// promenne i (tu si ulozime do promenne my_i) a jeji inkrementace. Proto
    		// vytvorime vnoreny blok (prostor platnosti promennych).
    		{
    			std::unique_lock<std::mutex> lock(mutex);  // Jelikoz chceme pracovat s promennou 'i'
    												   // tak potrebujeme vytvorit kritickou sekci

    			// Nyni muzeme provest operace s promennou 'i':
    			my_i = i;
    			i++;

    			// Vsimnete si, ze obe operace jsou v kriticke sekci. Nemuze se nam proto stat, ze
    			// by nejake vlakno vstoupilo mezi vyse uvedenou dvojici prikazu. Dalsi vlakno, ktere
    			// do kriticke sekce vstoupi, proto nacte jiz inkrementovanou hodnotu promenne 'i'.

    			// Zde promenna 'lock' zanika, a proto opoustime kritickou sekci.
    		}

    		// V promenne 'my_i' nyni mame unikatni index prvku ke zpracovani. Potrebujeme ale overit,
    		// ze tento prvek stale lezi v rozsahu pole:
    		if(my_i >= size) break;

    		// Nyni vime, ze je vse v poradku, a muzeme desifrovat prvek na pozici 'my_i'.
    		auto & enc = encrypted[my_i];
    		crypt.decrypt(enc.first, enc.second);
    	}
    };

    std::vector<std::thread> threads;
    for(unsigned t = 0; t < numThreads; ++t) {
	    threads.emplace_back(process);
    }

    for(auto& thread: threads) {
	    thread.join();
    }
}

void decrypt_threads_3(const PDVCrypt &crypt, std::vector<std::pair<std::string, enc_params>> &encrypted, unsigned int numThreads) {
    const auto size = static_cast<unsigned long>(encrypted.size());

    // Druhym zpusobem, jak synchronizovat pristup k promenne 'i' je pouziti
    // atomickych promennych. Ty nam zarucuji, ze nektere operace nad promennou
    // 'i' se vykonaji naraz (a zadne jine vlakno nas pri vykonavani operace
    // neprerusi).

    // Zmenime sdilenou promennou 'i' na atomickou:
    std::atomic<unsigned long> i {0};

    /* Nejprve chybne reseni:
     *   auto process = [&]() {
     *     while(i < size) {
     *       auto & enc = encrypted[i++];
     *       crypt.decrypt(enc.first, enc.second);
     *     }
     *   };
     * 
     * Ackoliv operace "nacteni predchozi hodnoty a inkrementace" (i++)
     * je atomicka. Dve po sobe jdouci operace, kontrola i < size a i++
     * ale atomicke nejsou. Mohlo by se nam tedy opet stat, ze nam jine
     * vlakno vstoupi mezi tyto dve operace (a promennou 'i' inkrementuje)
     * a hodnota promenne 'i' v dobe pristupu do pole bude >= size a
     * pristupovali bychom tak mimo vyhrazenou pamet.
     */
    
    // process je "vnorena" funkce (lambda funkce) void process(), ktera ma pristup
    // ke vsem promennym
    auto process = [&]() {
        while(i < size) {
            unsigned long thread_i = i++; // Zjistim si index, ktery mam zpracovavat
                                    // a soucasne 'i' inkrementuji. Zadne jine
                                    // vlakno proto nepouzije stejnou hodnotu
                                    // 'i'.

            // Nyni zkontroluji, ze plati currIdx < size. V opacnem pripade ukoncim vypocet:
            if(thread_i >= size) break;

            // Ted uz vim, ze mam v currIdx platny index a muzu ho pouzit:
            auto& enc = encrypted[thread_i];
            crypt.decrypt(enc.first, enc.second);
        }
    };

    std::vector<std::thread> threads;
    for(unsigned t = 0; t < numThreads; ++t) {
	    threads.emplace_back(process);
    }

    for(auto& thread: threads) {
	    thread.join();
    }
}

void decrypt_threads_4(const PDVCrypt &crypt, std::vector<std::pair<std::string, enc_params>> &encrypted, unsigned int numThreads) {
    const auto size = static_cast<unsigned long>(encrypted.size());
    vector<thread> threads;

    // Vypocitame si pocet prvku, ktere ma zpracovat jedno vlakno
    // Vsimnete si zaokrouhleni nahoru, abychom predesli situaci, kdy
    // posledni prvky v poli nebudou zpracovane.
    const auto batch_size = static_cast<unsigned long>(std::ceil(static_cast<double>(size) / numThreads));

    for(unsigned int t = 0 ; t < numThreads ; t++) {

        // Za pomoci 'chunkSize' zjistime zacatek a konec bloku aktualniho
        // vlakna s indexem 't':
        unsigned long begin = t * batch_size;
        unsigned long end = (t + 1) * batch_size;

        threads.emplace_back([&](unsigned long begin, unsigned long end) {
            // Kazde vlakno si pak zpracovava svuj blok dat [begin, end)
            for(unsigned int i = begin ; i < end ; i++) {
                auto & enc = encrypted[i];
                crypt.decrypt(enc.first, enc.second);
            }
        }, begin, end);
    }
    for(unsigned int t = 0 ; t < numThreads ; t++) {
        threads[t].join();
    }

}
