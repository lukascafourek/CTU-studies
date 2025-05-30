#include <thread>
#include <mutex>
#include <iostream>
#include <condition_variable>
#include <cstdlib>
#include <chrono>

using namespace std::literals::chrono_literals;


static bool value = false;
static bool running = true;

std::mutex m;

// Abychom mohli pouzivat podminkove promenne, musime si vytvorit
// promennou, ktera je sdilena vsemi vlakny, ktere ji vyuzivaji.
std::condition_variable condition_variable;


// Prvni vlakno ve smycce nahodne nastavuje hodnotu promenne 'value'
// Tuto hodnotu pak kontroluje druhe vlakno, vykonavajici 'logger'
void setter() {
	for(int i = 0 ; i < 100 ; i++) {
		{
			std::unique_lock<std::mutex> lock(m);
			value = (rand() % 2) == 0;
			std::cout << "Setting value = " << value << std::endl;

			// Ve chvili, kdy zmenim hodnotu promenne, ktera ovlivnuje
            // pravdivostni hodnotu podminky, mel bych notifikovat
            // cekajici vlakna. Ty si pak zkontroluji, zda je podminka,
            // na kterou cekaji splnena - a pripadne pokracuji dale ve
            // vykonavani sveho kodu, v pripade, ze doslo ke splneni.
			condition_variable.notify_all();

			// V nasem pripade neni zadny rozdil mezi cv.notify_one()
            // a cv.notify_all() funguji stejne, protoze mame pouze
            // jedno cekajici vlakno.
		}

		std::this_thread::sleep_for(100ms);
	}

	// Na zaver zmenime hodnotu jeste jednou a nastavime priznak
	// 'running' na false. Tim donutime vlakno vykonavajici 'logger'
	// skoncit.
	running = false;

	// na konci musime probudit getter, aby program skoncils
	condition_variable.notify_all();
}

// Druhe vlakno reaguje na zmenu v hodnote promenne 'value'
// Doplnte do vlakna podminkovou promennou tak, aby vlakno 
// nemuselo aktivne cekat na zmenu promenne (busy waiting)
void logger() {
	bool last_value = false;
	while(running) {
		std::unique_lock<std::mutex> lock(m);

		// Misto abychom pouze zkontrolovali podminku a pokracovali dale,
        // vykonavani kodu vlakna "zastavime" do doby, nez bude podminka
        // splnena.
        // Pomoci metody 'wait' uvolnime zamek 'lock' (jine vlakno si
        // proto muze zamknout mutex 'm' a prenastavit promennou 'value').
        // Zaroven ve chvili, kdy dochazi ke kontrole podminky dojde ke
        // znovu-zamknuti locku, a proto se pri kontrole podminky (a dalsim
        // behu kodu) nachazime v kriticke sekci.
		condition_variable.wait(lock, [&]{return !running || last_value != value;});

		// setter mohl skoncit v dobe cekani na zmenu
		if(running){
			// Ted uz vime, ze je podminka splnena - to zajistila condition_variable
			std::cout << "Value changed to " << value << std::endl;
			last_value = value;
		}
	}
}

int main() {
	std::thread logger_thread(logger);
	std::thread setter_thread(setter);
	setter_thread.join();
    logger_thread.join();

	return 0;
}