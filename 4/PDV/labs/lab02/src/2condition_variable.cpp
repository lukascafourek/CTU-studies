#include <thread>
#include <mutex>
#include <iostream>
#include <condition_variable>

using namespace std::chrono_literals;

static bool value = false;
static bool running = true;

static std::mutex m{};

// Prvni vlakno ve smycce periodicky nastavuje hodnotu promenne 'value'.
// Tuto hodnotu pak kontroluje druhe vlakno, vykonavajici 'logger'.
void setter() {
    for (size_t i = 0; i < 5; i++) {
        {
            std::unique_lock<std::mutex> lock{m};
            value = !value;
            std::cout << "Setting value = " << value << "\n";
        }
        std::this_thread::sleep_for(500ms);
    }
    std::unique_lock<std::mutex> lock{m};
    running = false;
}

// Druhe vlakno reaguje na zmenu v hodnote promenne 'value'.
// TODO: Doplnte do vlakna podminkovou promennou tak, aby nemuselo aktivne cekat na zmenu promenne (busy waiting)
void logger() {
    bool last_value = false;
    std::unique_lock<std::mutex> lock{m};
    while (running) {
        if (last_value != value) {
            std::cout << "Value changed to " << value << "\n";
            last_value = value;
        }

        lock.unlock();
        std::this_thread::sleep_for(10ms);
        lock.lock();
    }
}

int main() {
    std::thread logger_thread{logger};
    std::thread setter_thread{setter};

    setter_thread.join();
    logger_thread.join();

    return 0;
}
