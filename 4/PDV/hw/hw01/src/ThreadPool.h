#pragma once

#include <cstddef>
#include <list>
#include <vector>
#include <thread>
#include <condition_variable>

template<typename JobT, typename WorkerT>
class ThreadPool {
private:
    /** Fronta uloh. */
    std::list<JobT> job_queue{};
    /** Vlakna konzumentu zpracovavajicich ulohy. */
    std::vector<std::thread> worker_threads{};
    /** Funkce, kterou maji konzumenti vykonavat. */
    WorkerT worker_fn;
    /** Mutex pro synchronizaci přístupu ke frontě úkolů */
    std::mutex m;
    /** Condition variable pro synchronizaci přístupu ke frontě úkolů */
    std::condition_variable cv;

public:
    ThreadPool(size_t thread_count, WorkerT worker) : worker_fn(worker) {
        for (size_t i = 0; i < thread_count; i++) {
            worker_threads.push_back(std::thread([this]() {
                worker_loop();
            }));
        }
    }

    void process(const JobT job) {
        {
            std::unique_lock<std::mutex> lock(m);
            job_queue.push_back(job);
        }
        cv.notify_one();
    }

    /** Tato metoda nam umozni volajici funkci v main.cpp pockat na vsechna spustena vlakna konzumentu. */
    void join() {
        // Proiterujeme pres vsechna vlakna a pockame nez skonci.
        for (auto& worker_thread : worker_threads) {
            worker_thread.join();
        }
    }

private:
    void worker_loop() {
        while (true) {
            JobT job;
            {
                std::unique_lock<std::mutex> lock(m);
                cv.wait(lock, [&]{return !job_queue.empty();});
                job = job_queue.front(); // Precteme prvni ulohu ve fronte
                job_queue.pop_front(); // A ulohu odstranime
            }
            if (!job) {
                break; // Pokud je "zpracovana" uloha 0, skoncime
            }
            // Na zaver zavolame "worker_fn" funkci, ktera ulohu vykona
            worker_fn(job);
        }
    }
};
