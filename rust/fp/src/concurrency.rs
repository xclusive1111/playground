// Spawning thread
pub mod d {
    use std::{thread, process};
    use std::time::Duration;

    pub fn run() {
        let spawn = thread::spawn(|| {
            for i in 1..10 {
                println!("Spawn thread counting @ {}", i);
                thread::sleep(Duration::from_millis(10))
            }
        });

        for i in 1..5 {
            println!("Main thread counting @ {}", i);
            thread::sleep(Duration::from_millis(10))
        }

        // Call join() to wait for the spawn thread to finish.
        spawn.join().unwrap_or(());

        let s1 = String::from("Rust concurrency");
        let s2 = String::from("Fantastic");

        // By adding the `move` keyword before the closure, we force the closure
        // to take ownership of all values it's using, i.e `s1` & `s2`.
        let f = move || {
            println!("{} - {}", s1, s2);
        };

        // Calling f() still work because s1 & s2 haven't moved inside a spawn thread yet.
        f();

        let handle = thread::spawn(f);

        // s1 & s2 are moved, below code won't compile
        // println!("{} - {}", s1, s2);
        // f();
        handle.join();
    }
}

// Message passing
pub mod e {
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;

    pub fn run() {
        let (sender, receiver) = mpsc::channel();

        thread::spawn(move || {
            let val = String::from("Hi there");
            sender.send(val).unwrap();

            let val = String::from("How are you");
            sender.send(val).unwrap();

            // This thread takes ownership of the receiver and the communicating channel
            // will be closed after this thread completed.
        });

        // Calling recv() will block the main thread until there's message available
        // from the channel or the channel is closed. Below code will block forever because we have only sent
        // 2 messages to the channel, which are consumed above.
        let msg = receiver.recv().unwrap();
        println!("Got a message '{}'", msg);

        let msg = receiver.recv().unwrap();
        println!("Got a message '{}'", msg);

        // Below code will panic because we have only sent
        // 2 messages to the channel, which are already consumed above.
        // let msg = receiver.recv().unwrap();

        // Calling try_recv() doesn't block but will return a Result<T, E> immediately.
        for i in 1..3 {
            println!("Consuming {}", i);
            match receiver.try_recv() {
                Ok(s) => println!("Got a message '{}'", s),
                Err(e) => println!("Error: {:?}", e),
            }
        }

        let (sender, receiver) = mpsc::channel();

        thread::spawn(move || {
            let vals = vec![
                String::from("hi"),
                String::from("from"),
                String::from("the"),
                String::from("thread"),
            ];

            for val in vals {
                sender.send(val).unwrap();
                thread::sleep(Duration::from_millis(300));
            }
        });

        // We treat receiver as an iterator. When the channel is closed, iteration will end.
        for msg in receiver {
            println!("Got: {}", msg)
        }

        // Create multiple senders by cloning.
        let (sender, receiver) = mpsc::channel();
        let sender1 = mpsc::Sender::clone(&sender);

        thread::spawn(move || {
            let vals = vec![
                String::from("hi"),
                String::from("from"),
                String::from("the"),
                String::from("thread"),
            ];

            for val in vals {
                sender1.send(val).unwrap();
                thread::sleep(Duration::from_millis(100));
            }
        });

        thread::spawn(move || {
            let vals = vec!["0", "1", "2", "3"];

            for val in vals {
                sender.send(val.to_string()).unwrap();
                thread::sleep(Duration::from_millis(100));
            }
        });

        for msg in receiver {
            println!("Got: {}", msg)
        }
    }
}

pub mod f {
    use std::sync::{Mutex, Arc};
    use std::thread;

    pub fn run() {
        let m = Mutex::new(5);
        {
          let mut num = m.lock().unwrap();
          *num = 6;
        }
        println!("m = {:?}", m);

        // Arc stands for Atomic reference count.
        let counter = Arc::new(Mutex::new(0));
        let mut handles = vec![];

        for _ in 0..10 {
            let counter = Arc::clone(&counter);
            let handle = thread::spawn(move || {
                let mut num = counter.lock().unwrap();
                *num += 1;
            });
            handles.push(handle);
        }

        for handle in handles {
            handle.join().unwrap();
        }

        println!("counter = {:?}", *counter.lock().unwrap());
    }
}
