#[macro_use]
extern crate diesel;

pub mod model;
pub mod schema;

use diesel::prelude::*;

use std::env;
use model::*;
use diesel::result::Error;
use diesel::r2d2::{ConnectionManager, Pool, PooledConnection};

fn main() {
    use schema::broker_points::dsl::*;
    let pool = create_pool();
    let conn = pool.get().unwrap();
    let results: Result<Vec<BrokerPoint>, Error> = broker_points
        .filter(name.like("%khach%"))
        .limit(5)
        .load::<BrokerPoint>(&conn);
    let results = results.unwrap();
    println!("Len = {}", results.len());
}

type MySqlPool = Pool<ConnectionManager<MysqlConnection>>;
type MySqlPooledConnection = PooledConnection<ConnectionManager<MysqlConnection>>;

fn create_pool() -> MySqlPool {
    dotenv::dotenv().ok();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let manager = ConnectionManager::<MysqlConnection>::new(database_url);
    Pool::builder()
        .max_size(2)
        .build(manager)
        .expect("Failed to init connection pool")
}