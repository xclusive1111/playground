#[macro_use]
extern crate mysql;

use mysql::{QueryResult, Pool};
use serde::Serialize;

#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct Payment {
    pub customer_id: i32,
    pub amount: i32,
    pub account_name: Option<String>,
}

pub fn create_pool() -> Pool {
    mysql::Pool::new_manual(1, 10, "mysql://root:123456@localhost:3306/test").unwrap()
}

pub fn create_payments(pool: &Pool) {
    pool.prep_exec("DROP TABLE payment", ()).unwrap();

    // Let's create payment table.
    // Unwrap just to make sure no error happened.
    pool.prep_exec(r"CREATE TABLE payment (
                         customer_id int not null,
                         amount int not null,
                         account_name text
                     )", ()).unwrap();

    let payments = vec![
        Payment { customer_id: 1, amount: 2, account_name: None },
        Payment { customer_id: 3, amount: 4, account_name: Some("foo".into()) },
        Payment { customer_id: 5, amount: 6, account_name: None },
        Payment { customer_id: 7, amount: 8, account_name: None },
        Payment { customer_id: 9, amount: 10, account_name: Some("bar".into()) },
    ];

    // Let's insert payments to the database
    // We will use into_iter() because we do not need to map Stmt to anything else.
    // Also we assume that no error happened in `prepare`.
    for mut stmt in pool.prepare(r"
                INSERT INTO payment (customer_id, amount, account_name)
                VALUES (:customer_id, :amount, :account_name)"
    ).into_iter() {
        for p in payments.iter() {
            stmt.execute(params! {
                "customer_id" => p.customer_id,
                "amount" => p.amount,
                "account_name" => &p.account_name,
            }).unwrap();
        }
    }
}

pub fn get_payments(pool: &Pool) -> Vec<Payment> {
    pool
        .prep_exec("SELECT * FROM payment", ())
        .map(extract_payment)
        .unwrap().unwrap()
}

fn extract_payment(result: QueryResult) -> Result<Vec<Payment>, Box<dyn std::error::Error>> {
    let mut payments = vec![];

    for row in result.into_iter() {
        match row {
            Ok(row) => {
                let (customer_id, amount, account_name) = mysql::from_row(row);
                payments.push(Payment { customer_id, amount, account_name })
            }
            Err(x) => return Err(Box::new(x))
        }
    }

    Ok(payments)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn for_fun() {
        assert_eq!(2, 2)
    }
}
