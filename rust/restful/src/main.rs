use actix_web::{HttpServer, App, middleware, web, HttpRequest, Responder};
use restful::Payment;
use mysql::Pool;

#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    std::env::set_var("RUST_LOG", "actix_web=info");
    env_logger::init();

    HttpServer::new(|| {
        App::new()
            .data(restful::create_pool())
            .wrap(middleware::Logger::default())
            .service(web::resource("/index.html").to(|| async { "Hello world!" }))
            .service(web::resource("/").to(index))
            .route("/greet/{name}", web::get().to(greet))
            .route("/payments", web::post().to(create_payments))
            .route("/payments", web::get().to(list_payments))
    })
        .bind("127.0.0.1:4000")?
        .run()
        .await
}

async fn greet(req: HttpRequest) -> impl Responder {
    let name = req.match_info().get("name").unwrap_or("world");
    format!("Hello, {}!", &name)
}

async fn index<'a>(_: HttpRequest) -> &'a str {
    "Hello world!"
}

async fn create_payments<'a>(_: HttpRequest, db: web::Data<Pool>) -> &'a str {
    restful::create_payments(db.get_ref());
    "Payments created"
}

async fn list_payments<'a>(_: HttpRequest, db: web::Data<Pool>) -> impl Responder {
    let payments: Vec<Payment> = restful::get_payments(db.get_ref());
    web::Json(payments)
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::dev::Service;
    use actix_web::{http, test, web, App, Error};

    #[actix_rt::test]
    async fn test_index() -> std::io::Result<()> {
        let app = App::new().route("/", web::get().to(index));
        let mut app = test::init_service(app).await;

        let req = test::TestRequest::get().uri("/").to_request();
        let resp = app.call(req).await.unwrap();

        assert_eq!(resp.status(), http::StatusCode::OK);

        let resp_body = match resp.response().body().as_ref() {
            Some(actix_web::body::Body::Bytes(bytes)) => bytes,
            _ => panic!("Response error")
        };
        assert_eq!(resp_body, r##"Hello world!"##);

        Ok(())
    }
}