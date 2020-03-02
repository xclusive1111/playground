#[derive(Queryable)]
pub struct BrokerPoint {
    pub uid: String,
    pub id: i32,
    pub created_at: Option<i64>,
    pub updated_at: Option<i64>,
    pub status: Option<String>,
    pub name: String,
    pub avatar: Option<String>,
    pub commission_pay_point_uid: Option<String>,
    pub customer_source: String,
    pub customer_source_display: Option<String>,
    pub partner_uid: String,
    pub car_booking_commission: Option<f64>,
    pub car_airport_booking_commission: Option<f64>,
    pub assignee: Option<String>,
    pub contract_start_at: Option<String>,
    pub contract_start_at_unix: Option<i64>,
    pub contract_end_at: Option<String>,
    pub contract_end_at_unix: Option<i64>,
}

#[derive(Queryable)]
pub struct BrokerService {
    pub uid: String,
    pub created_at: Option<i64>,
    pub updated_at: Option<i64>,
    pub status: Option<String>,
    pub broker_uid: Option<String>,
    pub service_name: Option<String>,
    pub address: Option<String>,
    pub lat: Option<f64>,
    pub lng: Option<f64>,
    pub open_hour: i32,
    pub close_hour: i32,
    pub open_minute: Option<i32>,
    pub close_minute: Option<i32>,
    pub commission_value: Option<f64>,
    pub commission_unit: Option<i32>,
    pub about: Option<String>,
    pub rating: Option<f64>,
    pub visited_count: Option<i32>,
    pub avatar: Option<String>,
    pub background_image: Option<String>,
    pub open_time: Option<i64>,
    pub close_time: Option<i64>,
    pub online_member_cnt: Option<i32>,
    pub auto_accept_reservation: Option<String>,
    pub visible: Option<String>,
    categories: Option<String>,
}

