table! {
    broker_points (uid) {
        uid -> Varchar,
        id -> Integer,
        created_at -> Nullable<Bigint>,
        updated_at -> Nullable<Bigint>,
        status -> Nullable<Varchar>,
        name -> Varchar,
        avatar -> Nullable<Varchar>,
        commission_pay_point_uid -> Nullable<Varchar>,
        customer_source -> Varchar,
        customer_source_display -> Nullable<Varchar>,
        partner_uid -> Varchar,
        car_booking_commission -> Nullable<Double>,
        car_airport_booking_commission -> Nullable<Double>,
        assignee -> Nullable<Varchar>,
        contract_start_at -> Nullable<Varchar>,
        contract_start_at_unix -> Nullable<Bigint>,
        contract_end_at -> Nullable<Varchar>,
        contract_end_at_unix -> Nullable<Bigint>,
    }
}

table! {
    broker_services (uid) {
        uid -> Varchar,
        created_at -> Nullable<Bigint>,
        updated_at -> Nullable<Bigint>,
        status -> Nullable<Varchar>,
        broker_uid -> Nullable<Varchar>,
        service_name -> Nullable<Varchar>,
        address -> Nullable<Varchar>,
        lat -> Nullable<Double>,
        lng -> Nullable<Double>,
        open_hour -> Integer,
        close_hour -> Integer,
        open_minute -> Nullable<Integer>,
        close_minute -> Nullable<Integer>,
        commission_value -> Nullable<Double>,
        commission_unit -> Nullable<Integer>,
        about -> Nullable<Text>,
        rating -> Nullable<Decimal>,
        visited_count -> Nullable<Integer>,
        avatar -> Nullable<Text>,
        background_image -> Nullable<Text>,
        open_time -> Nullable<Bigint>,
        close_time -> Nullable<Bigint>,
        online_member_cnt -> Nullable<Integer>,
        auto_accept_reservation -> Nullable<Varchar>,
        visible -> Nullable<Varchar>,
        categories -> Nullable<Tinytext>,
    }
}

allow_tables_to_appear_in_same_query!(
    broker_points,
    broker_services,
);
