-- Your SQL goes here
-- Your SQL goes here
CREATE TABLE `broker_points` (
  `uid` varchar(255) NOT NULL,
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `created_at` bigint(20) DEFAULT NULL,
  `updated_at` bigint(20) DEFAULT NULL,
  `status` varchar(50) DEFAULT NULL,
  `name` varchar(255) NOT NULL,
  `avatar` varchar(255) DEFAULT NULL,
  `commission_pay_point_uid` varchar(255) DEFAULT NULL,
  `customer_source` varchar(255) NOT NULL,
  `customer_source_display` varchar(255) DEFAULT NULL,
  `partner_uid` varchar(255) NOT NULL,
  `car_booking_commission` double DEFAULT NULL,
  `car_airport_booking_commission` double DEFAULT NULL,
  `assignee` varchar(255) DEFAULT NULL,
  `contract_start_at` varchar(255) DEFAULT NULL,
  `contract_start_at_unix` bigint(20) DEFAULT NULL,
  `contract_end_at` varchar(255) DEFAULT NULL,
  `contract_end_at_unix` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  UNIQUE KEY `id` (`id`),
  KEY `idx_broker_points_uid` (`uid`),
  FULLTEXT KEY `idx_broker_points_name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci

