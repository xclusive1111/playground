-- Your SQL goes here
-- Your SQL goes here
CREATE TABLE `broker_services` (
  `uid` varchar(255) NOT NULL,
  `created_at` bigint(20) DEFAULT NULL,
  `updated_at` bigint(20) DEFAULT NULL,
  `status` varchar(50) DEFAULT NULL,
  `broker_uid` varchar(255) DEFAULT NULL,
  `service_name` varchar(255) DEFAULT NULL,
  `address` varchar(255) DEFAULT NULL,
  `lat` double DEFAULT NULL,
  `lng` double DEFAULT NULL,
  `open_hour` int(11) NOT NULL,
  `close_hour` int(11) NOT NULL,
  `open_minute` int(11) DEFAULT '0',
  `close_minute` int(11) DEFAULT '0',
  `commission_value` double DEFAULT '0',
  `commission_unit` int(11) DEFAULT NULL,
  `about` text,
  `rating` decimal(2,1) DEFAULT '0.0',
  `visited_count` int(11) DEFAULT '0',
  `avatar` text,
  `background_image` text,
  `open_time` bigint(20) DEFAULT NULL,
  `close_time` bigint(20) DEFAULT NULL,
  `online_member_cnt` int(11) DEFAULT '0',
  `auto_accept_reservation` varchar(255) DEFAULT 'DISABLE',
  `geo_point` geometry NOT NULL /*!80003 SRID 4326 */,
  `visible` varchar(255) DEFAULT 'ENABLE',
  `categories` tinytext,
  PRIMARY KEY (`uid`),
  KEY `idx_broker_services_uid` (`uid`),
  KEY `idx_broker_services_broker_uid` (`broker_uid`),
  SPATIAL KEY `geo_point` (`geo_point`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

ALTER TABLE broker_services
MODIFY COLUMN geo_point GEOMETRY SRID 4326 NOT NULL;

