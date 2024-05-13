use aftersales;

DROP TABLE IF EXISTS t_login;
CREATE TABLE t_login
(
    `login_id`     varchar(32) NOT NULL default '' COMMENT '登录ID',
    `mobile`       varchar(32) NOT NULL default '' COMMENT '唯一手机号',
    `deleted`      bigint      not null default 0 COMMENT '逻辑删除',
    `created_time` timestamp   not null default now() COMMENT '注册时间',
    `updated_time` timestamp   not null default now() COMMENT '上次登录时间',
    `created_id`   varchar(32) not null default '' COMMENT '创建者',
    `updated_id`   varchar(32) not null default '' COMMENT '更新者',
    `source`       tinyint     not null default 0 COMMENT '来源',
    `app_id`       varchar(64) not null default '' COMMENT '三方登录唯一标志',
    `login_type`   tinyint     not null default 0 COMMENT '登录类型;1:客户, 2:员工',
    PRIMARY KEY (login_id)
) COMMENT = '登录表';

CREATE INDEX union_idx_third_login ON t_login (source, app_id);
# mobile逻辑删除唯一索引
CREATE UNIQUE INDEX union_idx_logic_mobile ON t_login (mobile, deleted);

DROP TABLE IF EXISTS t_client_info;
CREATE TABLE t_client_info
(
    `login_id`     varchar(32)  NOT NULL default '' COMMENT '登录ID',
    `deleted`      bigint       not null default 0 COMMENT '逻辑删除',
    `created_time` timestamp    not null default now() COMMENT '注册时间',
    `updated_time` timestamp    not null default now() COMMENT '更新时间',
    `created_id`   varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`   varchar(32)  not null default '' COMMENT '更新者',
    `avatar`       varchar(512) not null default '' COMMENT '用户头像',
    `nickname`     varchar(64)  not null default '' COMMENT '用户昵称',
    `email`        varchar(512) not null default '' COMMENT '邮箱',
    PRIMARY KEY (login_id)
) COMMENT = '客户信息表';

DROP TABLE IF EXISTS t_employee_info;
CREATE TABLE t_employee_info
(
    `login_id`                varchar(32)      NOT NULL default '' COMMENT '登录ID',
    `deleted`                 bigint           not null default 0 COMMENT '逻辑删除',
    `created_time`            timestamp        not null default now() COMMENT '创建时间',
    `updated_time`            timestamp        not null default now() COMMENT '更新时间',
    `created_id`              varchar(32)      not null default '' COMMENT '创建者',
    `updated_id`              varchar(32)      not null default '' COMMENT '更新者',
    `employee_role`           tinyint          not null default 0 COMMENT '员工角色',
    `real_name`               varchar(64)      not null default '' COMMENT '员工姓名',
    `entry_time`              timestamp        not null default now() COMMENT '入职时间',
    `email`                   varchar(512)     not null default '' COMMENT '邮箱',
    `employee_desc`           text COMMENT '员工个人简介',
    `employee_pic_file_id`    varchar(32)      not null default '' COMMENT '员工照片文件ID',
    `work_no`                 int(10) zerofill not null default 0 auto_increment COMMENT '员工唯一工号',
    `direct_leader_mobile_id` varchar(32)      not null default '' COMMENT '直属上级',
    PRIMARY KEY (login_id)
) COMMENT = '员工信息表';


CREATE INDEX idx_real_name ON t_employee_info (real_name);
CREATE INDEX idx_employee_role ON t_employee_info (employee_role);

DROP TABLE IF EXISTS t_order;
CREATE TABLE t_order
(
    `order_id`            varchar(32)   NOT NULL default '' COMMENT '工单ID',
    `created_time`        timestamp     not null DEFAULT now() COMMENT '创建时间',
    `updated_time`        timestamp     not null DEFAULT now() COMMENT '更新时间',
    `created_id`          varchar(32)   not null default '' COMMENT '创建者',
    `updated_id`          varchar(32)   not null default '' COMMENT '更新者',
    `deleted`             bigint        not null default 0 COMMENT '逻辑删除',
    `sku_id`              varchar(32)   not null default '' COMMENT '最小销售单元ID(商品)',
    `fapiao_id`           varchar(32)   not null default '' COMMENT '发票ID',
    `sn`                  varchar(64)   not null default '' COMMENT '商品序列号',
    `order_type`          tinyint       not null default 0 COMMENT '工单类型;1:到店;2寄修',
    `client_fault_desc`   varchar(512)  not null default '' COMMENT '客户故障描述',
    `engineer_notice`     text COMMENT '工程师备注',
    `engineer_fault_desc` text COMMENT '工程师故障描述',
    `order_status`        tinyint       not null default 0 COMMENT '工单状态',
    `material_fee`        decimal(8, 2) not null default 0 COMMENT '物料费;用户支付物料费,默认所有物料总价*1.2',
    `manual_fee`          decimal(8, 2) not null default 0 COMMENT '工程师手工费;工程师手工费,默认50',
    `engineer_mobile_id`  varchar(32)   not null default '' COMMENT '受理工程师ID',
    `client_mobile_id`    varchar(32)   not null default '' COMMENT '工单创建客户ID',
    `center_id`           varchar(32)   not null default '' COMMENT '服务中心ID',
    `arrvial_time`        timestamp     not null default now() COMMENT '到店时间/取件时间',
    `transferring`        bool          not null default false COMMENT '是否流转中',
    `transfer_num`        int           not null default 0 COMMENT '流转次数',
    PRIMARY KEY (order_id)
) COMMENT = '工单';


CREATE INDEX idx_sn ON t_order (sn);
CREATE INDEX idx_fapiao_id ON t_order (fapiao_id);
CREATE INDEX idx_sku_id ON t_order (sku_id);
CREATE INDEX idx_client_mobile ON t_order (client_mobile_id);
CREATE INDEX idx_engineer_id ON t_order (engineer_mobile_id);
CREATE INDEX idx_center_id ON t_order (center_id);

DROP TABLE IF EXISTS t_order_upload;
CREATE TABLE t_order_upload
(
    `created_time`  timestamp   not null DEFAULT now() COMMENT '创建时间',
    `updated_time`  timestamp   not null DEFAULT now() COMMENT '更新时间',
    `created_id`    varchar(32) not null default '' COMMENT '创建者',
    `updated_id`    varchar(32) not null default '' COMMENT '更新者',
    `deleted`       bigint      not null default 0 COMMENT '逻辑删除',
    `file_id`       varchar(32) NOT NULL default '' COMMENT '对应文件ID',
    `uploader_type` tinyint     not null default 0 COMMENT '上传者类型;1:客户上传;2:工程师上传',
    `order_id`      varchar(32) not null default '' COMMENT '所属订单ID',
    `file_type`     tinyint     not null default 0 COMMENT '文件类型;1:图片;2:视频',
    PRIMARY KEY (file_id)
) COMMENT = '工单的附属文件';


CREATE INDEX idx_order_id ON t_order_upload (order_id);

DROP TABLE IF EXISTS t_order_status_log;
CREATE TABLE t_order_status_log
(
    `created_time`  timestamp   not null DEFAULT now() COMMENT '创建时间',
    `updated_time`  timestamp   not null DEFAULT now() COMMENT '更新时间',
    `created_id`    varchar(32) not null default '' COMMENT '创建者',
    `updated_id`    varchar(32) not null default '' COMMENT '更新者',
    `deleted`       bigint      not null default 0 COMMENT '逻辑删除',
    `log_id`        varchar(32) NOT NULL default '' COMMENT '日志ID',
    `order_id`      varchar(32) not null default '' COMMENT '工单ID',
    `order_status`  tinyint     not null default '' COMMENT '当前工单状态',
    `status_detail` text COMMENT '状态详情(JSON);每个状态对应不同的JAVABEAN信息',
    PRIMARY KEY (log_id)
) COMMENT = '工单状态日志';


CREATE INDEX union_idx_order ON t_order_status_log (order_id, order_status);

DROP TABLE IF EXISTS t_middle_order_material;
CREATE TABLE t_middle_order_material
(
    `created_time`    timestamp   not null DEFAULT now() COMMENT '创建时间',
    `updated_time`    timestamp   not null DEFAULT now() COMMENT '更新时间',
    `created_id`      varchar(32) not null default '' COMMENT '创建者',
    `updated_id`      varchar(32) not null default '' COMMENT '更新者',
    `deleted`         bigint      not null default 0 COMMENT '逻辑删除',
    `id`              varchar(32) NOT NULL default '' COMMENT '中间表ID',
    `order_id`        varchar(32) not null default '' COMMENT '工单ID',
    `material_id`     varchar(32) not null default '' COMMENT '物料ID',
    `material_amount` decimal(8, 6)        default 0 COMMENT '物料数量',
    PRIMARY KEY (id)
) COMMENT = '工单物料中间表';


CREATE INDEX union_idx_order_material ON t_middle_order_material (order_id, material_id);

DROP TABLE IF EXISTS t_fapiao;
CREATE TABLE t_fapiao
(
    `created_time` timestamp   not null DEFAULT now() COMMENT '创建时间',
    `updated_time` timestamp   not null DEFAULT now() COMMENT '更新时间',
    `created_id`   varchar(32) not null default '' COMMENT '创建者',
    `updated_id`   varchar(32) not null default '' COMMENT '更新者',
    `deleted`      bigint      not null default 0 COMMENT '逻辑删除',
    `fapiao_id`    varchar(32) NOT NULL default '' COMMENT '发票ID',
    `fapiao_no`    varchar(32) NOT NULL default '' COMMENT '发票号码',
    `fapiao_code`  varchar(32) NOT NULL default '' COMMENT '发票代码',
    `fapiao_info`  text COMMENT '发票信息(JSON)',
    `fapiao_time`  timestamp   not null default now() COMMENT '开票时间',
    PRIMARY KEY (fapiao_id)
) COMMENT = '发票信息';
# 发票号码唯一索引
CREATE UNIQUE INDEX union_idx_logic_fapiao_no ON t_fapiao (fapiao_no, deleted);

DROP TABLE IF EXISTS t_spu;
CREATE TABLE t_spu
(
    `created_time`      timestamp    not null DEFAULT now() COMMENT '创建时间',
    `updated_time`      timestamp    not null DEFAULT now() COMMENT '更新时间',
    `created_id`        varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`        varchar(32)  not null default '' COMMENT '更新者',
    `deleted`           bigint       not null default 0 COMMENT '逻辑删除',
    `spu_id`            varchar(32)  NOT NULL default '' COMMENT '商品ID',
    `category_id`       int          not null default 0 COMMENT '所属分类ID',
    `released_time`     timestamp    not null default now() COMMENT '商品发布日期',
    `spu_name`          varchar(256) not null default '' COMMENT '唯一商品名称',
    `spu_cover_file_id` varchar(32)  not null default '' COMMENT '商品封面图片ID',
    `spu_desc`          text COMMENT '商品富文本描述',
    `visible`           bool         not null default false COMMENT '是否对客户可见',
    PRIMARY KEY (spu_id)
) COMMENT = '商品';


CREATE INDEX idx_category_id ON t_spu (category_id);
# 商品名唯一索引
CREATE UNIQUE INDEX idx_spu_name ON t_spu (spu_name, deleted);

DROP TABLE IF EXISTS t_spu_category;
CREATE TABLE t_spu_category
(
    `created_time`       timestamp    not null DEFAULT now() COMMENT '创建时间',
    `updated_time`       timestamp    not null DEFAULT now() COMMENT '更新时间',
    `created_id`         varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`         varchar(32)  not null default '' COMMENT '更新者',
    `deleted`            bigint       not null default 0 COMMENT '逻辑删除',
    `visible`            bool         not null default false COMMENT '是否对用户可见',
    `category_id`        INT AUTO_INCREMENT COMMENT '分类编号',
    `parent_category_id` int(4)       not null default 0 COMMENT '父级分类编号',
    `category_name`      varchar(256) not null default '' COMMENT '分类名称',
    `category_level`     int(4)       not null default 0 COMMENT '分类级别',
    PRIMARY KEY (category_id)
) COMMENT = '商品分类';


CREATE INDEX idx_parent_category_id ON t_spu_category (parent_category_id);
CREATE INDEX idx_category_level ON t_spu_category (category_level);
# 分类名称逻辑删除唯一索引
CREATE UNIQUE INDEX union_idx_logic_category_name ON t_spu_category (category_name, deleted);

DROP TABLE IF EXISTS t_sku;
CREATE TABLE t_sku
(
    `created_time`      timestamp    not null DEFAULT now() COMMENT '创建时间',
    `updated_time`      timestamp    not null DEFAULT now() COMMENT '更新时间',
    `created_id`        varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`        varchar(32)  not null default '' COMMENT '更新者',
    `deleted`           bigint       not null default 0 COMMENT '逻辑删除',
    `sku_id`            varchar(32)  NOT NULL default '' COMMENT 'sku编号',
    `spu_id`            varchar(32)  not null default '' COMMENT '所属spu编号',
    `sku_cover_file_id` varchar(32)  not null default '' COMMENT 'sku封面展示图片ID',
    `sku_display_name`  varchar(256) not null default '' COMMENT 'sku唯一展示名称',
    `visible`           bool         not null default false COMMENT '该sku是否对用户可见',
    PRIMARY KEY (sku_id)
) COMMENT = '商品销售单元';


CREATE INDEX idx_spu_id ON t_sku (spu_id);
CREATE INDEX idx_visible ON t_sku (visible);
# sku逻辑删除唯一索引
CREATE UNIQUE INDEX union_idx_logic_sku_display_name ON t_sku (sku_display_name, deleted);

DROP TABLE IF EXISTS t_sku_attr;
CREATE TABLE t_sku_attr
(
    `created_time` timestamp            DEFAULT now() COMMENT '创建时间',
    `updated_time` timestamp            DEFAULT now() COMMENT '更新时间',
    `created_id`   varchar(32) not null default '' COMMENT '创建者',
    `updated_id`   varchar(32) not null default '' COMMENT '更新者',
    `deleted`      bigint      not null default 0 COMMENT '逻辑删除',
    `visible`      bool COMMENT '是否对用户可见',
    `attr_id`      varchar(32) NOT NULL COMMENT '属性ID',
    `sku_id`       varchar(32) COMMENT '所属skuID',
    `attr_name`    varchar(256) COMMENT '属性名称',
    `attr_value`   varchar(256) COMMENT '属性值',
    PRIMARY KEY (attr_id)
) COMMENT = 'sku属性';


CREATE INDEX idx_sku_id ON t_sku_attr (sku_id);

DROP TABLE IF EXISTS t_file;
CREATE TABLE t_file
(
    `created_time` timestamp             DEFAULT now() COMMENT '创建时间',
    `updated_time` timestamp             DEFAULT now() COMMENT '更新时间',
    `created_id`   varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`   varchar(32)  not null default '' COMMENT '更新者',
    `deleted`      bigint       not null default 0 COMMENT '逻辑删除',
    `file_id`      varchar(32)  NOT NULL default '' COMMENT '对象存储ID',
    `access_key`   varchar(512) not null default '' COMMENT '对象访问路径',
    `uploader_id`  varchar(32)  not null default '' COMMENT '上传者',
    PRIMARY KEY (file_id)
) COMMENT = '对象存储';

DROP TABLE IF EXISTS t_material;
CREATE TABLE t_material
(
    `created_time`           timestamp      not null DEFAULT now() COMMENT '创建时间',
    `updated_time`           timestamp      not null DEFAULT now() COMMENT '更新时间',
    `created_id`             varchar(32)    not null default '' COMMENT '创建者',
    `updated_id`             varchar(32)    not null default '' COMMENT '更新者',
    `deleted`                bigint         not null default 0 COMMENT '逻辑删除',
    `material_id`            varchar(32)    NOT NULL default '' COMMENT '物料ID',
    `material_name`          varchar(256)   not null default '' COMMENT '物料名称',
    `material_desc`          text COMMENT '物料富文本描述',
    `material_cover_file_id` varchar(32)    not null default '' COMMENT '物料封面描述图片',
    `unit`                   varchar(64)    not null default '' COMMENT '计量单位',
    `stock`                  decimal(10, 4) not null default 0 COMMENT '物料剩余库存',
    `cost`                   decimal(8, 2)  not null default 0 COMMENT '物料成本',
    `price`                  decimal(8, 2)  not null default 0 COMMENT '销售价格',
    `alert_num`              decimal(10, 4) not null default 0 COMMENT '库存告警阈值',
    PRIMARY KEY (material_id)
) COMMENT = '物料';

#
create unique index union_idx_logic_material_name ON t_material (material_name, deleted);


DROP TABLE IF EXISTS t_material_log;
CREATE TABLE t_material_log
(
    `created_time` timestamp      not null DEFAULT now() COMMENT '创建时间',
    `updated_time` timestamp      not null DEFAULT now() COMMENT '更新时间',
    `created_id`   varchar(32)    not null default '' COMMENT '创建者',
    `updated_id`   varchar(32)    not null default '' COMMENT '更新者',
    `deleted`      bigint         not null default 0 COMMENT '逻辑删除',
    `log_id`       varchar(32)    NOT NULL default '' COMMENT '日志ID',
    `material_id`  varchar(32)    not null default '' COMMENT '物料ID',
    `operator_id`  varchar(32)    not null default '' COMMENT '操作者ID',
    `action`       tinyint        not null default 0 COMMENT '日志事件;1:入库;2:出库',
    `delta`        decimal(10, 4) not null default 0 COMMENT '变动量',
    `log_detail`   text COMMENT '事件详情;因...入库等',
    PRIMARY KEY (log_id)
) COMMENT = '物料日志';


CREATE INDEX idx_material_id ON t_material_log (material_id);
CREATE INDEX idx_operator_id ON t_material_log (operator_id);

DROP TABLE IF EXISTS t_pay_order;
CREATE TABLE t_pay_order
(
    `created_time` timestamp     not null DEFAULT now() COMMENT '创建时间',
    `updated_time` timestamp     not null DEFAULT now() COMMENT '更新时间',
    `created_id`   varchar(32)   not null default '' COMMENT '创建者',
    `updated_id`   varchar(32)   not null default '' COMMENT '更新者',
    `deleted`      bigint        not null default 0 COMMENT '逻辑删除',
    `pay_order_id` varchar(32)   NOT NULL default '' COMMENT '工单支付ID',
    `order_id`     varchar(32)   not null default '' COMMENT '工单ID',
    `pay_method`   tinyint       not null default 0 COMMENT '支付方式',
    `pay_detail`   text COMMENT '支付记录详情(JSON)',
    `amount`       decimal(8, 2) not null default 0 COMMENT '订单金额',
    `pay_status`   tinyint       not null default 0 COMMENT '支付状态',
    PRIMARY KEY (pay_order_id)
) COMMENT = '工单支付记录';


CREATE INDEX idx_order_id ON t_pay_order (order_id);

DROP TABLE IF EXISTS t_api;
CREATE TABLE t_api
(
    `created_time` timestamp    not null DEFAULT now() COMMENT '创建时间',
    `updated_time` timestamp    not null DEFAULT now() COMMENT '更新时间',
    `created_id`   varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`   varchar(32)  not null default '' COMMENT '更新者',
    `deleted`      bigint       not null default 0 COMMENT '逻辑删除',
    `api_id`       INT AUTO_INCREMENT COMMENT 'APIID',
    `uri`          varchar(512) not null default '' COMMENT 'API访问URI',
    `method`       tinyint      not null default 0 COMMENT 'API访问方法',
    `api_comment`  varchar(256) not null default '' COMMENT 'API描述',
    PRIMARY KEY (api_id)
) COMMENT = 'API列表';

DROP TABLE IF EXISTS t_permission;
CREATE TABLE t_permission
(
    `created_time`    timestamp   not null DEFAULT now() COMMENT '创建时间',
    `updated_time`    timestamp   not null DEFAULT now() COMMENT '更新时间',
    `created_id`      varchar(32) not null default '' COMMENT '创建者',
    `updated_id`      varchar(32) not null default '' COMMENT '更新者',
    `deleted`         bigint      not null default 0 COMMENT '逻辑删除',
    `permission_id`   INT AUTO_INCREMENT COMMENT '权限ID',
    `permission_name` varchar(64) not null default '' COMMENT '权限名称',
    `permission_key`  varchar(64) not null default '' COMMENT '权限唯一key值',
    PRIMARY KEY (permission_id)
) COMMENT = '权限';

#
create unique index union_idx_logic_permission_key on t_permission (permission_key, deleted);

DROP TABLE IF EXISTS t_middle_permission_api;
CREATE TABLE t_middle_permission_api
(
    `created_time`  timestamp   not null DEFAULT now() COMMENT '创建时间',
    `updated_time`  timestamp   not null DEFAULT now() COMMENT '更新时间',
    `created_id`    varchar(32) not null default '' COMMENT '创建者',
    `updated_id`    varchar(32) not null default '' COMMENT '更新者',
    `deleted`       bigint      not null default 0 COMMENT '逻辑删除',
    `id`            INT AUTO_INCREMENT COMMENT '主键',
    `permission_id` int         not null default 0 COMMENT '权限ID',
    `api_id`        int         not null default 0 COMMENT 'APIID',
    PRIMARY KEY (id)
) COMMENT = '权限具有API中间表';

DROP TABLE IF EXISTS t_middle_login_permission;
CREATE TABLE t_middle_login_permission
(
    `created_time`  timestamp   not null DEFAULT now() COMMENT '创建时间',
    `updated_time`  timestamp   not null DEFAULT now() COMMENT '更新时间',
    `created_id`    varchar(32) not null default '' COMMENT '创建者',
    `updated_id`    varchar(32) not null default '' COMMENT '更新者',
    `deleted`       bigint      not null default 0 COMMENT '逻辑删除',
    `id`            varchar(32) NOT NULL default '' COMMENT '主键',
    `mobile_id`     varchar(32) not null default '' COMMENT '用户ID',
    `permission_id` int         not null default 0 COMMENT '权限ID',
    PRIMARY KEY (id)
) COMMENT = '用户具有权限中间表';


CREATE INDEX idx_mobile_id ON t_middle_login_permission (mobile_id);
CREATE INDEX idx_permission_id ON t_middle_login_permission (permission_id);

DROP TABLE IF EXISTS t_sms_log;
CREATE TABLE t_sms_log
(
    `created_time` timestamp    not null DEFAULT now() COMMENT '创建时间',
    `updated_time` timestamp    not null DEFAULT now() COMMENT '更新时间',
    `created_id`   varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`   varchar(32)  not null default '' COMMENT '更新者',
    `deleted`      bigint       not null default 0 COMMENT '逻辑删除',
    `log_id`       varchar(32)  not null default '' COMMENT '日志ID',
    `sms_type`     tinyint      not null default 0 COMMENT '推送类型',
    `mobile`       varchar(32)  not null default '' COMMENT '目标手机号',
    `result`       tinyint      not null default 0 COMMENT '推送结果',
    `response`     varchar(256) not null default '' COMMENT '响应结果',
    `detail`       varchar(256) not null default '' COMMENT '推送详情'
) COMMENT = '短信推送日志';


CREATE INDEX idx_mobile ON t_sms_log (mobile);

DROP TABLE IF EXISTS t_address;
CREATE TABLE t_address
(
    `created_time`   timestamp    not null DEFAULT now() COMMENT '创建时间',
    `updated_time`   timestamp    not null DEFAULT now() COMMENT '更新时间',
    `created_id`     varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`     varchar(32)  not null default '' COMMENT '更新者',
    `deleted`        bigint       not null default 0 COMMENT '逻辑删除',
    `address_id`     varchar(32)  NOT NULL default '' COMMENT '地址ID',
    `region`         varchar(256) not null default '' COMMENT '省市区',
    `address_detail` varchar(512) not null default '' COMMENT '详细地址',
    `login_id`       varchar(32)  not null default '' COMMENT '客户ID',
    `mobile`         varchar(32)  not null default '' COMMENT '联系方式',
    `defaulted`      bool         not null default false COMMENT '是否默认地址',
    `receiver`       varchar(64)  not null default '' COMMENT '收货人姓名',
    PRIMARY KEY (address_id)
) COMMENT = '客户联系地址';


CREATE INDEX idx_receiver ON t_address (receiver);
CREATE INDEX idx_mobile_id ON t_address (login_id);

DROP TABLE IF EXISTS t_client_service_center;
CREATE TABLE t_client_service_center
(
    `created_time`   timestamp    not null DEFAULT now() COMMENT '创建时间',
    `updated_time`   timestamp    not null DEFAULT now() COMMENT '更新时间',
    `created_id`     varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`     varchar(32)  not null default '' COMMENT '更新者',
    `deleted`        bigint       not null default 0 COMMENT '逻辑删除',
    `center_id`      varchar(32)  NOT NULL default '' COMMENT '地址ID',
    `region`         varchar(256) not null default '' COMMENT '省市区',
    `address_detail` varchar(512) not null default '' COMMENT '详细地址',
    `center_desc`    text COMMENT '中心详情',
    `mobile`         varchar(32)  not null default '' COMMENT '联系方式',
    `center_name`    varchar(64)  not null default '' COMMENT '服务中心名称',
    PRIMARY KEY (center_id)
) COMMENT = '客户服务中心';

DROP TABLE IF EXISTS t_rating;
CREATE TABLE t_rating
(
    `created_time` timestamp    not null DEFAULT now() COMMENT '创建时间',
    `updated_time` timestamp    not null DEFAULT now() COMMENT '更新时间',
    `created_id`   varchar(32)  not null default '' COMMENT '创建者',
    `updated_id`   varchar(32)  not null default '' COMMENT '更新者',
    `deleted`      bigint       not null default 0 COMMENT '逻辑删除',
    `rating_id`    varchar(32)  NOT NULL default '' COMMENT '评价ID',
    `order_id`     varchar(32)  not null default '' COMMENT '工单ID',
    `rating`       int          not null default 0 COMMENT '评分',
    `rating_type`  tinyint      not null default 0 COMMENT '评价类型',
    `content`      varchar(512) not null default '' COMMENT '评价内容(JSON:图片ID数组和文字)',
    PRIMARY KEY (rating_id)
) COMMENT = '工单评价';

