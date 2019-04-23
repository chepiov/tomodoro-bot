create extension if not exists "uuid-ossp";

create table user_log
(
    id         uuid default uuid_generate_v4()
        constraint user_log_pk
            primary key,
    chat_id    bigint                   not null,
    time       timestamp with time zone not null,
    descriptor varchar(20)              not null,
    log        varchar(100)             not null
);

comment on table user_log is 'user activity log';

