CREATE TABLE events (
    id serial8 PRIMARY KEY,
    source text NOT NULL,
    delivery text NULL,
    "type" text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    data jsonb NOT NULL
);

CREATE TABLE check_suites (
    id int8 PRIMARY KEY,
    repository_owner text NOT NULL,
    repository_name text NOT NULL,
    head_sha text NOT NULL,
    status text NOT NULL,
    conclusion text NULL
);

CREATE TABLE check_runs (
    id int8 PRIMARY KEY,
    check_suite__id int8 REFERENCES check_suites (id) ON DELETE cascade,
    name text NOT NULL,
    status text NOT NULL,
    conclusion text NULL,
    started_at timestamp with time zone NULL,
    completed_at timestamp with time zone NULL
);

CREATE TABLE rspec_summary (
    id serial8 PRIMARY KEY,
    examples integer NOT NULL,
    failed integer NOT NULL,
    pending integer NOT NULL,
    duration double precision NOT NULL
);

CREATE TABLE rspec_examples (
    id serial8 PRIMARY KEY,
    rspec_summary__id int8 REFERENCES rspec_summary (id) ON DELETE cascade,
    description text NOT NULL,
    full_description text NOT NULL,
    status text NOT NULL,
    file_path text NOT NULL,
    line_number integer NOT NULL,
    "exception" jsonb NOT NULL
);

